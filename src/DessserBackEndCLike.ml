open Batteries
open Stdint

open Dessser
open DessserMiscTypes
open DessserTools
module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes
module U = DessserCompilationUnit

let debug = false

let pp = Printf.fprintf

exception Missing_dependencies of Set.String.t

let () =
  Printexc.register_printer (function
    | Missing_dependencies deps ->
        Some (
          Printf.sprintf2 "Missing dependencies: %a"
            (pretty_enum_print String.print) (Set.String.enum deps))
    | _ ->
        None)

type emitter =
  ?name:string -> P.t -> E.env -> E.t -> (string IO.output -> unit) ->
    string

(* Avoid modifying the name when it's valid: *)
let valid_identifier s =
  let s =
    String.map (function
      | '-' | '+' | '|' | '&' | '.' | '*' | '/'
      | '[' | ']' | '(' | ')' | '"' -> '_'
      | c -> c
    ) s in
  if s = "" then "v" else
  if s.[0] >= 'a' && s.[0] <= 'z' ||
     s.[0] >= 'A' && s.[0] <= 'Z' ||
     s.[0] = '_' then
    s
  else
    "v_"^ s

module type CONFIG =
sig
  val id : backend_id
  val valid_source_name : string -> string
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val preferred_comp_extension : link -> string
  val compile_cmd : ?dev_mode:bool -> ?extra_search_paths:string list -> ?optim:int -> link:link -> string -> string -> string

  val type_identifier : P.t -> T.typ -> string

  val type_identifier_mn : P.t -> T.mn -> string

  val print_binding :
    P.t -> T.mn -> string -> (string IO.output -> unit) -> string IO.output -> unit

  val print_binding_toplevel :
    (* The initial int is a sequence for that compilation unit: *)
    int -> emitter -> string -> P.t -> E.env -> E.t -> unit

  val print_identifier_declaration :
    (* The initial int is a sequence for that compilation unit: *)
    int -> string -> P.t -> E.env -> E.t -> unit

  val print_comment : 'b IO.output -> ('a, 'b IO.output, unit) format -> 'a

  val print : emitter -> ?name:string -> P.t -> E.env -> E.t -> string

  val source_intro : U.t -> P.t -> string
  val source_outro : U.t -> P.t -> string

  (* TODO: find a way to factorize the print function itself *)
end

module Make (C : CONFIG) : BACKEND =
struct
  let id = C.id
  let print_comment = C.print_comment
  let preferred_def_extension = C.preferred_def_extension
  let preferred_decl_extension = C.preferred_decl_extension
  let preferred_comp_extension = C.preferred_comp_extension
  let type_identifier = C.type_identifier
  let type_identifier_mn = C.type_identifier_mn

  let valid_source_name fname =
    let ext = Filename.extension fname
    and no_ext = Filename.remove_extension fname in
    let basename_no_ext = Filename.basename no_ext
    and dirname = Filename.dirname fname in
    dirname ^"/"^ C.valid_source_name basename_no_ext ^ ext

  let compile_cmd = C.compile_cmd

  (* Find references to identifiers. Used to order definitions, so does not
   * need to include external identifiers, as they are defined outside of
   * dessser's scope. Yet, we need to pass [E.fold] a valid environment so it
   * can compute any expression's type. *)
  let get_depends compunit e =
    (* Then we want to find out which of the identifiers refers to any non
     * external identifiers. *)
    let ext =
      (* Start with external identifiers: *)
      List.map (fun (name, typ) ->
        E.Ops.ext_identifier name, typ
      ) compunit.U.external_identifiers in
    let init_l = U.environment compunit in
    E.fold Set.String.empty (fun set -> function
      | E0 (Identifier s) as e ->
          assert (s <> "") ;
          if List.mem_assoc e ext (* already defined externally *) ||
             Set.String.mem s set (* already known to be undefined *) ||
             not (List.exists (fun (e', _) -> E.eq e e') init_l.E.global) (* identifier defined in [e] itself *)
          then (
            set
          ) else (
            if debug then
              pp stdout "Expression depends on external identifier %S\n%!" s ;
            Set.String.add s set
          )
      | _ -> set
    ) e

  let emit ?name p l e f =
    let t = E.type_of l e in
    let n =
      match name with
      | Some n -> n
      | None -> U.gen_sym "id_" |> valid_identifier in
    pp p.P.def "%s%t\n" p.indent (C.print_binding p t n f) ;
    n

  let define i name p l e =
    C.print_comment p.P.def "%s" (E.to_pretty_string ?max_depth:None e) ;
    let name = valid_identifier name in
    C.print_binding_toplevel i emit name p l e

  let declare i name p l e =
    let name = valid_identifier name in
    C.print_identifier_declaration i name p l e

  let print_verbatims where p oc = function
    | [] ->
        ()
    | defs ->
        C.print_comment oc "Verbatim (%s)" where ;
        List.rev defs |>
        List.iter (fun verb ->
          Printf.sprintf2 "%a" verb.U.printer p |>
          String.print oc) ;
        String.print oc "\n"

  let print_source compunit p oc top_verbatim middle_verbatim bottom_verbatim =
    let output_identifier =
      let i = ref ~-1 in
      fun n p l e ->
        incr i ;
        (match p.P.context with
        | Definition -> define
        | Declaration -> declare) !i n p l e in
    (* [compunit] is full of identifiers (list of name * exp). Also,
     * [inline_verbatim] is a list of verbatim definitions to be printed
     * amongst other identifiers.
     * All of those must be output in any order compatible with dependencies.
     *)
    let identifiers =
      let print_identifier name id p l =
        P.new_top_level p (fun p ->
          match id.U.expr with
          | None ->
              invalid_arg ("identifiers: missing definition for "^ name)
          | Some expr ->
              output_identifier name p l expr) in
      List.map (fun (name, identifier, _) ->
        match identifier.U.expr with
        | None ->
            invalid_arg ("identifiers: missing definition for "^ name)
        | Some expr ->
            let deps = get_depends compunit expr in
            name, print_identifier name identifier, deps
      ) compunit.U.identifiers in
    let identifiers =
      let print_verbatim verb p _l =
        P.new_top_level p (fun p ->
          Printf.sprintf2 "%a" verb.U.printer p |>
          String.print p.P.def) in
      List.fold_left (fun identifiers verb ->
        if verb.U.backend = id && verb.U.location = U.Inline then
          (verb.U.name, print_verbatim verb, Set.String.of_list verb.dependencies) ::
            identifiers
        else
          identifiers
      ) identifiers compunit.U.verbatim_definitions in
    if debug then
      pp stdout "Identifiers:\n%a\n%!"
        (List.print ~first:"" ~last:"" ~sep:"" (fun oc (name, _, deps) ->
          pp oc "  name: %s\n  depends: %a\n\n"
            name
            (Set.String.print String.print) deps)) identifiers ;
    (* [loop] will print all definitions ordered by dependencies: *)
    let rec loop progress defined left_overs = function
      | [] ->
          if left_overs <> [] then (
            if not progress then (
              let missings =
                List.fold_left (fun set (_, _, deps) ->
                  Set.String.union deps set
                ) Set.String.empty left_overs in
              let really_missings =
                List.map (fun (n, _, _) -> n) left_overs |> Set.String.of_list |>
                Set.String.diff missings in
              if Set.String.is_empty really_missings then (
                if debug then
                  pp stdout "Remaining identifiers are mutually recursive" ;
                let l = U.environment compunit in
                List.iter (fun (_name, printer, _depends) ->
                  printer p l
                ) left_overs ;
              ) else
                raise (Missing_dependencies really_missings)
            ) else loop false defined [] left_overs
          )
      | (name, printer, depends) :: rest ->
          let missing_depends =
            Set.String.filter (fun name ->
              not (List.mem_assoc name compunit.U.external_identifiers) &&
              not (List.exists ((=) name) defined)
            ) depends in
          if Set.String.is_empty missing_depends then (
            if debug then
              pp stdout "Identifier %s depends on %d defined \
                         identifiers, emitting code...\n"
                name (Set.String.cardinal depends) ;
            let l =
              (* Build an environment with only the defined identifiers: *)
              let identifiers =
                List.filter (fun (n, _, _) ->
                  List.mem n defined
                ) compunit.U.identifiers
              and verbatim_definitions =
                List.filter (fun verb ->
                  List.mem verb.U.name defined
                ) compunit.U.verbatim_definitions in
              U.environment U.{ compunit with identifiers ;
                                              verbatim_definitions } in
            if debug then
              pp stdout "Environment: %a\n" E.print_environment l ;
            printer p l ;
            let defined = name :: defined in
            loop true defined left_overs rest
          ) else (
            if debug then
              pp stdout "Identifier %s has some undefined \
                         dependences, waiting...\n" name ;
            let left_overs' = (name, printer, missing_depends) :: left_overs in
            loop progress defined left_overs' rest
          ) in
    loop false [] [] identifiers ;
    let print_ios oc = function
      | [] ->
          ()
      | lst ->
          List.rev lst |>
          List.iter (String.print oc) in
    pp oc
      "%s%a%a%a%a%a%a%a%a%a%a%a%a%s"
      (C.source_intro compunit p)
      (print_verbatims "top" p) top_verbatim
      C.print_comment "------------"
      C.print_comment "Declarations"
      C.print_comment "------------"
      print_ios p.decls
      (print_verbatims "middle" p) middle_verbatim
      C.print_comment "-----------"
      C.print_comment "Definitions"
      C.print_comment "-----------"
      print_ios p.consts
      print_ios p.defs
      (print_verbatims "bottom" p) bottom_verbatim
      (C.source_outro compunit p)

  let print_definitions oc compunit =
    let verbatims loc =
      List.filter (fun verb ->
        verb.U.backend = id && verb.location = loc
      ) compunit.U.verbatim_definitions in
    let p = P.(make Definition compunit.external_types) in
    print_source compunit p oc
      (verbatims Top) (verbatims Middle) (verbatims Bottom)

  let print_declarations oc compunit =
    let p = P.(make Declaration compunit.U.external_types) in
    print_source compunit p oc [] [] []

end
