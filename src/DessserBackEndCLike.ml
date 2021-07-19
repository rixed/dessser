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

exception Missing_dependencies of string list

let () =
  Printexc.register_printer (function
    | Missing_dependencies deps ->
        Some (
          Printf.sprintf2 "Missing dependencies: %a"
            (pretty_list_print String.print) deps)
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

  val type_identifier : P.t -> T.t -> string

  val type_identifier_mn : P.t -> T.mn -> string

  val print_binding :
    P.t -> T.mn -> string -> (string IO.output -> unit) -> string IO.output -> unit

  val print_inline :
    P.t -> T.mn -> (string IO.output -> unit) -> string IO.output -> unit

  val print_binding_toplevel :
    emitter -> string -> P.t -> E.env -> E.t -> unit

  val print_identifier_declaration :
    string -> P.t -> E.env -> E.t -> unit

  val print_comment : 'b IO.output -> ('a, 'b IO.output, unit) format -> 'a

  val print : emitter -> ?name:string -> P.t -> E.env -> E.t -> string

  val source_intro : P.context -> string
  val source_outro : P.context -> string

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
    E.fold [] (fun lst -> function
      | E0 (Identifier s) as e ->
          assert (s <> "") ;
          if List.mem_assoc e ext (* already defined externally *) ||
             List.mem s lst (* already known to be undefined *) ||
             not (List.mem_assoc e init_l.E.global) (* identifier defined in [e] itself *)
          then (
            lst
          ) else (
            if debug then
              pp stdout "Expression depends on external identifier %S\n%!" s ;
            s :: lst
          )
      | _ -> lst
    ) e

  (* As inlined expressions may be reordered, those must all be stateless.
   * Include in here all operations that are cheap enough that it's OK to
   * compute them several times if required.
   * Arithmetic operators that fail with null are not inlinable (in C++)
   * but none of them count as cheap anyway. *)
  let can_inline = function
    | E.E0 (
        Param _ | Null _ |
        EndOfList _ | Float _ | String _ | Bool _ | Bytes _ |
        Identifier _ | ExtIdentifier _ |
        Char _ | Size _ |
        U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _ |
        I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _ |
        CopyField | SkipField | SetFieldNull)
    | E1 ((
        GetItem _ | GetField _ | GetAlt _ | IsNull | NotNull | Force _ | ToFloat |
        ToU8 | ToU16 | ToU24 | ToU32 | ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
        ToI8 | ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 | ToI64 | ToI128 |
        CharOfPtr | FloatOfPtr | U8OfPtr | I8OfPtr | U16OfPtr | I16OfPtr |
        U24OfPtr | I24OfPtr | U32OfPtr | I32OfPtr | U40OfPtr | I40OfPtr |
        U48OfPtr | I48OfPtr | U56OfPtr | I56OfPtr | U64OfPtr | I64OfPtr |
        U128OfPtr | I128OfPtr | FloatOfU64 | U64OfFloat | U8OfChar |
        CharOfU8 | SizeOfU32 | U32OfSize | AddressOfU64 | U64OfAddress |
        U8OfBool | BoolOfU8 |
        BitNot | StringLength | BytesLength | RemSize | Not | Abs | Neg |
        Head | Tail | Ignore | Identity), _)
    | E2 ((
        Nth | Gt | Ge | Eq | Add | Sub | Mul | Min | Max |
        BitAnd | BitOr | BitXor | LeftShift | RightShift | GetBit), _, _) ->
        true
    (* And and Or can not be inlined because all or part of their argument
     * may not be inlinable and would then escape the scope of the And/Or,
     * thus defeating any shortcutting that should take place. *)
    | _ ->
        false

  let emit ?name p l e f =
    let t = E.type_of l e in
    if name = None && can_inline e then (
      Printf.sprintf2 "%t" (C.print_inline p t f)
    ) else (
      let n =
        match name with
        | Some n -> n
        | None -> U.gen_sym "id_" |> valid_identifier in
      pp p.def "%s%t\n" p.indent (C.print_binding p t n f) ;
      n
    )

  let define name p l e =
    C.print_comment p.P.def "%s" (E.to_pretty_string ?max_depth:None e) ;
    let name = valid_identifier name in
    C.print_binding_toplevel emit name p l e

  let declare name p l e =
    let name = valid_identifier name in
    C.print_identifier_declaration name p l e

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
      match p.P.context with
      | Definition -> define
      | Declaration -> declare in
    (* [compunit] is full of identifiers (list of name * exp). Also,
     * [inline_verbatim] is a list of verbatim definitions to be printed
     * amongst other identifiers.
     * All of those must be output in any order compatible with dependencies.
     *)
    let identifiers =
      let print_identifier name id p l =
        P.new_top_level p (fun p ->
          output_identifier name p l id.U.expr) in
      List.map (fun (name, identifier, _) ->
        let deps = get_depends compunit identifier.U.expr in
        name, print_identifier name identifier, deps
      ) compunit.U.identifiers in
    let identifiers =
      let print_verbatim verb p _l =
        P.new_top_level p (fun p ->
          Printf.sprintf2 "%a" verb.U.printer p |>
          String.print p.P.def) in
      List.fold_left (fun identifiers verb ->
        if verb.U.backend = id && verb.U.location = U.Inline then
          (verb.U.name, print_verbatim verb, verb.dependencies) :: identifiers
        else
          identifiers
      ) identifiers compunit.U.verbatim_definitions in
    if debug then
      pp stdout "Identifiers:\n%a\n%!"
        (List.print ~first:"" ~last:"" ~sep:"" (fun oc (name, _, deps) ->
          pp oc "  name: %s\n  depends: %a\n\n"
            name
            (List.print String.print) deps)) identifiers ;
    (* [loop] will print all definitions ordered by dependencies: *)
    let rec loop progress defined left_overs = function
      | [] ->
          if left_overs <> [] then (
            if not progress then (
              let missings =
                List.fold_left (fun l (_, _, deps) ->
                  List.rev_append deps l
                ) [] left_overs in
              raise (Missing_dependencies missings)
            ) else loop false defined [] left_overs
          )
      | (name, printer, depends) :: rest ->
          let missing_depends =
            List.filter (fun name ->
              not (List.mem_assoc name compunit.U.external_identifiers) &&
              not (List.exists ((=) name) defined)
            ) depends in
          if missing_depends <> [] then (
            if debug then
              pp stdout "Identifier %s has some undefined \
                         dependences, waiting...\n" name ;
            let left_overs' = (name, printer, missing_depends) :: left_overs in
            loop progress defined left_overs' rest
          ) else (
            if debug then
              pp stdout "Identifier %s depends on %d defined \
                         identifiers, emitting code...\n"
                name (List.length depends) ;
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
          ) in
    loop false [] [] identifiers ;
    let print_ios oc = function
      | [] ->
          ()
      | lst ->
          List.rev lst |>
          List.iter (String.print oc) in
    pp oc
      "%s%a%a%a%a%a%a%a%a%a%a%a%s"
      (C.source_intro p.P.context)
      (print_verbatims "top" p) top_verbatim
      C.print_comment "------------"
      C.print_comment "Declarations"
      C.print_comment "------------"
      print_ios p.decls
      (print_verbatims "middle" p) middle_verbatim
      C.print_comment "-----------"
      C.print_comment "Definitions"
      C.print_comment "-----------"
      print_ios p.defs
      (print_verbatims "bottom" p) bottom_verbatim
      (C.source_outro p.P.context)

  let print_definitions oc compunit =
    let verbatims loc =
      List.filter (fun verb ->
        verb.U.backend = id && verb.location = loc
      ) compunit.U.verbatim_definitions in
    let p = P.(make Definition compunit.U.type_names compunit.external_types) in
    print_source compunit p oc
      (verbatims Top) (verbatims Middle) (verbatims Bottom)

  let print_declarations oc compunit =
    let p = P.(make Declaration compunit.U.type_names compunit.external_types) in
    print_source compunit p oc [] [] []

end
