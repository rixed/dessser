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

(* We need T.eq to be used for key comparisons instead of the generic comparison,
 * so that named types compare equal to their definition: *)
module RenamingHash = Hashtbl.Make (struct
  (* Key will be field name and parent type: *)
  type t = string * T.typ

  let equal (n1, t1) (n2, t2) =
    n1 = n2 && T.eq t1 t2

  (* Lots of collisions but safe: *)
  let hash (n, _t) = Hashtbl.hash n
end)

(* Uniquify in a human friendly way all record field and sum constructor
 * names.
 * Beware that when the exact same type is encountered several times in [t]
 * it is supposed to stay the same type after adaptation, so all instances
 * of [t] must have the same prefix! *)
let make_get_field_name mn =
  (* Hash from name to the list of original type and path this name is used
   * within: *)
  let renamings = Hashtbl.create 10 in
  let shortest_path p1 p2 =
    if List.compare_lengths p1 p2 > 0 then p2 else p1 in
  let record_name n vt path =
    let vt = T.develop vt in
    Hashtbl.modify_opt n (function
      | Some l ->
          let l, found =
            List.fold_left (fun (l, found) (vt', path') ->
              if T.eq vt vt' then (
                (vt, shortest_path path path') :: l, true
              ) else
                (vt', path') :: l, found
            ) ([], false) l in
          let l =
            if found then l else
            (vt, path) :: l in
          Some l
      | None ->
          Some [ vt, path ]
    ) renamings in
  let rec sensus_mn path mn =
    sensus_vt path mn.T.typ
  and sensus_vt path = function
    | TNamed (_, t) ->
        sensus_vt path t
    | TUsr { name ; def } ->
        sensus_vt (name :: path) def
    | TVec (_, mn) ->
        sensus_mn path mn
    | TArr mn ->
        sensus_mn path mn
    | TSet (_, mn) ->
        sensus_mn path mn
    | TTup mns ->
        Array.iter (sensus_mn path) mns
    | TRec mns as vt ->
        Array.iter (fun (n, mn) ->
          record_name n vt path ;
          sensus_mn (n :: path) mn
        ) mns
    | TSum mns as vt ->
        Array.iter (fun (n, mn) ->
          record_name n vt path ;
          sensus_mn (n :: path) mn
        ) mns
    | TMap (kmn, vmn) ->
        sensus_mn path kmn ;
        sensus_mn path vmn
    | _ ->
        () in
  (* Gather information on all field/constructor names: *)
  sensus_mn [] mn ;
  (* Shorten all paths so long as all paths to any given name used for same
   * kind of types are distinct: *)
  (* Helper function: tells if the given path is already present for that
   * kind of type, in a given list of (vt * path): *)
  let same_kind vt vt' =
    match vt, vt' with
    | T.TSum _, T.TSum _ | TRec _, TRec _ -> true | _ -> false in
  let is_present vt path l =
    List.exists (fun (vt', path') -> same_kind vt vt' && path = path') l in
  (* Shorten (from the left) every path until singleton or already present *)
  Hashtbl.map_inplace (fun _n l ->
    List.fold_left (fun l' (vt, path) ->
      let rec shorten path =
        match path with
        | [] ->
            (vt, path) :: l'
        | _ :: path' ->
            if is_present vt path' l ||
               is_present vt path' l'
            then (vt, path) :: l'
            else shorten path' in
      shorten (List.rev path)
    ) [] l
  ) renamings ;
  (* Now precompute a simple hash from name and type to new unique name: *)
  let uniq_field_name n vt =
    let l = Hashtbl.find renamings n in
    let num_vts =
      List.fold_left (fun num (vt', _) ->
        if same_kind vt vt' then num + 1 else num
      ) 0 l in
    if num_vts > 1 then
      let _vt, path = List.find (fun (vt', _) -> T.eq vt vt') l in
      if path = [] then n else String.join "_" path ^"_"^ n
    else
      n in
  (* Precompute all names: *)
  let field_names = RenamingHash.create 10 in
  Hashtbl.iter (fun n l ->
    List.iter (fun (vt, _) ->
      let n' = uniq_field_name n vt in
      if n' <> n then
        RenamingHash.modify_opt (n, vt) (function
          | None -> Some n'
          | Some n'' ->
              Some (
                if String.length n' < String.length n'' then n'
                else n'')
        ) field_names
    ) l
  ) renamings ;
  fun n t ->
    let t = T.develop t |> T.shrink in
    RenamingHash.find_default field_names (n, t) n

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
    (* The int is a sequence in a recursive set of definitions, or 0: *)
    recurs:bool -> rec_seq:int -> emitter -> string -> P.t -> E.env -> E.t -> unit

  val print_identifier_declaration :
    (* The initial int is a sequence for that compilation unit: *)
    recurs:bool -> rec_seq:int -> string -> P.t -> E.env -> E.t -> unit

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

  let define ~recurs ~rec_seq name p l e =
    C.print_comment p.P.def "%s" (E.to_pretty_string ?max_depth:None e) ;
    let name = valid_identifier name in
    C.print_binding_toplevel ~recurs ~rec_seq emit name p l e

  let declare ~recurs ~rec_seq name p l e =
    let name = valid_identifier name in
    C.print_identifier_declaration ~recurs ~rec_seq name p l e

  let print_verbatims where p oc = function
    | [] ->
        ()
    | defs ->
        C.print_comment oc "Verbatim (%s)" where ;
        List.rev defs |>
        List.iter (fun verb ->
          Printf.sprintf2 "%a" (verb.U.printer ~recurs:false ~rec_seq:0) p |>
          String.print oc) ;
        String.print oc "\n"

  let print_source compunit p oc top_verbatim middle_verbatim bottom_verbatim =
    let output_identifier n ~recurs ~rec_seq p l e =
      (match p.P.context with
      | Definition -> define
      | Declaration -> declare) ~recurs ~rec_seq n p l e in
    (* [compunit] is full of identifiers (list of name * exp). Also,
     * [inline_verbatim] is a list of verbatim definitions to be printed
     * amongst other identifiers.
     * All of those must be output in any order compatible with dependencies.
     *)
    let identifiers =
      let print_identifier name id ~recurs ~rec_seq p l =
        P.new_top_level p (fun p ->
          match id.U.expr with
          | None ->
              invalid_arg ("identifiers: missing definition for "^ name)
          | Some expr ->
              output_identifier name ~recurs ~rec_seq p l expr) in
      List.map (fun (name, identifier, _) ->
        match identifier.U.expr with
        | None ->
            invalid_arg ("identifiers: missing definition for "^ name)
        | Some expr ->
            let deps = get_depends compunit expr in
            name, print_identifier name identifier, deps
      ) compunit.U.identifiers in
    let identifiers =
      let print_verbatim verb ~recurs ~rec_seq p _l =
        P.new_top_level p (fun p ->
          Printf.sprintf2 "%a" (verb.U.printer ~recurs ~rec_seq) p |>
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
                List.iteri (fun i (_name, printer, _depends) ->
                  printer ~recurs:true ~rec_seq:i p l
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
            printer ~recurs:false ~rec_seq:0 p l ;
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
    let p = P.(make compunit.U.module_name Definition compunit.external_types) in
    print_source compunit p oc
      (verbatims Top) (verbatims Middle) (verbatims Bottom)

  let print_declarations oc compunit =
    let p = P.(make compunit.U.module_name Declaration compunit.external_types) in
    print_source compunit p oc [] [] []

end
