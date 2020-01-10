open Batteries
open Stdint
open Dessser

exception Missing_dependencies of string list

type print_state =
  { decl : string IO.output ;
    def : string IO.output ;
    mutable indent : string ;
    mutable declared : Set.String.t }

let make_print_state () =
  { decl = IO.output_string () ;
    def = IO.output_string () ;
    indent = "" ;
    declared = Set.String.empty }

let indent_more p f =
  let indent = p.indent in
  p.indent <- p.indent ^"  " ;
  finally (fun () -> p.indent <- indent)
    f ()

let declared_type p t f =
  let id =
    dump t |>
    Digest.string |>
    Digest.to_hex in
  if Set.String.mem id p.declared then id
  else (
    p.declared <- Set.String.add id p.declared ;
    let indent = p.indent in
    p.indent <- "" ;
    f p.decl id ;
    p.indent <- indent ;
    id
  )

type emitter =
  print_state ->
  (e * Dessser.typ) list ->
  e -> (string Batteries.IO.output -> unit) ->
    string

let valid_identifier s =
  if s = "" then "v_" else
  if s.[0] >= 'a' && s.[0] <= 'z' || s.[0] = '_' then s ^"_" else
  "v_"^ s ^"_"

module type CONFIG =
sig
  val preferred_file_extension : string

  val type_identifier : print_state -> Type.t -> string

  val print_binding :
    string -> string -> ('a IO.output -> unit) -> 'a IO.output -> unit

  val print_comment : 'a IO.output -> string -> unit

  val print_binding_toplevel :
    emitter -> string -> print_state -> (e * Type.t) list -> e -> unit

  val print : emitter -> print_state -> (e * Type.t) list -> e -> string

  val source_intro : string

  (* TODO: find a way to factorize the print function itself *)
end

let gen_sym =
  let name_seq = ref (-1) in
  fun pref ->
    incr name_seq ;
    pref ^ string_of_int !name_seq

module Make (C : CONFIG) : BACKEND =
struct

  let preferred_file_extension = C.preferred_file_extension

  let gen_sym () = valid_identifier (gen_sym "id_")

  type identifier =
    { public : bool ; expr : e }

  type state =
    { identifiers : (string * identifier) list }

  let make_state () =
    { identifiers = [] }

  (* Find references to external identifiers: *)
  let get_depends e =
    Expression.fold [] [] (fun lst l -> function
      | Identifier s as e ->
          assert (s <> "") ;
          if List.mem_assoc e l || List.mem s lst then lst else (
            pp stdout "Cannot find identifier %S in %a\n%!"
              s (List.print (fun oc (e, _) -> Expression.print oc e)) l ;
            s :: lst
          )
      | _ -> lst
    ) e

  let identifier_of_expression state ?name expr =
    let name, public =
      match name with
      | None ->
          gen_sym (), false
      | Some name ->
          name, true in
    let identifier = { public ; expr } in
    (* TODO: add already defined identifiers in the environment: *)
    Expression.type_check [] expr ;
    { identifiers = (name, identifier) :: state.identifiers },
    Expression.Identifier name,
    valid_identifier name

  let find_or_declare_type _p _t =
    assert false

  let emit p l e f =
    let n = gen_sym () in
    let t = Expression.type_of l e in
    let tn = C.type_identifier p t in
    pp p.def "%s%t\n" p.indent (C.print_binding n tn f) ;
    n

  let define name p l e =
    let name = valid_identifier name in
    C.print_binding_toplevel emit name p l e

  let print_source state oc =
    (* state is full of identifiers (list of name * exp). Output them in any order
     * as long as dependencies are defined before used. *)
    let identifiers =
      List.map (fun (name, identifier) ->
        name, get_depends identifier.expr, identifier.expr
      ) state.identifiers in
    pp stdout "Identifiers:\n%a\n%!"
      (List.print ~first:"" ~last:"" ~sep:"" (fun oc (name, depends, e) ->
        pp oc "  name: %s\n  depends: %a\n  expression: %a\n\n"
          name
          (List.print String.print) depends
          (Expression.print ?max_depth:None) e)) identifiers ;
    let p = make_print_state () in
    let rec loop progress defined left_overs = function
      | [] ->
          if left_overs <> [] then (
            if not progress then (
              let missings = List.map (fun (n, _, _) -> n) left_overs in
              raise (Missing_dependencies missings)
            ) else loop false defined [] left_overs
          )
      | (name, depends, e) :: rest ->
          let missing_depends =
            List.filter (fun name ->
              not (List.mem_assoc name defined)
            ) depends in
          if missing_depends <> [] then
            loop progress defined ((name, missing_depends, e) :: left_overs) rest
          else (
            let open Expression in
            let l = List.map (fun (name, t) -> Identifier name, t) defined in
            define name p l e ;
            let t = type_of l e in
            let defined = (name, t) :: defined in
            loop true defined left_overs rest
          ) in
    loop false [] [] identifiers ;
    Printf.fprintf oc
      "%s\n\n\
       %a\n\
       %s\n\n\
       %a\n\n\
       %s\n\n"
      C.source_intro
      C.print_comment "Declarations"
      (IO.close_out p.decl)
      C.print_comment "Definitions"
      (IO.close_out p.def)
end
