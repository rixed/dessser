open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions

let debug = false

exception Missing_dependencies of string list

type print_state =
  { mutable decl : string IO.output ;
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
    (* Write in a temp string to avoid being interrupted by another
     * declaration: *)
    let oc = IO.output_string ()
    and indent = p.indent in
    p.indent <- "" ;
    f oc id ;
    p.indent <- indent ;
    String.print p.decl (IO.close_out oc) ;
    id
  )

type emitter =
  ?name:string ->
  print_state ->
  (e * typ) list ->
  e -> (string IO.output -> unit) ->
    string

(* Avoid modifying the name when it's valid: *)
let valid_identifier s =
  if s = "" then "v" else
  if s.[0] >= 'a' && s.[0] <= 'z' || s.[0] = '_' then s else
  "v_"^ s

module type CONFIG =
sig
  val valid_identifier : string -> string
  val preferred_def_extension : string
  val preferred_decl_extension : string
  val compile_cmd : optim:int -> link:bool -> string -> string -> string

  val type_identifier : print_state -> typ -> string

  val print_binding :
    string -> string -> ('a IO.output -> unit) -> 'a IO.output -> unit

  val print_binding_toplevel :
    emitter -> string -> print_state -> (e * typ) list -> e -> unit

  val print_identifier_declaration :
    string -> print_state -> (e * typ) list -> e -> unit

  val print_comment : unit IO.output -> ('a, unit IO.output, unit) format -> 'a

  val print : ?name:string -> emitter -> print_state -> (e * typ) list -> e -> string

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
  let print_comment = C.print_comment
  let preferred_def_extension = C.preferred_def_extension
  let preferred_decl_extension = C.preferred_decl_extension
  let compile_cmd = C.compile_cmd

  let gen_sym () = valid_identifier (gen_sym "id_")

  type identifier =
    { public : bool ; expr : e }

  type state =
    { identifiers : (string * identifier) list ;
      external_identifiers : (string * typ) list }

  let make_state () =
    { identifiers = [] ; external_identifiers = [] }

  (* Find references to external identifiers: *)
  let get_depends l e =
    fold_expr [] l (fun lst l -> function
      | E0 (Identifier s) as e ->
          assert (s <> "") ;
          if List.mem_assoc e l || List.mem s lst then lst else (
            pp stdout "Cannot find identifier %S in %a\n%!"
              s (List.print (fun oc (e, _) -> print_expr oc e)) l ;
            s :: lst
          )
      | _ -> lst
    ) e

  let add_external_identifier state name typ =
    { state with external_identifiers =
        (name, typ) :: state.external_identifiers }

  let identifier_of_expression state ?name expr =
    let name, public =
      match name with
      | None ->
          gen_sym (), false
      | Some name ->
          name, true in
    let identifier = { public ; expr } in
    (* Start with external identifiers: *)
    let l =
      List.map (fun (name, typ) ->
        Ops.identifier name, typ
      ) state.external_identifiers in
    (* ...and already defined identifiers in the environment: *)
    let l =
      List.fold_left (fun l (name, id) ->
        (Ops.identifier name, type_of l id.expr) :: l
      ) l state.identifiers in
    type_check l expr ;
    if type_of l expr = TVoid then
      invalid_arg "identifier_of_expression of type void" ;
    { state with
        identifiers = (name, identifier) :: state.identifiers },
    E0 (Identifier name),
    valid_identifier name

  let find_or_declare_type _p _t =
    assert false

  let emit ?name p l e f =
    let n = match name with Some n -> n | None -> gen_sym () in
    let t = type_of l e in
    let tn = C.type_identifier p t in
    pp p.def "%s%t\n" p.indent (C.print_binding n tn f) ;
    n

  let define name p l e =
    let name = valid_identifier name in
    C.print_binding_toplevel emit name p l e

  let declare name p l e =
    let name = valid_identifier name in
    C.print_identifier_declaration name p l e

  let print_source output_identifier state oc =
    (* state is full of identifiers (list of name * exp). Output them in any order
     * as long as dependencies are defined before used. *)
    let identifiers =
      List.map (fun (name, identifier) ->
        let l =
          List.map (fun (name, typ) ->
            Ops.identifier name, typ
          ) state.external_identifiers in
        let deps = get_depends l identifier.expr in
        name, deps, identifier.expr
      ) state.identifiers in
    if debug then
      pp stdout "Identifiers:\n%a\n%!"
        (List.print ~first:"" ~last:"" ~sep:"" (fun oc (name, depends, e) ->
          pp oc "  name: %s\n  depends: %a\n  expression: %a\n\n"
            name
            (List.print String.print) depends
            (print_expr ?max_depth:None) e)) identifiers ;
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
            let l =
              List.map (fun (name, t) ->
                E0 (Identifier name), t
              ) defined in
            output_identifier name p l e ;
            let t = type_of l e in
            let defined = (name, t) :: defined in
            loop true defined left_overs rest
          ) in
    loop false state.external_identifiers [] identifiers ;
    Printf.fprintf oc
      "%s\n\n\
       %a\n\
       %s\n\n\
       %a\n\n\
       %s"
      C.source_intro
      C.print_comment "Declarations"
      (IO.close_out p.decl)
      C.print_comment "Definitions"
      (IO.close_out p.def)

  let print_definitions state oc =
    print_source define state oc

  let print_declarations state oc =
    print_source declare state oc
end
