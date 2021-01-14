module E = DessserExpressions
module T = DessserTypes

type t =
  { identifiers : (string * identifier * T.t) list ;
    external_identifiers : (string * T.t) list }

and identifier =
  { public : bool ; expr : E.t }

let make () =
  { identifiers = [] ; external_identifiers = [] }

(* Extract the currently defined environment from the state: *)
let environment state =
  (* Start with external identifiers: *)
  let l =
    List.map (fun (name, typ) ->
      E.Ops.ext_identifier name, typ
    ) state.external_identifiers in
  (* ...and already defined identifiers in the environment: *)
  let l =
    List.fold_left (fun l (name, _, t) ->
      (E.Ops.identifier name, t) :: l
    ) l state.identifiers in
  l

let add_external_identifier state name typ =
  { state with external_identifiers =
      (name, typ) :: state.external_identifiers }

let gen_sym =
  let name_seq = ref (-1) in
  fun pref ->
    incr name_seq ;
    pref ^ string_of_int !name_seq

(* Returns the new compilation unit, the Identifier expression to use in new
 * expressions, and the identifier name in the source code. *)
let add_identifier_of_expression state ?name expr =
  let name, public =
    match name with
    | None ->
        gen_sym "anon_", false
    | Some name ->
        name, true in
  let identifier = { public ; expr } in
  let l = environment state in
  E.type_check l expr ;
  let t = E.type_of l expr in
  { state with
      identifiers = (name, identifier, t) :: state.identifiers },
  E.E0 (Identifier name),
  name
