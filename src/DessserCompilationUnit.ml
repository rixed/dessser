module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes

type t =
  (* FIXME: maps not lists *)
  { identifiers : (string * identifier * T.t) list ;
    external_identifiers : (string * T.t) list ;
    verbatim_definitions :
      (T.backend_id * verbatim_location * (P.t, unit) BatIO.printer) list }

and identifier =
  { public : bool ; expr : E.t }

and verbatim_location =
  | Top (* Before any declarations *)
  | Middle (* After declarations *)
  | Bottom (* After declarations and definitions *)

let make () =
  { identifiers = [] ; external_identifiers = [] ; verbatim_definitions = [] }

(* Extract the currently defined environment from the compunit: *)
let environment compunit =
  (* Start with external identifiers: *)
  let l =
    List.map (fun (name, typ) ->
      E.Ops.ext_identifier name, typ
    ) compunit.external_identifiers in
  (* ...and already defined identifiers in the environment: *)
  let l =
    List.fold_left (fun l (name, _, t) ->
      (E.Ops.identifier name, t) :: l
    ) l compunit.identifiers in
  l

let add_external_identifier compunit name typ =
  { compunit with external_identifiers =
      (name, typ) :: compunit.external_identifiers }

let gen_sym =
  let name_seq = ref (-1) in
  fun pref ->
    incr name_seq ;
    pref ^ string_of_int !name_seq

(* Returns the new compilation unit, the Identifier expression to use in new
 * expressions, and the identifier name in the source code. *)
let add_identifier_of_expression compunit ?name expr =
  let name, public =
    match name with
    | None ->
        gen_sym "anon_", false
    | Some name ->
        name, true in
  let identifier = { public ; expr } in
  let l = environment compunit in
  E.type_check l expr ;
  let t = E.type_of l expr in
  { compunit with
      identifiers = (name, identifier, t) :: compunit.identifiers },
  E.E0 (Identifier name),
  name

let add_verbatim_definition compunit ?(location=Middle) backend f =
  { compunit with
    verbatim_definitions =
      (backend, location, f) :: compunit.verbatim_definitions }
