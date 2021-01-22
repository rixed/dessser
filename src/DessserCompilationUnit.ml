module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes

type t =
  (* FIXME: maps not lists *)
  { identifiers : (string * identifier * T.t) list ;
    external_identifiers : (string * T.t) list ;
    verbatim_definitions : verbatim_definition list }

and verbatim_definition =
  { name : string ; (* or empty string *)
    typ : T.t ;
    dependencies : string list ;
    backend : T.backend_id ;
    location : verbatim_location ;
    printer : (P.t, unit) BatIO.printer }

and identifier =
  { public : bool ; expr : E.t }

and verbatim_location =
  | Inline (* According to declared names and dependencies *)
  | Top    (* Before any declarations *)
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
  (* Then already defined identifiers: *)
  let l =
    List.fold_left (fun l (name, _, t) ->
      (E.Ops.identifier name, t) :: l
    ) l compunit.identifiers in
  (* Finally, we also want to be able to access verbatim identifiers: *)
  let l =
    List.fold_left (fun l verb ->
      (* Assume identifiers will be available in all relevant backends when
       * they are used. Also assume any named identifier is accessible from
       * everywhere (ie. including those already defined for Bottom location).
       *)
      if verb.name = "" then l else
        (E.Ops.identifier verb.name, verb.typ) :: l
    ) l compunit.verbatim_definitions in
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

let make_verbatim_definition
      ?(name="") ?(typ=T.(Value (required Unit))) ?(dependencies=[])
      ?(location=Inline) ~backend printer =
  if location = Inline && name = "" then
    invalid_arg "make_verbatim_definition: Inline definitions must be named" ;
  { name ;
    typ ;
    dependencies ;
    backend ;
    location ;
    printer }

(* Verbatim definitions can then be accessed as normal identifiers: *)
let add_verbatim_definition
      compunit ?name ?typ ?dependencies ?location ~backend printer =
  { compunit with
    verbatim_definitions =
      make_verbatim_definition
        ?name ?typ ?dependencies ?location ~backend printer ::
        compunit.verbatim_definitions }
