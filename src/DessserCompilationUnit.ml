open DessserMiscTypes
module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes

let debug = false

type t =
  (* FIXME: maps not lists *)
  { identifiers : (string * identifier * T.mn) list ;
    external_identifiers : (string * T.mn) list ;
    verbatim_definitions : verbatim_definition list ;
    external_types : (string * (P.t -> backend_id -> string)) list ;
    (* User may prefer some specific name for some types: *)
    type_names : (T.t * string) list }

and verbatim_definition =
  { name : string ; (* or empty string *)
    typ : T.mn ;
    dependencies : string list ;
    backend : backend_id ;
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
  { identifiers = [] ;
    external_identifiers = [] ;
    verbatim_definitions = [] ;
    external_types = [] ;
    type_names = [] }

let name_type compunit t name =
  let t = T.shrink t in
  match List.assoc t compunit.type_names with
  | exception Not_found ->
      { compunit with type_names = (t, name) :: compunit.type_names }
  | name' ->
      Printf.sprintf "name_type: type %S already named %S" name name' |>
      invalid_arg

(* Extract the currently defined (global) environment from the compunit: *)
let environment compunit =
  (* Start with external identifiers: *)
  let g =
    List.map (fun (name, typ) ->
      E.Ops.ext_identifier name, typ
    ) compunit.external_identifiers in
  (* Then already defined identifiers: *)
  let g =
    List.fold_left (fun g (name, _, t) ->
      (E.Ops.identifier name, t) :: g
    ) g compunit.identifiers in
  (* Finally, we also want to be able to access verbatim identifiers: *)
  let g =
    List.fold_left (fun g verb ->
      (* Assume identifiers will be available in all relevant backends when
       * they are used. Also assume any named identifier is accessible from
       * everywhere (ie. including those already defined for Bottom location).
       *)
      if verb.name = "" then g else
        (E.Ops.identifier verb.name, verb.typ) :: g
    ) g compunit.verbatim_definitions in
  E.{ global = g ; local = [] ; name = None }

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
  let l = environment compunit in
  if debug then
    BatPrintf.eprintf "add_identifier_of_expression: type checking%s\n"
      (E.to_pretty_string expr) ;
  E.type_check l expr ;
  let t = E.type_of l expr in
  if debug then
    BatPrintf.eprintf "  …of type: %a\n" T.print_mn t ;
  let expr =
    if !DessserEval.inline_level > 0 then (
      let expr = DessserEval.peval l expr in
      if debug then (
        BatPrintf.eprintf "  …simplified into%s\n" (E.to_pretty_string expr) ;
        (* Check that the expression types are equivalent: *)
        E.type_check l expr
      ) ;
      assert (T.eq_mn t (E.type_of l expr)) ;
      expr
    ) else expr in
  let identifier = { public ; expr } in
  { compunit with
      identifiers = (name, identifier, t) :: compunit.identifiers },
  E.E0 (Identifier name),
  name

let get_type_of_identifier compunit name =
  let _, _, t =
    List.find (fun (name', _id, _t) -> name = name') compunit.identifiers in
  t

let make_verbatim_definition
      ?(name="") ?(typ=T.void) ?(dependencies=[])
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

(*
 * External types
 * They can be manipulated only by external identifiers.
 *)

let register_external_type compunit name f =
  { compunit with
      external_types = (name, f) :: compunit.external_types }

let is_external_type_registered compunit name =
  List.mem_assoc name compunit.external_types
