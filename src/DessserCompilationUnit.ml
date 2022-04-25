open DessserMiscTypes
module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes
module TC = DessserTypeCheck

let debug = ref false

type t =
  { module_name : string ;
    (* FIXME: maps not lists *)
    identifiers : (string * identifier * T.mn) list ; (* TODO: add definition *)
    external_identifiers : (string * T.mn) list ;
    verbatim_definitions : verbatim_definition list ;
    external_types : (string * (P.t -> backend_id -> string)) list }

and verbatim_definition =
  { name : string ; (* or empty string *)
    typ : T.mn ;
    dependencies : string list ;
    backend : backend_id ;
    location : verbatim_location ;
    printer : recurs:bool -> rec_seq:int -> (P.t, unit) BatIO.printer }

and identifier =
  { public : bool ;
    mutable expr : E.t option }

and verbatim_location =
  | Inline (* According to declared names and dependencies *)
  | Top    (* Before any declarations *)
  | Middle (* After declarations *)
  | Bottom (* After declarations and definitions *)

let make module_name =
  { module_name ;
    identifiers = [] ;
    external_identifiers = [] ;
    verbatim_definitions = [] ;
    external_types = [] }

(* Extract the currently defined (global) environment from the compunit: *)
let environment compunit =
  (* Start with external identifiers: *)
  let g =
    List.map (fun (name, typ) ->
      E.Ops.ext_identifier name, E.make_binding None typ
    ) compunit.external_identifiers in
  (* Then already defined identifiers: *)
  let g =
    List.fold_left (fun g (name, _, t) ->
      (E.Ops.identifier name, E.make_binding None t) :: g
    ) g compunit.identifiers in
  (* Finally, we also want to be able to access verbatim identifiers: *)
  let g =
    List.fold_left (fun g verb ->
      (* Assume identifiers will be available in all relevant backends when
       * they are used. Also assume any named identifier is accessible from
       * everywhere (ie. including those already defined for Bottom location).
       *)
      if verb.name = "" then g else
        (E.Ops.identifier verb.name, E.make_binding None verb.typ) :: g
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

exception Already_defined of string

let () =
  Printexc.register_printer (function
    | Already_defined name ->
        Some (Printf.sprintf "Expression %s is already defined" name)
    | _ ->
        None)

(* Declare that a given identifier exists with a given type, but without giving
 * its expression as of yet. Useful for mutually recursive expressions.
 * [add_identifier_of_expression] will have to be called later to set the
 * actual expression before the code can be generated though. *)
let add_identifier_of_type compunit ?name mn =
  let name, public =
    match name with
    | None ->
        gen_sym "anon_", false
    | Some name ->
        name, true in
  let l = environment compunit in
  if E.defined name l then
    raise (Already_defined name) ;
  if !debug then
    BatPrintf.eprintf "add_identifier_of_type name=%s, type=%a\n"
      name
      T.print_mn mn ;
  let identifier = { public ; expr = None } in
  { compunit with
      identifiers = (name, identifier, mn) :: compunit.identifiers },
  T.E0 (Identifier name),
  name

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
  if !debug then
    BatPrintf.eprintf "add_identifier_of_expression name=%s: type checking%s\n"
      name
      (E.to_pretty_string expr) ;
  let expr = TC.type_check l expr in
  let t = E.type_of l expr in
  if !debug then
    BatPrintf.eprintf "  …of type: %a\n" T.print_mn t ;
  let expr =
    if !DessserEval.inline_level > 0 then (
      let expr = DessserEval.peval l expr in
      if !debug then (
        BatPrintf.eprintf "  …simplified into%s\n" (E.to_pretty_string expr) ;
        (* Check that the expression types are equivalent: *)
        ignore (TC.type_check l expr)
      ) ;
      assert (T.eq_mn t (E.type_of l expr)) ;
      expr
    ) else expr in
  let compunit =
    try
      (* Try to set the expression of a previously declared identifier: *)
      List.iter (fun (name', identifier', t') ->
        if name = name' then
          if identifier'.expr = None then
            if T.eq_mn t t' then (
              identifier'.expr <- Some expr ;
              raise Exit
            ) else (
              BatPrintf.sprintf2 "Definition for identifier %s has type %a \
                                  but was declared of type %a"
                name
                T.print_mn t
                T.print_mn t' |>
              failwith
            )
          else
            raise (Already_defined name)
      ) compunit.identifiers ;
      (* Couldn't exit: add a new identifier: *)
      let identifier = { public ; expr = Some expr } in
      { compunit with
          identifiers = (name, identifier, t) :: compunit.identifiers }
    with Exit ->
      compunit in
  compunit,
  T.E0 (Identifier name),
  name

(* Reify one layer of lambda functions from compunit, and return the new
   one, and a flag that's false if nothing was done. *)
let reify_some_lambdas compunit =
  let modified = ref false in
  let compunit' = ref compunit in
  List.iter (fun (_name, identifier, _mn) ->
    let reify_expression e =
      E.map ~enter_functions:false (function
        | T.E1 (Function _, _) as f ->
            let u, id, _ = add_identifier_of_expression !compunit' f in
            compunit' := u ;
            modified := true ;
            id
        | e ->
            e
      ) e in
    match identifier.expr with
    | None ->
        ()
    | Some (T.E1 (Function ts, body)) ->
        (* Top level functions are not lambdas: *)
        (* The new compunit is reusing the previous one's identifiers so let's
           merely modify it: *)
        identifier.expr <- Some (T.E1 (Function ts, reify_expression body))
    | Some e ->
        (* The new compunit is reusing the previous one's identifiers so let's
           merely modify it: *)
        identifier.expr <- Some (reify_expression e)
    ) compunit.identifiers ;
  !compunit', !modified

(* Should be called only after partial-eval had a chance to optimise some
 * of those lambdas *)
let rec reify_lambdas compunit =
  let compunit', modified = reify_some_lambdas compunit in
  if modified then reify_lambdas compunit' else compunit

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
