(* Backend for the Dessser Intermediate Language *)
open Batteries
open Dessser
module T = DessserTypes
module E = DessserExpressions

type T.backend_id += DIL

let id = DIL

type identifier =
  { public : bool ; expr : E.t }

type state =
  { identifiers : (string * identifier * T.t) list ;
    external_identifiers : (string * T.t) list }

let make_state () =
  { identifiers = [] ; external_identifiers = [] }

let print_definitions state oc =
  (* Print in the order of definition: *)
  List.rev state.identifiers |>
  List.iter (fun (name, { expr ; _ }, _) ->
    Format.(fprintf str_formatter "@[<hov 2>(define@ %s@ %a)@]"
      name
      E.pretty_print expr) ;
    Format.flush_str_formatter () |> String.print oc)

let print_declarations _state _oc =
  (* TODO: a header with all those types? *)
  ()

let print_comment oc fmt =
  Printf.fprintf oc ("; " ^^ fmt ^^ "\n")

let add_external_identifier state name typ =
  { state with external_identifiers =
      (name, typ) :: state.external_identifiers }

let gen_sym =
  let name_seq = ref (-1) in
  fun () ->
    incr name_seq ;
    "_"^ string_of_int !name_seq

let environment state =
  (* Start with external identifiers: *)
  let l =
    List.map (fun (name, typ) ->
      E.Ops.identifier name, typ
    ) state.external_identifiers in
  (* ...and already defined identifiers in the environment: *)
  let l =
    List.fold_left (fun l (name, _, t) ->
      (E.Ops.identifier name, t) :: l
    ) l state.identifiers in
  l

let identifier_of_expression state ?name expr =
  let name, public =
    match name with
    | None ->
        gen_sym (), false
    | Some name ->
        name, true in
  let identifier = { public ; expr } in
  let l = environment state in
  E.type_check l expr ;
  let t = E.type_of l expr in
  if t = TVoid then
    invalid_arg "identifier_of_expression of type void" ;
  { state with
      identifiers = (name, identifier, t) :: state.identifiers },
  E.E0 (Identifier name),
  name

let valid_source_name s = s

let preferred_def_extension = "dil"

let preferred_decl_extension = "dild"

let compile_cmd ~optim ~link _src _dst =
  ignore optim ; ignore link ;
  Printf.printf "Won't compile Desser Inetermediate Language (DIL).\n" ;
  "true"
