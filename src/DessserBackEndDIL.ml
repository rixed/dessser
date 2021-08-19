(* Backend for the Dessser Intermediate Language *)
open Batteries

open Dessser
open DessserMiscTypes
module E = DessserExpressions
module P = DessserPrinter
module T = DessserTypes
module U = DessserCompilationUnit

let id = DIL

(* The only place where we need explicit types in DIL are in strings for instance
 * in the null or list expressions. There, types are expressed in the syntax
 * printed/parsed by DessserTypes: *)
let type_identifier _p t =
  Printf.sprintf2 "%a" T.print t |>
  String.quote

let type_identifier_mn _p t =
  Printf.sprintf2 "%a" T.print_mn t |>
  String.quote

let print_definitions oc compunit =
  (* Print in the order of definition: *)
  List.rev compunit.U.identifiers |>
  List.iter (function
    | name, U.{ expr = Some expr ; _ }, _ ->
        Format.(fprintf str_formatter "@[<hov 2>(define@ %s@ %a)@]"
          name
          (E.pretty_print ?max_depth:None) expr) ;
        Format.flush_str_formatter () |> String.print oc
    | name, U.{ expr = None ; _ }, _ ->
        invalid_arg ("print_definitions: missing definition for "^ name))

let print_declarations _oc _compunit =
  (* TODO: a header with all those types? *)
  ()

let print_comment oc fmt =
  Printf.fprintf oc ("; " ^^ fmt ^^ "\n")

let valid_source_name s = s

let preferred_def_extension = "dil"

let preferred_decl_extension = "dild"

let preferred_comp_extension _ =
  invalid_arg "DIL has no preferred_comp_extension"

let compile_cmd ?dev_mode ?extra_search_paths ?(optim=0) ~link _src _dst =
  ignore optim ; ignore link ; ignore dev_mode ; ignore extra_search_paths ;
  Printf.printf "Won't compile Desser Inetermediate Language (DIL).\n" ;
  "true"

let adapt_type t = t
