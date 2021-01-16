(* Backend for the Dessser Intermediate Language *)
open Batteries
open Dessser
module E = DessserExpressions
module T = DessserTypes
module U = DessserCompilationUnit

type T.backend_id += DIL

let id = DIL

(* The only place where we need explicit types in DIL are in strings for instance
 * in the null or list expressions. There, types are expressed in the syntax
 * printed/parsed by DessserTypes: *)
let type_identifier _p t =
  Printf.sprintf2 "%a" T.print t |>
  String.quote

let print_definitions oc compunit =
  (* Print in the order of definition: *)
  List.rev compunit.U.identifiers |>
  List.iter (fun (name, U.{ expr ; _ }, _) ->
    Format.(fprintf str_formatter "@[<hov 2>(define@ %s@ %a)@]"
      name
      E.pretty_print expr) ;
    Format.flush_str_formatter () |> String.print oc)

let print_declarations _oc _compunit =
  (* TODO: a header with all those types? *)
  ()

let print_comment oc fmt =
  Printf.fprintf oc ("; " ^^ fmt ^^ "\n")

let valid_source_name s = s

let preferred_def_extension = "dil"

let preferred_decl_extension = "dild"

let compile_cmd ~optim ~link _src _dst =
  ignore optim ; ignore link ;
  Printf.printf "Won't compile Desser Inetermediate Language (DIL).\n" ;
  "true"
