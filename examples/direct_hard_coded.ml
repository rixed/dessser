(* Simplest possible case of a deser from ocaml to sexpr and back, for an hardcoded
 * format.
 * This program takes an s-expressions of the given format on the command line,
 * and pretty print it.
 * The s-expression hardcoded format is:
 *
 * ( bool i8 ( i8 i8 i8 ) )
 *
 * ie a bool, an i8, then an array of 3 i8s.
 *)
open Batteries

(* For dynlink to succeed, make sure those modules are linked in: *)
let () =
  let _ = SerDataBytes0.make_buffer 0 in
  let _ = Stdint.Uint8.of_int 0 in
  let _ = BatOption.may ignore None in
  ()

module SerData = SerDataBytes
module Desser = Dessert.DesSer (RowBinary.Des) (SExpr.Ser (*DevNull.Ser*))

(*
module IRConverter =
struct
  open OffshoringIR
  include DefaultConv

  let id_conv pathname varname =
    Format.printf "id_conv %s %s@." pathname varname ;
    match pathname, varname with
    | "SerDataBytes0", "peek_byte" ->
        "foobar"
    | _ ->
        id_conv pathname varname
end
*)
let parse_user_expr typ =
  let c = Desser.desser typ in
  Format.printf "OCaml code to print the value: %a@."
    Codelib.print_code c ;
(*  (* Try offshoring: *)
  (match OffshoringIR.(offshore (module IRConverter)) c with
  | exception e ->
      Format.printf "Cannot offshore: %s@."
        (Printexc.to_string e)
  | _ ->
      Format.printf "Could offshore just fine.@.") *)
  let c =
    .<
      let rec loop_tuples src =
        if SerDataBytes0.rem src > 0 then (
          let dst = SerDataBytes0.make_buffer 5_000 in
          let src, dst = .~c (src, dst) in
          SerDataBytes0.print dst ;
          loop_tuples src
        ) in
      Printf.printf "Reading data...\n%!" ;
      let user_expr = IO.read_all stdin in
      let src = SerDataBytes0.of_string user_expr in
      Printf.printf "starting...\n%!" ;
      loop_tuples src
    >. in
  Runnative.run c

let search_path_of name =
  let cmd = Printf.sprintf "ocamlfind query %S" name in
  let _, path = Unix.run_and_read cmd in
  let path = String.trim path in
  Format.printf "Will look for %S in %S@.%!" name path ;
  path

let () =
  Runnative.add_search_path "src" ;
  Runnative.add_search_path (search_path_of "batteries") ;
  Runnative.add_search_path (search_path_of "stdint") ;

  let typ =
    let open Dessert in
    (*TBool*)
    (*TI8*)
    (*TTup [| TBool ; TI8 |]*)
    (*TTup [| TBool ; TI8 ; TVec (3, TI8) |]*)
    let nullable = true in
    Type.(make  (TTup [|
      make TString ;
      make TU64 ;
      make TU64 ;
      make TU8 ;
      make TString ;
      make TU8 ;
      make TString ;
      make ~nullable TU32 ;
      make ~nullable TU32 ;
      make TU64 ;
      make TU64 ;
      make TU32 ;
      make TU32 ;
      make ~nullable TU32 ;
      make ~nullable TString ;
      make ~nullable TU32 ;
      make ~nullable TString ;
      make ~nullable TU32 ;
      make ~nullable TString ;
      make TU16 ;
      make TU16 ;
      make TU8 ;
      make TU8 ;
      make ~nullable TU32 ;
      make ~nullable TU32 ;
      make TU32 ;
      make TString ;
      make TU64 ;
      make TU64 ;
      make TU64 ;  (* SHould be U32 *)
      make TU64 ;  (* SHould be U32 *)
      make TU64 ;
      make TU64 ;
      make ~nullable TString
    |]))
  in
  try
    parse_user_expr typ
  with Dynlink.Error e ->
    Format.printf "Linking error: %s@." (Dynlink.error_message e)
