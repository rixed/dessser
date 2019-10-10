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

(* For dynlink to succeed, as it will call functions from SerDataBytes0: *)
module MustBeLinkedIn = SerDataBytes0
module SerData = SerDataBytes
module IntRepr = IntReprOCaml
module Desser = Dessert.DesSer (SExpr.MakeDes (IntRepr)) (SExpr.MakeSer (IntRepr))

let parse_user_expr typ user_expr =
  let src = SerData.of_string .<user_expr>.
  and dst = SerData.make_buffer .<500>. in
  let c = Desser.desser typ in
  Format.printf "OCaml code to print the value: %a@."
    Codelib.print_code c ;
  let c =
    .<
      let src, dst = .~c (.~src, .~dst) in
      SerDataBytes0.print dst ;
      let unparsed = Bytes.length src.SerDataBytes0.data - src.offset in
      if unparsed > 0 then
        Format.printf "Warning: %d bytes of garbage at the end of user input@."
          unparsed
    >. in
  Runnative.run c

let () =
  let die msg =
    Printf.eprintf "%s" msg ;
    exit 1 in
  if Array.length Sys.argv < 2 then
    die "argument must be an expression" ;

  Runnative.add_search_path "src" ;

  let user_expr = Sys.argv.(1) in
  Format.printf "Pretty printing %S...@." user_expr ;

  let typ =
    let open Dessert in
    (*TBool*)
    (*TI8*)
    (*TTup [| TBool ; TI8 |]*)
    TTup [| TBool ; TI8 ; TVec (3, TI8) |]
  in
  try
    parse_user_expr typ user_expr
  with Dynlink.Error e ->
    Format.printf "Linking error: %s@." (Dynlink.error_message e)
