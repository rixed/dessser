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

module SerData = SerDataBytes0.Raw
module IntRepr = IntReprOCaml0.Make (SerData)
module SExpr = SExpr0.Make (IntRepr)

let () =
  let die msg =
    Printf.eprintf "%s" msg ;
    exit 1 in
  if Array.length Sys.argv < 2 then
    die "argument must be an expression" ;

  let user_expr = Sys.argv.(1) in
  Printf.printf "Pretty printing: %s\n" user_expr ;

  (* Just output something for now: *)
  let p = SerDataBytes0.Raw.make_buffer 100 in
  let v =
    IntRepr.(
      VTuple [
        VBool (boolv_of_const true) ;
        VI8 (i8v_of_const 42) ;
        VVec (arr_of_const [
          VI8 (i8v_of_const 1) ;
          VI8 (i8v_of_const 2) ;
          VI8 (i8v_of_const 3) ]) ]) in
  let p = SExpr.ser p v in
  Printf.printf "Result: %s\n" (Bytes.sub_string p.data 0 p.offset)
