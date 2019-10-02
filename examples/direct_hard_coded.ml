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

module SerData0 = SerDataBytes0.Raw
module IntRepr0 = IntReprOCaml0.Make (SerData0)
module SExpr00 = SExpr0.Make (IntRepr0)

let print_sexpr _user_expr =
  (* Just output something for now: *)
  let p = SerData0.make_buffer 100 in
  let v =
    IntRepr0.(
      VTuple (IntRepr0.lengthv_of_const 3,
              IntRepr0.tuplev_of_const [
                VBool (boolv_of_const true) ;
                VI8 (i8v_of_const 42) ;
                VVec (IntRepr0.lengthv_of_const 3,
                      vecv_of_const [
                        VI8 (i8v_of_const 1) ;
                        VI8 (i8v_of_const 2) ;
                        VI8 (i8v_of_const 3) ]) ])) in
  let p = SExpr00.ser p v in
  Format.printf "Result: %s@." (Bytes.sub_string p.data 0 p.offset)

module IntRepr1 = IntReprOCaml1.Raw (SerData0)
module SerData1 = IntRepr1.SerData
module SExpr01 = SExpr0.Make (IntRepr1)

let print_sexpr_ocaml_code _user_expr =
  let p = SerData1.make_buffer .<100>. in
  let v =
    IntRepr1.(
      VTuple (IntRepr1.lengthv_of_const 3,
              IntRepr1.tuplev_of_const [
                VBool (boolv_of_const true) ;
                VI8 (i8v_of_const 42) ;
                VVec (IntRepr1.lengthv_of_const 3,
                      vecv_of_const [
                        VI8 (i8v_of_const 1) ;
                        VI8 (i8v_of_const 2) ;
                        VI8 (i8v_of_const 3) ]) ])) in
  let p = SExpr01.ser p v in
  Format.printf "OCaml code producing that value: %a@."
    Codelib.format_code (Codelib.close_code p)

let () =
  let die msg =
    Printf.eprintf "%s" msg ;
    exit 1 in
  if Array.length Sys.argv < 2 then
    die "argument must be an expression" ;

  let user_expr = Sys.argv.(1) in
  Printf.printf "Pretty printing: %s\n" user_expr ;

  print_sexpr user_expr ;
  print_sexpr_ocaml_code user_expr
