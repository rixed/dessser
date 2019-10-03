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

module SerData0 = SerDataBytes0
module IntRepr0 = IntReprOCaml0
module SExpr0 = SExpr.Make (IntRepr0)

let print_sexpr _user_expr =
  (* Just output something for now: *)
  let p = SerData0.make_buffer 100 in
  let v =
    IntRepr0.(VTuple (tupv_of_const [
      VBool (boolv_of_const true) ;
      VI8 (i8v_of_const 42) ;
      VVec (vecv_of_const [
        VI8 (i8v_of_const 1) ;
        VI8 (i8v_of_const 2) ;
        VI8 (i8v_of_const 3) ]) ]))  in
  SExpr0.ser p v

module SerData1 = SerDataBytes1
module IntRepr1 = IntReprOCaml1
module SExpr1 = SExpr.Make (IntRepr1)

let print_sexpr_ocaml_code _user_expr =
  let p = SerData1.make_buffer .<100>. in
  let v =
    IntRepr1.(VTuple (tupv_of_const [
       VBool (boolv_of_const true) ;
       VI8 (i8v_of_const 42) ;
       VVec (vecv_of_const [
         VI8 (i8v_of_const 1) ;
         VI8 (i8v_of_const 2) ;
         VI8 (i8v_of_const 3) ]) ])) in
  let seq = SExpr1.ser p v in
  Format.printf "OCaml code producing that value: %a@."
    Codelib.print_code seq ;
  Runnative.add_search_path "src" ;
  Runnative.run seq

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
