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
module SerDes = SExpr.Make (IntRepr)

(*
let print_sexpr v =
  (* Just output something for now: *)
  let p = SerData0.make_buffer 100 in
(*  let v =
    IntRepr0.(VTuple (tupv_of_const [
      VBool (boolv_of_const true) ;
      VI8 (i8v_of_const 42) ;
      VVec (vecv_of_const [
        VI8 (i8v_of_const 1) ;
        VI8 (i8v_of_const 2) ;
        VI8 (i8v_of_const 3) ]) ]))  in *)
  let p = SExpr0.ser p v in
  Format.printf "Serialized: %s@." (Bytes.sub_string p.data 0 p.offset)

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
  let p = SExpr1.ser p v in
  Format.printf "OCaml code producing the value: %a@."
    Codelib.print_code p ;
  let p = Runnative.run p in
  Format.printf "Serialized: %s@." (Bytes.sub_string p.data 0 p.offset)
*)
let typ =
  let open BerSerdes in
  TTuple [| TBool ; TI8 |]
  (*TTuple [| TBool ; TI8 ; TVec (3, TI8) |]*)

let parse_user_expr user_expr =
  let pc = SerData.of_string .<user_expr>. in
  let v_p = SerDes.des typ pc in
  match v_p with
  | VPBool c ->
      let c =
        .<
          SerDataBytes0.print Format.pp_print_bool .~c
        >. in
      Format.printf "OCaml code producing a bool value: %a@."
        Codelib.print_code c ;
      Runnative.run c
  | VPI8 c ->
      let c =
        .<
          SerDataBytes0.print Format.pp_print_int .~c
        >. in
      Format.printf "OCaml code producing a i8 value: %a@."
        Codelib.print_code c ;
      Runnative.run c
  | _ -> assert false


let () =
  let die msg =
    Printf.eprintf "%s" msg ;
    exit 1 in
  if Array.length Sys.argv < 2 then
    die "argument must be an expression" ;

  Runnative.add_search_path "src" ;

  let user_expr = Sys.argv.(1) in
  Format.printf "Pretty printing %S...@." user_expr ;

  try
    parse_user_expr user_expr
  with Dynlink.Error e ->
    Format.printf "Linking error: %s@." (Dynlink.error_message e)
  (*print_sexpr_ocaml_code user_val*)
