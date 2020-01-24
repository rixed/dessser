(* Random genrator for types and expressions *)
open Batteries
open Stdint
open DessserTypes
open QCheck

let mac_type_gen =
  Gen.sized (fun n _st ->
    match n mod 22 with
    | 0 -> TFloat
    | 1 -> TString
    | 2 -> TBool
    | 3 -> TChar
    | 4 -> TU8
    | 5 -> TU16
    | 6 -> TU24
    | 7 -> TU32
    | 8 -> TU40
    | 9 -> TU48
    | 10 -> TU56
    | 11 -> TU64
    | 12 -> TU128
    | 13 -> TI8
    | 14 -> TI16
    | 15 -> TI24
    | 16 -> TI32
    | 17 -> TI40
    | 18 -> TI48
    | 19 -> TI56
    | 20 -> TI64
    | 21 -> TI128
    | _ -> assert false)

let user_type_gen =
  (* This module is linked after DessserTypes and therefore is initialized after
   * it, so it is OK to get default user types now: *)
  let user_type_keys = Hashtbl.keys user_types |> Array.of_enum in
  Gen.(sized (fun n _st ->
    let k = user_type_keys.(n mod Array.length user_type_keys) in
    Hashtbl.find user_types k))

let tiny_array gen =
  let open Gen in
  array_size (int_range 1 5) gen

let field_name_gen =
  let open Gen in
  let all_chars = "abcdefghijklmnopqrstuvwxyz" in
  let gen = map (fun n -> all_chars.[n mod String.length all_chars]) nat in
  string_size ~gen (int_range 5 12)

let rec value_type_gen depth =
  let open Gen in
  if depth > 0 then
    let mn_gen = maybe_nullable_gen (depth - 1) in
    let lst =
      [ 4, map (fun mt -> Mac mt) mac_type_gen ;
        1, map (fun ut -> Usr ut) user_type_gen ;
        2, map2 (fun dim mn -> TVec (dim, mn)) nat mn_gen ;
        2, map (fun mn -> TList mn) mn_gen ;
        2, map (fun mns -> TTup mns) (tiny_array mn_gen) ;
        2, map (fun fs -> TRec fs) (tiny_array (pair field_name_gen mn_gen)) ;
        1, map2 (fun k v -> TMap (k, v)) mn_gen mn_gen ] in
    frequency lst
  else
    map (fun mt -> Mac mt) mac_type_gen

and maybe_nullable_gen st =
  Gen.(fix (fun _self depth ->
    map2 (fun b mn ->
      if b then Nullable mn else NotNullable mn
    ) bool (value_type_gen depth)
  )) st

let test () =
  Gen.generate ~n:3 (maybe_nullable_gen 3) |>
  List.iter (fun mn ->
    Printf.printf "%a\n" print_maybe_nullable mn)
