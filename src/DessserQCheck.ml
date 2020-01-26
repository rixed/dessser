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
        2, map2 (fun dim mn -> TVec (dim, mn)) (int_range 1 10) mn_gen ;
        2, map (fun mn -> TList mn) mn_gen ;
        2, map (fun mns -> TTup mns) (tiny_array mn_gen) ;
        2, map (fun fs -> TRec fs) (tiny_array (pair field_name_gen mn_gen)) ;
        1, map2 (fun k v -> TMap (k, v)) mn_gen mn_gen ] in
    frequency lst
  else
    map (fun mt -> Mac mt) mac_type_gen

and maybe_nullable_gen st =
  Gen.(fix (fun _self depth ->
    map2 (fun b vt ->
      if b then Nullable vt else NotNullable vt
    ) bool (value_type_gen depth)
  )) st

let maybe_nullable_gen =
  Gen.(sized_size (int_bound 4) maybe_nullable_gen)

let rec size_of_value_type = function
  | Mac _ | Usr _ -> 1
  | TVec (_, mn) | TList mn -> size_of_maybe_nullable mn
  | TTup typs ->
      Array.fold_left (fun s mn -> s + size_of_maybe_nullable mn) 0 typs
  | TRec typs ->
      Array.fold_left (fun s (_, mn) -> s + size_of_maybe_nullable mn) 0 typs
  | TMap (k, v) ->
      size_of_maybe_nullable k + size_of_maybe_nullable v

and size_of_maybe_nullable = function
  | Nullable vt | NotNullable vt -> size_of_value_type vt

let shrink_mac_type mt =
  let to_simplest =
    [ TString ; TFloat ;
      TI128 ; TU128 ; TI64 ; TU64 ; TI56 ; TU56 ; TI48 ; TU48 ; TI40 ; TU40 ;
      TI32 ; TU32 ; TI24 ; TU24 ; TI16 ; TU16 ; TI8 ; TU8 ; TChar ; TBool ] in
  let rec loop = function
    | [] -> Iter.empty
    | mt'::rest when mt' = mt ->
        if rest = [] then Iter.empty else Iter.of_list rest
    | _::rest ->
        loop rest in
  loop to_simplest

let rec shrink_value_type =
  let vt_of_mn = function NotNullable vt | Nullable vt -> vt
  in
  function
  | Mac mt ->
      (fun f ->
        shrink_mac_type mt (fun mt -> f (Mac mt)))
  | Usr _ ->
      Iter.empty
  | TVec (dim, mn) ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f (vt_of_mn mn) ;
          f (TVec (dim, mn))))
  | TList mn ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f (TList mn) ;
          f (vt_of_mn mn)))
  | TTup mns ->
      (fun f ->
        Array.iter (fun mn -> shrink_maybe_nullable mn (f % vt_of_mn)) mns ;
        let shrink_mns =
          Shrink.filter (fun mns -> Array.length mns > 1)
            (Shrink.array ~shrink:shrink_maybe_nullable) mns |>
          Iter.map (fun mns -> TTup mns) in
        shrink_mns f)
  | TRec mns ->
      (fun f ->
        Array.iter (fun (_, mn) -> shrink_maybe_nullable mn (f % vt_of_mn)) mns ;
        let shrink_mns =
          let shrink (fn, mn) =
            Iter.map (fun mn -> fn, mn) (shrink_maybe_nullable mn) in
          Shrink.filter (fun mns -> Array.length mns > 1)
            (Shrink.array ~shrink) mns |>
          Iter.map (fun mns -> TRec mns) in
        shrink_mns f)
  | TMap (k, v) ->
      (fun f ->
        shrink_maybe_nullable k (f % vt_of_mn) ;
        shrink_maybe_nullable v (f % vt_of_mn) ;
        let shrink_kv =
          (Shrink.pair shrink_maybe_nullable shrink_maybe_nullable) (k, v) |>
          Iter.map (fun (k, v) -> TMap (k, v)) in
        shrink_kv f)

and shrink_maybe_nullable = function
  | Nullable vt ->
      (fun f ->
        shrink_value_type vt (fun vt ->
          f (NotNullable vt) ;
          f (Nullable vt)))
  | NotNullable vt ->
      (fun f ->
        shrink_value_type vt (fun vt -> f (NotNullable vt)))

let maybe_nullable =
  let print = IO.to_string print_maybe_nullable
  and small = size_of_maybe_nullable
  and shrink = shrink_maybe_nullable in
  make ~print ~small ~shrink maybe_nullable_gen

(*$inject
   open Batteries
   module T = DessserTypes *)

(*$Q maybe_nullable & ~count:10_000
  maybe_nullable (fun mn -> \
    let str = IO.to_string T.print_maybe_nullable mn in \
    let mn' = T.Parser.maybe_nullable_of_string str in \
    T.eq mn' mn)
*)

let test () =
  Gen.generate ~n:3 maybe_nullable_gen |>
  List.iter (fun mn ->
    Printf.printf "%a\n" print_maybe_nullable mn)
