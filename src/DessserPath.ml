(* Paths are used to locate subfield types within compound types.
 * Head of the list is the index of the considered type child, then
 * the index of the grandchild, and so on.
 * Lists and vectors are entered but need only one index: 0, as all
 * indices share the same subtype. *)
open Batteries

open DessserTools
module T = DessserTypes
module E = DessserExpressions

type t = int list

(* FIXME: a data structure that can be appended/prepended/matched from both ends *)
let append i path = path @ [i]

let print oc p =
  List.print ~first:"/" ~last:"" ~sep:"/" Int.print oc p

let of_string s =
  if s = "" || s.[0] <> '/' then
    Printf.sprintf "%S" s |> invalid_arg ;
  String.lchop s |>
  String.split_on_char '/' |>
  List.map int_of_string

let to_string = IO.to_string print

(* Return both the type and field name: *)
let type_and_name_of_path t path =
  let rec loop field_name t = function
    | [] -> t, field_name
    | i :: path ->
        let rec type_of = function
          | T.(Unknown | Base _ | Ext _ | Map _) ->
              assert false
          | Usr t ->
              type_of t.def
          | Vec (dim, mn) ->
              assert (i < dim) ;
              loop (string_of_int i) mn path
          | Lst mn ->
              loop (string_of_int i) mn path
          | Set (_, mn) ->
              loop (string_of_int i) mn path
          | Tup mns ->
              assert (i < Array.length mns) ;
              loop ("field_"^ string_of_int i) mns.(i) path
          | Rec mns ->
              assert (i < Array.length mns) ;
              loop (fst mns.(i)) (snd mns.(i)) path
          | Sum cs ->
              assert (i < Array.length cs) ;
              loop (fst cs.(i)) (snd cs.(i)) path in
        type_of t.vtyp in
  loop "" t path

let type_of_path mn path =
  fst (type_and_name_of_path mn path)

let type_of_parent mn path =
  let path = list_drop_last path in
  type_of_path mn path

(*$inject
  open DessserTypes
  let test_t =
    required (Tup [|
      required (Base U8) ;
      optional (Base String) ;
      optional (Vec (2, required (Base Char))) |])
*)

(*$= type_of_path & ~printer:(BatIO.to_string print_maybe_nullable)
  test_t (type_of_path test_t [])
  (required (Base U8)) (type_of_path test_t [0])
  (optional (Base String)) (type_of_path test_t [1])
  (optional (Vec (2, required (Base Char)))) (type_of_path test_t [2])
  (required (Base Char)) (type_of_path test_t [2; 0])
*)
