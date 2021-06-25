(* Paths are used to locate subfield types within compound types.
 * Head of the list is the index of the considered type child, then
 * the index of the grandchild, and so on.
 * Lists and vectors are entered but need only one index: 0, as all
 * indices share the same subtype. *)
open Batteries

open DessserTools
module T = DessserTypes
module E = DessserExpressions

type comp = CompTime of int | RunTime of E.t

let print_comp oc = function
  | CompTime i -> Int.print oc i
  | RunTime e -> Printf.fprintf oc "<%a>" (E.print ?max_depth:None) e

type t = comp list

(* FIXME: a data structure that can be appended/prepended/matched from both ends *)
let append i path = path @ [i]

let print oc p =
  List.print ~first:"/" ~last:"" ~sep:"/" print_comp oc p

let of_string s =
  if s = "" || s.[0] <> '/' then
    Printf.sprintf "%S" s |> invalid_arg ;
  String.lchop s |>
  String.split_on_char '/' |>
  List.map (fun i -> CompTime (int_of_string i))

let to_string = IO.to_string print

(* Return both the type and field name: *)
let type_and_name_of_path t0 path =
  let rec loop field_name t path =
    if t.T.typ = T.This then loop field_name t0 path else
    match path with
    | [] -> t, field_name
    | CompTime i :: path ->
        let rec type_of = function
          | T.(Unknown | Base _ | Ext _ | Map _ | Ref _ |
               Size | Bit | Byte | Word | DWord | QWord | OWord |
               Void | Ptr | Address | Bytes | Mask | Function _) ->
              assert false
          | This ->
              assert false (* Already handled above *)
          | Usr t ->
              type_of t.def
          | Vec (dim, mn) ->
              assert (i < dim) ;
              loop (string_of_int i) mn path
          | Lst mn | SList mn ->
              loop (string_of_int i) mn path
          | Set (_, mn) ->
              loop (string_of_int i) mn path
          | Tup mns ->
              assert (i < Array.length mns) ;
              loop ("field_"^ string_of_int i) mns.(i) path
          | Pair (mn1, mn2) ->
              assert (i < 2) ;
              let mn = if i = 0 then mn1 else mn2 in
              loop ("field_"^ string_of_int i) mn path
          | Rec mns ->
              assert (i < Array.length mns) ;
              loop (fst mns.(i)) (snd mns.(i)) path
          | Sum cs ->
              assert (i < Array.length cs) ;
              loop (fst cs.(i)) (snd cs.(i)) path in
        type_of t.typ
    | RunTime _ :: path ->
        (* For some of those types we can compute the subtype but not the
         * associated field name, which therefore must not be used.
         * Here it's given a caracteristic name for debugging purposes only: *)
        let no_fieldname = "__no_fieldname_at_runtime__" in
        let rec type_of = function
          | T.(Unknown | Base _ | Ext _ | Map _ | Ref _ |
               Size | Bit | Byte | Word | DWord | QWord | OWord |
               Void | Ptr | Address | Bytes | Mask | Function _) ->
              assert false
          | This ->
              assert false (* Already handled above *)
          | Usr t ->
              type_of t.def
          | Vec (_, mn) ->
              loop no_fieldname mn path
          | Lst mn | SList mn ->
              loop no_fieldname mn path
          | Set (_, mn) ->
              loop no_fieldname mn path
          | Tup _ | Rec _ | Sum _ | Pair _ ->
              invalid_arg "type_and_name_of_path on tup/rec/sum + runtime path"
        in
        type_of t.typ in
  loop "" t0 path

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

(*$= type_of_path & ~printer:(BatIO.to_string print_mn)
  test_t (type_of_path test_t [])
  (required (Base U8)) (type_of_path test_t [CompTime 0])
  (optional (Base String)) (type_of_path test_t [CompTime 1])
  (optional (Vec (2, required (Base Char)))) (type_of_path test_t [CompTime 2])
  (required (Base Char)) (type_of_path test_t [CompTime 2; CompTime 0])
*)
