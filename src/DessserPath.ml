(* Paths are used to locate subfield types within compound types.
 * Head of the list is the index of the considered type child, then
 * the index of the grandchild, and so on.
 * Lists and vectors are entered but need only one index: 0, as all
 * indices share the same subtype.
 * The index of any component can be known either at compile time or
 * only run time but in both cases they are integers.
 * It is to be expected that all component of any given path will either be
 * known at compile time or run time. *)
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

(* Return both the type (never "this") and field name: *)
let type_and_index_of_path t0 path =
  let no_index = ~-7 in
  let rec loop index mn path =
    match mn.T.typ, path with
    | Named (_, t), _ ->
        loop index { mn with typ = t } path
    (* Handle This before the end of path so we do not end on a "this": *)
    | This n, _ ->
        let t = T.find_this n in
        loop index { mn with typ = t } path
    | _, [] ->
        mn, index
    | T.(Unknown | Base _ | Ext _ | Map _ |
         Size |
         Void | Ptr | Address | Bytes | Mask | Function _), _ ->
        assert false
    | Usr { def ; _ }, _ ->
        loop index { mn with typ = def } path
    (* CompTime *)
    | Vec (dim, mn), CompTime i :: path  ->
        assert (i < dim) ;
        loop i mn path
    | (Arr mn | Lst mn | Set (_, mn)), CompTime i :: path  ->
        loop i mn path
    | Tup mns, CompTime i :: path  ->
        assert (i < Array.length mns) ;
        loop i mns.(i) path
    | Rec mns, CompTime i :: path  ->
        assert (i < Array.length mns) ;
        loop i (snd mns.(i)) path
    | Sum cs, CompTime i :: path  ->
        assert (i < Array.length cs) ;
        loop i (snd cs.(i)) path
    (* RunTime *)
    (* For some of those types we can compute the subtype but not the
     * associated field index, which therefore must not be used.
     * Here it's given a characteristic value for debugging purposes only: *)
    | (Vec (_, mn) | Arr mn | Lst mn | Set (_, mn)), RunTime _ :: path ->
        loop no_index mn path
    | (Tup _ | Rec _ | Sum _), RunTime _ :: _ ->
        invalid_arg "type_and_index_of_path on tup/rec/sum + runtime path"
  in
  loop no_index t0 path

let type_of_path mn path =
  fst (type_and_index_of_path mn path)

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
