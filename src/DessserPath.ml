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
    | TNamed (_, t), _ ->
        loop index { mn with typ = t } path
    (* Handle This before the end of path so we do not end on a "this": *)
    | TThis n, _ ->
        let t = T.find_this n in
        loop index { mn with typ = t } path
    | TUsr { def ; _ }, _ ->
        loop index { mn with typ = def } path
    | _, [] ->
        mn, index
    | (TUnknown | TExt _ | TMap _ | TBool | TChar | TFloat | TString |
       TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 |
       TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
       TSize | TVoid | TPtr | TAddress | TBytes | TMask | TFunction _), _ ->
        assert false
    (* CompTime *)
    | TVec (dim, mn), CompTime i :: path  ->
        assert (i < dim) ;
        loop i mn path
    | (TArr mn | TLst mn | TSet (_, mn)), CompTime i :: path  ->
        loop i mn path
    | TTup mns, CompTime i :: path  ->
        assert (i < Array.length mns) ;
        loop i mns.(i) path
    | TRec mns, CompTime i :: path  ->
        assert (i < Array.length mns) ;
        loop i (snd mns.(i)) path
    | TSum cs, CompTime i :: path  ->
        assert (i < Array.length cs) ;
        loop i (snd cs.(i)) path
    (* RunTime *)
    (* For some of those types we can compute the subtype but not the
     * associated field index, which therefore must not be used.
     * Here it's given a characteristic value for debugging purposes only: *)
    | (TVec (_, mn) | TArr mn | TLst mn | TSet (_, mn)), RunTime _ :: path ->
        loop no_index mn path
    | (TTup _ | TRec _ | TSum _), RunTime _ :: _ ->
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
    required (tup [|
      u8 ;
      nstring ;
      optional (vec 2 char) |])
*)

(*$= type_of_path & ~printer:(BatIO.to_string print_mn)
  test_t (type_of_path test_t [])
  u8 (type_of_path test_t [CompTime 0])
  nstring (type_of_path test_t [CompTime 1])
  (optional (vec 2 char)) (type_of_path test_t [CompTime 2])
  char (type_of_path test_t [CompTime 2; CompTime 0])
*)
