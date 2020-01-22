(* We might want to serialize into a value allocated on the heap.
 * The form of which depends on the backend of course, but every backend
 * must allow construction of arbitrarily nested data structures.
 * Of course, it is then also possible to serialize from a heap value
 * into anything else.
 *
 * For such a format, the pointers used to write/read during resp.
 * serialization/deserialization are unused. *)
open Batteries
open Dessser
open DessserTypes
open DessserExpressions

type heapvalue_state = path ref

let next path =
  match List.rev !path with
  | [] -> ()
  | i::rest -> path := List.rev ((i+1) :: rest)

let enter path =
  path := !path @ [0]

let leave path =
  match List.rev !path with
  | [] -> assert false
  | _::rest -> path := List.rev rest

module Ser : SER =
struct
  type state = heapvalue_state * maybe_nullable
  let ptr mtyp = valueptr mtyp

  (* [p] must ben a ValuePtr. *)
  let start mtyp p =
    (ref [], mtyp), p

  let stop (path, _) p =
    assert (!path = []) ;
    p

  type ser = state -> e -> e -> e

  let set_field (path_ref, mtyp) v p =
    let path = !path_ref in
    let v = if is_nullable (type_of_path mtyp path) then ToNullable v else v in
    SetField (path, p, v)

  let sfloat st id p = set_field st id p
  let sstring st id p = set_field st id p
  let sbool st id p = set_field st id p
  let si8 st id p = set_field st id p
  let si16 st id p = set_field st id p
  let si24 st id p = set_field st id p
  let si32 st id p = set_field st id p
  let si40 st id p = set_field st id p
  let si48 st id p = set_field st id p
  let si56 st id p = set_field st id p
  let si64 st id p = set_field st id p
  let si128 st id p = set_field st id p
  let su8 st id p = set_field st id p
  let su16 st id p = set_field st id p
  let su24 st id p = set_field st id p
  let su32 st id p = set_field st id p
  let su40 st id p = set_field st id p
  let su48 st id p = set_field st id p
  let su56 st id p = set_field st id p
  let su64 st id p = set_field st id p
  let su128 st id p = set_field st id p
  let schar st id p = set_field st id p

  let tup_opn (path, _) _ p = enter path ; p

  let tup_cls (path, _) p = leave path ; p

  let tup_sep _n (path, _) p = next path ; p

  let rec_opn (path, _) _ p = enter path ; p

  let rec_cls (path, _) p = leave path ; p

  let rec_sep _n (path, _) p = next path ; p

  let vec_opn (path, _) _ _ p = enter path ; p

  let vec_cls (path, _) p = leave path ; p

  let vec_sep _idx (path, _) p = next path ; p

  let list_opn (path, _) _ _ p = enter path ; p

  let list_cls (path, _) p = leave path ; p

  let list_sep (path, _) p = next path ; p

  let nullable _ p = p

  (* Do not update the path as it's done in the other branch of the alternative
   * (see note in dessser function). *)
  let snull t (path, _) p =
    SetField (!path, p, Null t)

  let snotnull _t _st p = p

  type ssizer = maybe_nullable -> path -> e -> ssize
  let todo_ssize () = failwith "TODO: ssize for HeapValue"
  let ssize_of_float _ _ _ = todo_ssize ()
  let ssize_of_string _ _ _ = todo_ssize ()
  let ssize_of_bool _ _ _ = todo_ssize ()
  let ssize_of_char _ _ _ = todo_ssize ()
  let ssize_of_i8 _ _ _ = todo_ssize ()
  let ssize_of_i16 _ _ _ = todo_ssize ()
  let ssize_of_i24 _ _ _ = todo_ssize ()
  let ssize_of_i32 _ _ _ = todo_ssize ()
  let ssize_of_i40 _ _ _ = todo_ssize ()
  let ssize_of_i48 _ _ _ = todo_ssize ()
  let ssize_of_i56 _ _ _ = todo_ssize ()
  let ssize_of_i64 _ _ _ = todo_ssize ()
  let ssize_of_i128 _ _ _ = todo_ssize ()
  let ssize_of_u8 _ _ _ = todo_ssize ()
  let ssize_of_u16 _ _ _ = todo_ssize ()
  let ssize_of_u24 _ _ _ = todo_ssize ()
  let ssize_of_u32 _ _ _ = todo_ssize ()
  let ssize_of_u40 _ _ _ = todo_ssize ()
  let ssize_of_u48 _ _ _ = todo_ssize ()
  let ssize_of_u56 _ _ _ = todo_ssize ()
  let ssize_of_u64 _ _ _ = todo_ssize ()
  let ssize_of_u128 _ _ _ = todo_ssize ()
  let ssize_of_tup _ _ _ = todo_ssize ()
  let ssize_of_rec _ _ _ = todo_ssize ()
  let ssize_of_vec _ _ _ = todo_ssize ()
  let ssize_of_list _ _ _ = todo_ssize ()
  let ssize_of_null _ _ = todo_ssize ()
end

module Des : DES =
struct
  type state = heapvalue_state * maybe_nullable
  let ptr mtyp = valueptr mtyp

  (* The pointer that's given to us must have been obtained from a
   * HeapValue.Ser. *)
  let start mtyp p =
    (ref [], mtyp), p

  let stop (path, _) p =
    assert (!path = []) ;
    p

  type des = state -> e -> e

  (* Beware that Dessser expect deserializers to return only not-nullables. *)
  let get_field (path_ref, mtyp) p =
    let path = !path_ref in
    let v = GetField (path, p) in
    let v = if is_nullable (type_of_path mtyp path) then ToNotNullable v else v in
    Pair (v, p)

  let dfloat st p = get_field st p
  let dstring st p = get_field st p
  let dbool st p = get_field st p
  let di8 st p = get_field st p
  let di16 st p = get_field st p
  let di24 st p = get_field st p
  let di32 st p = get_field st p
  let di40 st p = get_field st p
  let di48 st p = get_field st p
  let di56 st p = get_field st p
  let di64 st p = get_field st p
  let di128 st p = get_field st p
  let du8 st p = get_field st p
  let du16 st p = get_field st p
  let du24 st p = get_field st p
  let du32 st p = get_field st p
  let du40 st p = get_field st p
  let du48 st p = get_field st p
  let du56 st p = get_field st p
  let du64 st p = get_field st p
  let du128 st p = get_field st p
  let dchar st p = get_field st p

  let tup_opn (path, _) _ p = enter path ; p

  let tup_cls (path, _) p = leave path ; p

  let tup_sep _idx (path, _) p = next path ; p

  let rec_opn (path, _) _ p = enter path ; p

  let rec_cls (path, _) p = leave path ; p

  let rec_sep _n (path, _) p = next path ; p

  let vec_opn (path, _) _ _ p = enter path ; p

  let vec_cls (path, _) p = leave path ; p

  let vec_sep _ (path, _) p = next path ; p

  let list_opn (path, _ as st) _ p =
    enter path ;
    let lst = get_field st p in
    Pair (ListLength lst, p)

  let list_cls (path, _) p = leave path ; p

  let list_sep (path, _) p = next path ; p

  (* Will be called on every nullable fields before any attempt to deserialize
   * the value: *)
  let is_null (path, _) p =
    FieldIsNull (!path, p)

  (* Do not update the path as it's done in the other branch of the alternative
   * (see note in dessser function). *)
  let dnull _t _st p = p
  let dnotnull _t _st p = p
end

(* Module to compute the sersize of a heap value: *)
module SerSizer (Ser : SER) : sig
    val sersize : maybe_nullable -> (*dataptr*) e -> (*size*size*) e
  end =
struct
  (* Returns a pair of size identifier holding the const and dyn size of
   * the heap value pointed by the pointer identifier [src].
   * [src] must be a pointer to a heap value, as returned by the above
   * Ser module. *)
  let sersize mtyp src =
    let add_size sizes sz =
      MapPair (sizes,
        func [|size; size|] (fun fid ->
          match sz with
          | ConstSize s ->
              Pair (Add (Size s, Param (fid, 0)), Param (fid, 1))
          | DynSize s ->
              Pair (Param (fid, 0), Add (s, Param (fid, 1))))) in
    (* Add that value size to the passed size pair: *)
    let rec ssize_mtype path sizes v = function
      | Mac TFloat ->
          Ser.ssize_of_float mtyp path v |> add_size sizes
      | Mac TString ->
          Ser.ssize_of_string mtyp path v |> add_size sizes
      | Mac TBool ->
          Ser.ssize_of_bool mtyp path v |> add_size sizes
      | Mac TChar ->
          Ser.ssize_of_char mtyp path v |> add_size sizes
      | Mac TI8 ->
          Ser.ssize_of_i8 mtyp path v |> add_size sizes
      | Mac TI16 ->
          Ser.ssize_of_i16 mtyp path v |> add_size sizes
      | Mac TI24 ->
          Ser.ssize_of_i24 mtyp path v |> add_size sizes
      | Mac TI32 ->
          Ser.ssize_of_i32 mtyp path v |> add_size sizes
      | Mac TI40 ->
          Ser.ssize_of_i40 mtyp path v |> add_size sizes
      | Mac TI48 ->
          Ser.ssize_of_i48 mtyp path v |> add_size sizes
      | Mac TI56 ->
          Ser.ssize_of_i56 mtyp path v |> add_size sizes
      | Mac TI64 ->
          Ser.ssize_of_i64 mtyp path v |> add_size sizes
      | Mac TI128 ->
          Ser.ssize_of_i128 mtyp path v |> add_size sizes
      | Mac TU8 ->
          Ser.ssize_of_u8 mtyp path v |> add_size sizes
      | Mac TU16 ->
          Ser.ssize_of_u16 mtyp path v |> add_size sizes
      | Mac TU24 ->
          Ser.ssize_of_u24 mtyp path v |> add_size sizes
      | Mac TU32 ->
          Ser.ssize_of_u32 mtyp path v |> add_size sizes
      | Mac TU40 ->
          Ser.ssize_of_u40 mtyp path v |> add_size sizes
      | Mac TU48 ->
          Ser.ssize_of_u48 mtyp path v |> add_size sizes
      | Mac TU56 ->
          Ser.ssize_of_u56 mtyp path v |> add_size sizes
      | Mac TU64 ->
          Ser.ssize_of_u64 mtyp path v |> add_size sizes
      | Mac TU128 ->
          Ser.ssize_of_u128 mtyp path v |> add_size sizes
      | Usr t ->
          ssize_mtype path sizes v t.def
      (* Compound types require recursion: *)
      | TTup mtyps ->
          let sizes =
            Ser.ssize_of_tup mtyp path v |> add_size sizes in
          Array.fold_lefti (fun sizes i _ ->
            Let ("sizes", sizes, sersize_ (path @ [i]) src (Identifier "sizes"))
          ) sizes mtyps
      | TRec mtyps ->
          let sizes =
            Ser.ssize_of_rec mtyp path v |> add_size sizes in
          Array.fold_lefti (fun sizes i _ ->
            sersize_ (path @ [i]) src sizes
          ) sizes mtyps
      | TVec (dim, _) ->
          let sizes =
            Ser.ssize_of_vec mtyp path v |> add_size sizes in
          let rec loop sizes i =
            if i >= dim then sizes else
            let sizes = sersize_ (path @ [i]) src sizes in
            loop sizes (i + 1) in
          loop sizes 0
      | TList _typ ->
          assert false
      | TMap _ ->
          assert false (* no value of map type *)

    and sersize_ path src sizes =
      let sub_mtyp = type_of_path mtyp path in
      if is_nullable sub_mtyp then
        let comment =
          Printf.sprintf2 "sersize of path %a" print_path path in
        Comment (comment,
          Choose (FieldIsNull (path, src),
            add_size sizes (Ser.ssize_of_null sub_mtyp path),
            let sub_mtyp' = to_value_type sub_mtyp in
            ssize_mtype path sizes (ToNotNullable (GetField (path, src))) sub_mtyp'))
      else
        let sub_mtyp' = to_value_type sub_mtyp in
        ssize_mtype path sizes (GetField (path, src)) sub_mtyp'
    in
    let sizes = Pair (Size 0, Size 0) in
    sersize_ [] src sizes
end
