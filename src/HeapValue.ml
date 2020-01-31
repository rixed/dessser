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
open Ops
module T = DessserTypes

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
  let ptr vtyp = valueptr vtyp

  (* [p] must ben a ValuePtr. *)
  let start vtyp p =
    (ref [], vtyp), p

  let stop (path, _) p =
    assert (!path = []) ;
    p

  type ser = state -> e -> e -> e

  let set_field_ (path_ref, vtyp) v p =
    let path = !path_ref in
    let v = if is_nullable (type_of_path vtyp path) then to_nullable v else v in
    set_field path p v

  let sfloat st id p = set_field_ st id p
  let sstring st id p = set_field_ st id p
  let sbool st id p = set_field_ st id p
  let si8 st id p = set_field_ st id p
  let si16 st id p = set_field_ st id p
  let si24 st id p = set_field_ st id p
  let si32 st id p = set_field_ st id p
  let si40 st id p = set_field_ st id p
  let si48 st id p = set_field_ st id p
  let si56 st id p = set_field_ st id p
  let si64 st id p = set_field_ st id p
  let si128 st id p = set_field_ st id p
  let su8 st id p = set_field_ st id p
  let su16 st id p = set_field_ st id p
  let su24 st id p = set_field_ st id p
  let su32 st id p = set_field_ st id p
  let su40 st id p = set_field_ st id p
  let su48 st id p = set_field_ st id p
  let su56 st id p = set_field_ st id p
  let su64 st id p = set_field_ st id p
  let su128 st id p = set_field_ st id p
  let schar st id p = set_field_ st id p

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
    set_field !path p (null t)

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
  let ptr vtyp = valueptr vtyp

  (* The pointer that's given to us must have been obtained from a
   * HeapValue.Ser. *)
  let start vtyp p =
    (ref [], vtyp), p

  let stop (path, _) p =
    assert (!path = []) ;
    p

  type des = state -> e -> e

  (* Beware that Dessser expect deserializers to return only not-nullables. *)
  let get_field (path_ref, vtyp) p =
    let path = !path_ref in
    let v = get_field path p in
    let v =
      if is_nullable (type_of_path vtyp path) then to_not_nullable v else v in
    pair v p

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

  let list_opn = KnownSize (fun (path, _ as st) _ p ->
    enter path ;
    let lst = get_field st p in
    pair (list_length lst) p)

  let list_cls (path, _) p = leave path ; p

  let list_sep (path, _) p = next path ; p

  (* Will be called on every nullable fields before any attempt to deserialize
   * the value: *)
  let is_null (path, _) p =
    field_is_null !path p

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
  let sersize vtyp src =
    let add_size sizes sz =
      map_pair sizes
        (func2 T.size T.size (fun s1 s2 ->
          match sz with
          | ConstSize s ->
              pair (add (size s) s1) s2
          | DynSize s ->
              pair s1 (add s s2))) in
    (* Add that value size to the passed size pair: *)
    let rec ssize_vtype path sizes v = function
      | Mac TFloat ->
          Ser.ssize_of_float vtyp path v |> add_size sizes
      | Mac TString ->
          Ser.ssize_of_string vtyp path v |> add_size sizes
      | Mac TBool ->
          Ser.ssize_of_bool vtyp path v |> add_size sizes
      | Mac TChar ->
          Ser.ssize_of_char vtyp path v |> add_size sizes
      | Mac TI8 ->
          Ser.ssize_of_i8 vtyp path v |> add_size sizes
      | Mac TI16 ->
          Ser.ssize_of_i16 vtyp path v |> add_size sizes
      | Mac TI24 ->
          Ser.ssize_of_i24 vtyp path v |> add_size sizes
      | Mac TI32 ->
          Ser.ssize_of_i32 vtyp path v |> add_size sizes
      | Mac TI40 ->
          Ser.ssize_of_i40 vtyp path v |> add_size sizes
      | Mac TI48 ->
          Ser.ssize_of_i48 vtyp path v |> add_size sizes
      | Mac TI56 ->
          Ser.ssize_of_i56 vtyp path v |> add_size sizes
      | Mac TI64 ->
          Ser.ssize_of_i64 vtyp path v |> add_size sizes
      | Mac TI128 ->
          Ser.ssize_of_i128 vtyp path v |> add_size sizes
      | Mac TU8 ->
          Ser.ssize_of_u8 vtyp path v |> add_size sizes
      | Mac TU16 ->
          Ser.ssize_of_u16 vtyp path v |> add_size sizes
      | Mac TU24 ->
          Ser.ssize_of_u24 vtyp path v |> add_size sizes
      | Mac TU32 ->
          Ser.ssize_of_u32 vtyp path v |> add_size sizes
      | Mac TU40 ->
          Ser.ssize_of_u40 vtyp path v |> add_size sizes
      | Mac TU48 ->
          Ser.ssize_of_u48 vtyp path v |> add_size sizes
      | Mac TU56 ->
          Ser.ssize_of_u56 vtyp path v |> add_size sizes
      | Mac TU64 ->
          Ser.ssize_of_u64 vtyp path v |> add_size sizes
      | Mac TU128 ->
          Ser.ssize_of_u128 vtyp path v |> add_size sizes
      | Usr t ->
          ssize_vtype path sizes v t.def
      (* Compound types require recursion: *)
      | TTup vtyps ->
          let sizes =
            Ser.ssize_of_tup vtyp path v |> add_size sizes in
          Array.fold_lefti (fun sizes i _ ->
            let_ "sizes" sizes (sersize_ (path @ [i]) src (identifier "sizes"))
          ) sizes vtyps
      | TRec vtyps ->
          let sizes =
            Ser.ssize_of_rec vtyp path v |> add_size sizes in
          Array.fold_lefti (fun sizes i _ ->
            let_ "sizes" sizes (sersize_ (path @ [i]) src (identifier "sizes"))
          ) sizes vtyps
      | TVec (dim, _) ->
          let sizes =
            Ser.ssize_of_vec vtyp path v |> add_size sizes in
          let rec loop sizes i =
            if i >= dim then sizes else
            let sizes =
              let_ "sizes" sizes (sersize_ (path @ [i]) src (identifier "sizes")) in
            loop sizes (i + 1) in
          loop sizes 0
      | TList _typ ->
          assert false
      | TMap _ ->
          assert false (* no value of map type *)

    and sersize_ path src sizes =
      let sub_vtyp = type_of_path vtyp path in
      if is_nullable sub_vtyp then
        comment
          (Printf.sprintf2 "sersize of path %a" print_path path)
          (choose ~cond:(field_is_null path src)
            (add_size sizes (Ser.ssize_of_null sub_vtyp path))
            (let sub_vtyp' = to_value_type sub_vtyp in
            ssize_vtype path sizes (to_not_nullable (get_field path src)) sub_vtyp'))
      else
        let sub_vtyp' = to_value_type sub_vtyp in
        ssize_vtype path sizes (get_field path src) sub_vtyp'
    in
    let sizes = pair (size 0) (size 0) in
    sersize_ [] src sizes
end
