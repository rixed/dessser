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

module Ser : SER =
struct
  type state = unit
  let ptr mn = valueptr mn

  (* [p] must ben a ValuePtr. *)
  let start _mn0 p = (), p

  let stop () p = p

  type ser = state -> maybe_nullable -> path -> e -> e -> e

  let sfield mn0 path v p =
    let v =
      if is_nullable (type_of_path mn0 path) then
        to_nullable v
      else v in
    (* FIXME: same as in gfield, path is wrong within lists! *)
    set_field path p v

  let sfloat () = sfield
  let sstring () = sfield
  let sbool () = sfield
  let si8 () = sfield
  let si16 () = sfield
  let si24 () = sfield
  let si32 () = sfield
  let si40 () = sfield
  let si48 () = sfield
  let si56 () = sfield
  let si64 () = sfield
  let si128 () = sfield
  let su8 () = sfield
  let su16 () = sfield
  let su24 () = sfield
  let su32 () = sfield
  let su40 () = sfield
  let su48 () = sfield
  let su56 () = sfield
  let su64 () = sfield
  let su128 () = sfield
  let schar () = sfield

  let tup_opn () _ _ _ p = p

  let tup_cls () _ _ p = p

  let tup_sep _n () _ _ p = p

  let rec_opn () _ _ _ p = p

  let rec_cls () _ _ p = p

  let rec_sep _n () _ _ p = p

  let vec_opn () _ _ _ _ p = p

  let vec_cls () _ _ p = p

  let vec_sep _n () _ _ p = p

  let list_opn () _ _ _ _ p = p

  let list_cls () _ _ p = p

  let list_sep () _ _ p = p

  let nullable () _ _ p = p

  (* Do not update the path as it's done in the other branch of the alternative
   * (see note in dessser function). *)
  let snull t () _ path p =
    set_field path p (null t)

  let snotnull _t () _ _ p = p

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
  type state = unit
  let ptr mn = valueptr mn

  (* The pointer that's given to us must have been obtained from a
   * HeapValue.Ser. *)
  let start _mn0 p = (), p

  let stop () p = p

  type des = state -> maybe_nullable -> path -> e -> e

  (* Beware that Dessser expect deserializers to return only not-nullables. *)
  let dfield mn0 path p =
    (* FIXME: get_field won't work within lists because the path index is then
     * always 0! *)
    let v = get_field path p in
    let v =
      if is_nullable (type_of_path mn0 path) then
        to_not_nullable v
      else v in
    pair v p

  let dfloat () = dfield
  let dstring () = dfield
  let dbool () = dfield
  let di8 () = dfield
  let di16 () = dfield
  let di24 () = dfield
  let di32 () = dfield
  let di40 () = dfield
  let di48 () = dfield
  let di56 () = dfield
  let di64 () = dfield
  let di128 () = dfield
  let du8 () = dfield
  let du16 () = dfield
  let du24 () = dfield
  let du32 () = dfield
  let du40 () = dfield
  let du48 () = dfield
  let du56 () = dfield
  let du64 () = dfield
  let du128 () = dfield
  let dchar () = dfield

  let tup_opn () _ _ _ p = p

  let tup_cls () _ _ p = p

  let tup_sep _idx () _ _ p = p

  let rec_opn () _ _ _ p = p

  let rec_cls () _ _ p = p

  let rec_sep _n () _ _ p = p

  let vec_opn () _ _ _ _ p = p

  let vec_cls () _ _ p = p

  let vec_sep _ () _ _ p = p

  let list_opn = KnownSize (fun () mn0 path _ p ->
    let lst = get_field path p in
    pair
      (list_length (
        if is_nullable (type_of_path mn0 path) then
          to_not_nullable lst
        else
          lst))
      p)

  let list_cls () _ _ p = p

  let list_sep () _ _ p = p

  (* Will be called on every nullable fields before any attempt to deserialize
   * the value: *)
  let is_null () _ path p =
    field_is_null path p

  (* Do not update the path as it's done in the other branch of the alternative
   * (see note in dessser function). *)
  let dnull _t () _ _ p = p
  let dnotnull _t () _ _ p = p
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
