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

type heapvalue_state = path ref

let next st =
  match !st with
  | [] -> ()
  | i::rest -> st := (i+1) :: rest

let enter st =
  st := 0 :: !st

let leave st =
  match !st with
  | [] -> assert false
  | _::rest -> st := rest

module Ser : SER =
struct
  type state = heapvalue_state * (* next ser value is nullable: *) bool ref
  let ptr vtyp = valueptr vtyp

  open Expression

  (* [p] must ben a ValuePtr. *)
  let start _vtyp p =
    (* Note: nullability will be set before the first value is serialized *)
    (ref [], ref false), p

  let stop (path, _) p =
    assert (!path = []) ;
    p

  type ser = state -> e -> e -> e

  let set_field (path_ref, nullable_ref) v p =
    let path = !path_ref in
    let v = if !nullable_ref then Nullable v else v in
    nullable_ref := false ;
    SetField (path, p, v)

  let sfloat st id p = set_field st id p
  let sstring st id p = set_field st id p
  let sbool st id p = set_field st id p
  let si8 st id p = set_field st id p
  let si16 st id p = set_field st id p
  let si32 st id p = set_field st id p
  let si64 st id p = set_field st id p
  let si128 st id p = set_field st id p
  let su8 st id p = set_field st id p
  let su16 st id p = set_field st id p
  let su32 st id p = set_field st id p
  let su64 st id p = set_field st id p
  let su128 st id p = set_field st id p
  let schar st id p = set_field st id p

  let tup_opn (path, _) _ p = enter path ; p

  let tup_cls (path, _) p = leave path ; p

  let tup_sep _n (path, _) p = next path ; p

  let rec_opn (path, _) _ p = enter path ; p

  let rec_cls (path, _) p = leave path ; p

  let rec_sep _n (path, _) p = next path ; p

  (* FIXME: vectors/lists/maps: in order to be convertible to/from a heap value
   * they must be also entered/left. *)

  let vec_opn _st _ _ p = p

  let vec_cls _st p = p

  let vec_sep _idx _st p = p

  let list_opn _st _ _n p = p

  let list_cls _st p = p

  let list_sep _st p = p

  let nullable (_, nullable_ref) p =
    nullable_ref := true ;
    p

  (* Do not update the state as it's done in the other branch of the alternative
   * (see note in dessser function). *)
  let snull t (path, _) p =
    SetField (!path, p, Null t)

  let snotnull _t _st p = p

  type ssizer = vtyp -> path -> e -> ssize
  let todo_ssize () = failwith "TODO: ssize for HeapValue"
  let ssize_of_float _ _ _ = todo_ssize ()
  let ssize_of_string _ _ _ = todo_ssize ()
  let ssize_of_bool _ _ _ = todo_ssize ()
  let ssize_of_char _ _ _ = todo_ssize ()
  let ssize_of_i8 _ _ _ = todo_ssize ()
  let ssize_of_i16 _ _ _ = todo_ssize ()
  let ssize_of_i32 _ _ _ = todo_ssize ()
  let ssize_of_i64 _ _ _ = todo_ssize ()
  let ssize_of_i128 _ _ _ = todo_ssize ()
  let ssize_of_u8 _ _ _ = todo_ssize ()
  let ssize_of_u16 _ _ _ = todo_ssize ()
  let ssize_of_u32 _ _ _ = todo_ssize ()
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
  type state = heapvalue_state * bool ref
  let ptr vtyp = valueptr vtyp

  open Expression

  (* The pointer that's given to us must have been obtained from a
   * HeapValue.Ser. *)
  let start _vtyp p =
    (ref [], ref false), p

  let stop (path, _) p =
    assert (!path = []) ;
    p

  type des = state -> e -> e

  (* Beware that Dessser expect deserializers to return only not-nullables. *)
  let get_field (path_ref, nullable_ref) p =
    let path = !path_ref in
    let v = GetField (path, p) in
    let v = if !nullable_ref then NotNullable v else v in
    nullable_ref := false ;
    Pair (v, p)

  let dfloat st p = get_field st p
  let dstring st p = get_field st p
  let dbool st p = get_field st p
  let di8 st p = get_field st p
  let di16 st p = get_field st p
  let di32 st p = get_field st p
  let di64 st p = get_field st p
  let di128 st p = get_field st p
  let du8 st p = get_field st p
  let du16 st p = get_field st p
  let du32 st p = get_field st p
  let du64 st p = get_field st p
  let du128 st p = get_field st p
  let dchar st p = get_field st p

  let tup_opn (path, _) _ p = enter path ; p

  let tup_cls (path, _) p = leave path ; p

  let tup_sep _idx (path, _) p = next path ; p

  let rec_opn (path, _) _ p = enter path ; p

  let rec_cls (path, _) p = leave path ; p

  let rec_sep _n (path, _) p = next path ; p

  let vec_opn _st _ _ p = p

  let vec_cls _st p = p

  let vec_sep _n _st p = p

  let list_opn st _ p =
    let lst = get_field st p in
    Pair (ListLength lst, p)

  let list_cls _st p = p

  let list_sep _st p = p

  (* Will be called on every nullable fields before any attempt to deserialize
   * the value: *)
  let is_null (path, nullable_ref) p =
    nullable_ref := true ;
    FieldIsNull (!path, p)

  (* Do not update the state as it's done in the other branch of the alternative
   * (see note in dessser function). *)
  let dnull _t _st p = p
  let dnotnull _t _st p = p
end

(* Module to compute the sersize of a heap value: *)
module SerSizer (Ser : SER) : sig
    val sersize : ValueType.t -> (*dataptr*) e -> (*size*size*) e
  end =
struct
  open Expression

  (* Returns a pair of size identifier holding the const and dyn size of
   * the heap value pointed by the pointer identifier [src].
   * [src] must be a pointer to a heap value, as returned by the above
   * Ser module. *)
  let sersize vtyp src =
    let add_size sizes sz =
      MapPair (sizes,
        func [|size; size|] (fun fid ->
          match sz with
          | ConstSize s ->
              Pair (Add (Size s, Param (fid, 0)), Param (fid, 1))
          | DynSize s ->
              Pair (Param (fid, 0), Add (s, Param (fid, 1))))) in
    (* Add that value size to the passed size pair: *)
    let rec ssize_vtype path sizes v = function
      | ValueType.Float ->
          Ser.ssize_of_float vtyp path v |> add_size sizes
      | String ->
          Ser.ssize_of_string vtyp path v |> add_size sizes
      | Bool ->
          Ser.ssize_of_bool vtyp path v |> add_size sizes
      | Char ->
          Ser.ssize_of_char vtyp path v |> add_size sizes
      | I8 ->
          Ser.ssize_of_i8 vtyp path v |> add_size sizes
      | I16 ->
          Ser.ssize_of_i16 vtyp path v |> add_size sizes
      | I32 ->
          Ser.ssize_of_i32 vtyp path v |> add_size sizes
      | I64 ->
          Ser.ssize_of_i64 vtyp path v |> add_size sizes
      | I128 ->
          Ser.ssize_of_i128 vtyp path v |> add_size sizes
      | U8 ->
          Ser.ssize_of_u8 vtyp path v |> add_size sizes
      | U16 ->
          Ser.ssize_of_u16 vtyp path v |> add_size sizes
      | U32 ->
          Ser.ssize_of_u32 vtyp path v |> add_size sizes
      | U64 ->
          Ser.ssize_of_u64 vtyp path v |> add_size sizes
      | U128 ->
          Ser.ssize_of_u128 vtyp path v |> add_size sizes
      (* Compound types require recursion: *)
      | Tup vtyps ->
          let sizes =
            Ser.ssize_of_tup vtyp path v |> add_size sizes in
          Array.fold_lefti (fun sizes i _ ->
            Let ("sizes", sizes, sersize_ (i::path) src (Identifier "sizes"))
          ) sizes vtyps
      | Rec vtyps ->
          let sizes =
            Ser.ssize_of_rec vtyp path v |> add_size sizes in
          Array.fold_lefti (fun sizes i _ ->
            sersize_ (i::path) src sizes
          ) sizes vtyps
      | Vec (dim, _) ->
          let sizes =
            Ser.ssize_of_vec vtyp path v |> add_size sizes in
          let rec loop sizes i =
            if i >= dim then sizes else
            let sizes = sersize_ (i::path) src sizes in
            loop sizes (i + 1) in
          loop sizes 0
      | List _typ ->
          assert false

    and sersize_ path src sizes =
      let sub_vtyp = ValueType.type_of_path vtyp path in
      if ValueType.is_nullable sub_vtyp then
        let comment =
          Printf.sprintf2 "sersize of path %a" ValueType.print_path path in
        Comment (comment,
          Choose (FieldIsNull (path, src),
            add_size sizes (Ser.ssize_of_null sub_vtyp path),
            let sub_vtyp' = ValueType.to_not_nullable sub_vtyp in
            ssize_vtype path sizes (NotNullable (GetField (path, src))) sub_vtyp'))
      else
        let sub_vtyp' = ValueType.to_not_nullable sub_vtyp in
        ssize_vtype path sizes (GetField (path, src)) sub_vtyp'
    in
    let sizes = Pair (Size 0, Size 0) in
    sersize_ [] src sizes
end
