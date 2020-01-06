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

let is_nullable = function
  | [] -> assert false
  | frame :: _ -> ValueType.is_nullable frame.typ

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type state = unit

  (* We store the root pointer into the returned pointer, that will then
   * be passed and returned unchanged all the way so that the caller end
   * up with it at the end. The pointer object must then be able to
   * transport the address of any allocated value regardless of type. *)
  let start typ oc _p =
    (* For backends that can alloc uninitialized values:
     *   Will define the type, then alloc an uninitialized  value of that
     *   type and return an identifier for it. Fields will be initialized
     *   as serialization progresses.
     * For backends that can not alloc uninitialized values:
     *   Will define the type, then make up a name and return it. Construction
     *   and allocation will happen as the serialize progresses. *)
    (), BE.alloc_value oc typ

  let stop _oc () p = p

  type 'a ser = BE.output -> frame list -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let set_field oc frames () id p =
    if is_nullable frames then
      BE.set_nullable_field oc frames p (Some id)
    else
      BE.set_field oc frames p id ;
    p

  let sfloat oc frames st id p = set_field oc frames st id p
  let sstring oc frames st id p = set_field oc frames st id p
  let sbool oc frames st id p = set_field oc frames st id p
  let si8 oc frames st id p = set_field oc frames st id p
  let si16 oc frames st id p = set_field oc frames st id p
  let si32 oc frames st id p = set_field oc frames st id p
  let si64 oc frames st id p = set_field oc frames st id p
  let si128 oc frames st id p = set_field oc frames st id p
  let su8 oc frames st id p = set_field oc frames st id p
  let su16 oc frames st id p = set_field oc frames st id p
  let su32 oc frames st id p = set_field oc frames st id p
  let su64 oc frames st id p = set_field oc frames st id p
  let su128 oc frames st id p = set_field oc frames st id p
  let schar oc frames st id p = set_field oc frames st id p

  let tup_opn _oc _frames () p = p

  let tup_cls _oc _frames () p = p

  let tup_sep _n _oc _frames () p = p

  let rec_opn _oc _frames () p = p

  let rec_cls _oc _frames () p = p

  let rec_sep _fname _oc _frames () p = p

  let vec_opn _oc _frames () p = p

  let vec_cls _oc _frames () p = p

  let vec_sep _idx _oc _frames () p = p

  let list_opn _oc _frames () _n p = p

  let list_cls _oc _frames () p = p

  let list_sep _oc _frames () p = p

  let nullable _oc _frames () p = p

  let snull oc frames () p =
    BE.set_nullable_field oc frames p None ;
    p

  let snotnull _oc _frames () p = p

  type 'a ssizer = BE.output -> frame list -> 'a -> ssize
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

module Des (BE : BACKEND) : DES with module BE = BE =
struct
  module BE = BE

  type state = unit

  (* The pointer that's given to us must have been obtained from a
   * HeapValue.Ser. *)
  let start _typ _oc p = (), p
  let stop _oc () p = p

  type 'a des = BE.output -> frame list -> state -> [`Pointer] id -> 'a * [`Pointer] id

  (* P is supposed to be that fake pointer identifier returned by the
   * HeapValue.Ser.
   * Returns an id that points to the current value, starting from p. *)
  let get_field oc frames () p =
    if is_nullable frames then
      BE.get_nullable_field oc frames p
    else
      BE.get_field oc frames p

  let dfloat oc frames st p = get_field oc frames st p |> Identifier.to_float, p
  let dstring oc frames st p = get_field oc frames st p |> Identifier.to_string, p
  let dbool oc frames st p = get_field oc frames st p |> Identifier.to_bool, p
  let di8 oc frames st p = get_field oc frames st p |> Identifier.to_i8, p
  let di16 oc frames st p = get_field oc frames st p |> Identifier.to_i16, p
  let di32 oc frames st p = get_field oc frames st p |> Identifier.to_i32, p
  let di64 oc frames st p = get_field oc frames st p |> Identifier.to_i64, p
  let di128 oc frames st p = get_field oc frames st p |> Identifier.to_i128, p
  let du8 oc frames st p = get_field oc frames st p |> Identifier.to_u8, p
  let du16 oc frames st p = get_field oc frames st p |> Identifier.to_u16, p
  let du32 oc frames st p = get_field oc frames st p |> Identifier.to_u32, p
  let du64 oc frames st p = get_field oc frames st p |> Identifier.to_u64, p
  let du128 oc frames st p = get_field oc frames st p |> Identifier.to_u128, p
  let dchar oc frames st p = get_field oc frames st p |> Identifier.to_char, p

  let tup_opn _oc _frames () p = p

  let tup_cls _oc _frames () p = p

  let tup_sep _idx _oc _frames () p = p

  let rec_opn _oc _frames () p = p

  let rec_cls _oc _frames () p = p

  let rec_sep _fname _oc _frames () p = p

  let vec_opn _oc _frames () p = p

  let vec_cls _oc _frames () p = p

  let vec_sep _n _oc _frames () p = p

  let list_opn oc frames () p =
    let lst = get_field oc frames () p |>
              Identifier.to_list in
    BE.length_of_list oc lst, p

  let list_cls _oc _frames () p = p

  let list_sep _oc _frames () p = p

  (* Will be called before any attempt to deserialize the value *)
  let is_null oc frames () p =
    BE.bool_not oc (BE.field_is_set oc frames p)

  let dnull _oc _frames () p = p
  let dnotnull _oc _frames () p = p
end

(* Module to compute the sersize of a heap value: *)
module SerSizer (Ser : SER) : sig
    val sersize : ValueType.t -> Ser.BE.output -> [`Pointer] id ->
                    ([`Size] id * [`Size] id)
  end =
struct
  module BE = Ser.BE
  
  let t_pair_sizes = Type.(TPair (TSize, TSize))

  (* Returns a pair of size identifier holding the const and dyn size of
   * the heap value pointed by the pointer identifier [src].
   * [src] must be a pointer to a heap value, as returned by the above
   * Ser module. *)
  let sersize typ oc src =
    let size_0 = BE.size_of_const oc 0 in
    let add_size sizes = function
      | ConstSize sz ->
          BE.make_pair oc t_pair_sizes
            (BE.(size_add oc
               (size_of_const oc sz)
               (pair_fst oc sizes)))
            (BE.pair_snd oc sizes)
      | DynSize sz ->
          BE.make_pair oc t_pair_sizes
            (BE.pair_fst oc sizes)
            (BE.(size_add oc
               sz (pair_snd oc sizes))) in
    (* Add that value size to the passed size pair: *)
    let rec ssize_structure sizes oc frames v = function
      | ValueType.TFloat ->
          Ser.ssize_of_float oc frames (Identifier.to_float v) |> add_size sizes
      | ValueType.TString ->
          Ser.ssize_of_string oc frames (Identifier.to_string v) |> add_size sizes
      | ValueType.TBool ->
          Ser.ssize_of_bool oc frames (Identifier.to_bool v) |> add_size sizes
      | ValueType.TChar ->
          Ser.ssize_of_char oc frames (Identifier.to_char v) |> add_size sizes
      | ValueType.TI8 ->
          Ser.ssize_of_i8 oc frames (Identifier.to_i8 v) |> add_size sizes
      | ValueType.TI16 ->
          Ser.ssize_of_i16 oc frames (Identifier.to_i16 v) |> add_size sizes
      | ValueType.TI32 ->
          Ser.ssize_of_i32 oc frames (Identifier.to_i32 v) |> add_size sizes
      | ValueType.TI64 ->
          Ser.ssize_of_i64 oc frames (Identifier.to_i64 v) |> add_size sizes
      | ValueType.TI128 ->
          Ser.ssize_of_i128 oc frames (Identifier.to_i128 v) |> add_size sizes
      | ValueType.TU8 ->
          Ser.ssize_of_u8 oc frames (Identifier.to_u8 v) |> add_size sizes
      | ValueType.TU16 ->
          Ser.ssize_of_u16 oc frames (Identifier.to_u16 v) |> add_size sizes
      | ValueType.TU32 ->
          Ser.ssize_of_u32 oc frames (Identifier.to_u32 v) |> add_size sizes
      | ValueType.TU64 ->
          Ser.ssize_of_u64 oc frames (Identifier.to_u64 v) |> add_size sizes
      | ValueType.TU128 ->
          Ser.ssize_of_u128 oc frames (Identifier.to_u128 v) |> add_size sizes
      (* Compound types require recursion: *)
      | ValueType.TTup vtyps ->
          let sizes =
            Ser.ssize_of_tup oc frames (Identifier.to_tup v) |> add_size sizes in
          Array.fold_lefti (fun sizes i typ ->
            let subframes = { typ ; index = i ; name = "" } :: frames in
            sersize_ oc subframes src sizes
          ) sizes vtyps
      | ValueType.TRec vtyps ->
          let sizes =
            Ser.ssize_of_rec oc frames (Identifier.to_rec v) |> add_size sizes in
          Array.fold_lefti (fun sizes i (name, typ) ->
            let subframes = { typ ; index = i ; name } :: frames in
            sersize_ oc subframes src sizes
          ) sizes vtyps
      | ValueType.TVec (dim, typ) ->
          let sizes =
            Ser.ssize_of_vec oc frames (Identifier.to_vec v) |> add_size sizes in
          let rec loop sizes i =
            if i >= dim then sizes else
            let subframes = { typ ; index = i ; name = "" } :: frames in
            let sizes = sersize_ oc subframes src sizes in
            loop sizes (i + 1) in
          loop sizes 0
      | ValueType.TList _typ ->
          assert false

    and sersize_ oc frames src sizes =
      let typ = (List.hd frames).typ in
      if ValueType.is_nullable typ then
        let cond = BE.field_is_set oc frames src in
        BE.choose oc ~cond
          (fun oc ->
            let v = BE.get_nullable_field oc frames src in
            ssize_structure sizes oc frames v (ValueType.to_not_nullable typ))
          (fun oc ->
            add_size sizes (Ser.ssize_of_null oc frames))
      else
        let v = BE.get_field oc frames src in
        ssize_structure sizes oc frames v (ValueType.to_not_nullable typ)
  in
  let sizes =
    BE.make_pair oc t_pair_sizes size_0 size_0 in
  let sizes = sersize_ oc [ { typ ; index = 0 ; name = "" } ] src sizes in
  (* Returns the two sizes: *)
  BE.pair_fst oc sizes,
  BE.pair_snd oc sizes
end
