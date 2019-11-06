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
  | frame :: _ ->
      frame.typ.Types.nullable

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
  let ssize_of_null _ _ = todo_ssize ()
end

module Des (BE : BACKEND) : DES with module BE = BE =
struct
  module BE = BE
  module T = Types

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

  let dfloat oc frames st p = get_field oc frames st p |> Identifier.float_of_any, p
  let dstring oc frames st p = get_field oc frames st p |> Identifier.string_of_any, p
  let dbool oc frames st p = get_field oc frames st p |> Identifier.bool_of_any, p
  let di8 oc frames st p = get_field oc frames st p |> Identifier.i8_of_any, p
  let di16 oc frames st p = get_field oc frames st p |> Identifier.i16_of_any, p
  let di32 oc frames st p = get_field oc frames st p |> Identifier.i32_of_any, p
  let di64 oc frames st p = get_field oc frames st p |> Identifier.i64_of_any, p
  let di128 oc frames st p = get_field oc frames st p |> Identifier.i128_of_any, p
  let du8 oc frames st p = get_field oc frames st p |> Identifier.u8_of_any, p
  let du16 oc frames st p = get_field oc frames st p |> Identifier.u16_of_any, p
  let du32 oc frames st p = get_field oc frames st p |> Identifier.u32_of_any, p
  let du64 oc frames st p = get_field oc frames st p |> Identifier.u64_of_any, p
  let du128 oc frames st p = get_field oc frames st p |> Identifier.u128_of_any, p
  let dchar oc frames st p = get_field oc frames st p |> Identifier.char_of_any, p

  let tup_opn _oc _frames () p = p

  let tup_cls _oc _frames () p = p

  let tup_sep _idx _oc _frames () p = p

  let rec_opn _oc _frames () p = p

  let rec_cls _oc _frames () p = p

  let rec_sep _fname _oc _frames () p = p

  let vec_opn _oc _frames () p = p

  let vec_cls _oc _frames () p = p

  let vec_sep _n _oc _frames () p = p

  (* Will be called before any attempt to deserialize the value *)
  let is_null oc frames () p =
    BE.bool_not oc (BE.field_is_set oc frames p)

  let dnull _oc _frames () p = p
  let dnotnull _oc _frames () p = p
end

(* Module to compute the sersize of a heap value: *)
module SerSizer (Ser : SER) : sig
    val sersize : Types.t -> Ser.BE.output -> [`Pointer] id ->
                    ([`Size] id * [`Size] id)
  end =
struct
  module BE = Ser.BE
  
  let t_pair_sizes = Types.(make (TPair (make TSize, make TSize)))

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
      | Types.TFloat ->
          Ser.ssize_of_float oc frames (Identifier.float_of_any v) |> add_size sizes
      | Types.TString ->
          Ser.ssize_of_string oc frames (Identifier.string_of_any v) |> add_size sizes
      | Types.TBool ->
          Ser.ssize_of_bool oc frames (Identifier.bool_of_any v) |> add_size sizes
      | Types.TChar ->
          Ser.ssize_of_char oc frames (Identifier.char_of_any v) |> add_size sizes
      | Types.TI8 ->
          Ser.ssize_of_i8 oc frames (Identifier.i8_of_any v) |> add_size sizes
      | Types.TI16 ->
          Ser.ssize_of_i16 oc frames (Identifier.i16_of_any v) |> add_size sizes
      | Types.TI32 ->
          Ser.ssize_of_i32 oc frames (Identifier.i32_of_any v) |> add_size sizes
      | Types.TI64 ->
          Ser.ssize_of_i64 oc frames (Identifier.i64_of_any v) |> add_size sizes
      | Types.TI128 ->
          Ser.ssize_of_i128 oc frames (Identifier.i128_of_any v) |> add_size sizes
      | Types.TU8 ->
          Ser.ssize_of_u8 oc frames (Identifier.u8_of_any v) |> add_size sizes
      | Types.TU16 ->
          Ser.ssize_of_u16 oc frames (Identifier.u16_of_any v) |> add_size sizes
      | Types.TU32 ->
          Ser.ssize_of_u32 oc frames (Identifier.u32_of_any v) |> add_size sizes
      | Types.TU64 ->
          Ser.ssize_of_u64 oc frames (Identifier.u64_of_any v) |> add_size sizes
      | Types.TU128 ->
          Ser.ssize_of_u128 oc frames (Identifier.u128_of_any v) |> add_size sizes
      (* Compound types require recursion: *)
      | Types.TTup typs ->
          let sizes =
            Ser.ssize_of_tup oc frames (Identifier.tup_of_any v) |> add_size sizes in
          Array.fold_lefti (fun sizes i typ ->
            let subframes = { typ ; index = i } :: frames in
            sersize_ oc subframes src sizes
          ) sizes typs
      | Types.TRec typs ->
          let sizes =
            Ser.ssize_of_rec oc frames (Identifier.rec_of_any v) |> add_size sizes in
          Array.fold_lefti (fun sizes i (_name, typ) ->
            let subframes = { typ ; index = i } :: frames in
            sersize_ oc subframes src sizes
          ) sizes typs
      | Types.TVec (dim, typ) ->
          let sizes =
            Ser.ssize_of_vec oc frames (Identifier.vec_of_any v) |> add_size sizes in
          let rec loop sizes i =
            if i >= dim then sizes else
            let subframes = { typ ; index = i } :: frames in
            let sizes = sersize_ oc subframes src sizes in
            loop sizes (i + 1) in
          loop sizes 0
      | _ ->
          assert false
    and sersize_ oc frames src sizes =
      let typ = (List.hd frames).typ in
      if typ.nullable then
        let cond = BE.field_is_set oc frames src in
        BE.choose oc ~cond
          (fun oc ->
            let v = BE.get_nullable_field oc frames src in
            ssize_structure sizes oc frames v typ.structure)
          (fun oc ->
            add_size sizes (Ser.ssize_of_null oc frames))
      else
        let v = BE.get_field oc frames src in
        ssize_structure sizes oc frames v typ.structure
  in
  let sizes =
    BE.make_pair oc t_pair_sizes size_0 size_0 in
  let sizes = sersize_ oc [ { typ ; index = 0 } ] src sizes in
  (* Returns the two sizes: *)
  BE.pair_fst oc sizes,
  BE.pair_snd oc sizes
end
