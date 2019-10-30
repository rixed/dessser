(* We might want to serialize into a value allocated on the heap.
 * The form of which depends on the backend of course, but every backend
 * must allow construction of arbitrarily nested data structures.
 * Of course, it is then also possible to serialize from a heap value
 * into anything else.
 *
 * For such a format, the pointers used to write/read during resp.
 * serialization/deserialization are unused. *)
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
  let init_state typ oc _p =
    (* For backends that can alloc uninitialized values:
     *   Will define the type, then alloc an uninitialized  value of that
     *   type and return an identifier for it. Fields will be initialized
     *   as serialization progresses.
     * For backends that can not alloc uninitialized values:
     *   Will define the type, then make up a name and return it. Construction
     *   and allocation will happen as the serialize progresses. *)
    (), BE.alloc_value oc typ

  type 'a ser = BE.output -> frame list -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let set_field oc frames () id p =
    if is_nullable frames then
      BE.set_nullable_field oc frames p (Some id)
    else
      BE.set_field oc frames p id

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
    BE.set_nullable_field oc frames p None

  let snotnull _oc _frames () p = p
end

module Des (BE : BACKEND) : DES with module BE = BE =
struct
  module BE = BE
  module T = Types

  type state = unit

  (* The pointer that's given to us must have been obtained from a
   * HeapValue.Ser. *)
  let init_state _typ _oc p = (), p

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
