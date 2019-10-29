(* This is a (de)serializer for ClickHouse RowBinary format, which obvious
 * mapping between our [typ] and ClichHouse'.
 *
 * Here is what ClickHouse doc has to say about that format:
 *
 * "Numbers is written in little endian, fixed width. For example, UInt64 takes
 * 8 bytes. DateTime is written as UInt32 with unix timestamp value. Date is
 * written as UInt16 with number of days since 1970-01-01 in value. String is
 * written as length in varint (unsigned LEB128) format and then bytes of
 * string. FixedString is written as just its bytes. Array is written as length
 * in varint (unsigned LEB128) format and then all elements, contiguously"
 *)
open Batteries
open Stdint
open Dessser
open MyLifts

module Common =
struct
  module SerData = IntRepr.SerData
  type pointer = SerData.pointer

  let size_1 = SerData.size_of_const 1
  let u8v_128 = IntRepr.U8.of_const 0b1000_0000
  let u8v_7 = IntRepr.U8.of_const 7
end

module Des : DES =
struct
  include Common
  type 'a des = (pointer -> 'a * pointer) code

  (*external float_of_bytes : bytes -> bla = "float_of_bytes"*)
  let float pc =
    SerData.read_qword pc |>
    IntRepr.map_fst IntRepr.Float.of_qword

  let dfloat = .< fun p -> .~(float .<p>.) >.

  let read_leb128 pc =
    let cond =
      .<
        fun leb128_byte ->
          .~(IntRepr.(U8.gt (U8.of_byte .<leb128_byte>.) (lift_uint8 u8v_128)))
      >.
    and reduce =
      .<
        fun (leb128, shft) leb128_byte ->
          .~(IntRepr.(U32.add (U32.shift_left (U32.of_byte .<leb128_byte>.) .<shft>.)
                               .<leb128>.)),
          .~(IntRepr.(U8.add .<shft>. (lift_uint8 u8v_7)))
      >. in
    let len_shft_p =
      IntRepr.read_while ~cond ~reduce .<(.~IntRepr.U32.zero, .~IntRepr.U8.zero), .~pc>. in
    (* Still have to add the last byte: *)
    .<
      let (leb128, shft), p = .~len_shft_p in
      .~(SerData.read_byte .<p>. |>
         IntRepr.map_fst (fun leb128_fin ->
           IntRepr.(U32.add
                     (U32.shift_left (U32.of_byte leb128_fin) .<shft>.)
                     .<leb128>.)))
    >.

  let string pc =
    read_leb128 pc |>
    IntRepr.map_fst IntRepr.U32.to_size |>
    SerData.read_bytes |>
    IntRepr.(map_fst stringv_of_bytes)

  let dstring = .< fun p -> .~(string .<p>.) >.

  let bool pc =
    SerData.read_byte pc |>
    IntRepr.(map_fst (fun bc -> U8.of_byte bc |> U8.to_boolv))

  let dbool = .< fun p -> .~(bool .<p>.) >.

  let i8 pc =
    SerData.read_byte pc |>
    IntRepr.(map_fst (I8.of_byte))

  let di8 = .< fun p -> .~(i8 .<p>.) >.

  let u8 pc =
    SerData.read_byte pc |>
    IntRepr.(map_fst U8.of_byte)

  let du8 = .< fun p -> .~(u8 .<p>.) >.

  let i16 pc =
    SerData.read_word pc |>
    IntRepr.(map_fst I16.of_word)

  let di16 = .< fun p -> .~(i16 .<p>.) >.

  let u16 pc =
    SerData.read_word pc |>
    IntRepr.(map_fst U16.of_word)

  let du16 = .< fun p -> .~(u16 .<p>.) >.

  let i32 pc =
    SerData.read_dword pc |>
    IntRepr.(map_fst I32.of_dword)

  let di32 = .< fun p -> .~(i32 .<p>.) >.

  let u32 pc =
    SerData.read_dword pc |>
    IntRepr.(map_fst U32.of_dword)

  let du32 = .< fun p -> .~(u32 .<p>.) >.

  let i64 pc =
    SerData.read_qword pc |>
    IntRepr.(map_fst I64.of_qword)

  let di64 = .< fun p -> .~(i64 .<p>.) >.

  let u64 pc =
    SerData.read_qword pc |>
    IntRepr.(map_fst U64.of_qword)

  let du64 = .< fun p -> .~(u64 .<p>.) >.

  let i128 pc =
    SerData.read_oword pc |>
    IntRepr.(map_fst I128.of_oword)

  let di128 = .< fun p -> .~(i128 .<p>.) >.

  let u128 pc =
    SerData.read_oword pc |>
    IntRepr.(map_fst U128.of_oword)

  let du128 = .< fun p -> .~(u128 .<p>.) >.

  (* Items of a tuples are just concatenated together: *)
  let tup_opn _typs pc = pc
  let tup_cls _typs pc = pc
  let tup_sep _typs _n pc = pc

  (* Vectors: ClickHouse does not distinguish between vectors (or known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and out lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn _dim _typ pc = pc
  let vec_cls _dim _typ pc = pc
  let vec_sep _dim _typ _n pc = pc

  (* TODO: lists *)

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null _typ pc =
    IntRepr.(U8.to_boolv (U8.of_byte (SerData.peek_byte pc)))

  let dnull pc = SerData.add pc .<size_1>.
  let dnotnull = dnull
end
