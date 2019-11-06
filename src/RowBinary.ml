open Batteries
open Dessser

module Des (BE : BACKEND) : DES with module BE = BE =
struct
  module BE = BE
  module T = Types

  type state = unit
  let start _typ _oc p = (), p
  let stop _oc () p = p

  type 'a des = BE.output -> frame list -> state -> [`Pointer] id -> 'a * [`Pointer] id

  let dfloat oc _frames () p =
    let w, p = BE.read_qword oc p in
    BE.float_of_qword oc w, p

  let read_leb128 oc p =
    let t_pair_u32_u8 = T.(make (TPair (make TU32, make TU8)))
    and t_byte = T.(make TByte)
    and t_bool = T.(make TBool)
    in
    let cond =
      BE.print_function1 oc t_byte t_bool (fun oc b ->
        BE.U8.gt oc (BE.U8.of_byte oc b) (BE.U8.of_const_int oc 128))
    and reduce =
      BE.print_function2 oc t_pair_u32_u8 t_byte t_pair_u32_u8 (fun oc leb_shft_tup byte ->
        let leb = Identifier.u32_of_any (BE.pair_fst oc leb_shft_tup)
        and shft = Identifier.u8_of_any (BE.pair_snd oc leb_shft_tup) in
        BE.make_pair oc t_pair_u32_u8
          (BE.U32.add oc (BE.U32.shift_left oc (BE.U32.of_byte oc byte) shft) leb)
          (BE.U8.add oc shft (BE.U8.of_const_int oc 7)))
    in
    let u32_zero = BE.U32.of_const_int oc 0
    and u8_zero = BE.U8.of_const_int oc 0 in
    let init =
      BE.make_pair oc t_pair_u32_u8 u32_zero u8_zero in
    let leb_shft_tup, p = BE.read_while oc ~cond ~reduce init p in
    (* Still have to add the last byte: *)
    let last_b, p = BE.read_byte oc p in
    let leb = BE.pair_fst oc leb_shft_tup
    and shft = BE.pair_snd oc leb_shft_tup in
    BE.size_of_u32 oc (BE.U32.add oc (BE.U32.shift_left oc (BE.U32.of_byte oc last_b) shft) leb),
    p

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring oc _frames () p =
    let len, p = read_leb128 oc p in
    let bs, p = BE.read_bytes oc p len in
    BE.string_of_bytes oc bs,
    p

  let dbool oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.(bool_not oc U8.(eq oc (of_byte oc b) (of_const_int oc 0))),
    p

  let dchar oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.char_of_byte oc b,
    p

  let di8 oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.I8.of_byte oc b,
    p

  let du8 oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.U8.of_byte oc b,
    p

  let di16 oc _frames () p =
    let w, p = BE.read_word oc p in
    BE.I16.of_word oc w,
    p

  let du16 oc _frames () p =
    let w, p = BE.read_word oc p in
    BE.U16.of_word oc w,
    p

  let di32 oc _frames () p =
    let w, p = BE.read_dword oc p in
    BE.I32.of_dword oc w,
    p

  let du32 oc _frames () p =
    let w, p = BE.read_dword oc p in
    BE.U32.of_dword oc w,
    p

  let di64 oc _frames () p =
    let w, p = BE.read_qword oc p in
    BE.I64.of_qword oc w,
    p

  let du64 oc _frames () p =
    let w, p = BE.read_qword oc p in
    BE.U64.of_qword oc w,
    p

  let di128 oc _frames () p =
    let w, p = BE.read_oword oc p in
    BE.I128.of_oword oc w,
    p

  let du128 oc _frames () p =
    let w, p = BE.read_oword oc p in
    BE.U128.of_oword oc w,
    p

  (* Items of a tuples are just concatenated together: *)
  let tup_opn _oc _frames () p = p
  let tup_cls _oc _frames () p = p
  let tup_sep _n _oc _frames () p = p

  let rec_opn _oc _frames () p = p
  let rec_cls _oc _frames () p = p
  let rec_sep _n _oc _frames () p = p

  (* Vectors: ClickHouse does not distinguish between vectors (or known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and out lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn _oc _frames () p = p
  let vec_cls _oc _frames () p = p
  let vec_sep _n _oc _frames () p = p

  (* TODO: lists *)

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null oc _frames () p =
    let b = BE.peek_byte oc p in
    BE.U8.(eq oc (of_byte oc b) (of_const_int oc 1))

  let dnull oc _frames () p =
    BE.pointer_add oc p (BE.size_of_const oc 1)

  let dnotnull = dnull
end


