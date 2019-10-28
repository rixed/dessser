open Batteries
open Dessert

module Des (BE : BACKEND) : DES with module BE = BE =
struct
  module BE = BE
  module T = Types

  type pointer = Identifier.t
  type 'a des = BE.output -> pointer -> 'a * pointer

  let dfloat oc p =
    let w, p = BE.read_qword oc p in
    BE.float_of_qword oc w, p

  let read_leb128 oc p =
    let t_pair_u32_u8 = T.(make (TTup [| make TU32 ; make TU8 |]))
    and t_byte = T.(make TByte)
    and t_bool = T.(make TBool)
    in
    let cond =
      BE.print_function1 oc t_bool t_byte (fun oc b ->
        BE.U8.gt oc (BE.U8.of_byte oc b) (BE.U8.of_const_int oc 128))
    and reduce =
      BE.print_function2 oc t_pair_u32_u8 t_pair_u32_u8 t_byte (fun oc leb_shft_tup byte ->
        let leb = BE.tuple_get oc leb_shft_tup 0
        and shft = BE.tuple_get oc leb_shft_tup 1 in
        BE.make_tuple oc t_pair_u32_u8 [|
          BE.U32.add oc (BE.U32.shift_left oc (BE.U32.of_byte oc leb) shft) byte ;
          BE.U8.add oc shft (BE.U8.of_const_int oc 7)
        |])
    in
    let u32_zero = BE.U32.of_const_int oc 0
    and u8_zero = BE.U8.of_const_int oc 0 in
    let init = BE.make_tuple oc t_pair_u32_u8 [| u32_zero ; u8_zero |] in
    let leb_shft_tup, p = BE.read_while oc ~cond ~reduce init p in
    (* Still have to add the last byte: *)
    let last_b, p = BE.read_byte oc p in
    let leb = BE.tuple_get oc leb_shft_tup 0
    and shft = BE.tuple_get oc leb_shft_tup 1 in
    BE.U32.add oc (BE.U32.shift_left oc (BE.U32.of_byte oc last_b) shft) leb,
    p

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring oc p =
    let len, p = read_leb128 oc p in
    let bs, p = BE.read_bytes oc p len in
    BE.string_of_bytes oc bs,
    p

  let dbool oc p =
    let b, p = BE.read_byte oc p in
    BE.(bool_not oc U8.(eq oc (of_byte oc b) (of_const_int oc 0))),
    p

  let di8 oc p =
    let b, p = BE.read_byte oc p in
    BE.I8.of_byte oc b,
    p

  let du8 oc p =
    let b, p = BE.read_byte oc p in
    BE.U8.of_byte oc b,
    p

  let di16 oc p =
    let w, p = BE.read_word oc p in
    BE.I16.of_word oc w,
    p

  let du16 oc p =
    let w, p = BE.read_word oc p in
    BE.U16.of_word oc w,
    p

  let di32 oc p =
    let w, p = BE.read_dword oc p in
    BE.I32.of_dword oc w,
    p

  let du32 oc p =
    let w, p = BE.read_dword oc p in
    BE.U32.of_dword oc w,
    p

  let di64 oc p =
    let w, p = BE.read_qword oc p in
    BE.I64.of_qword oc w,
    p

  let du64 oc p =
    let w, p = BE.read_qword oc p in
    BE.U64.of_qword oc w,
    p

  let di128 oc p =
    let w, p = BE.read_oword oc p in
    BE.I128.of_oword oc w,
    p

  let du128 oc p =
    let w, p = BE.read_oword oc p in
    BE.U128.of_oword oc w,
    p

  (* Items of a tuples are just concatenated together: *)
  let tup_opn _typs _oc p = p
  let tup_cls _typs _oc p = p
  let tup_sep _typs _n _oc p = p

  (* Vectors: ClickHouse does not distinguish between vectors (or known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and out lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn _dim _typ _oc p = p
  let vec_cls _dim _typ _oc p = p
  let vec_sep _dim _typ _oc _n p = p

  (* TODO: lists *)

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null _typ oc p =
    let b = BE.peek_byte oc p in
    BE.U8.(eq oc (of_byte oc b) (of_const_int oc 1))

  let dnull oc p = BE.pointer_add oc p (BE.size_of_const oc 1)
  let dnotnull = dnull
end


