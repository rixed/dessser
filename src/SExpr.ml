open Stdint
open Dessser

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type state = unit
  let start _typ _oc p = (), p
  let stop _oc () p = p

  type 'a ser = BE.output -> frame list -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let sfloat oc _frames () v p =
    let str = BE.string_of_float oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let byte_of_char oc c =
    BE.U8.(to_byte oc (of_const_int oc (Char.code c)))

  let sstring oc _frames () v p =
    let quo = byte_of_char oc '"' in
    let p = BE.write_byte oc p quo in
    let v = BE.bytes_of_string oc v in
    let p = BE.write_bytes oc p v in
    BE.write_byte oc p quo

  let sbool oc _frames () v p =
    let byte =
      BE.choose oc ~cond:v
        (fun oc -> byte_of_char oc 'T')
        (fun oc -> byte_of_char oc 'F') in
    BE.write_byte oc p byte

  let si8 oc _frames () v p =
    let str = BE.I8.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let si16 oc _frames () v p =
    let str = BE.I16.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let si32 oc _frames () v p =
    let str = BE.I32.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let si64 oc _frames () v p =
    let str = BE.I64.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let si128 oc _frames () v p =
    let str = BE.I128.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let schar oc _frames () v p =
    let b = BE.byte_of_char oc v in
    BE.write_byte oc p b

  let su8 oc _frames () v p =
    let str = BE.U8.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su16 oc _frames () v p =
    let str = BE.U16.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su32 oc _frames () v p =
    let str = BE.U32.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su64 oc _frames () v p =
    let str = BE.U64.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su128 oc _frames () v p =
    let str = BE.U128.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  (* Could also write the field names with the value in a pair... *)
  let tup_opn oc _frames () p =
    BE.write_byte oc p (byte_of_char oc '(')

  let tup_cls oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ')')

  let tup_sep _idx oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ' ')

  let rec_opn oc _frames () p =
    BE.write_byte oc p (byte_of_char oc '(')

  let rec_cls oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ')')

  let rec_sep _idx oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ' ')

  let vec_opn oc _frames () p =
    BE.write_byte oc p (byte_of_char oc '[')

  let vec_cls oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ']')

  let vec_sep _n oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ' ')

  let list_opn oc _frames () _n p =
    BE.write_byte oc p (byte_of_char oc '[')

  let list_cls oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ']')

  let list_sep oc _frames () p =
    BE.write_byte oc p (byte_of_char oc ' ')

  let nullable _oc _frames () p = p
  let snull oc _frames () p =
    let null = BE.dword_of_const oc (Uint32.of_int32 0x6c_6c_75_6el) in
    BE.write_dword oc p null

  let snotnull _oc _frames () p = p

  type 'a ssizer = BE.output -> frame list -> 'a -> ssize
  let todo_ssize () = failwith "TODO: ssize for SExpr"
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
