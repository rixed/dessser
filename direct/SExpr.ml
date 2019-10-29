open Stdint
open Dessser

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type state = unit
  let init_state _typ _oc p = (), p

  type 'a ser = BE.output -> unit -> 'a -> [`Pointer] id -> [`Pointer] id

  let sfloat oc () v p =
    let str = BE.string_of_float oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let byte_of_char oc c =
    BE.U8.(to_byte oc (of_const_int oc (Char.code c)))

  let sstring oc () v p =
    let quo = byte_of_char oc '"' in
    let p = BE.write_byte oc p quo in
    let v = BE.bytes_of_string oc v in
    let p = BE.write_bytes oc p v in
    BE.write_byte oc p quo

  let sbool oc () v p =
    let byte =
      BE.choose oc ~cond:v
        (fun oc -> byte_of_char oc 'T')
        (fun oc -> byte_of_char oc 'F') in
    BE.write_byte oc p byte

  let si8 _oc () _v _p = assert false
  let si16 _oc () _v _p = assert false
  let si32 _oc () _v _p = assert false
  let si64 _oc () _v _p = assert false
  let si128 _oc () _v _p = assert false

  let su8 oc () v p =
    let str = BE.U8.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su16 oc () v p =
    let str = BE.U16.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su32 oc () v p =
    let str = BE.U32.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su64 oc () v p =
    let str = BE.U64.to_string oc v in
    let bytes = BE.bytes_of_string oc str in
    BE.write_bytes oc p bytes

  let su128 _oc () _v _p = assert false

  (* Could also write the field names with the value in a pair... *)
  let tup_opn _typs oc () p =
    BE.write_byte oc p (byte_of_char oc '(')

  let tup_cls _typs oc () p =
    BE.write_byte oc p (byte_of_char oc ')')

  let tup_sep _typs _idx oc () p =
    BE.write_byte oc p (byte_of_char oc ' ')

  let rec_opn _typs oc () p =
    BE.write_byte oc p (byte_of_char oc '(')

  let rec_cls _typs oc () p =
    BE.write_byte oc p (byte_of_char oc ')')

  let rec_sep _typs _idx oc () p =
    BE.write_byte oc p (byte_of_char oc ' ')

  let vec_opn _dim _typ _oc () _p = assert false
  let vec_cls _dim _typ _oc () _p = assert false
  let vec_sep _dim _typ _n _oc () _p = assert false

  let nullable _typs _oc () p = p
  let snull oc () p =
    let null = BE.dword_of_const oc (Uint32.of_int32 0x6c_6c_75_6el) in
    BE.write_dword oc p null

  let snotnull _oc () p = p
end
