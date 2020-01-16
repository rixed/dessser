open Stdint
open Dessser

module Ser : SER =
struct

  type state = unit
  let ptr _vtyp = dataptr

  let start _v p = (), p
  let stop () p = p

  type ser = state -> e -> e -> e

  open Expression

  let sfloat () v p =
    WriteBytes (p, BytesOfString (StringOfFloat v))

  let byte_of_char c =
    ByteOfU8 (U8OfChar (Char c))

  let sstring () v p =
    let quo = byte_of_char '"' in
    let p = WriteByte (p, quo) in
    let v = BytesOfString v in
    let p = WriteBytes (p, v) in
    WriteByte (p, quo)

  let sbool () v p =
    WriteByte (p, Choose (v, byte_of_char 'T', byte_of_char 'F'))

  let si () v p =
    WriteBytes (p, BytesOfString (StringOfInt v))

  let si8 = si
  let si16 = si
  let si32 = si
  let si64 = si
  let si128 = si
  let su8 = si
  let su16 = si
  let su32 = si
  let su64 = si
  let su128 = si

  let schar () v p =
    WriteByte (p, ByteOfU8 (U8OfChar v))

  (* Could also write the field names with the value in a pair... *)
  let tup_opn () _ p =
    WriteByte (p, byte_of_char '(')

  let tup_cls () p =
    WriteByte (p, byte_of_char ')')

  let tup_sep _idx () p =
    WriteByte (p, byte_of_char ' ')

  let rec_opn () _ p =
    WriteByte (p, byte_of_char '(')

  let rec_cls () p =
    WriteByte (p, byte_of_char ')')

  let rec_sep _idx () p =
    WriteByte (p, byte_of_char ' ')

  let vec_opn () _ _ p =
    WriteByte (p, byte_of_char '[')

  let vec_cls () p =
    WriteByte (p, byte_of_char ']')

  let vec_sep _n () p =
    WriteByte (p, byte_of_char ' ')

  let list_opn () _ p _n =
    WriteByte (p, byte_of_char '[')

  let list_cls () p =
    WriteByte (p, byte_of_char ']')

  let list_sep () p =
    WriteByte (p, byte_of_char ' ')

  let nullable () p = p

  let snull _t () p =
    WriteDWord (LittleEndian, p, DWord (Uint32.of_int32 0x6c_6c_75_6el))

  let snotnull _t () p = p

  type ssizer = vtyp -> path -> e -> ssize
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
