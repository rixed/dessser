open Stdint
open Dessser
open DessserTypes
open DessserExpressions

module Ser : SER =
struct

  open Ops

  type state = unit
  let ptr _vtyp = dataptr

  let start _v p = (), p
  let stop () p = p

  type ser = state -> e -> e -> e

  let sfloat () v p =
    write_bytes p (bytes_of_string (string_of_float v))

  let sstring () v p =
    let quo = byte_of_const_char '"' in
    let p = write_byte p quo in
    let v = bytes_of_string v in
    let p = write_bytes p v in
    write_byte p quo

  let sbool () v p =
    write_byte p (choose v (byte_of_const_char 'T') (byte_of_const_char 'F'))

  let si () v p =
    write_bytes p (bytes_of_string (string_of_int v))

  let si8 = si
  let si16 = si
  let si24 = si
  let si32 = si
  let si40 = si
  let si48 = si
  let si56 = si
  let si64 = si
  let si128 = si
  let su8 = si
  let su16 = si
  let su24 = si
  let su32 = si
  let su40 = si
  let su48 = si
  let su56 = si
  let su64 = si
  let su128 = si

  let schar () v p =
    write_byte p (byte_of_char v)

  (* Could also write the field names with the value in a pair... *)
  let tup_opn () _ p =
    write_byte p (byte_of_const_char '(')

  let tup_cls () p =
    write_byte p (byte_of_const_char ')')

  let tup_sep _idx () p =
    write_byte p (byte_of_const_char ' ')

  let rec_opn () _ p =
    write_byte p (byte_of_const_char '(')

  let rec_cls () p =
    write_byte p (byte_of_const_char ')')

  let rec_sep _idx () p =
    write_byte p (byte_of_const_char ' ')

  let vec_opn () _ _ p =
    write_byte p (byte_of_const_char '[')

  let vec_cls () p =
    write_byte p (byte_of_const_char ']')

  let vec_sep _n () p =
    write_byte p (byte_of_const_char ' ')

  let list_opn () _ p _n =
    write_byte p (byte_of_const_char '[')

  let list_cls () p =
    write_byte p (byte_of_const_char ']')

  let list_sep () p =
    write_byte p (byte_of_const_char ' ')

  let nullable () p = p

  let snull _t () p =
    write_dword LittleEndian p (dword (Uint32.of_int32 0x6c_6c_75_6el))

  let snotnull _t () p = p

  type ssizer = maybe_nullable -> path -> e -> ssize
  let todo_ssize () = failwith "TODO: ssize for SExpr"
  let ssize_of_float _ _ _ = todo_ssize ()
  let ssize_of_string _ _ _ = todo_ssize ()
  let ssize_of_bool _ _ _ = todo_ssize ()
  let ssize_of_char _ _ _ = todo_ssize ()
  let ssize_of_i8 _ _ _ = todo_ssize ()
  let ssize_of_i16 _ _ _ = todo_ssize ()
  let ssize_of_i24 _ _ _ = todo_ssize ()
  let ssize_of_i32 _ _ _ = todo_ssize ()
  let ssize_of_i40 _ _ _ = todo_ssize ()
  let ssize_of_i48 _ _ _ = todo_ssize ()
  let ssize_of_i56 _ _ _ = todo_ssize ()
  let ssize_of_i64 _ _ _ = todo_ssize ()
  let ssize_of_i128 _ _ _ = todo_ssize ()
  let ssize_of_u8 _ _ _ = todo_ssize ()
  let ssize_of_u16 _ _ _ = todo_ssize ()
  let ssize_of_u24 _ _ _ = todo_ssize ()
  let ssize_of_u32 _ _ _ = todo_ssize ()
  let ssize_of_u40 _ _ _ = todo_ssize ()
  let ssize_of_u48 _ _ _ = todo_ssize ()
  let ssize_of_u56 _ _ _ = todo_ssize ()
  let ssize_of_u64 _ _ _ = todo_ssize ()
  let ssize_of_u128 _ _ _ = todo_ssize ()
  let ssize_of_tup _ _ _ = todo_ssize ()
  let ssize_of_rec _ _ _ = todo_ssize ()
  let ssize_of_vec _ _ _ = todo_ssize ()
  let ssize_of_list _ _ _ = todo_ssize ()
  let ssize_of_null _ _ = todo_ssize ()
end
