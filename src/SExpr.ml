open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open Ops
module T = DessserTypes

let list_prefix_length = true

module Des : DES =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _mn p = (), p
  let stop () p = p
  type des = state -> maybe_nullable -> path -> e -> e

  let skip n p = data_ptr_add p (size n)
  let skip1 = skip 1

  let di op () _ _ p =
    (* Accumulate everything up to the next space or parenthesis, and then
     * run [op] to convert from a string: *)
    let cond = func1 T.byte (fun b ->
      (* No other options as we would meet a space before a '(' or '"': *)
      not_ (or_ (eq b (byte_of_const_char ' '))
                (eq b (byte_of_const_char ')'))))
    and init = bytes_of_string (string "")
    and reduce = func2 T.bytes T.byte append_byte in
    let str_p = read_while ~cond ~reduce ~init ~pos:p in
    with_sploded_pair "dfloat" str_p (fun str p ->
      pair (op (string_of_bytes str)) p)

  let dfloat = di float_of_string

  let dbool () _ _ p =
    with_sploded_pair "dbool" (read_byte p) (fun b p ->
      pair (eq b (byte_of_const_char 'T')) p)

  (* Read a string of bytes and process them through [conv]: *)
  let dbytes conv p =
    (* Skip the double-quote: *)
    let p = skip1 p in
    (* Read up to next double-quote: *)
    (* FIXME: handle escaping backslash! *)
    let cond = func1 T.byte (fun b -> not_ (eq b (byte_of_const_char '"')))
    and init = bytes_of_string (string "")
    and reduce = func2 T.bytes T.byte append_byte in
    let str_p = read_while ~cond ~reduce ~init ~pos:p in
    with_sploded_pair "dfloat" str_p (fun str p ->
      (* Skip the closing double-quote: *)
      let p = skip1 p in
      pair (conv str) p)

  let dstring () _ _ p = dbytes string_of_bytes p
  (* Chars are encoded as single char strings *)
  let dchar () _ _ p = dbytes (char_of_string % string_of_bytes) p

  let di8 = di i8_of_string
  let du8 = di u8_of_string
  let di16 = di i16_of_string
  let du16 = di u16_of_string
  let di24 = di i24_of_string
  let du24 = di u24_of_string
  let di32 = di i32_of_string
  let du32 = di u32_of_string
  let di40 = di i40_of_string
  let du40 = di u40_of_string
  let di48 = di i48_of_string
  let du48 = di u48_of_string
  let di56 = di i56_of_string
  let du56 = di u56_of_string
  let di64 = di i64_of_string
  let du64 = di u64_of_string
  let di128 = di i128_of_string
  let du128 = di u128_of_string

  let tup_opn () _ _ _ p = skip1 p
  let tup_cls () _ _ p = skip1 p
  let tup_sep _n () _ _ p = skip1 p

  let rec_opn () _ _ _ p = skip1 p
  let rec_cls () _ _ p = skip1 p
  let rec_sep _n () _ _ p = skip1 p

  let vec_opn () _ _ _ _ p = skip1 p
  let vec_cls () _ _ p = skip1 p
  let vec_sep _n () _ _ p = skip1 p

  let list_opn =
    if list_prefix_length then
      KnownSize (fun () vtyp0 path _ p ->
        with_sploded_pair "list_opn" (du32 () vtyp0 path p) (fun v p ->
          pair v (skip 2 p)))
    else
      UnknownSize (
        (fun () _ _ _ p -> skip1 p),
        (fun () _ _ p ->
          eq (peek_byte p (size 0)) (byte_of_const_char ')')))
  let list_cls () _ _ p = skip1 p
  let list_sep () _ _ p = skip1 p

  let is_null () _ _ p =
    (* TODO: For this to work even when there is less than 4 bytes to read [and_]
     * must short cut! *)
    (* NULL *)
    and_ (eq (peek_byte p (size 0)) (byte 0x6e))
         (and_ (eq (peek_byte p (size 1)) (byte 0x75))
               (and_ (eq (peek_byte p (size 2)) (byte 0x6c))
                     (eq (peek_byte p (size 3)) (byte 0x6c))))

  let dnull _t () _ _ p =
    data_ptr_add p (size 4)

  let dnotnull _t () _ _ p = p
end

module Ser : SER =
struct

  type state = unit
  let ptr _vtyp = dataptr

  let start _v p = (), p
  let stop () p = p

  type ser = state -> maybe_nullable -> path -> e -> e -> e

  let sfloat () _ _ v p =
    write_bytes p (bytes_of_string (string_of_float v))

  let sbytes v p =
    let quo = byte_of_const_char '"' in
    let p = write_byte p quo in
    (* FIXME: escape double quotes: *)
    let p = write_bytes p v in
    write_byte p quo

  let sstring () _ _ v p = sbytes (bytes_of_string v) p
  let schar () _ _ v p = sbytes (bytes_of_string (string_of_char v)) p

  let sbool () _ _ v p =
    write_byte p (choose v (byte_of_const_char 'T') (byte_of_const_char 'F'))

  let si () _ _ v p =
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

  (* Could also write the field names with the value in a pair... *)
  let tup_opn () _ _ _ p =
    write_byte p (byte_of_const_char '(')

  let tup_cls () _ _ p =
    write_byte p (byte_of_const_char ')')

  let tup_sep _idx () _ _ p =
    write_byte p (byte_of_const_char ' ')

  let rec_opn () _ _ _ p =
    write_byte p (byte_of_const_char '(')

  let rec_cls () _ _ p =
    write_byte p (byte_of_const_char ')')

  let rec_sep _idx () _ _ p =
    write_byte p (byte_of_const_char ' ')

  let vec_opn () _ _ _ _ p =
    write_byte p (byte_of_const_char '(')

  let vec_cls () _ _ p =
    write_byte p (byte_of_const_char ')')

  let vec_sep _n () _ _ p =
    write_byte p (byte_of_const_char ' ')

  let list_opn () vtyp0 path _ n p =
    let p =
      if list_prefix_length then
        match n with
        | Some n ->
            let p = su32 () vtyp0 path n p in
            write_byte p (byte_of_const_char ' ')
        | None ->
            failwith "SExpr.Set needs list length upfront"
      else
        p in
    write_byte p (byte_of_const_char '(')

  let list_cls () _ _ p =
    write_byte p (byte_of_const_char ')')

  let list_sep () _ _ p =
    write_byte p (byte_of_const_char ' ')

  let nullable () _ _ p = p

  let snull _t () _ _ p =
    write_dword LittleEndian p (dword (Uint32.of_int32 0x6c_6c_75_6el))

  let snotnull _t () _ _ p = p

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
