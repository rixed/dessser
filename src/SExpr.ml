open Batteries
open Stdint
open Dessser
module T = DessserTypes
module E = DessserExpressions
open E.Ops

let list_prefix_length = true

module Ser : SER =
struct
  type state = unit

  let ptr _vtyp = T.dataptr

  let start _v p = (), p

  let stop () p = p

  type ser = state -> T.maybe_nullable -> T.path -> E.t -> E.t -> E.t

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
    write_bytes p (bytes_of_string (string_of_int_ v))

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

  let tup_sep _n () _ _ p =
    write_byte p (byte_of_const_char ' ')

  let rec_opn () _ _ _ p =
    write_byte p (byte_of_const_char '(')

  let rec_cls () _ _ p =
    write_byte p (byte_of_const_char ')')

  let rec_sep _n () _ _ p =
    write_byte p (byte_of_const_char ' ')

  let sum_opn st mn0 path mns lbl p =
    let p = tup_opn st mn0 path mns p in
    let p = su16 st mn0 path lbl p in
    tup_sep 0 st mn0 path p

  let sum_cls st mn0 path p =
    tup_cls st mn0 path p

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
            failwith "SExpr.Ser needs list length upfront"
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

  type ssizer = T.maybe_nullable -> T.path -> E.t -> ssize
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
  let ssize_of_sum _ _ _ = todo_ssize ()
  let ssize_of_vec _ _ _ = todo_ssize ()
  let ssize_of_list _ _ _ = todo_ssize ()
  let ssize_of_null _ _ = todo_ssize ()
end

module Des : DES =
struct
  type state = unit

  let ptr _vtyp = T.dataptr

  let start _mn p = (), p

  let stop () p = p

  type des = state -> T.maybe_nullable -> T.path -> E.t -> E.t

  let skip n p = data_ptr_add p (size n)

  let skip1 = skip 1

  (* Accumulate bytes into a string that is then converted with [op]: *)
  let di op () _ _ p =
    (* Accumulate everything up to the next space or parenthesis, and then
     * run [op] to convert from a string: *)
    let cond = E.func1 T.byte (fun _l b ->
      (* No other options as we would meet a space before a '(' or '"': *)
      not_ (or_ (eq b (byte_of_const_char ' '))
                (eq b (byte_of_const_char ')'))))
    and init = bytes_of_string (string "")
    and reduce = E.func2 T.bytes T.byte (fun _l -> append_byte) in
    let str_p = read_while ~cond ~reduce ~init ~pos:p in
    E.with_sploded_pair "di" str_p (fun str p ->
      pair (op (string_of_bytes str)) p)

  let tup_cls () _ _ p = skip1 p

  let tup_sep _n () _ _ p = skip1 p

  let dfloat = di float_of_string

  let dbool () _ _ p =
    E.with_sploded_pair "dbool" (read_byte p) (fun b p ->
      pair (eq b (byte_of_const_char 'T')) p)

  (* Read a string of bytes and process them through [conv]: *)
  let dbytes conv p =
    (* Skip the double-quote: *)
    let p = skip1 p in
    (* Read up to next double-quote: *)
    (* FIXME: handle escaping backslash! *)
    let cond = E.func1 T.byte (fun _l b -> not_ (eq b (byte_of_const_char '"')))
    and init = bytes_of_string (string "")
    and reduce = E.func2 T.bytes T.byte (fun _l -> append_byte) in
    let str_p = read_while ~cond ~reduce ~init ~pos:p in
    E.with_sploded_pair "dbytes" str_p (fun str p ->
      (* Skip the closing double-quote: *)
      let p = skip1 p in
      pair (conv str) p)

  let dstring () _ _ p = dbytes string_of_bytes p
  (* Chars are encoded as single char strings *)
  let dchar () _ _ p = dbytes (char_of_string % string_of_bytes) p

  (* Accumulate digits into a value with the given reducer: *)
  let fold init reduce () _ _ p =
    (* Accumulate everything up to the next space or parenthesis, and then
     * run [op] to convert from a string: *)
    let cond = E.func1 T.byte (fun _l b ->
      (* both ')' and ' ' are smaller than '0': *)
      ge b (byte_of_const_char '0')) in
    read_while ~cond ~reduce ~init ~pos:p

  let int_reducer int_type base of_byte =
    E.func2 int_type T.byte (fun _l n b ->
      add
        (mul n base)
        (of_byte (sub b (byte (Uint8.of_int (Char.code '0'))))))

  let unsigned int_type zero ten of_byte =
    fold zero (int_reducer int_type ten of_byte)

  let signed int_type zero one neg_one ten of_byte st mn0 path p =
    E.with_sploded_pair "maybe_sign1" (read_byte p) (fun b p' ->
      E.with_sploded_pair "maybe_sign2"
        (choose ~cond:(eq b (byte (Uint8.of_int (Char.code '-'))))
                ~then_:(pair p' neg_one)
                ~else_:(pair p one))
        (fun p sign ->
          let cont = unsigned int_type zero ten of_byte st mn0 path p in
          E.with_sploded_pair "maybe_sign3" cont (fun v p ->
            pair (mul sign v) p)))

  let di8 =
    signed T.i8 (i8 Int8.zero) (i8 Int8.one) (i8 (Int8.of_int ~-1)) (i8 (Int8.of_int 10)) (to_i8 % u8_of_byte)
  let du8 =
    unsigned T.u8 (u8 Uint8.zero) (u8 (Uint8.of_int 10)) u8_of_byte
  let di16 =
    signed T.i16 (i16 Int16.zero) (i16 Int16.one) (i16 (Int16.of_int ~-1)) (i16 (Int16.of_int 10)) (to_i16 % u8_of_byte)
  let du16 =
    unsigned T.u16 (u16 Uint16.zero) (u16 (Uint16.of_int 10)) (to_u16 % u8_of_byte)
  let di24 =
    signed T.i24 (i24 Int24.zero) (i24 Int24.one) (i24 (Int24.of_int ~-1)) (i24 (Int24.of_int 10)) (to_i24 % u8_of_byte)
  let du24 =
    unsigned T.u24 (u24 Uint24.zero) (u24 (Uint24.of_int 10)) (to_u24 % u8_of_byte)
  let di32 =
    signed T.i32 (i32 0l) (i32 1l) (i32 (-1l)) (i32 10l) (to_i32 % u8_of_byte)
  let du32 =
    unsigned T.u32 (u32 Uint32.zero) (u32 (Uint32.of_int 10)) (to_u32 % u8_of_byte)
  let di40 =
    signed T.i40 (i40 Int40.zero) (i40 Int40.one) (i40 (Int40.of_int ~-1)) (i40 (Int40.of_int 10)) (to_i40 % u8_of_byte)
  let du40 =
    unsigned T.u40 (u40 Uint40.zero) (u40 (Uint40.of_int 10)) (to_u40 % u8_of_byte)
  let di48 =
    signed T.i48 (i48 Int48.zero) (i48 Int48.one) (i48 (Int48.of_int ~-1)) (i48 (Int48.of_int 10)) (to_i48 % u8_of_byte)
  let du48 =
    unsigned T.u48 (u48 Uint48.zero) (u48 (Uint48.of_int 10)) (to_u48 % u8_of_byte)
  let di56 =
    signed T.i56 (i56 Int56.zero) (i56 Int56.one) (i56 (Int56.of_int ~-1)) (i56 (Int56.of_int 10)) (to_i56 % u8_of_byte)
  let du56 =
    unsigned T.u56 (u56 Uint56.zero) (u56 (Uint56.of_int 10)) (to_u56 % u8_of_byte)
  let di64 =
    signed T.i64 (i64 Int64.zero) (i64 Int64.one) (i64 (Int64.of_int ~-1)) (i64 (Int64.of_int 10)) (to_i64 % u8_of_byte)
  let du64 =
    unsigned T.u64 (u64 Uint64.zero) (u64 (Uint64.of_int 10)) (to_u64 % u8_of_byte)
  let di128 =
    signed T.i128 (i128 Int128.zero) (i128 Int128.one) (i128 (Int128.of_int ~-1)) (i128 (Int128.of_int 10)) (to_i128 % u8_of_byte)
  let du128 =
    unsigned T.u128 (u128 Uint128.zero) (u128 (Uint128.of_int 10)) (to_u128 % u8_of_byte)

  let tup_opn () _ _ _ p = skip1 p

  let tup_cls () _ _ p = skip1 p

  let tup_sep _n () _ _ p = skip1 p

  let rec_opn () _ _ _ p = skip1 p

  let rec_cls () _ _ p = skip1 p

  let rec_sep _n () _ _ p = skip1 p

  (* Sums are encoded as a pair of numeric label and value: *)
  let sum_opn st mn0 path mns p =
    let p = tup_opn st mn0 path mns p in
    let c_p = du16 st mn0 path p in
    E.with_sploded_pair "sum_opn" c_p (fun c p ->
      let p = tup_sep 0 st mn0 path p in
      pair c p)

  let sum_cls st mn0 path p =
    tup_cls st mn0 path p

  let vec_opn () _ _ _ _ p = skip1 p

  let vec_cls () _ _ p = skip1 p

  let vec_sep _n () _ _ p = skip1 p

  let list_opn =
    if list_prefix_length then
      KnownSize (fun () vtyp0 path _ p ->
        E.with_sploded_pair "list_opn" (du32 () vtyp0 path p) (fun v p ->
          pair v (skip 2 p)))
    else
      UnknownSize (
        (fun () _ _ _ p -> skip1 p),
        (fun () _ _ p ->
          eq (peek_byte p (size 0)) (byte_of_const_char ')')))

  let list_cls () _ _ p = skip1 p

  let list_sep () _ _ p = skip1 p

  let is_null () _ _ p =
    (* NULL *)
    and_ (eq (peek_byte p (size 0)) (byte (Uint8.of_int 0x6e)))
         (and_ (eq (peek_byte p (size 1)) (byte (Uint8.of_int 0x75)))
               (and_ (eq (peek_byte p (size 2)) (byte (Uint8.of_int 0x6c)))
                     (eq (peek_byte p (size 3)) (byte (Uint8.of_int 0x6c)))))

  let dnull _t () _ _ p = skip 4 p

  let dnotnull _t () _ _ p = p
end
