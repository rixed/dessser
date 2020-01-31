open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open Ops
module T = DessserTypes

module Des : DES =
struct

  type state = unit
  let ptr _vtyp = dataptr

  let start _vtyp p = (), p
  let stop () p = p
  type des = state -> e -> e

  let dfloat () p =
    let w_p = read_qword LittleEndian p in
    with_sploded_pair "dfloat" w_p (fun w p ->
      pair (float_of_qword w) p)

  let read_leb128 p =
    let t_u32_u8 = TPair (T.u32, T.u8) in
    let_ "leb_shft_ptr"
      (read_while
        ~cond:(comment "Condition for read_leb128"
          (func1 T.byte (fun b -> ge b (byte 128))))
        ~reduce:(comment "Reducer for read_leb128"
          (func2 t_u32_u8 T.byte (fun leb_shft b ->
            let byte = log_and (u8_of_byte b) (u8 127) in
            let leb = fst leb_shft
            and shft = snd leb_shft in
            pair (add  (left_shift (to_u32 byte) shft) leb)
                 (add shft (u8 7)))))
        ~init:(pair (u32 Uint32.zero) (u8 0))
        ~pos:p)
      (* Still have to add the last byte (which is <128): *)
      (comment "Last byte from read_leb128"
        (with_sploded_pair "leb128_1" (identifier "leb_shft_ptr") (fun leb_shft ptr ->
          with_sploded_pair "leb128_2" (read_byte ptr) (fun last_b ptr ->
            pair
              (size_of_u32 (add (left_shift (to_u32 (u8_of_byte last_b))
                                            (snd leb_shft))
                                (fst leb_shft)))
              ptr))))

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring () p =
    with_sploded_pair "dstring" (read_leb128 p) (fun len p ->
      with_sploded_pair "dstring" (read_bytes p len) (fun bs p ->
        pair (string_of_bytes bs) p))

  let dbool () p =
    with_sploded_pair "dbool" (read_byte p) (fun b p ->
      pair (not_ (eq (u8_of_byte b) (u8 0))) p)

  let dchar () p =
    with_sploded_pair "dchar" (read_byte p) (fun b p ->
      pair (char_of_u8 (u8_of_byte b)) p)

  let di8 () p =
    with_sploded_pair "di8" (read_byte p) (fun b p ->
      pair (to_i8 (u8_of_byte b)) p)

  let du8 () p =
    with_sploded_pair "du8" (read_byte p) (fun b p ->
      pair (u8_of_byte b) p)

  let di16 () p =
    with_sploded_pair "di16" (read_word LittleEndian p) (fun w p ->
      pair (to_i16 (u16_of_word w)) p)

  let du16 () p =
    with_sploded_pair "du16" (read_word LittleEndian p) (fun w p ->
      pair (u16_of_word w) p)

  let di24 () p =
    with_sploded_pair "di24" (read_dword LittleEndian p) (fun w p ->
      pair (to_i24 (u32_of_dword w)) p)

  let du24 () p =
    with_sploded_pair "du24" (read_dword LittleEndian p) (fun w p ->
      pair (to_u24 (u32_of_dword w)) p)

  let di32 () p =
    with_sploded_pair "di32" (read_dword LittleEndian p) (fun w p ->
      pair (to_i32 (u32_of_dword w)) p)

  let du32 () p =
    with_sploded_pair "du32" (read_dword LittleEndian p) (fun w p ->
      pair (u32_of_dword w) p)

  let di40 () p =
    with_sploded_pair "di40" (read_qword LittleEndian p) (fun w p ->
      pair (to_i40 (u64_of_qword w)) p)

  let du40 () p =
    with_sploded_pair "du40" (read_qword LittleEndian p) (fun w p ->
      pair (to_u40 (u64_of_qword w)) p)

  let di48 () p =
    with_sploded_pair "di48" (read_qword LittleEndian p) (fun w p ->
      pair (to_i48 (u64_of_qword w)) p)

  let du48 () p =
    with_sploded_pair "du48" (read_qword LittleEndian p) (fun w p ->
      pair (to_u48 (u64_of_qword w)) p)

  let di56 () p =
    with_sploded_pair "di56" (read_qword LittleEndian p) (fun w p ->
      pair (to_i56 (u64_of_qword w)) p)

  let du56 () p =
    with_sploded_pair "du56" (read_qword LittleEndian p) (fun w p ->
      pair (to_u56 (u64_of_qword w)) p)

  let di64 () p =
    with_sploded_pair "di64" (read_qword LittleEndian p) (fun w p ->
      pair (to_i64 (u64_of_qword w)) p)

  let du64 () p =
    with_sploded_pair "du64" (read_qword LittleEndian p) (fun w p ->
      pair (u64_of_qword w) p)

  let di128 () p =
    with_sploded_pair "di128" (read_oword LittleEndian p) (fun w p ->
      pair (to_i128 (u128_of_oword w)) p)

  let du128 () p =
    with_sploded_pair "di128" (read_oword LittleEndian p) (fun w p ->
      pair (u128_of_oword w) p)

  (* Items of a tuples are just concatenated together: *)
  let tup_opn () _ p = p
  let tup_cls () p = p
  let tup_sep _n () p = p

  let rec_opn () _ p = p
  let rec_cls () p = p
  let rec_sep _n () p = p

  (* Vectors: ClickHouse does not distinguish between vectors (of known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and our lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn () _ _ p = p
  let vec_cls () p = p
  let vec_sep _n () p = p

  let list_opn = KnownSize
    (fun () _ p ->
      with_sploded_pair "list_opn" (read_leb128 p) (fun dim p ->
        pair (u32_of_size dim) p))

  let list_cls () p = p
  let list_sep () p = p

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null () p =
    eq (peek_byte p (size 0)) (byte 1)

  let dnull _t () p =
    data_ptr_add p (size 1)

  let dnotnull = dnull
end

module Ser : SER =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _vtyp p = (), p
  let stop () p = p
  type ser = state -> e -> e -> e

  let sfloat () v p =
    write_qword LittleEndian p (qword_of_float v)

  let write_leb128 p v =
    let t_ptr_sz = TPair (TDataPtr, T.u32) in
    fst (
      loop_until
        ~body:(comment "Loop body for write_leb128"
          (func1 t_ptr_sz (fun p_wlen ->
            with_sploded_pair "write_leb128" p_wlen (fun p wlen ->
              let b =
                choose ~cond:(gt (u32 (Uint32.of_int 128)) wlen)
                  (log_and (to_u8 wlen) (u8 127))
                  (log_or (to_u8 wlen) (u8 128)) in
              pair
                (write_byte p b)
                (right_shift wlen (u8 7))))))
        ~cond:(comment "Condition for write_leb128 (until wlen is 0)"
          (func1 t_ptr_sz (fun ptr_sz -> gt (snd ptr_sz) (u32 Uint32.zero))))
        ~init:(pair p (u32_of_size v)))

  let sstring () v p =
    let p = write_leb128 p (string_length v) in
    write_bytes p (bytes_of_string v)

  let sbool () v p =
    write_byte p (byte_of_u8 (u8_of_bool v))

  let schar () v p =
    write_byte p (byte_of_u8 (u8_of_char v))

  let si8 () v p =
    write_byte p (byte_of_u8 (to_u8 v))

  let si16 () v p =
    write_word LittleEndian p (word_of_u16 (to_u16 v))

  let si32 () v p =
    write_dword LittleEndian p (dword_of_u32 (to_u32 v))

  let si24 = si32

  let si64 () v p =
    write_qword LittleEndian p (qword_of_u64 (to_u64 v))

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 () v p =
    write_oword LittleEndian p (oword_of_u128 (to_u128 v))

  let su8 () v p =
    write_byte p (byte_of_u8 v)

  let su16 () v p =
    write_word LittleEndian p (word_of_u16 v)

  let su32 () v p =
    write_dword LittleEndian p (dword_of_u32 v)

  let su24 = su32

  let su64 () v p =
    write_qword LittleEndian p (qword_of_u64 v)

  let su40 = su64

  let su48 = su64

  let su56 = su64

  let su128 () v p =
    write_oword LittleEndian p (oword_of_u128 v)

  let tup_opn () _ p = p
  let tup_cls () p = p
  let tup_sep _idx () p = p

  let rec_opn () _ p = p
  let rec_cls () p = p
  let rec_sep _idx () p = p

  let vec_opn () _ _  p = p
  let vec_cls () p = p
  let vec_sep _idx () p = p

  let list_opn () _ n p =
    write_leb128 p (size_of_u32 n)

  let list_cls () p = p
  let list_sep () p = p

  let nullable () p = p

  let snull _t () p =
    write_byte p (byte 1)

  let snotnull _t () p =
    write_byte p (byte 0)

  type ssizer = maybe_nullable -> path -> e -> ssize

  let ssize_of_float _ _ _ = ConstSize 8
  let ssize_of_bool _ _ _ = ConstSize 1
  let ssize_of_char _ _ _ = ConstSize 1
  let ssize_of_i8 _ _ _ = ConstSize 1
  let ssize_of_u8 _ _ _ = ConstSize 1
  let ssize_of_i16 _ _ _ = ConstSize 2
  let ssize_of_u16 _ _ _ = ConstSize 2
  let ssize_of_i32 _ _ _ = ConstSize 4
  let ssize_of_u32 _ _ _ = ConstSize 4
  let ssize_of_i24 = ssize_of_i32
  let ssize_of_u24 = ssize_of_u32
  let ssize_of_i64 _ _ _ = ConstSize 8
  let ssize_of_u64 _ _ _ = ConstSize 8
  let ssize_of_i40 = ssize_of_i64
  let ssize_of_u40 = ssize_of_u64
  let ssize_of_i48 = ssize_of_i64
  let ssize_of_u48 = ssize_of_u64
  let ssize_of_i56 = ssize_of_i64
  let ssize_of_u56 = ssize_of_u64
  let ssize_of_i128 _ _ _ = ConstSize 16
  let ssize_of_u128 _ _ _ = ConstSize 16

  let ssize_of_tup _ _ _ = ConstSize 0
  let ssize_of_rec _ _ _ = ConstSize 0
  let ssize_of_vec _ _ _ = ConstSize 0

  let ssize_of_leb128 n =
    let t_u32_u32 = TPair (T.u32, T.u32) in
    size_of_u32 (fst (
      loop_while
        ~cond:(comment "Condition for ssize_of_leb128"
          (func1 t_u32_u32 (fun lebsz_n ->
            with_sploded_pair "ssize_of_leb128" lebsz_n (fun lebsz n ->
              let max_len_for_lebsz = left_shift lebsz (u8 7) in
              ge n max_len_for_lebsz))))
        ~body:(comment "Loop for ssize_of_leb128"
          (func1 t_u32_u32 (fun lebsz_n ->
            with_sploded_pair "ssize_of_leb128" lebsz_n (fun lebsz n ->
              pair (add lebsz (u32 Uint32.one)) n))))
        ~init:(pair (u32 Uint32.one) n)))

  (* SerSize of a list is the size of the LEB128 prefix, same as for
   * ssize_of_string below) *)
  let ssize_of_list _ _ lst =
    DynSize (ssize_of_leb128 (list_length lst))

  let ssize_of_null _ _ = ConstSize 1

  (* Size of a string is it's length in bytes + the size of the LEB128 prefix,
   * which size is 1 bytes per group of 7 bits. *)
  let ssize_of_string _ _ v =
    DynSize (
      let_ "wlen" (u32_of_size (string_length v))
        (add (ssize_of_leb128 (identifier "wlen"))
             (identifier "wlen")))
end
