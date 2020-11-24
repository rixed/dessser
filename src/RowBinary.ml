open Batteries
open Stdint
open Dessser
module T = DessserTypes
module E = DessserExpressions
open E.Ops

module Ser : SER with type config = unit =
struct
  type config = unit
  type state = unit
  let ptr _mn = T.dataptr

  let start ?(config=()) _mn p = config, p
  let stop () p = p
  type ser = state -> T.maybe_nullable -> T.path -> E.t -> E.t -> E.t

  let sfloat () _ _ v p =
    write_qword LittleEndian p (qword_of_float v)

  (* v must be a u32: *)
  let write_leb128 p v =
    let t_ptr_sz = T.TPair (TDataPtr, T.u32) in
    first (
      loop_until
        ~body:(comment "Loop body for write_leb128"
          (E.func1 t_ptr_sz (fun _l p_wlen ->
            E.with_sploded_pair "write_leb128" p_wlen (fun p wlen ->
              let b =
                byte_of_u8 (
                  choose ~cond:(gt (u32 (Uint32.of_int 128)) wlen)
                    ~then_:(log_and (to_u8 wlen) (u8 (Uint8.of_int 127)))
                    ~else_:(log_or (to_u8 wlen) (u8 (Uint8.of_int 128)))) in
              pair
                (write_byte p b)
                (right_shift wlen (u8 (Uint8.of_int 7)))))))
        ~cond:(comment "Condition for write_leb128 (until wlen is 0)"
          (E.func1 t_ptr_sz (fun _l ptr_sz -> gt (secnd ptr_sz) (u32 Uint32.zero))))
        ~init:(pair p v))

  let sstring () _ _ v p =
    let p = write_leb128 p (string_length v) in
    write_bytes p (bytes_of_string v)

  let sbool () _ _ v p =
    write_byte p (byte_of_u8 (u8_of_bool v))

  let schar () _ _ v p =
    write_byte p (byte_of_u8 (u8_of_char v))

  let si8 () _ _ v p =
    write_byte p (byte_of_u8 (to_u8 v))

  let si16 () _ _ v p =
    write_word LittleEndian p (word_of_u16 (to_u16 v))

  let si32 () _ _ v p =
    write_dword LittleEndian p (dword_of_u32 (to_u32 v))

  let si24 () vtyp0 path v p =
    si32 () vtyp0 path (to_i32 v) p

  let si64 () _ _ v p =
    write_qword LittleEndian p (qword_of_u64 (to_u64 v))

  let si40 () vtyp0 path v p =
    si64 () vtyp0 path (to_i64 v) p

  let si48 = si40

  let si56 = si40

  let si128 () _ _ v p =
    write_oword LittleEndian p (oword_of_u128 (to_u128 v))

  let su8 () _ _ v p =
    write_byte p (byte_of_u8 v)

  let su16 () _ _ v p =
    write_word LittleEndian p (word_of_u16 v)

  let su32 () _ _ v p =
    write_dword LittleEndian p (dword_of_u32 v)

  let su24 () vtyp0 path v p =
    su32 () vtyp0 path (to_u32 v) p

  let su64 () _ _ v p =
    write_qword LittleEndian p (qword_of_u64 v)

  let su40 () vtyp0 path v p =
    su64 () vtyp0 path (to_u64 v) p

  let su48 = su40

  let su56 = su40

  let su128 () _ _ v p =
    write_oword LittleEndian p (oword_of_u128 v)

  let tup_opn () _ _ _ p = p
  let tup_cls () _ _ p = p
  let tup_sep () _ _ p = p

  let rec_opn () _ _ _ p = p
  let rec_cls () _ _ p = p
  let rec_sep () _ _ p = p

  let sum_opn st mn0 path mns lbl p =
    let p = tup_opn st mn0 path mns p in
    let p = su16 st mn0 path lbl p in
    tup_sep st mn0 path p

  let sum_cls st mn0 path p =
    tup_cls st mn0 path p

  let vec_opn () _ _ _ _  p = p
  let vec_cls () _ _ p = p
  let vec_sep () _ _ p = p

  let list_opn () _ _ _ n p =
    let n = match n with
      | Some n -> n
      | None -> failwith "RowBinary.Ser needs list size upfront" in
    write_leb128 p n

  let list_cls () _ _ p = p
  let list_sep () _ _ p = p

  let nullable () _ _ p = p

  let snull _t () _ _ p =
    write_byte p (byte Uint8.one)

  let snotnull _t () _ _ p =
    write_byte p (byte Uint8.zero)

  type ssizer = T.maybe_nullable -> T.path -> E.t -> ssize

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
  let ssize_of_sum = ssize_of_u16
  let ssize_of_vec _ _ _ = ConstSize 0

  let ssize_of_leb128 n =
    let t_u32_u32 = T.TPair (T.u32, T.u32) in
    size_of_u32 (first (
      loop_while
        ~cond:(comment "Condition for ssize_of_leb128"
          (E.func1 t_u32_u32 (fun _l lebsz_n ->
            E.with_sploded_pair "ssize_of_leb128" lebsz_n (fun lebsz n ->
              let max_len_for_lebsz = left_shift lebsz (u8 (Uint8.of_int 7)) in
              ge n max_len_for_lebsz))))
        ~body:(comment "Loop for ssize_of_leb128"
          (E.func1 t_u32_u32 (fun _l lebsz_n ->
            E.with_sploded_pair "ssize_of_leb128" lebsz_n (fun lebsz n ->
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
      let_ "wlen" (string_length v)
        ~in_:(add (ssize_of_leb128 (identifier "wlen"))
                  (identifier "wlen")))
end

module Des : DES with type config = unit =
struct
  type config = unit
  type state = unit
  let ptr _mn = T.dataptr

  let start ?(config=()) _mn p = config, p
  let stop () p = p
  type des = state -> T.maybe_nullable -> T.path -> E.t -> E.t

  let dfloat () _ _ p =
    let w_p = read_qword LittleEndian p in
    E.with_sploded_pair "dfloat" w_p (fun w p ->
      pair (float_of_qword w) p)

  (* Returns a size and a dataptr: *)
  let read_leb128 p =
    let t_u32_u8 = T.TPair (T.u32, T.u8) in
    let_ "leb_shft_ptr"
      (read_while
        ~cond:(comment "Condition for read_leb128"
          (E.func1 T.byte (fun _l b -> ge b (byte (Uint8.of_int 128)))))
        ~reduce:(comment "Reducer for read_leb128"
          (E.func2 t_u32_u8 T.byte (fun _l leb_shft b ->
            let byte = log_and (u8_of_byte b) (u8 (Uint8.of_int 127)) in
            let leb = first leb_shft
            and shft = secnd leb_shft in
            pair (add  (left_shift (to_u32 byte) shft) leb)
                 (add shft (u8 (Uint8.of_int 7))))))
        ~init:(pair (u32 Uint32.zero) (u8 Uint8.zero))
        ~pos:p)
      (* Still have to add the last byte (which is <128): *)
      ~in_:(
        comment "Last byte from read_leb128"
          (E.with_sploded_pair "leb128_1" (identifier "leb_shft_ptr") (fun leb_shft ptr ->
            E.with_sploded_pair "leb128_2" (read_byte ptr) (fun last_b ptr ->
              pair
                (size_of_u32 (add (left_shift (to_u32 (u8_of_byte last_b))
                                              (secnd leb_shft))
                                  (first leb_shft)))
                ptr))))

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring () _ _ p =
    E.with_sploded_pair "dstring1" (read_leb128 p) (fun len p ->
      E.with_sploded_pair "dstring2" (read_bytes p len) (fun bs p ->
        pair (string_of_bytes bs) p))

  let dbool () _ _ p =
    E.with_sploded_pair "dbool" (read_byte p) (fun b p ->
      pair (not_ (eq (u8_of_byte b) (u8 Uint8.zero))) p)

  let dchar ()_ _  p =
    E.with_sploded_pair "dchar" (read_byte p) (fun b p ->
      pair (char_of_u8 (u8_of_byte b)) p)

  let di8 () _ _ p =
    E.with_sploded_pair "di8" (read_byte p) (fun b p ->
      pair (to_i8 (u8_of_byte b)) p)

  let du8 () _ _ p =
    E.with_sploded_pair "du8" (read_byte p) (fun b p ->
      pair (u8_of_byte b) p)

  let di16 () _ _ p =
    E.with_sploded_pair "di16" (read_word LittleEndian p) (fun w p ->
      pair (to_i16 (u16_of_word w)) p)

  let du16 () _ _ p =
    E.with_sploded_pair "du16" (read_word LittleEndian p) (fun w p ->
      pair (u16_of_word w) p)

  let di24 () _ _ p =
    E.with_sploded_pair "di24" (read_dword LittleEndian p) (fun w p ->
      pair (to_i24 (u32_of_dword w)) p)

  let du24 () _ _ p =
    E.with_sploded_pair "du24" (read_dword LittleEndian p) (fun w p ->
      pair (to_u24 (u32_of_dword w)) p)

  let di32 () _ _ p =
    E.with_sploded_pair "di32" (read_dword LittleEndian p) (fun w p ->
      pair (to_i32 (u32_of_dword w)) p)

  let du32 () _ _ p =
    E.with_sploded_pair "du32" (read_dword LittleEndian p) (fun w p ->
      pair (u32_of_dword w) p)

  let di40 () _ _ p =
    E.with_sploded_pair "di40" (read_qword LittleEndian p) (fun w p ->
      pair (to_i40 (u64_of_qword w)) p)

  let du40 () _ _ p =
    E.with_sploded_pair "du40" (read_qword LittleEndian p) (fun w p ->
      pair (to_u40 (u64_of_qword w)) p)

  let di48 () _ _ p =
    E.with_sploded_pair "di48" (read_qword LittleEndian p) (fun w p ->
      pair (to_i48 (u64_of_qword w)) p)

  let du48 () _ _ p =
    E.with_sploded_pair "du48" (read_qword LittleEndian p) (fun w p ->
      pair (to_u48 (u64_of_qword w)) p)

  let di56 () _ _ p =
    E.with_sploded_pair "di56" (read_qword LittleEndian p) (fun w p ->
      pair (to_i56 (u64_of_qword w)) p)

  let du56 () _ _ p =
    E.with_sploded_pair "du56" (read_qword LittleEndian p) (fun w p ->
      pair (to_u56 (u64_of_qword w)) p)

  let di64 () _ _ p =
    E.with_sploded_pair "di64" (read_qword LittleEndian p) (fun w p ->
      pair (to_i64 (u64_of_qword w)) p)

  let du64 () _ _ p =
    E.with_sploded_pair "du64" (read_qword LittleEndian p) (fun w p ->
      pair (u64_of_qword w) p)

  let di128 () _ _ p =
    E.with_sploded_pair "di128" (read_oword LittleEndian p) (fun w p ->
      pair (to_i128 (u128_of_oword w)) p)

  let du128 () _ _ p =
    E.with_sploded_pair "di128" (read_oword LittleEndian p) (fun w p ->
      pair (u128_of_oword w) p)

  (* Items of a tuples are just concatenated together: *)
  let tup_opn () _ _ _ p = p
  let tup_cls () _ _ p = p
  let tup_sep () _ _ p = p

  let rec_opn () _ _ _ p = p
  let rec_cls () _ _ p = p
  let rec_sep () _ _ p = p

  (* RowBinary has no sum types, so we encode the value as a pair: *)
  let sum_opn st mn0 path mns p =
    let p = tup_opn st mn0 path mns p in
    let c_p = du16 st mn0 path p in
    E.with_sploded_pair "sum_opn" c_p (fun c p ->
      let p = tup_sep st mn0 path p in
      pair c p)

  let sum_cls st mn0 path p =
    tup_cls st mn0 path p

  (* Vectors: ClickHouse does not distinguish between vectors (of known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and our lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn () _ _ _ _ p = p
  let vec_cls () _ _ p = p
  let vec_sep () _ _ p = p

  let list_opn () = KnownSize
    (fun _ _ _ p ->
      E.with_sploded_pair "list_opn" (read_leb128 p) (fun dim p ->
        pair (u32_of_size dim) p))

  let list_cls () _ _ p = p
  let list_sep () _ _ p = p

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null () _ _ p =
    eq (peek_byte p (size 0)) (byte Uint8.one)

  let dnull _t () _ _ p =
    data_ptr_add p (size 1)

  let dnotnull = dnull
end
