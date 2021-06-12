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
  let ptr _mn = T.DataPtr

  let start ?(config=()) _mn _l p = config, p
  let stop () _l p = p
  type ser = state -> T.maybe_nullable -> T.path -> E.env -> E.t -> E.t -> E.t

  let sfloat () _ _ _ v p =
    write_qword LittleEndian p (qword_of_float v)

  (* v must be a u32: *)
  let write_leb128 l p v =
    let t_ptr_sz = T.Pair (E.type_of l p, T.u32) in
    first (
      loop_until
        ~body:(comment "Loop body for write_leb128"
          (E.func1 ~l t_ptr_sz (fun l p_wlen ->
            E.with_sploded_pair ~l "write_leb128" p_wlen (fun _l p wlen ->
              let b =
                byte_of_u8 (
                  if_ (gt (u32 (Uint32.of_int 128)) wlen)
                    ~then_:(bit_and (to_u8 wlen) (u8 (Uint8.of_int 127)))
                    ~else_:(bit_or (to_u8 wlen) (u8 (Uint8.of_int 128)))) in
              make_pair
                (write_byte p b)
                (right_shift wlen (u8 (Uint8.of_int 7)))))))
        ~cond:(comment "Condition for write_leb128 (until wlen is 0)"
          (E.func1 ~l t_ptr_sz (fun _l ptr_sz ->
            gt (secnd ptr_sz) (u32 Uint32.zero))))
        ~init:(make_pair p v))

  let sstring () _ _ l v p =
    let p = write_leb128 l p (string_length v) in
    write_bytes p (bytes_of_string v)

  let sbool () _ _ _ v p =
    write_byte p (byte_of_u8 (u8_of_bool v))

  let schar () _ _ _ v p =
    write_byte p (byte_of_u8 (u8_of_char v))

  let si8 () _ _ _ v p =
    write_byte p (byte_of_u8 (to_u8 v))

  let si16 () _ _ _ v p =
    write_word LittleEndian p (word_of_u16 (to_u16 v))

  let si32 () _ _ _ v p =
    write_dword LittleEndian p (dword_of_u32 (to_u32 v))

  let si24 () vtyp0 path l v p =
    si32 () vtyp0 path l (to_i32 v) p

  let si64 () _ _ _ v p =
    write_qword LittleEndian p (qword_of_u64 (to_u64 v))

  let si40 () vtyp0 path l v p =
    si64 () vtyp0 path l (to_i64 v) p

  let si48 = si40

  let si56 = si40

  let si128 () _ _ _ v p =
    write_oword LittleEndian p (oword_of_u128 (to_u128 v))

  let su8 () _ _ _ v p =
    write_byte p (byte_of_u8 v)

  let su16 () _ _ _ v p =
    write_word LittleEndian p (word_of_u16 v)

  let su32 () _ _ _ v p =
    write_dword LittleEndian p (dword_of_u32 v)

  let su24 () vtyp0 path l v p =
    su32 () vtyp0 path l (to_u32 v) p

  let su64 () _ _ _ v p =
    write_qword LittleEndian p (qword_of_u64 v)

  let su40 () vtyp0 path l v p =
    su64 () vtyp0 path l (to_u64 v) p

  let su48 = su40

  let su56 = su40

  let su128 () _ _ _ v p =
    write_oword LittleEndian p (oword_of_u128 v)

  let tup_opn () _ _ _ _ p = p
  let tup_cls () _ _ _ p = p
  let tup_sep () _ _ _ p = p

  let rec_opn () _ _ _ _ p = p
  let rec_cls () _ _ _ p = p
  let rec_sep () _ _ _ p = p

  let sum_opn st mn0 path mns l lbl p =
    let p = tup_opn st mn0 path mns l p in
    let p = su16 st mn0 path l lbl p in
    tup_sep st mn0 path l p

  let sum_cls st mn0 path l p =
    tup_cls st mn0 path l p

  let vec_opn () _ _ _ _  _ p = p
  let vec_cls () _ _ _ p = p
  let vec_sep () _ _ _ p = p

  let list_opn () _ _ _ n l p =
    let n = match n with
      | Some n -> n
      | None -> failwith "RowBinary.Ser needs list size upfront" in
    write_leb128 l p n

  let list_cls () _ _ _ p = p
  let list_sep () _ _ _ p = p

  let nullable () _ _ _ p = p

  let snull _t () _ _ _ p =
    write_byte p (byte Uint8.one)

  let snotnull _t () _ _ _ p =
    write_byte p (byte Uint8.zero)

  type ssizer = T.maybe_nullable -> T.path -> E.env -> E.t -> ssize

  let ssize_of_float _ _ _ _ = ConstSize 8
  let ssize_of_bool _ _ _ _ = ConstSize 1
  let ssize_of_char _ _ _ _ = ConstSize 1
  let ssize_of_i8 _ _ _ _ = ConstSize 1
  let ssize_of_u8 _ _ _ _ = ConstSize 1
  let ssize_of_i16 _ _ _ _ = ConstSize 2
  let ssize_of_u16 _ _ _ _ = ConstSize 2
  let ssize_of_i32 _ _ _ _ = ConstSize 4
  let ssize_of_u32 _ _ _ _ = ConstSize 4
  let ssize_of_i24 = ssize_of_i32
  let ssize_of_u24 = ssize_of_u32
  let ssize_of_i64 _ _ _ _ = ConstSize 8
  let ssize_of_u64 _ _ _ _ = ConstSize 8
  let ssize_of_i40 = ssize_of_i64
  let ssize_of_u40 = ssize_of_u64
  let ssize_of_i48 = ssize_of_i64
  let ssize_of_u48 = ssize_of_u64
  let ssize_of_i56 = ssize_of_i64
  let ssize_of_u56 = ssize_of_u64
  let ssize_of_i128 _ _ _ _ = ConstSize 16
  let ssize_of_u128 _ _ _ _ = ConstSize 16

  let ssize_of_tup _ _ _ _ = ConstSize 0
  let ssize_of_rec _ _ _ _ = ConstSize 0
  let ssize_of_sum = ssize_of_u16
  let ssize_of_vec _ _ _ _ = ConstSize 0

  let ssize_of_leb128 l n =
    let t_u32_u32 = T.Pair (T.u32, T.u32) in
    size_of_u32 (first (
      loop_while
        ~cond:(comment "Condition for ssize_of_leb128"
          (E.func1 ~l t_u32_u32 (fun l lebsz_n ->
            E.with_sploded_pair ~l "ssize_of_leb128" lebsz_n (fun _l lebsz n ->
              let max_len_for_lebsz = left_shift lebsz (u8 (Uint8.of_int 7)) in
              ge n max_len_for_lebsz))))
        ~body:(comment "Loop for ssize_of_leb128"
          (E.func1 ~l t_u32_u32 (fun l lebsz_n ->
            E.with_sploded_pair ~l "ssize_of_leb128" lebsz_n (fun _l lebsz n ->
              make_pair (add lebsz (u32 Uint32.one)) n))))
        ~init:(make_pair (u32 Uint32.one) n)))

  (* SerSize of a list is the size of the LEB128 prefix, same as for
   * ssize_of_string below) *)
  let ssize_of_list _ _ l lst =
    DynSize (ssize_of_leb128 l (cardinality lst))

  let ssize_of_null _ _ = ConstSize 1

  (* Size of a string is it's length in bytes + the size of the LEB128 prefix,
   * which size is 1 bytes per group of 7 bits. *)
  let ssize_of_string _ _ l v =
    DynSize (
      let_ ~l ~name:"wlen" (string_length v) (fun l wlen ->
        add (ssize_of_leb128 l wlen) wlen))

  let ssize_start ?(config=()) _ =
    ignore config ;
    ConstSize 0
end

module Des : DES with type config = unit =
struct
  type config = unit
  type state = unit
  let ptr _mn = T.DataPtr

  let start ?(config=()) _mn _l p = config, p
  let stop () _l p = p
  type des = state -> T.maybe_nullable -> T.path -> E.env -> E.t -> E.t

  let dfloat () _ _ l p =
    let w_p = read_qword LittleEndian p in
    E.with_sploded_pair ~l "dfloat" w_p (fun _l w p ->
      make_pair (float_of_qword w) p)

  (* Returns a size and a DataPtr: *)
  let read_leb128 l p =
    let t_u32_u8 = T.Pair (T.u32, T.u8) in
    let_ ~l ~name:"leb_shft_ptr"
      (read_while
        ~cond:(comment "Condition for read_leb128"
          (E.func2 ~l t_u32_u8 T.Byte (fun _l _ b ->
            ge b (byte (Uint8.of_int 128)))))
        ~reduce:(comment "Reducer for read_leb128"
          (E.func2 ~l t_u32_u8 T.Byte (fun _l leb_shft b ->
            let byte = bit_and (u8_of_byte b) (u8 (Uint8.of_int 127)) in
            let leb = first leb_shft
            and shft = secnd leb_shft in
            make_pair (add  (left_shift (to_u32 byte) shft) leb)
                 (add shft (u8 (Uint8.of_int 7))))))
        ~init:(make_pair (u32 Uint32.zero) (u8 Uint8.zero))
        ~pos:p)
      (* Still have to add the last byte (which is <128): *)
      (fun l leb_shft_ptr ->
        comment "Last byte from read_leb128"
          (E.with_sploded_pair ~l "leb128_1" leb_shft_ptr (fun l leb_shft ptr ->
            E.with_sploded_pair ~l "leb128_2" (read_byte ptr) (fun _l last_b ptr ->
              make_pair
                (size_of_u32 (add (left_shift (to_u32 (u8_of_byte last_b))
                                              (secnd leb_shft))
                                  (first leb_shft)))
                ptr))))

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * Tup type: *)
  let des typ = ignore typ

  let dstring () _ _ l p =
    E.with_sploded_pair ~l "dstring1" (read_leb128 l p) (fun l len p ->
      E.with_sploded_pair ~l "dstring2" (read_bytes p len) (fun _l bs p ->
        make_pair (string_of_bytes bs) p))

  let dbool () _ _ l p =
    E.with_sploded_pair ~l "dbool" (read_byte p) (fun _l b p ->
      make_pair (not_ (eq (u8_of_byte b) (u8 Uint8.zero))) p)

  let dchar () _ _ l p =
    E.with_sploded_pair ~l "dchar" (read_byte p) (fun _l b p ->
      make_pair (char_of_u8 (u8_of_byte b)) p)

  let di8 () _ _ l p =
    E.with_sploded_pair ~l "di8" (read_byte p) (fun _l b p ->
      make_pair (to_i8 (u8_of_byte b)) p)

  let du8 () _ _ l p =
    E.with_sploded_pair ~l "du8" (read_byte p) (fun _l b p ->
      make_pair (u8_of_byte b) p)

  let di16 () _ _ l p =
    E.with_sploded_pair ~l "di16" (read_word LittleEndian p) (fun _l w p ->
      make_pair (to_i16 (u16_of_word w)) p)

  let du16 () _ _ l p =
    E.with_sploded_pair ~l "du16" (read_word LittleEndian p) (fun _l w p ->
      make_pair (u16_of_word w) p)

  let di24 () _ _ l p =
    E.with_sploded_pair ~l "di24" (read_dword LittleEndian p) (fun _l w p ->
      make_pair (to_i24 (u32_of_dword w)) p)

  let du24 () _ _ l p =
    E.with_sploded_pair ~l "du24" (read_dword LittleEndian p) (fun _l w p ->
      make_pair (to_u24 (u32_of_dword w)) p)

  let di32 () _ _ l p =
    E.with_sploded_pair ~l "di32" (read_dword LittleEndian p) (fun _l w p ->
      make_pair (to_i32 (u32_of_dword w)) p)

  let du32 () _ _ l p =
    E.with_sploded_pair ~l "du32" (read_dword LittleEndian p) (fun _l w p ->
      make_pair (u32_of_dword w) p)

  let di40 () _ _ l p =
    E.with_sploded_pair ~l "di40" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_i40 (u64_of_qword w)) p)

  let du40 () _ _ l p =
    E.with_sploded_pair ~l "du40" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_u40 (u64_of_qword w)) p)

  let di48 () _ _ l p =
    E.with_sploded_pair ~l "di48" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_i48 (u64_of_qword w)) p)

  let du48 () _ _ l p =
    E.with_sploded_pair ~l "du48" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_u48 (u64_of_qword w)) p)

  let di56 () _ _ l p =
    E.with_sploded_pair ~l "di56" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_i56 (u64_of_qword w)) p)

  let du56 () _ _ l p =
    E.with_sploded_pair ~l "du56" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_u56 (u64_of_qword w)) p)

  let di64 () _ _ l p =
    E.with_sploded_pair ~l "di64" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (to_i64 (u64_of_qword w)) p)

  let du64 () _ _ l p =
    E.with_sploded_pair ~l "du64" (read_qword LittleEndian p) (fun _l w p ->
      make_pair (u64_of_qword w) p)

  let di128 () _ _ l p =
    E.with_sploded_pair ~l "di128" (read_oword LittleEndian p) (fun _l w p ->
      make_pair (to_i128 (u128_of_oword w)) p)

  let du128 () _ _ l p =
    E.with_sploded_pair ~l "di128" (read_oword LittleEndian p) (fun _l w p ->
      make_pair (u128_of_oword w) p)

  (* Items of a tuples are just concatenated together: *)
  let tup_opn () _ _ _ _ p = p
  let tup_cls () _ _ _ p = p
  let tup_sep () _ _ _ p = p

  let rec_opn () _ _ _ _ p = p
  let rec_cls () _ _ _ p = p
  let rec_sep () _ _ _ p = p

  (* RowBinary has no sum types, so we encode the value as a pair: *)
  let sum_opn st mn0 path mns l p =
    let p = tup_opn st mn0 path mns l p in
    let c_p = du16 st mn0 path l p in
    E.with_sploded_pair ~l "sum_opn" c_p (fun l c p ->
      let p = tup_sep st mn0 path l p in
      make_pair c p)

  let sum_cls st mn0 path l p =
    tup_cls st mn0 path l p

  (* Vectors: ClickHouse does not distinguish between vectors (of known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and our lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn () _ _ _ _ _ p = p
  let vec_cls () _ _ _ p = p
  let vec_sep () _ _ _ p = p

  let list_opn () = KnownSize
    (fun _ _ _ l p ->
      E.with_sploded_pair ~l "list_opn" (read_leb128 l p) (fun _l dim p ->
        make_pair (u32_of_size dim) p))

  let list_cls () _ _ _ p = p
  let list_sep () _ _ _ p = p

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null () _ _ _ p =
    eq (peek_byte p (size 0)) (byte Uint8.one)

  let dnull _t () _ _ _ p =
    data_ptr_add p (size 1)

  let dnotnull = dnull
end
