open Batteries
open Stdint

open Dessser
open DessserMiscTypes
module T = DessserTypes
module E = DessserExpressions
module Path = DessserPath
open E.Ops

module Ser : SER with type config = unit =
struct
  let id = RowBinary
  type config = unit
  type state = unit
  let ptr _mn = T.ptr

  let start ?(config=()) _mn p =
    config, p

  let stop () p = p
  type ser = state -> T.mn -> Path.t -> E.t -> E.t -> E.t

  let sfloat () _ _ v p =
    write_u64 LittleEndian p (u64_of_float v)

  (* v must be a u32: *)
  let write_leb128 p v =
    let_ ~name:"leb128_sz" (make_ref v) (fun v_ref ->
      let_ ~name:"leb128_ptr" (make_ref p) (fun p_ref ->
        let v = get_ref v_ref in
        seq [
          while_
            (comment "Loop body for write_leb128"
              (let b =
                if_ (lt v (u32 (Uint32.of_int 128)))
                  ~then_:(to_u8 v)
                  ~else_:(bit_or (to_u8 v) (u8_of_int 128)) in
              seq [
                set_ref p_ref (write_u8 (get_ref p_ref) b) ;
                set_ref v_ref (right_shift v (u8_of_int 7)) ;
                (comment "Condition for write_leb128 (until v is 0)"
                  (gt v (u32 Uint32.zero))) ]))
            nop ;
          get_ref p_ref ]))

  let sstring () _ _ v p =
    let p = write_leb128 p (string_length v) in
    write_bytes p (bytes_of_string v)

  let sbool () _ _ v p =
    write_u8 p (u8_of_bool v)

  let schar () _ _ v p =
    write_u8 p (u8_of_char v)

  let si8 () _ _ v p =
    write_u8 p (to_u8 v)

  let si16 () _ _ v p =
    write_u16 LittleEndian p (to_u16 v)

  let si32 () _ _ v p =
    write_u32 LittleEndian p (to_u32 v)

  let si24 () mn0 path v p =
    si32 () mn0 path (to_i32 v) p

  let si64 () _ _ v p =
    write_u64 LittleEndian p (to_u64 v)

  let si40 () mn0 path v p =
    si64 () mn0 path (to_i64 v) p

  let si48 = si40

  let si56 = si40

  let si128 () _ _ v p =
    write_u128 LittleEndian p (to_u128 v)

  let su8 () _ _ v p =
    write_u8 p v

  let su16 () _ _ v p =
    write_u16 LittleEndian p v

  let su32 () _ _ v p =
    write_u32 LittleEndian p v

  let su24 () mn0 path v p =
    su32 () mn0 path (to_u32 v) p

  let su64 () _ _ v p =
    write_u64 LittleEndian p v

  let su40 () mn0 path v p =
    su64 () mn0 path (to_u64 v) p

  let su48 = su40

  let su56 = su40

  let su128 () _ _ v p =
    write_u128 LittleEndian p v

  let sext f () _ _ v p =
    f v p

  let tup_opn _ () _ _ p = p
  let tup_cls () _ _ p = p
  let tup_sep () _ _ p = p

  let rec_opn _ () _ _ p = p
  let rec_cls () _ _ p = p
  let rec_sep () _ _ p = p

  let sum_opn mns lbl st mn0 path p =
    let p = tup_opn mns st mn0 path p in
    let p = su16 st mn0 path lbl p in
    tup_sep st mn0 path p

  let sum_cls _lbl st mn0 path p =
    tup_cls st mn0 path p

  let vec_opn _ _ () _ __ p = p
  let vec_cls () _ _ p = p
  let vec_sep () _ _ p = p

  let arr_opn _ n () _ _ p =
    let n = match n with
      | Some n -> n
      | None -> failwith "RowBinary.Ser needs list size upfront" in
    write_leb128 p n

  let arr_cls () _ _ p = p
  let arr_sep () _ _ p = p

  let nullable () _ _ p = p

  let snull _t () _ _ p =
    write_u8 p (u8_of_int 1)

  let snotnull _t () _ _ p =
    write_u8 p (u8_of_int 0)

  type ssizer = T.mn -> Path.t -> E.t -> E.t

  let ssize_of_float _ _ _ = size 8
  let ssize_of_bool _ _ _ = size 1
  let ssize_of_char _ _ _ = size 1
  let ssize_of_i8 _ _ _ = size 1
  let ssize_of_u8 _ _ _ = size 1
  let ssize_of_i16 _ _ _ = size 2
  let ssize_of_u16 _ _ _ = size 2
  let ssize_of_i32 _ _ _ = size 4
  let ssize_of_u32 _ _ _ = size 4
  let ssize_of_i24 = ssize_of_i32
  let ssize_of_u24 = ssize_of_u32
  let ssize_of_i64 _ _ _ = size 8
  let ssize_of_u64 _ _ _ = size 8
  let ssize_of_i40 = ssize_of_i64
  let ssize_of_u40 = ssize_of_u64
  let ssize_of_i48 = ssize_of_i64
  let ssize_of_u48 = ssize_of_u64
  let ssize_of_i56 = ssize_of_i64
  let ssize_of_u56 = ssize_of_u64
  let ssize_of_i128 _ _ _ = size 16
  let ssize_of_u128 _ _ _ = size 16

  let ssize_of_tup _ _ _ = size 0
  let ssize_of_rec _ _ _ = size 0
  let ssize_of_sum = ssize_of_u16
  let ssize_of_vec _ _ _ = size 0

  let ssize_of_leb128 n =
    let_ ~name:"n_ref" (make_ref n) (fun n_ref ->
      let_ ~name:"lebsz_ref" (make_ref (u32_of_int 1)) (fun lebsz_ref ->
        seq [
          while_
            (comment "Condition for ssize_of_leb128"
              (let max_len = left_shift (get_ref lebsz_ref) (u8_of_int 7) in
              ge (get_ref n_ref) max_len))
            (comment "Loop for ssize_of_leb128"
              (set_ref lebsz_ref (add (get_ref lebsz_ref) (u32_of_int 1)))) ;
          size_of_u32 (get_ref lebsz_ref) ]))

  (* SerSize of a list is the size of the LEB128 prefix, same as for
   * ssize_of_string below) *)
  let ssize_of_arr _ _ lst =
    ssize_of_leb128 (cardinality lst)

  let ssize_of_null _ _ = size 1

  (* Size of a string is its length in bytes + the size of the LEB128 prefix,
   * which size is 1 bytes per group of 7 bits. *)
  let ssize_of_string _ _ v =
    let_ ~name:"wlen" (string_length v) (fun wlen ->
      add (ssize_of_leb128 wlen) (size_of_u32 wlen))

  let ssize_start ?(config=()) _ =
    ignore config ;
    size 0
end

module Des : DES with type config = unit =
struct
  let id = RowBinary
  type config = unit
  type state = unit
  let ptr _mn = T.ptr

  let start ?(config=()) _mn p =
    config, p

  let stop () p = p
  type des = state -> T.mn -> Path.t -> E.t -> E.t

  let dfloat () _ _ p =
    let w_p = read_u64 LittleEndian p in
    E.with_sploded_pair "dfloat" w_p (fun w p ->
      make_pair (float_of_u64 w) p)

  (* Returns a size and a Ptr: *)
  let read_leb128 p =
    let_ ~name:"leb_ref" (make_ref (u32_of_int 0)) (fun leb_ref ->
      let leb = get_ref leb_ref in
      let_ ~name:"shft_ref" (make_ref (u8_of_int 0)) (fun shft_ref ->
        let shft = get_ref shft_ref in
        let_ ~name:"p_ref" (make_ref p) (fun p_ref ->
          let p = get_ref p_ref in
          seq [
            while_
              (E.with_sploded_pair "leb128" (read_u8 p) (fun b p' ->
                let b' = bit_and b (u8_of_int 127) in
                seq [
                  set_ref p_ref p' ;
                  set_ref leb_ref (bit_or (left_shift (to_u32 b') shft) leb) ;
                  set_ref shft_ref (add shft (u8_of_int 7)) ;
                  (comment "Condition for read_leb128"
                    (ge b (u8_of_int 128))) ]))
              nop ;
            make_pair (size_of_u32 leb) p ])))

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * Tup type: *)
  let des typ = ignore typ

  let dstring () _ _ p =
    E.with_sploded_pair "dstring1" (read_leb128 p) (fun len p ->
      E.with_sploded_pair "dstring2" (read_bytes p len) (fun bs p ->
        make_pair (string_of_bytes bs) p))

  let dbool () _ _ p =
    E.with_sploded_pair "dbool" (read_u8 p) (fun b p ->
      make_pair (not_ (eq b (u8 Uint8.zero))) p)

  let dchar () _ _ p =
    E.with_sploded_pair "dchar" (read_u8 p) (fun b p ->
      make_pair (char_of_u8 b) p)

  let di8 () _ _ p =
    E.with_sploded_pair "di8" (read_u8 p) (fun b p ->
      make_pair (to_i8 b) p)

  let du8 () _ _ p =
    E.with_sploded_pair "du8" (read_u8 p) (fun b p ->
      make_pair b p)

  let di16 () _ _ p =
    E.with_sploded_pair "di16" (read_u16 LittleEndian p) (fun w p ->
      make_pair (to_i16 w) p)

  let du16 () _ _ p =
    E.with_sploded_pair "du16" (read_u16 LittleEndian p) (fun w p ->
      make_pair w p)

  let di24 () _ _ p =
    E.with_sploded_pair "di24" (read_u32 LittleEndian p) (fun w p ->
      make_pair (to_i24 w) p)

  let du24 () _ _ p =
    E.with_sploded_pair "du24" (read_u32 LittleEndian p) (fun w p ->
      make_pair (to_u24 w) p)

  let di32 () _ _ p =
    E.with_sploded_pair "di32" (read_u32 LittleEndian p) (fun w p ->
      make_pair (to_i32 w) p)

  let du32 () _ _ p =
    E.with_sploded_pair "du32" (read_u32 LittleEndian p) (fun w p ->
      make_pair w p)

  let di40 () _ _ p =
    E.with_sploded_pair "di40" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_i40 w) p)

  let du40 () _ _ p =
    E.with_sploded_pair "du40" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_u40 w) p)

  let di48 () _ _ p =
    E.with_sploded_pair "di48" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_i48 w) p)

  let du48 () _ _ p =
    E.with_sploded_pair "du48" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_u48 w) p)

  let di56 () _ _ p =
    E.with_sploded_pair "di56" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_i56 w) p)

  let du56 () _ _ p =
    E.with_sploded_pair "du56" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_u56 w) p)

  let di64 () _ _ p =
    E.with_sploded_pair "di64" (read_u64 LittleEndian p) (fun w p ->
      make_pair (to_i64 w) p)

  let du64 () _ _ p =
    E.with_sploded_pair "du64" (read_u64 LittleEndian p) (fun w p ->
      make_pair w p)

  let di128 () _ _ p =
    E.with_sploded_pair "di128" (read_u128 LittleEndian p) (fun w p ->
      make_pair (to_i128 w) p)

  let du128 () _ _ p =
    E.with_sploded_pair "di128" (read_u128 LittleEndian p) (fun w p ->
      make_pair w p)

  let dext f () _ _ p =
    f p

  (* Items of a tuples are just concatenated together: *)
  let tup_opn _ () _ _ p = p
  let tup_cls () _ _ p = p
  let tup_sep () _ _ p = p

  let rec_opn _ () _ _ p = p
  let rec_cls () _ _ p = p
  let rec_sep () _ _ p = p

  (* RowBinary has no sum types, so we encode the value as a pair: *)
  let sum_opn mns st mn0 path p =
    let p = tup_opn mns st mn0 path p in
    let c_p = du16 st mn0 path p in
    E.with_sploded_pair "sum_opn" c_p (fun c p ->
      let p = tup_sep st mn0 path p in
      make_pair c p)

  let sum_cls _lbl st mn0 path p =
    tup_cls st mn0 path p

  (* Vectors: ClickHouse does not distinguish between vectors (of known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and our lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn _ _ () _ _ p = p
  let vec_cls () _ _ p = p
  let vec_sep () _ _ p = p

  let arr_opn () = KnownSize
    (fun _ _ _ p ->
      E.with_sploded_pair "list_opn" (read_leb128 p) (fun dim p ->
        make_pair (u32_of_size dim) p))

  let arr_cls () _ _ p = p
  let arr_sep () _ _ p = p

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null () _ _ p =
    eq (peek_u8 p (size 0)) (u8_of_int 1)

  let dnull _t () _ _ p =
    ptr_add p (size 1)

  let dnotnull = dnull
end
