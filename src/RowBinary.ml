open Batteries
open Dessser

module Des (BE : BACKEND) : DES with module BE = BE =
struct
  module BE = BE
  module T = Type

  type state = unit
  let start _typ _oc p = (), p
  let stop _oc () p = p

  type 'a des = BE.output -> frame list -> state -> [`Pointer] id -> 'a * [`Pointer] id

  let dfloat oc _frames () p =
    let w, p = BE.read_qword oc p in
    BE.float_of_qword oc w, p

  let read_leb128 oc =
    let t_pair_u32_u8 = T.(TPair (u32, u8)) in
    let cond =
      BE.function1 oc T.TByte T.bool (fun oc b ->
        BE.comment oc "Condition for read_leb128" ;
        BE.U8.ge oc (BE.U8.of_byte oc b) (BE.U8.of_const_int oc 128))
    and reduce =
      BE.function2 oc t_pair_u32_u8 T.TByte t_pair_u32_u8
        (fun oc leb_shft_tup byte ->
          BE.comment oc "Reducer for read_leb128" ;
          let byte =
            let c127 = BE.U8.of_const_int oc 127 in
            BE.U8.(to_byte oc (log_and oc (of_byte oc byte) c127)) in
          let leb = Identifier.to_u32 (BE.pair_fst oc leb_shft_tup)
          and shft = Identifier.to_u8 (BE.pair_snd oc leb_shft_tup) in
          BE.make_pair oc t_pair_u32_u8
            (BE.U32.add oc (BE.U32.shift_left oc (BE.U32.of_byte oc byte) shft) leb)
            (BE.U8.add oc shft (BE.U8.of_const_int oc 7))) in
    fun p ->
      let u32_zero = BE.U32.of_const_int oc 0
      and u8_zero = BE.U8.of_const_int oc 0 in
      let init =
        BE.make_pair oc t_pair_u32_u8 u32_zero u8_zero in
      let leb_shft_tup, p = BE.read_while oc ~cond ~reduce init p in
      (* Still have to add the last byte (which is <128): *)
      let last_b, p = BE.read_byte oc p in
      let leb = BE.pair_fst oc leb_shft_tup
      and shft = BE.pair_snd oc leb_shft_tup in
      BE.size_of_u32 oc (BE.U32.add oc (BE.U32.shift_left oc (BE.U32.of_byte oc last_b) shft) leb),
      p

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring oc =
    let read_leb128 = read_leb128 oc in
    fun _frames () p ->
      let len, p = read_leb128 p in
      let bs, p = BE.read_bytes oc p len in
      BE.string_of_bytes oc bs,
      p

  let dbool oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.(bool_not oc U8.(eq oc (of_byte oc b) (of_const_int oc 0))),
    p

  let dchar oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.char_of_byte oc b,
    p

  let di8 oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.I8.of_byte oc b,
    p

  let du8 oc _frames () p =
    let b, p = BE.read_byte oc p in
    BE.U8.of_byte oc b,
    p

  let di16 oc _frames () p =
    let w, p = BE.read_word oc p in
    BE.I16.of_word oc w,
    p

  let du16 oc _frames () p =
    let w, p = BE.read_word oc p in
    BE.U16.of_word oc w,
    p

  let di32 oc _frames () p =
    let w, p = BE.read_dword oc p in
    BE.I32.of_dword oc w,
    p

  let du32 oc _frames () p =
    let w, p = BE.read_dword oc p in
    BE.U32.of_dword oc w,
    p

  let di64 oc _frames () p =
    let w, p = BE.read_qword oc p in
    BE.I64.of_qword oc w,
    p

  let du64 oc _frames () p =
    let w, p = BE.read_qword oc p in
    BE.U64.of_qword oc w,
    p

  let di128 oc _frames () p =
    let w, p = BE.read_oword oc p in
    BE.I128.of_oword oc w,
    p

  let du128 oc _frames () p =
    let w, p = BE.read_oword oc p in
    BE.U128.of_oword oc w,
    p

  (* Items of a tuples are just concatenated together: *)
  let tup_opn _oc _frames () p = p
  let tup_cls _oc _frames () p = p
  let tup_sep _n _oc _frames () p = p

  let rec_opn _oc _frames () p = p
  let rec_cls _oc _frames () p = p
  let rec_sep _n _oc _frames () p = p

  (* Vectors: ClickHouse does not distinguish between vectors (of known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and our lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn _oc _frames () p = p
  let vec_cls _oc _frames () p = p
  let vec_sep _n _oc _frames () p = p

  let list_opn oc _frames () p =
    let dim, p = read_leb128 oc p in
    BE.u32_of_size oc dim, p

  let list_cls _oc _frames () p = p
  let list_sep _oc _frames () p = p

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null oc _frames () p =
    let b = BE.peek_byte oc p in
    BE.U8.(eq oc (of_byte oc b) (of_const_int oc 1))

  let dnull oc _frames () p =
    BE.pointer_add oc p (BE.size_of_const oc 1)

  let dnotnull = dnull
end

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE
  module T = Type

  type state = unit
  let start _typ _oc p = (), p
  let stop _oc () p = p

  type 'a ser = BE.output -> frame list -> state -> 'a ->  [`Pointer] id -> [`Pointer] id

  let sfloat oc _frames () v p =
    let w = BE.qword_of_float oc v in
    BE.write_qword oc p w

  (* Note: Reuse the same cond and loop functions from call to call *)
  let write_leb128 oc =
    (* Here, using TSize instead of TU32 would crash at runtime.
     * There is actually no connection between this type t_ptr_sz et the
     * parameters used by make_pair.
     * This would be easier to unify those types if we used the actual
     * types as type parameters for identifiers: have, say, TU8 Identifier.t
     * instead of [`U8] Identifier. *)
    let t_ptr_sz = T.(TPair (TPointer, u32 (*TSize*))) in
    (* loop until wlen is 0: *)
    let cond =
      BE.function1 oc t_ptr_sz T.bool (fun oc p_wlen ->
        BE.comment oc "Condition for write_leb128" ;
        let wlen = BE.pair_snd oc p_wlen in
        BE.U32.(gt oc wlen (of_const_int oc 0)))
    and loop =
      BE.function1 oc t_ptr_sz t_ptr_sz (fun oc p_wlen ->
        BE.comment oc "Loop for write_leb128" ;
        let p = BE.pair_fst oc p_wlen
        and wlen = BE.pair_snd oc p_wlen in
        let is_last = BE.U32.(gt oc (of_const_int oc 128) wlen) in
        let b = BE.U32.to_u8 oc wlen in
        let b =
          BE.choose oc is_last
            (fun oc -> BE.U8.(log_and oc b (of_const_int oc 127)))
            (fun oc -> BE.U8.(log_or oc b (of_const_int oc 128))) in
        let p = BE.(write_byte oc p (U8.to_byte oc b)) in
        let wlen = BE.(U32.shift_right oc wlen (U8.of_const_int oc 7)) in
        BE.make_pair oc t_ptr_sz p wlen) in
    fun p v ->
      let wlen = BE.u32_of_size oc v in
      let p_wlen = BE.make_pair oc t_ptr_sz p wlen in
      let p_wlen = BE.loop_until oc ~loop ~cond p_wlen in
      BE.pair_fst oc p_wlen

  let sstring oc =
    let write_leb128 = write_leb128 oc in
    fun _frames () v p ->
      let len = BE.length_of_string oc v in
      let p = write_leb128 p len in
      let v = BE.bytes_of_string oc v in
      BE.write_bytes oc p v

  let sbool oc _frames () v p =
    let u8 = BE.u8_of_bool oc v in
    let b = BE.byte_of_u8 oc u8 in
    BE.write_byte oc p b

  let schar oc _frames () v p =
    let b = BE.byte_of_char oc v in
    BE.write_byte oc p b

  let si8 oc _frames () v p =
    let b = BE.I8.to_byte oc v in
    BE.write_byte oc p b

  let si16 oc _frames () v p =
    let w = BE.I16.to_word oc v in
    BE.write_word oc p w

  let si32 oc _frames () v p =
    let w = BE.I32.to_dword oc v in
    BE.write_dword oc p w

  let si64 oc _frames () v p =
    let w = BE.I64.to_qword oc v in
    BE.write_qword oc p w

  let si128 oc _frames () v p =
    let w = BE.I128.to_oword oc v in
    BE.write_oword oc p w

  let su8 oc _frames () v p =
    let b = BE.U8.to_byte oc v in
    BE.write_byte oc p b

  let su16 oc _frames () v p =
    let w = BE.U16.to_word oc v in
    BE.write_word oc p w

  let su32 oc _frames () v p =
    let w = BE.U32.to_dword oc v in
    BE.write_dword oc p w

  let su64 oc _frames () v p =
    let w = BE.U64.to_qword oc v in
    BE.write_qword oc p w

  let su128 oc _frames () v p =
    let w = BE.U128.to_oword oc v in
    BE.write_oword oc p w

  let tup_opn _oc _frames () p = p
  let tup_cls _oc _frames () p = p
  let tup_sep _idx _oc _frames () p = p

  let rec_opn _oc _frames () p = p
  let rec_cls _oc _frames () p = p
  let rec_sep _idx _oc _frames () p = p

  let vec_opn _oc _frames () p = p
  let vec_cls _oc _frames () p = p
  let vec_sep _idx _oc _frames () p = p

  let list_opn oc _frames () n p =
    let write_leb128 = write_leb128 oc in
    let n = BE.size_of_u32 oc n in
    write_leb128 p n
  let list_cls _oc _frames () p = p
  let list_sep _oc _frames () p = p

  let nullable _oc _frames () p = p

  let snull oc _frames () p =
    BE.write_byte oc p (BE.byte_of_const oc 1)

  let snotnull oc _frames () p =
    BE.write_byte oc p (BE.byte_of_const oc 0)

  type 'a ssizer = BE.output -> frame list -> 'a -> ssize

  let ssize_of_float _oc _frames _ = ConstSize 8
  let ssize_of_bool _oc _frames _ = ConstSize 1
  let ssize_of_char _oc _frames _ = ConstSize 1
  let ssize_of_i8 _oc _frames _ = ConstSize 1
  let ssize_of_u8 _oc _frames _ = ConstSize 1
  let ssize_of_i16 _oc _frames _ = ConstSize 2
  let ssize_of_u16 _oc _frames _ = ConstSize 2
  let ssize_of_i32 _oc _frames _ = ConstSize 4
  let ssize_of_u32 _oc _frames _ = ConstSize 4
  let ssize_of_i64 _oc _frames _ = ConstSize 8
  let ssize_of_u64 _oc _frames _ = ConstSize 8
  let ssize_of_i128 _oc _frames _ = ConstSize 16
  let ssize_of_u128 _oc _frames _ = ConstSize 16

  let ssize_of_tup _oc _frames _ = ConstSize 0
  let ssize_of_rec _oc _frames _ = ConstSize 0
  let ssize_of_vec _oc _frames _ = ConstSize 0

  let ssize_of_leb128 oc =
    let t_pair_u32_u32 = T.(TPair (u32, u32)) in
    let cond =
      BE.function1 oc t_pair_u32_u32 T.bool (fun oc lebsz_n ->
        BE.comment oc "Condition for ssize_of_leb128" ;
        let lebsz = BE.pair_fst oc lebsz_n
        and n = BE.pair_snd oc lebsz_n in
        let max_len_for_lebsz =
          BE.U32.shift_left oc lebsz (BE.U8.of_const_int oc 7) in
        BE.U32.ge oc n max_len_for_lebsz)
    and loop =
      BE.function1 oc T.u32 T.u32 (fun oc lebsz_n ->
        BE.comment oc "Loop for ssize_of_leb128" ;
        let lebsz = BE.pair_fst oc lebsz_n
        and n = BE.pair_snd oc lebsz_n in
        let lebsz = BE.U32.(add oc lebsz (of_const_int oc 1)) in
        BE.make_pair oc t_pair_u32_u32 lebsz n) in
    fun n ->
      let lebsz = BE.U32.of_const_int oc 1 in
      let lebsz_n = BE.make_pair oc t_pair_u32_u32 lebsz n in
      let lebsz_n = BE.loop_while oc cond loop lebsz_n in
      BE.pair_fst oc lebsz_n

  (* SerSize of a list is the size of the LEB128 prefix, same as for
   * ssize_of_string below) *)
  let ssize_of_list oc =
    let ssize_of_leb128 = ssize_of_leb128 oc in
    fun _frames lst ->
      let n = BE.length_of_list oc lst in
      let lebsz = ssize_of_leb128 n in
      DynSize (BE.size_of_u32 oc lebsz)

  let ssize_of_null _oc _frames = ConstSize 1

  (* Size of a string is it's length in bytes + the size of the LEB128 prefix,
   * which size is 1 bytes per group of 7 bits. *)
  let ssize_of_string oc =
    let ssize_of_leb128 = ssize_of_leb128 oc in
    fun _frames v ->
      let len = BE.length_of_string oc v in
      let wlen = BE.u32_of_size oc len in
      let lebsz = ssize_of_leb128 wlen in
      let totsz = BE.U32.add oc lebsz wlen in
      DynSize (BE.size_of_u32 oc totsz)
end
