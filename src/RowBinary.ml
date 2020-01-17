open Batteries
open Stdint
open Dessser

module Des : DES =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _vtyp p = (), p
  let stop () p = p
  type des = state -> e -> e

  open Expression

  let dfloat () p =
    let w_p = ReadQWord (LittleEndian, p) in
    MapPair (w_p,
      func [|qword; dataptr|] (fun fid ->
        Pair (FloatOfQWord (Param (fid, 0)), (Param (fid, 1)))))

  let read_leb128 p =
    let t_u32_u8 = Type.Pair (u32, u8) in
    Let (
      "leb_shft_ptr", ReadWhile (
        Comment ("Condition for read_leb128",
          func [|byte|] (fun fid -> Ge (Param (fid, 0), Byte 128))),
        Comment ("Reducer for read_leb128",
          func [|t_u32_u8; byte|] (fun fid ->
            let byte = LogAnd (U8OfByte (Param (fid, 1)), U8 127) in
            let leb = Fst (Param (fid, 0))
            and shft = Snd (Param (fid, 0)) in
            Pair (Add (LeftShift (ToU32 byte, shft), leb),
                  Add (shft, U8 7)))),
        Pair (U32 Uint32.zero, U8 0),
        p),
      (* Still have to add the last byte (which is <128): *)
      Comment ("Last byte from read_leb128",
        with_sploded_pair "leb128" (Identifier "leb_shft_ptr") (fun leb_shft ptr ->
          with_sploded_pair "leb128" (ReadByte ptr) (fun last_b ptr ->
            Pair (
              SizeOfU32 (Add (LeftShift (ToU32 (U8OfByte last_b),
                                        (Snd leb_shft)),
                              Fst leb_shft)),
              ptr)))))

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring () p =
    with_sploded_pair "dstring" (read_leb128 p) (fun len p ->
      with_sploded_pair "dstring" (ReadBytes (p, len)) (fun bs p ->
        Pair (StringOfBytes bs, p)))

  let dbool () p =
    with_sploded_pair "dbool" (ReadByte p) (fun b p ->
      Pair (Not (Eq (U8OfByte b, U8 0)), p))

  let dchar () p =
    with_sploded_pair "dchar" (ReadByte p) (fun b p ->
      Pair (CharOfU8 (U8OfByte b), p))

  let di8 () p =
    with_sploded_pair "di8" (ReadByte p) (fun b p ->
      Pair (ToI8 (U8OfByte b), p))

  let du8 () p =
    with_sploded_pair "du8" (ReadByte p) (fun b p ->
      Pair (U8OfByte b, p))

  let di16 () p =
    with_sploded_pair "di16" (ReadWord (LittleEndian, p)) (fun w p ->
      Pair (ToI16 (U16OfWord w), p))

  let du16 () p =
    with_sploded_pair "du16" (ReadWord (LittleEndian, p)) (fun w p ->
      Pair (U16OfWord w, p))

  let di24 () p =
    with_sploded_pair "di24" (ReadDWord (LittleEndian, p)) (fun w p ->
      Pair (ToI24 (U32OfDWord w), p))

  let du24 () p =
    with_sploded_pair "du24" (ReadDWord (LittleEndian, p)) (fun w p ->
      Pair (ToU24 (U32OfDWord w), p))

  let di32 () p =
    with_sploded_pair "di32" (ReadDWord (LittleEndian, p)) (fun w p ->
      Pair (ToI32 (U32OfDWord w), p))

  let du32 () p =
    with_sploded_pair "du32" (ReadDWord (LittleEndian, p)) (fun w p ->
      Pair (U32OfDWord w, p))

  let di40 () p =
    with_sploded_pair "di40" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToI40 (U64OfQWord w), p))

  let du40 () p =
    with_sploded_pair "du40" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToU40 (U64OfQWord w), p))

  let di48 () p =
    with_sploded_pair "di48" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToI48 (U64OfQWord w), p))

  let du48 () p =
    with_sploded_pair "du48" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToU48 (U64OfQWord w), p))

  let di56 () p =
    with_sploded_pair "di56" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToI56 (U64OfQWord w), p))

  let du56 () p =
    with_sploded_pair "du56" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToU56 (U64OfQWord w), p))

  let di64 () p =
    with_sploded_pair "di64" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (ToI64 (U64OfQWord w), p))

  let du64 () p =
    with_sploded_pair "du64" (ReadQWord (LittleEndian, p)) (fun w p ->
      Pair (U64OfQWord w, p))

  let di128 () p =
    with_sploded_pair "di128" (ReadOWord (LittleEndian, p)) (fun w p ->
      Pair (ToI128 (U128OfOWord w), p))

  let du128 () p =
    with_sploded_pair "di128" (ReadOWord (LittleEndian, p)) (fun w p ->
      Pair (U128OfOWord w, p))

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

  let list_opn () _ p =
    with_sploded_pair "list_opn" (read_leb128 p) (fun dim p ->
      Pair (U32OfSize dim, p))

  let list_cls () p = p
  let list_sep () p = p

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null () p =
    Eq (PeekByte (p, Size 0), Byte 1)

  let dnull _t () p =
    DataPtrAdd (p, Size 1)

  let dnotnull = dnull
end

module Ser : SER =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _vtyp p = (), p
  let stop () p = p
  type ser = state -> e -> e -> e

  open Expression

  let sfloat () v p =
    WriteQWord (LittleEndian, p, QWordOfFloat v)

  let write_leb128 p v =
    let t_ptr_sz = Type.(Pair (DataPtr, u32)) in
    Fst (
      LoopUntil (
        Comment ("Loop body for write_leb128",
          func [|t_ptr_sz|] (fun fid ->
            with_sploded_pair "write_leb128" (Param (fid, 0)) (fun p wlen ->
              let b =
                Choose (Gt (U32 (Uint32.of_int 128), wlen),
                  LogAnd (ToU8 wlen, U8 127),
                  LogOr (ToU8 wlen, U8 128)) in
              Pair (
                WriteByte (p, b),
                RightShift (wlen, U8 7))))),
        Comment ("Condition for write_leb128 (until wlen is 0)",
          func [|t_ptr_sz|] (fun fid -> Gt (Snd (Param (fid, 0)), U32 Uint32.zero))),
        Pair (p, U32OfSize v)))

  let sstring () v p =
    let p = write_leb128 p (StringLength v) in
    WriteBytes (p, BytesOfString v)

  let sbool () v p =
    WriteByte (p, ByteOfU8 (U8OfBool v))

  let schar () v p =
    WriteByte (p, ByteOfU8 (U8OfChar v))

  let si8 () v p =
    WriteByte (p, ByteOfU8 (ToU8 v))

  let si16 () v p =
    WriteWord (LittleEndian, p, WordOfU16 (ToU16 v))

  let si32 () v p =
    WriteDWord (LittleEndian, p, DWordOfU32 (ToU32 v))

  let si24 = si32

  let si64 () v p =
    WriteQWord (LittleEndian, p, QWordOfU64 (ToU64 v))

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 () v p =
    WriteOWord (LittleEndian, p, OWordOfU128 (ToU128 v))

  let su8 () v p =
    WriteByte (p, ByteOfU8 v)

  let su16 () v p =
    WriteWord (LittleEndian, p, WordOfU16 v)

  let su32 () v p =
    WriteDWord (LittleEndian, p, DWordOfU32 v)

  let su24 = su32

  let su64 () v p =
    WriteQWord (LittleEndian, p, QWordOfU64 v)

  let su40 = su64

  let su48 = su64

  let su56 = su64

  let su128 () v p =
    WriteOWord (LittleEndian, p, OWordOfU128 v)

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
    write_leb128 p (SizeOfU32 n)

  let list_cls () p = p
  let list_sep () p = p

  let nullable () p = p

  let snull _t () p =
    WriteByte (p, Byte 1)

  let snotnull _t () p =
    WriteByte (p, Byte 0)

  type ssizer = vtyp -> path -> e -> ssize

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
    let t_u32_u32 = Type.(Pair (u32, u32)) in
    SizeOfU32 (Fst (
      LoopWhile (
        Comment ("Condition for ssize_of_leb128",
          func [|t_u32_u32|] (fun fid ->
            with_sploded_pair "ssize_of_leb128" (Param (fid, 0)) (fun lebsz n ->
              let max_len_for_lebsz = LeftShift (lebsz, U8 7) in
              Ge (n, max_len_for_lebsz)))),
        Comment ("Loop for ssize_of_leb128",
          func [|t_u32_u32|] (fun fid ->
            with_sploded_pair "ssize_of_leb128" (Param (fid, 0)) (fun lebsz n ->
              Pair (Add (lebsz, U32 Uint32.one), n)))),
        Pair (U32 Uint32.one, n))))

  (* SerSize of a list is the size of the LEB128 prefix, same as for
   * ssize_of_string below) *)
  let ssize_of_list _ _ lst =
    DynSize (ssize_of_leb128 (ListLength lst))

  let ssize_of_null _ _ = ConstSize 1

  (* Size of a string is it's length in bytes + the size of the LEB128 prefix,
   * which size is 1 bytes per group of 7 bits. *)
  let ssize_of_string _ _ v =
    DynSize (
      Let ("wlen", U32OfSize (StringLength v),
        Add (ssize_of_leb128 (Identifier "wlen"), (Identifier "wlen"))))
end
