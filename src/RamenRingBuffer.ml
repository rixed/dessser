open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions

(* Size of the word stored in the ringbuffer, in bytes. *)
let ringbuf_word_size = ref 4

module Ser : SER =
struct
  type state =
    (* That int count the nullable fields (ie. bit index of the next nullable
     * in the nullmask) *)
    { mutable nullmasks : (int * (* DataPtr *) e) list }
  let ptr _vtyp = dataptr

  let start _vtyp p =
    { nullmasks = [] }, p

  let stop _st p = p

  let push_nullmask st p = st.nullmasks <- (0, p) :: st.nullmasks
  let pop_nullmask st = st.nullmasks <- List.tl st.nullmasks

  (* Realign the pointer on a multiple of [ringbuf_word_size].
   * [extra_bytes] modulo [ringbuf_word_size] gives the number of bytes
   * that's been written after the last word boundary. *)
  let align_dyn p extra_bytes =
    let wsize = Size !ringbuf_word_size in
    let extra_bytes = Rem (extra_bytes, wsize) in
    let padding_len = Sub (wsize, extra_bytes) in
    Choose (Gt (wsize, padding_len),
      DataPtrAdd (p, padding_len),
      p)

  let align_const p extra_bytes =
    let extra_bytes = extra_bytes mod !ringbuf_word_size in
    let padding_len = !ringbuf_word_size - extra_bytes in
    if !ringbuf_word_size > extra_bytes then
      DataPtrAdd (p, Size padding_len)
    else
      p

  (* Zero a nullmask known at compile time and advance the pointer *)
  let zero_nullmask_const bits p =
    let sz = (bits + 7) / 8 in
    let p = BlitBytes (p, Byte 0, Size sz) in
    align_const p sz

  (* Zero the nullmask known only at runtime and advance the pointer *)
  let zero_nullmask_dyn bits p =
    let sz = RightShift (Add (bits, Size 7), U8 3) in
    let p = BlitBytes (p, Byte 0, sz) in
    align_dyn p sz

  type ser = state -> e -> e -> e

  let sfloat _st v p =
    WriteQWord (LittleEndian, p, QWordOfFloat v)

  let sstring _st v p =
    let len = StringLength v in
    let p = WriteDWord (LittleEndian, p, DWordOfU32 len) in
    let bytes = BytesOfString v in
    let p = WriteBytes (p, bytes) in
    align_dyn p (SizeOfU32 len)

  let sbool _st v p =
    let p = WriteByte (p, ByteOfU8 (U8OfBool v)) in
    align_const p 1

  let schar _st v p =
    let p = WriteByte (p, ByteOfU8 (U8OfChar v)) in
    align_const p 1

  let si8 _st v p =
    let p = WriteByte (p, ByteOfU8 v) in
    align_const p 1

  let si16 _st v p =
    let p = WriteWord (LittleEndian, p, WordOfU16 v) in
    align_const p 2

  let si32 _st v p =
    let p = WriteDWord (LittleEndian, p, DWordOfU32 (ToU32 v)) in
    align_const p 4

  let si24 = si32

  let si64 _st v p =
    let p = WriteQWord (LittleEndian, p, QWordOfU64 (ToU64 v)) in
    align_const p 8

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 _st v p =
    let p = WriteOWord (LittleEndian, p, OWordOfU128 (ToU128 v)) in
    align_const p 16

  let su8 _st v p =
    let p = WriteByte (p, ByteOfU8 v) in
    align_const p 1

  let su16 _st v p =
    let p = WriteWord (LittleEndian, p, WordOfU16 v) in
    align_const p 2

  let su32 _st v p =
    let p = WriteDWord (LittleEndian, p, DWordOfU32 v) in
    align_const p 4

  let su24 st v p = su32 st (ToU32 v) p

  let su64 _st v p =
    let p = WriteQWord (LittleEndian, p, QWordOfU64 v) in
    align_const p 8

  let su40 st v p = su64 st (ToU64 v) p

  let su48 st v p = su64 st (ToU64 v) p

  let su56 st v p = su64 st (ToU64 v) p

  let su128 _st v p =
    let p = WriteOWord (LittleEndian, p, OWordOfU128 v) in
    align_const p 16

  let tup_opn_with_typs vtyps st p =
    (* inside tuples have one nullmask bit per item regardless of nullability *)
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        Array.fold_left (fun c typ ->
          if is_nullable typ then c + 1 else c
        ) 0 vtyps
      else
        Array.length vtyps in
    push_nullmask st p ;
    zero_nullmask_const nullmask_bits p

  let tup_opn st vtyps p =
    tup_opn_with_typs vtyps st p

  let tup_cls st p =
    pop_nullmask st ;
    p

  let tup_sep _idx _st p = p

  let is_private name =
    String.length name > 0 && name.[0] = '_'

  let record_field_cmp (n1, _) (n2, _) =
    String.compare n1 n2

  let tuple_typs_of_record vtyps =
    (* Like tuples, with fields in alphabetic order, with private fields
     * omitted: *)
    let vtyps =
      Array.filter (fun (name, _typ) -> not (is_private name)) vtyps in
    Array.fast_sort record_field_cmp vtyps ;
    Array.map snd vtyps

  let rec_opn st vtyps p =
    let vtyps = tuple_typs_of_record vtyps in
    tup_opn_with_typs vtyps st p

  let rec_cls st p =
    pop_nullmask st ;
    p

  let rec_sep _fname _st p = p

  let vec_opn st dim vtyp p =
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        if is_nullable vtyp then dim else 0
      else
        dim in
    push_nullmask st p ;
    zero_nullmask_const nullmask_bits p

  let vec_cls st p =
    pop_nullmask st ;
    p

  let vec_sep _idx _st p = p

  let list_opn st vtyp n p =
    let outermost = st.nullmasks = [] in
    let p = WriteDWord (LittleEndian, p, DWordOfU32 n) in
    let nullmask_bits =
      if outermost then
        if is_nullable vtyp then n else U32 Uint32.zero
      else
        n in
    push_nullmask st p ;
    zero_nullmask_dyn nullmask_bits p

  let list_cls st p =
    pop_nullmask st ;
    p

  let list_sep _st p = p

  (* This is called before serializing the null/notnull, but that's
   * our best opportunity to increment the bit index: *)
  let nullable st p =
    (match st.nullmasks with
    | [] -> ()
    | (bi, p) :: r ->
        st.nullmasks <- (bi + 1, p) :: r) ;
    Comment ("Advance nullmask bit index", p)

  (* The nullmask has been zeroed already: *)
  let snull _t _st p = p

  let snotnull _t st p =
    (* When we encode a non-null nullable value we must also set its bit in
     * the nullmask: *)
    match st.nullmasks with
    | [] -> p
    | (bi, p) :: _ ->
        Comment ("Set the nullmask bit",
          SetBit (p, U32 (Uint32.of_int (bi-1)), Bit true))

  type ssizer = maybe_nullable -> path -> e -> ssize

  let round_up_const n =
    ConstSize (
      ((n + !ringbuf_word_size - 1) / !ringbuf_word_size) * !ringbuf_word_size)

  let round_up_const_bits b =
    let n = (b + 7) / 8 in
    round_up_const n

  let round_up_dyn sz =
    let mask = Size (!ringbuf_word_size - 1) in
    LogAnd (
      Add (sz, mask),
      LogXor (mask, SizeOfU32 (U32 (Uint32.of_int64 0xFFFF_FFFFL))))

  (* HeapValue will iterate over the whole tree of values but we want to
   * hide anything that's below a private field: *)
  let unless_private vtyp path k =
    let rec loop path =
      match path with
      | [] | [_] ->
          (* Reached the bottom of the stack without meeting a private field
           * name *)
          k ()
      | idx :: path ->
        (match type_of_path vtyp path with
        | Nullable (TRec typs)
        | NotNullable (TRec typs) ->
            let name, _ = typs.(idx) in
            if is_private name then
              ConstSize 0
            else
              loop path
        | _ ->
            loop path) in
    loop path

  (* SerSize of the whole string: *)
  let ssize_of_string vtyp path id =
    unless_private vtyp path (fun () ->
      let sz = SizeOfU32 (StringLength id) in
      let headsz = Size !ringbuf_word_size in
      DynSize (Add (headsz ,round_up_dyn sz)))

  (* SerSize of the list header: *)
  let ssize_of_list vtyp path _id =
    unless_private vtyp path (fun () ->
      ConstSize !ringbuf_word_size)

  let ssize_of_float vtyp path _ = unless_private vtyp path (fun () -> round_up_const 8)
  let ssize_of_bool vtyp path _ = unless_private vtyp path (fun () -> round_up_const 1)
  let ssize_of_i8 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 1)
  let ssize_of_i16 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 2)
  let ssize_of_i24 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 3)
  let ssize_of_i32 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 4)
  let ssize_of_i40 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 5)
  let ssize_of_i48 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 6)
  let ssize_of_i56 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 7)
  let ssize_of_i64 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 8)
  let ssize_of_i128 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 16)
  let ssize_of_u8 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 1)
  let ssize_of_u16 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 2)
  let ssize_of_u24 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 3)
  let ssize_of_u32 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 4)
  let ssize_of_u40 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 5)
  let ssize_of_u48 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 6)
  let ssize_of_u56 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 7)
  let ssize_of_u64 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 8)
  let ssize_of_u128 vtyp path _ = unless_private vtyp path (fun () -> round_up_const 16)
  let ssize_of_char vtyp path _ = unless_private vtyp path (fun () -> round_up_const_bits 1)

  let ssize_of_tup vtyp path _ =
    unless_private vtyp path (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost = path = []
      and typs =
        match type_of_path vtyp path with
        | Nullable (TTup typs)
        | NotNullable (TTup typs) ->
            typs
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost then
          Array.length typs
        else
          Array.fold_left (fun c typ ->
            if is_nullable typ then c + 1 else c
          ) 0 typs))

  let ssize_of_rec vtyp path _ =
    unless_private vtyp path (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost = path = []
      and typs =
        match type_of_path vtyp path with
        | Nullable (TRec typs)
        | NotNullable (TRec typs) ->
            typs
        | _ -> assert false in
      let typs = Array.filter_map (fun (name, typ) ->
        if is_private name then None else Some typ
      ) typs in
      round_up_const_bits (
        if is_outermost then
          Array.length typs
        else
          Array.fold_left (fun c typ ->
            if is_nullable typ then c + 1 else c
          ) 0 typs))

  let ssize_of_vec vtyp path _ =
    unless_private vtyp path (fun () ->
      let is_outermost = path = []
      and dim, typ =
        match type_of_path vtyp path with
        | Nullable (TVec (dim, typ))
        | NotNullable (TVec (dim, typ)) ->
            dim, typ
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost || is_nullable typ then dim else 0))

  let ssize_of_null vtyp path =
    unless_private vtyp path (fun () -> ConstSize 0)
end
