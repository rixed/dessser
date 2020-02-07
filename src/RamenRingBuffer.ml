open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open Ops
module T = DessserTypes

(* Size of the word stored in the ringbuffer, in bytes. *)
let ringbuf_word_size = ref 4

module Ser : SER =
struct
  type state =
    (* That int count the nullable fields (ie. bit index of the next nullable
     * in the nullmask) *)
    { mutable nullmasks : int list }
  let ptr _mn = dataptr

  let start _mn p =
    { nullmasks = [] }, p

  let stop _st p = p

  let push_nullmask st p =
    st.nullmasks <- 0 :: st.nullmasks ;
    data_ptr_push p

  let pop_nullmask st p =
    st.nullmasks <- List.tl st.nullmasks ;
    data_ptr_pop p

  (* Realign the pointer on a multiple of [ringbuf_word_size].
   * [extra_bytes] modulo [ringbuf_word_size] gives the number of bytes
   * that's been written after the last word boundary. *)
  let align_dyn p extra_bytes =
    let wsize = size !ringbuf_word_size in
    let extra_bytes = rem extra_bytes wsize in
    let padding_len = sub wsize extra_bytes in
    choose ~cond:(gt wsize padding_len)
      (data_ptr_add p padding_len)
      p

  let align_const p extra_bytes =
    let extra_bytes = extra_bytes mod !ringbuf_word_size in
    let padding_len = !ringbuf_word_size - extra_bytes in
    if !ringbuf_word_size > extra_bytes then
      data_ptr_add p (size padding_len)
    else
      p

  (* Zero a nullmask known at compile time and advance the pointer *)
  let zero_nullmask_const bits p =
    let sz = (bits + 7) / 8 in
    let p = blit_byte p (byte 0) (size sz) in
    align_const p sz

  (* Zero the nullmask known only at runtime and advance the pointer *)
  let zero_nullmask_dyn bits p =
    let sz = right_shift (add bits (size 7)) (u8 3) in
    let p = blit_byte p (byte 0) sz in
    align_dyn p sz

  type ser = state -> maybe_nullable -> path -> e -> e -> e

  let sfloat _st _ _ v p =
    write_qword LittleEndian p (qword_of_float v)

  let sstring _st _ _ v p =
    let len = string_length v in
    let p = write_dword LittleEndian p (dword_of_u32 len) in
    let bytes = bytes_of_string v in
    let p = write_bytes p bytes in
    align_dyn p (size_of_u32 len)

  let sbool _st _ _ v p =
    let p = write_byte p (byte_of_bool v) in
    align_const p 1

  let schar _st _ _ v p =
    let p = write_byte p (byte_of_u8 (u8_of_char v)) in
    align_const p 1

  let si8 _st _ _ v p =
    let p = write_byte p (byte_of_u8 v) in
    align_const p 1

  let si16 _st _ _ v p =
    let p = write_word LittleEndian p (word_of_u16 v) in
    align_const p 2

  let si32 _st _ _ v p =
    let p = write_dword LittleEndian p (dword_of_u32 (to_u32 v)) in
    align_const p 4

  let si24 = si32

  let si64 _st _ _ v p =
    let p = write_qword LittleEndian p (qword_of_u64 (to_u64 v)) in
    align_const p 8

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 _st _ _ v p =
    let p = write_oword LittleEndian p (oword_of_u128 (to_u128 v)) in
    align_const p 16

  let su8 _st _ _ v p =
    let p = write_byte p (byte_of_u8 v) in
    align_const p 1

  let su16 _st _ _ v p =
    let p = write_word LittleEndian p (word_of_u16 v) in
    align_const p 2

  let su32 _st _ _ v p =
    let p = write_dword LittleEndian p (dword_of_u32 v) in
    align_const p 4

  let su24 st vt0 path v p =
    su32 st vt0 path (to_u32 v) p

  let su64 _st _ _ v p =
    let p = write_qword LittleEndian p (qword_of_u64 v) in
    align_const p 8

  let su40 st vt0 path v p = su64 st vt0 path (to_u64 v) p

  let su48 st vt0 path v p = su64 st vt0 path (to_u64 v) p

  let su56 st vt0 path v p = su64 st vt0 path (to_u64 v) p

  let su128 _st _ _ v p =
    let p = write_oword LittleEndian p (oword_of_u128 v) in
    align_const p 16

  let tup_rec_opn mns st p =
    (* inside tuples have one nullmask bit per item regardless of nullability *)
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        Array.fold_left (fun c typ ->
          if is_nullable typ then c + 1 else c
        ) 0 mns
      else
        Array.length mns in
    let p = push_nullmask st p in
    zero_nullmask_const nullmask_bits p

  let tup_opn st _ _ mns p =
    tup_rec_opn mns st p

  let tup_cls st _ _ p =
    pop_nullmask st p

  let tup_sep _idx _st _ _ p = p

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
    Array.map Pervasives.snd vtyps

  let rec_opn st _ _ mns p =
    let mns = tuple_typs_of_record mns in
    tup_rec_opn mns st p

  let rec_cls st _ _ p =
    pop_nullmask st p

  let rec_sep _fname _st _ _ p = p

  let vec_opn st _ _ dim mn p =
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        if is_nullable mn then dim else 0
      else
        dim in
    let p = push_nullmask st p in
    zero_nullmask_const nullmask_bits p

  let vec_cls st _ _ p =
    pop_nullmask st p

  let vec_sep _idx _st _ _ p = p

  let list_opn st _ _ mn n p =
    let n = match n with
      | Some n -> n
      | None -> failwith "RamenRingBuffer.Ser needs list size upfront" in
    let outermost = st.nullmasks = [] in
    let p = write_dword LittleEndian p (dword_of_u32 n) in
    let nullmask_bits =
      if outermost then
        if is_nullable mn then n else u32 Uint32.zero
      else
        n in
    let p = push_nullmask st p in
    zero_nullmask_dyn nullmask_bits p

  let list_cls st _ _ p =
    pop_nullmask st p

  let list_sep _st _ _ p = p

  (* This is called before serializing the null/notnull, but that's
   * our best opportunity to increment the bit index: *)
  let nullable st _ _ p =
    (match st.nullmasks with
    | [] -> ()
    | bi :: r ->
        st.nullmasks <- (bi + 1) :: r) ;
    comment "Advance nullmask bit index" p

  (* The nullmask has been zeroed already: *)
  let snull _t _st _ _ p = p

  let snotnull _t st _ _ p =
    (* When we encode a non-null nullable value we must also set its bit in
     * the nullmask: *)
    match st.nullmasks with
    | [] -> p
    | bi :: _ ->
        comment "Set the nullmask bit"
          (Seq [
            ignore_
              (set_bit (data_ptr_pop p) (size (bi-1)) (bit true)) ;
            p ])

  type ssizer = maybe_nullable -> path -> e -> ssize

  let round_up_const n =
    ConstSize (
      ((n + !ringbuf_word_size - 1) / !ringbuf_word_size) * !ringbuf_word_size)

  let round_up_const_bits b =
    let n = (b + 7) / 8 in
    round_up_const n

  let round_up_dyn sz =
    let mask = size (!ringbuf_word_size - 1) in
    log_and
      (add sz mask)
      (log_xor mask (size_of_u32 (u32 (Uint32.of_int64 0xFFFF_FFFFL))))

  (* HeapValue will iterate over the whole tree of values but we want to
   * hide anything that's below a private field: *)
  let unless_private mn path k =
    let rec loop path mn =
      match path with
      | [] ->
          (* Reached the leaf type without meeting a private field name *)
          k ()
      | idx :: rest ->
        (match mn with
        | Nullable (TRec mns)
        | NotNullable (TRec mns) ->
            let name, mn = mns.(idx) in
            if is_private name then
              ConstSize 0
            else
              loop rest mn
        | Nullable (TTup mns)
        | NotNullable (TTup mns) ->
            loop rest mns.(idx)
        | Nullable (TVec (d, mn))
        | NotNullable (TVec (d, mn)) ->
            assert (idx < d) ;
            loop rest mn
        | Nullable (TList mn)
        | NotNullable (TList mn) ->
            loop rest mn
        | _ ->
            assert false)
    in
    loop path mn

  (* SerSize of the whole string: *)
  let ssize_of_string mn path id =
    unless_private mn path (fun () ->
      let sz = size_of_u32 (string_length id) in
      let headsz = size !ringbuf_word_size in
      DynSize (add headsz (round_up_dyn sz)))

  (* SerSize of the list header: *)
  let ssize_of_list mn path _id =
    unless_private mn path (fun () ->
      ConstSize !ringbuf_word_size)

  let ssize_of_float mn path _ = unless_private mn path (fun () -> round_up_const 8)
  let ssize_of_bool mn path _ = unless_private mn path (fun () -> round_up_const 1)
  let ssize_of_i8 mn path _ = unless_private mn path (fun () -> round_up_const 1)
  let ssize_of_i16 mn path _ = unless_private mn path (fun () -> round_up_const 2)
  let ssize_of_i24 mn path _ = unless_private mn path (fun () -> round_up_const 3)
  let ssize_of_i32 mn path _ = unless_private mn path (fun () -> round_up_const 4)
  let ssize_of_i40 mn path _ = unless_private mn path (fun () -> round_up_const 5)
  let ssize_of_i48 mn path _ = unless_private mn path (fun () -> round_up_const 6)
  let ssize_of_i56 mn path _ = unless_private mn path (fun () -> round_up_const 7)
  let ssize_of_i64 mn path _ = unless_private mn path (fun () -> round_up_const 8)
  let ssize_of_i128 mn path _ = unless_private mn path (fun () -> round_up_const 16)
  let ssize_of_u8 mn path _ = unless_private mn path (fun () -> round_up_const 1)
  let ssize_of_u16 mn path _ = unless_private mn path (fun () -> round_up_const 2)
  let ssize_of_u24 mn path _ = unless_private mn path (fun () -> round_up_const 3)
  let ssize_of_u32 mn path _ = unless_private mn path (fun () -> round_up_const 4)
  let ssize_of_u40 mn path _ = unless_private mn path (fun () -> round_up_const 5)
  let ssize_of_u48 mn path _ = unless_private mn path (fun () -> round_up_const 6)
  let ssize_of_u56 mn path _ = unless_private mn path (fun () -> round_up_const 7)
  let ssize_of_u64 mn path _ = unless_private mn path (fun () -> round_up_const 8)
  let ssize_of_u128 mn path _ = unless_private mn path (fun () -> round_up_const 16)
  let ssize_of_char mn path _ = unless_private mn path (fun () -> round_up_const_bits 1)

  let ssize_of_tup mn path _ =
    unless_private mn path (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost = path = []
      and typs =
        match type_of_path mn path with
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

  let ssize_of_rec mn path _ =
    unless_private mn path (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost = path = []
      and typs =
        match type_of_path mn path with
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

  let ssize_of_vec mn path _ =
    unless_private mn path (fun () ->
      let is_outermost = path = []
      and dim, typ =
        match type_of_path mn path with
        | Nullable (TVec (dim, typ))
        | NotNullable (TVec (dim, typ)) ->
            dim, typ
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost || is_nullable typ then dim else 0))

  let ssize_of_null _mn _path = ConstSize 0
end
