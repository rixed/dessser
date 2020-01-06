open Batteries
open Stdint
open Dessser

(* Size of the word stored in the ringbuffer, in bytes. *)
let ringbuf_word_size = ref 4

module Ser (BE : BACKEND) : SER with module BE = BE =
struct
  module BE = BE

  type state =
    (* That int count the nullable fields (ie. bit index of the next nullable
     * in the nullmask) *)
    { mutable nullmasks : (int * [`Pointer] id) list }

  let start _typ _oc p =
    { nullmasks = [] }, p
  let stop _oc _st p = p

  let push_nullmask st p = st.nullmasks <- (0, p) :: st.nullmasks
  let pop_nullmask st = st.nullmasks <- List.tl st.nullmasks

  (* Realign the pointer on a multiple of [ringbuf_word_size].
   * [extra_bytes] modulo [ringbuf_word_size] gives the number of bytes
   * that's been written after the last word boundary. *)
  let align_dyn oc p extra_bytes =
    let word_size = BE.U32.of_const_int oc !ringbuf_word_size in
    let extra_bytes =
      BE.(U32.(rem oc extra_bytes word_size)) in
    let padding_len =
      BE.(U32.( sub oc word_size extra_bytes)) in
    let cond =
      BE.U32.(gt oc word_size padding_len) in
    BE.choose oc ~cond
      (fun oc -> BE.(pointer_add oc p (size_of_u32 oc padding_len)))
      (fun _oc -> p)

  let align_const oc p extra_bytes =
    let extra_bytes = extra_bytes mod !ringbuf_word_size in
    let padding_len = !ringbuf_word_size - extra_bytes in
    if !ringbuf_word_size > extra_bytes then
      BE.(pointer_add oc p (size_of_const oc padding_len))
    else
      p

  (* Zero a nullmask known at compile time and advance the pointer *)
  let zero_nullmask_const oc bits p =
    let sz = (bits + 7) / 8 in
    let p =
      BE.blit_bytes oc p
        BE.(byte_of_const oc 0)
        BE.(size_of_const oc sz) in
    align_const oc p sz

  (* Zero the nullmask known only at runtime and advance the pointer *)
  let zero_nullmask_dyn oc bits p =
    let sz = BE.(U32.(shift_right oc (add oc bits (of_const_int oc 7))
                                     (U8.of_const_int oc 3))) in
    let p =
      BE.blit_bytes oc p
        BE.(byte_of_const oc 0)
        BE.(size_of_u32 oc sz) in
    align_dyn oc p sz

  type 'a ser = BE.output -> frame list -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let sfloat oc _frames _st v p =
    BE.write_qword oc p (BE.Float.to_qword oc v)

  let sstring oc _frames _st v p =
    let len = BE.length_of_string oc v in
    let p = BE.write_dword oc p BE.(u32_of_size oc len |> U32.to_dword oc) in
    let bytes = BE.bytes_of_string oc v in
    let p = BE.write_bytes oc p bytes in
    align_dyn oc p (BE.u32_of_size oc len)

  let sbool oc _frames _st v p =
    let byte = BE.(U8.to_byte oc (u8_of_bool oc v)) in
    let p = BE.write_byte oc p byte in
    align_const oc p 1

  let schar oc _frames _st v p =
    let p = BE.(write_byte oc p (byte_of_char oc v)) in
    align_const oc p 1

  let si8 oc _frames _st v p =
    let p = BE.(write_byte oc p (I8.to_byte oc v)) in
    align_const oc p 1

  let si16 oc _frames _st v p =
    let p = BE.(write_word oc p (I16.to_word oc v)) in
    align_const oc p 2

  let si32 oc _frames _st v p =
    let p = BE.(write_dword oc p (I32.to_dword oc v)) in
    align_const oc p 4

  let si64 oc _frames _st v p =
    let p = BE.(write_qword oc p (I64.to_qword oc v)) in
    align_const oc p 8

  let si128 oc _frames _st v p =
    let p = BE.(write_oword oc p (I128.to_oword oc v)) in
    align_const oc p 16

  let su8 oc _frames _st v p =
    let p = BE.(write_byte oc p (U8.to_byte oc v)) in
    align_const oc p 1

  let su16 oc _frames _st v p =
    let p = BE.(write_word oc p (U16.to_word oc v)) in
    align_const oc p 2

  let su32 oc _frames _st v p =
    let p = BE.(write_dword oc p (U32.to_dword oc v)) in
    align_const oc p 4

  let su64 oc _frames _st v p =
    let p = BE.(write_qword oc p (U64.to_qword oc v)) in
    align_const oc p 8

  let su128 oc _frames _st v p =
    let p = BE.(write_oword oc p (U128.to_oword oc v)) in
    align_const oc p 16

  let get_tup_typs frames =
    match frames with
    | { typ = ValueType.(Nullable (TTup typs) | NotNullable (TTup typs)) ; _ } :: _ ->
        typs
    | _ ->
        assert false

  let tup_opn_with_typs oc typs st p =
    (* inside tuples have one nullmask bit per item regardless of nullability *)
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        Array.fold_left (fun c typ ->
          if ValueType.is_nullable typ then c + 1 else c
        ) 0 typs
      else
        Array.length typs in
    push_nullmask st p ;
    zero_nullmask_const oc nullmask_bits p

  let tup_opn oc frames st p =
    let typs = get_tup_typs frames in
    tup_opn_with_typs oc typs st p

  let tup_cls _oc _frames st p =
    pop_nullmask st ;
    p

  let tup_sep _idx _oc _frames _st p = p

  let is_private name =
    String.length name > 0 && name.[0] = '_'

  let record_field_cmp (n1, _) (n2, _) =
    String.compare n1 n2

  let tuple_typs_of_record typs =
    (* Like tuples, with fields in alphabetic order, with private fields
     * omitted: *)
    let typs =
      Array.filter (fun (name, _typ) -> not (is_private name)) typs in
    Array.fast_sort record_field_cmp typs ;
    Array.map snd typs

  let get_rec_typs frames =
    match frames with
    | { typ = ValueType.(Nullable (TRec typs) | NotNullable (TRec typs)) ; _ } :: _ ->
        typs
    | _ ->
        assert false

  let rec_opn oc frames st p =
    let typs = get_rec_typs frames in
    let typs = tuple_typs_of_record typs in
    tup_opn_with_typs oc typs st p

  let rec_cls _oc _frames st p =
    pop_nullmask st ;
    p

  let rec_sep _fname _oc _frames _st p = p

  let get_vec_typs frames =
    match frames with
    | { typ = ValueType.(Nullable (TVec (dim, typ)) | NotNullable (TVec (dim, typ))) ; _ } :: _ ->
        dim, typ
    | _ ->
        assert false

  let vec_opn oc frames st p =
    let dim, typ = get_vec_typs frames in
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        if ValueType.is_nullable typ then dim else 0
      else
        dim in
    push_nullmask st p ;
    zero_nullmask_const oc nullmask_bits p

  let vec_cls _oc _frames st p =
    pop_nullmask st ;
    p

  let vec_sep _idx _oc _frames _st p = p

  let get_list_typs frames =
    match frames with
    | { typ = ValueType.(Nullable (TList typ) | NotNullable (TList typ)) ; _ } :: _ ->
        typ
    | _ ->
        assert false

  let list_opn oc frames st n p =
    let typ = get_list_typs frames in
    let outermost = st.nullmasks = [] in
    let p = BE.write_dword oc p BE.U32.(to_dword oc n) in
    let nullmask_bits =
      if outermost then
        if ValueType.is_nullable typ then n else (BE.U32.of_const_int oc 0)
      else
        n in
    push_nullmask st p ;
    zero_nullmask_dyn oc nullmask_bits p

  let list_cls _oc _frames st p =
    pop_nullmask st ;
    p

  let list_sep _oc _frames _st p = p

  (* This is called before serializing the null/notnull, but that's
   * our best opportunity to increment the bit index: *)
  (* FIXME: do this using frames for each sXXX function and get rid of
   * SER.nullable *)
  let nullable oc _frames st p =
    (match st.nullmasks with
    | [] -> ()
    | (bi, p) :: r ->
        BE.comment oc "Advance nullmask bit index" ;
        st.nullmasks <- (bi + 1, p) :: r) ;
    p

  (* The nullmask has been zeroed already: *)
  let snull _oc _frames _st p = p

  let snotnull oc _frames st p =
    (* When we encode a non-null nullable value we must also set its bit in
     * the nullmask: *)
    (match st.nullmasks with
    | [] -> ()
    | (bi, p) :: _ ->
        BE.comment oc "Set the nullmask bit" ;
        BE.(set_bit oc p (U32.of_const_int oc (bi-1)) (bit_of_const oc true))) ;
    p

  type 'a ssizer = BE.output -> frame list -> 'a -> ssize

  let round_up_const n =
    ConstSize (
      ((n + !ringbuf_word_size - 1) / !ringbuf_word_size) * !ringbuf_word_size)

  let round_up_const_bits b =
    let n = (b + 7) / 8 in
    round_up_const n

  let round_up_dyn oc sz =
    let mask =
      BE.U32.of_const_int oc (!ringbuf_word_size - 1) in
    BE.(size_of_u32 oc U32.(
      log_and oc
        (add oc (u32_of_size oc sz) mask)
        (log_xor oc mask (u32_of_const oc (Uint32.of_int64 0xFFFF_FFFFL)))))

  (* HeapValue will iterate over the whole tree of values but we want to
   * hide anything that's below a private field: *)
  let or_private frames k =
    let rec loop = function
      | [] | [ _ ] ->
          (* Reached the bottom of the stack without meeting a private field
           * name *)
          k ()
      | top_frame :: (parent_frame :: _ as tail) ->
          (match parent_frame.typ with
          | Nullable (TRec typs) | NotNullable (TRec typs) ->
              let name, _ = typs.(top_frame.index) in
              if is_private name then
                ConstSize 0
              else
                loop tail
          | _ ->
              loop tail) in
    loop frames

  (* SerSize of the whole string: *)
  let ssize_of_string oc frames id =
    or_private frames (fun () ->
      let sz = BE.length_of_string oc id in
      let headsz = BE.size_of_const oc !ringbuf_word_size in
      DynSize (BE.size_add oc headsz (round_up_dyn oc sz)))

  (* SerSize of the list header: *)
  let ssize_of_list _oc frames _id =
    or_private frames (fun () ->
      ConstSize !ringbuf_word_size)

  let ssize_of_float _ frames _ = or_private frames (fun () -> round_up_const 8)
  let ssize_of_bool _ frames _ = or_private frames (fun () -> round_up_const 1)
  let ssize_of_i8 _ frames _ = or_private frames (fun () -> round_up_const 1)
  let ssize_of_i16 _ frames _ = or_private frames (fun () -> round_up_const 2)
  let ssize_of_i32 _ frames _ = or_private frames (fun () -> round_up_const 4)
  let ssize_of_i64 _ frames _ = or_private frames (fun () -> round_up_const 8)
  let ssize_of_i128 _ frames _ = or_private frames (fun () -> round_up_const 16)
  let ssize_of_u8 _ frames _ = or_private frames (fun () -> round_up_const 1)
  let ssize_of_u16 _ frames _ = or_private frames (fun () -> round_up_const 2)
  let ssize_of_u32 _ frames _ = or_private frames (fun () -> round_up_const 4)
  let ssize_of_u64 _ frames _ = or_private frames (fun () -> round_up_const 8)
  let ssize_of_u128 _ frames _ = or_private frames (fun () -> round_up_const 16)
  let ssize_of_char oc frames id = ssize_of_u8 oc frames id

  let ssize_of_tup _ frames _ =
    or_private frames (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost, typs =
        match frames with
        | [ { typ = ValueType.(Nullable (TTup typs) | NotNullable (TTup typs)) ; _ } ] ->
            true, typs
        | { typ = ValueType.(Nullable (TTup typs) | NotNullable (TTup typs)) ; _ } :: _ ->
            false, typs
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost then
          Array.length typs
        else
          Array.fold_left (fun c typ ->
            if ValueType.is_nullable typ then c + 1 else c) 0 typs))

  let ssize_of_rec _ frames _ =
    or_private frames (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost, typs =
        match frames with
        | [ { typ = ValueType.(Nullable (TRec typs) | NotNullable (TRec typs)) ; _ } ] ->
            true, typs
        | { typ = ValueType.(Nullable (TRec typs) | NotNullable (TRec typs)) ; _ } :: _ ->
            false, typs
        | _ -> assert false in
      let typs = Array.filter_map (fun (name, typ) ->
        if is_private name then None else Some typ
      ) typs in
      round_up_const_bits (
        if is_outermost then
          Array.length typs
        else
          Array.fold_left (fun c typ ->
            if ValueType.is_nullable typ then c + 1 else c) 0 typs))

  let ssize_of_vec _ frames _ =
    or_private frames (fun () ->
      let is_outermost, dim, typ =
        match frames with
        | [ { typ = ValueType.(Nullable (TVec (dim, typ)) | NotNullable (TVec (dim, typ))) ; _ } ] ->
            true, dim, typ
        | { typ = ValueType.(Nullable (TVec (dim, typ)) | NotNullable (TVec (dim, typ))) ; _ } :: _ ->
            false, dim, typ
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost || ValueType.is_nullable typ then dim else 0))

  let ssize_of_null _ frames =
    or_private frames (fun () -> ConstSize 0)
end
