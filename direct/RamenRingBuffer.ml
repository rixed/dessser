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

  let init_state _typ _oc p =
    { nullmasks = [] }, p

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

  let zero_nullmask oc bits p =
    let sz = (bits + 7) / 8 in
    let p =
      BE.blit_bytes oc p
        BE.(byte_of_const oc 0)
        BE.(size_of_const oc sz) in
    align_const oc p sz

  type 'a ser = BE.output -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  let sfloat oc _st v p =
    BE.write_dword oc p (BE.Float.to_dword oc v)

  let sstring oc _st v p =
    let len = BE.length_of_string oc v in
    let p = BE.write_dword oc p BE.(u32_of_size oc len |> U32.to_dword oc) in
    let bytes = BE.bytes_of_string oc v in
    let p = BE.write_bytes oc p bytes in
    align_dyn oc p (BE.u32_of_size oc len)

  let sbool oc _st v p =
    let byte = BE.(U8.to_byte oc (u8_of_bool oc v)) in
    let p = BE.write_byte oc p byte in
    align_const oc p 1

  let si8 oc _st v p =
    let p = BE.(write_byte oc p (I8.to_byte oc v)) in
    align_const oc p 1

  let si16 oc _st v p =
    let p = BE.(write_word oc p (I16.to_word oc v)) in
    align_const oc p 2

  let si32 oc _st v p =
    let p = BE.(write_dword oc p (I32.to_dword oc v)) in
    align_const oc p 4

  let si64 oc _st v p =
    let p = BE.(write_qword oc p (I64.to_qword oc v)) in
    align_const oc p 8

  let si128 oc _st v p =
    let p = BE.(write_oword oc p (I128.to_oword oc v)) in
    align_const oc p 16

  let su8 oc _st v p =
    let p = BE.(write_byte oc p (U8.to_byte oc v)) in
    align_const oc p 1

  let su16 oc _st v p =
    let p = BE.(write_word oc p (U16.to_word oc v)) in
    align_const oc p 2

  let su32 oc _st v p =
    let p = BE.(write_dword oc p (U32.to_dword oc v)) in
    align_const oc p 4

  let su64 oc _st v p =
    let p = BE.(write_qword oc p (U64.to_qword oc v)) in
    align_const oc p 8

  let su128 oc _st v p =
    let p = BE.(write_oword oc p (U128.to_oword oc v)) in
    align_const oc p 16

  let tup_opn typs oc st p =
    (* inside tuples have one nullmask bit per item regardless of nullability *)
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        Array.fold_left (fun c typ ->
          if typ.Types.nullable then c + 1 else c
        ) 0 typs
      else
        Array.length typs in
    push_nullmask st p ;
    zero_nullmask oc nullmask_bits p

  let tup_cls _typs _oc st p =
    pop_nullmask st ;
    p

  let tup_sep _typs _idx _oc _st p = p

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

  let rec_opn typs oc st p =
    let typs = tuple_typs_of_record typs in
    tup_opn typs oc st p

  let rec_cls typs oc st p =
    let typs = tuple_typs_of_record typs in
    tup_cls typs oc st p

  let rec_sep _typs _fname _oc _st p = p

  let vec_opn dim typ oc st p =
    let outermost = st.nullmasks = [] in
    let nullmask_bits =
      if outermost then
        if typ.Types.nullable then dim else 0
      else
        dim in
    push_nullmask st p ;
    zero_nullmask oc nullmask_bits p

  let vec_cls _dim _typ _oc st p =
    pop_nullmask st ;
    p

  let vec_sep _dim _typ _idx _oc _st p = p

  (* This is called before serializing the null/notnull, but that's
   * our best opportunity to increment the bit index: *)
  let nullable _typ oc st p =
    (match st.nullmasks with
    | [] -> ()
    | (bi, p) :: r ->
        BE.comment oc "Advance nullmask bit index" ;
        st.nullmasks <- (bi + 1, p) :: r) ;
    p

  (* The nullmask has been zeroed already: *)
  let snull _oc _st p = p

  let snotnull oc st p =
    (* When we encode a non-null nullable value we must also set its bit in
     * the nullmask: *)
    (match st.nullmasks with
    | [] -> ()
    | (bi, p) :: _ ->
        BE.comment oc "Set the nullmask bit" ;
        BE.(set_bit oc p (U32.of_const_int oc (bi-1)) (bit_of_const oc true))) ;
    p
end
