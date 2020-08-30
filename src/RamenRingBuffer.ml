open Batteries
open Stdint
open DessserTools
open Dessser
module T = DessserTypes
module E = DessserExpressions
open E.Ops

(* Size of the word stored in the ringbuffer, in bytes. *)
let ringbuf_word_size = ref 4

(* Realign the pointer on a multiple of [ringbuf_word_size].
 * [extra_bytes] modulo [ringbuf_word_size] gives the number of bytes
 * that's been written after the last word boundary.
 * [extra_bytes] must be a size valued expression. *)
let align_dyn p extra_bytes =
  let wsize = size !ringbuf_word_size in
  let extra_bytes = rem extra_bytes wsize in
  let padding_len = sub wsize extra_bytes in
  choose ~cond:(gt wsize padding_len)
    ~then_:(data_ptr_add p padding_len)
    ~else_:p

let align_const p extra_bytes =
  let extra_bytes = extra_bytes mod !ringbuf_word_size in
  let padding_len = !ringbuf_word_size - extra_bytes in
  if !ringbuf_word_size > extra_bytes then
    data_ptr_add p (size padding_len)
  else
    p

(* RamenRingBuffer can only des/ser types which record fields are ordered: *)
let rec_field_cmp (n1, _) (n2, _) =
  String.compare n1 n2

let rec order_rec_fields mn =
  let rec order_value_type = function
    | T.TRec mns ->
        Array.fast_sort rec_field_cmp mns ;
        T.TRec (Array.map (fun (name, mn) -> name, order_rec_fields mn) mns)
    | T.TTup mns ->
        T.TTup (Array.map order_rec_fields mns)
    | T.TVec (dim, mn) ->
        T.TVec (dim, order_rec_fields mn)
    | T.TList mn ->
        T.TList (order_rec_fields mn)
    | T.Usr ut ->
        order_value_type ut.def
    | mn -> mn in
  match mn with
  | T.NotNullable vt -> T.NotNullable (order_value_type vt)
  | T.Nullable vt -> T.Nullable (order_value_type vt)

let rec are_rec_fields_ordered mn =
  let rec aux = function
    | T.TRec mns ->
        array_for_alli (fun i (_name, mn) ->
          are_rec_fields_ordered mn &&
          i = 0 || rec_field_cmp mns.(i-1) mns.(i) <= 0
        ) mns
    | T.TTup mns ->
        Array.for_all are_rec_fields_ordered mns
    | T.TVec (_, mn) | T.TList mn ->
        are_rec_fields_ordered mn
    | T.Usr ut ->
        aux ut.def
    | _ ->
        true in
  match mn with
  | T.NotNullable vt -> aux vt
  | T.Nullable vt -> aux vt

let is_private name =
  String.length name > 0 && name.[0] = '_'

let tuple_typs_of_record mns =
  (* Like tuples but with fields in alphabetic order, with private fields
   * omitted: *)
  Array.filter_map (fun (name, typ) ->
    if is_private name then None else Some typ
  ) mns

(* We use a stack of "frames" at pointer to nullmask + nullbit index.
 * Our "data pointer" is therefore actually composed of the data pointer
 * itself (p) and a stack (stk): *)
let t_frame = T.(pair dataptr size)

let leave_frame p_stk =
  E.with_sploded_pair "leave_frame" p_stk (fun p stk ->
    pair p (tail stk))

(* Set the next nullbit and return the new stack with increased nullbit
 * position: *)
let set_nullbit stk =
  E.with_sploded_pair "set_nullbit" (head stk) (fun p bi ->
    seq [ debug (string "set nullbit at ") ;
          debug (string_of_int_ bi) ;
          debug (char '\n') ;
          set_bit p bi (bit true) ;
          let frame = pair p (add bi (size 1)) in
          cons frame (tail stk) ])

let skip_nullbit stk =
  E.with_sploded_pair "skip_nullbit" (head stk) (fun p bi ->
    seq [ debug (string "skip nullbit at ") ;
          debug (string_of_int_ bi) ;
          debug (char '\n') ;
          let frame = pair p (add bi (size 1)) in
          cons frame (tail stk) ])

let set_nullbit_to bit stk =
  (if bit then set_nullbit else skip_nullbit) stk

(* Set (or skip) the nullbit if necessary.
 * Due to a default of the format (FIXME) even non-nullable values can
 * require a nullbit (when they are items of a inner compound type).
 * Therefore it is not enough to use the [nullable] callback (yet).
 * So this function, which is called before any actual value is serialized,
 * finds out if a nullbit is needed and then set it. *)
let may_set_nullbit bit mn0 path stk =
  match path with
  | [] ->
      (* This is the outermost value.
       * In theory, since only compound values have nullmasks, then single scalar
       * nullable values cannot be encoded. Which is annoying because singleton
       * tuples cannot be encoded either (which is a good thing). So can't single
       * field record, which is more debatable. Therefore the only way to encode
       * a single value is to wrap it in a one dimensional non-nullable vector or
       * list. (FIXME)
       * Therefore if it's nullable we have a problem: *)
      assert (not (T.is_nullable mn0)) ;
      stk
  | [_] ->
      (* This is an item of the outermost value. It has a nullbit only
       * if nullable, and its nullbit position in the nullmask is in the
       * stack: *)
      if T.is_nullable (T.type_of_path mn0 path) then
        set_nullbit_to bit stk
      else
        stk
  | _ ->
      (* This is an item from an inner compound value. It always has a
       * nullbit for historical reasons (FIXME). *)
      set_nullbit_to bit stk

(* TODO: check a nullbit is present for this type before sploding *)
let may_skip_nullbit mn0 path p_stk =
  E.with_sploded_pair "may_skip_nullbit" p_stk (fun p stk ->
    let stk = may_set_nullbit false mn0 path stk in
    pair p stk)

module Ser : SER =
struct
  type state = unit

  let ptr _mn = T.(pair dataptr (slist t_frame))

  (* Few helper functions: *)

  (* Zero a nullmask known at compile time and advance the pointer *)
  let zero_nullmask_const bits p =
    let sz = (bits + 7) / 8 in
    let p = blit_byte p (byte 0) (size sz) in
    align_const p sz

  (* Zero the nullmask known only at runtime and advance the pointer *)
  let zero_nullmask_dyn bits p =
    let sz = right_shift (add (u32_of_int 7) bits) (u8 3) in
    let p = blit_byte p (byte 0) (size_of_u32 sz) in
    align_dyn p (size_of_u32 sz)

  (* Enter a new compound type by zeroing a nullmask and setting up a new
   * frame for it, all this after having set its own nullbit if needed: *)
  let enter_frame to_expr zero_nullmask nullmask_bits mn0 path p_stk =
    E.with_sploded_pair "enter_frame" p_stk (fun p stk ->
      let stk = may_set_nullbit true mn0 path stk in
      let new_frame = pair p (size 0) in
      seq [ debug (string "ser: enter a new frame at ") ;
            debug (string_of_int_ (data_ptr_offset p)) ;
            debug (string " with ") ;
            debug (string_of_int_ (to_expr nullmask_bits)) ;
            debug (string " nullbits\n") ;
            pair
              (zero_nullmask nullmask_bits p)
              (cons new_frame stk) ])

  (* Enter a new compound type by zeroing a nullmask and setting up a new
   * frame for it, all this after having set its own nullbit if needed: *)
  let enter_frame_dyn = enter_frame identity zero_nullmask_dyn
  let enter_frame_const = enter_frame u8 zero_nullmask_const

  let with_data_ptr p_stk f =
    E.with_sploded_pair "with_data_ptr" p_stk (fun p stk ->
      pair (f p) stk)

  let with_nullbit_done mn0 path p_stk f =
    E.with_sploded_pair "with_nullbit_done" p_stk (fun p stk ->
      let stk = may_set_nullbit true mn0 path stk in
      let p = f p in
      pair p stk)

  let start mn p =
    assert (are_rec_fields_ordered mn) ;
    (), pair p (end_of_list t_frame)

  let stop () p_stk =
    (* TODO: assert tail stk = end_of_list *)
    first p_stk

  type ser = state -> T.maybe_nullable -> T.path -> E.t -> E.t -> E.t

  let sfloat () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      seq [ debug (string "ser a float at ") ;
            debug (string_of_int_ (data_ptr_offset p)) ;
            debug (char '\n') ;
            write_qword LittleEndian p (qword_of_float v) ])

  let sstring () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let len = string_length v in
      let p =
        seq [ debug (string "ser a string at ") ;
              debug (string_of_int_ (data_ptr_offset p)) ;
              debug (string " of length ") ;
              debug (string_of_int_ len) ;
              debug (char '\n') ;
              write_dword LittleEndian p (dword_of_u32 len) ] in
      let bytes = bytes_of_string v in
      let p = write_bytes p bytes in
      align_dyn p (size_of_u32 len))

  let sbool () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_byte p (byte_of_bool v) in
      align_const p 1)

  let schar () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_byte p (byte_of_u8 (u8_of_char v)) in
      align_const p 1)

  let si8 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_byte p (byte_of_u8 (to_u8 v)) in
      align_const p 1)

  let si16 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_word LittleEndian p (word_of_u16 (to_u16 v)) in
      align_const p 2)

  let si32 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_dword LittleEndian p (dword_of_u32 (to_u32 v)) in
      align_const p 4)

  let si24 = si32

  let si64 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_qword LittleEndian p (qword_of_u64 (to_u64 v)) in
      align_const p 8)

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_oword LittleEndian p (oword_of_u128 (to_u128 v)) in
      align_const p 16)

  let su8 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_byte p (byte_of_u8 v) in
      align_const p 1)

  let su16 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_word LittleEndian p (word_of_u16 v) in
      align_const p 2)

  let su32 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_dword LittleEndian p (dword_of_u32 v) in
      align_const p 4)

  let su24 () vt0 path v p = su32 () vt0 path (to_u32 v) p

  let su64 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_qword LittleEndian p (qword_of_u64 v) in
      align_const p 8)

  let su40 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su48 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su56 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su128 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = write_oword LittleEndian p (oword_of_u128 v) in
      align_const p 16)

  (* outermost = when opening a compound type, the path is
   * currently empty: *)
  let is_outermost = function
    | [] -> true
    | _ -> false

  (* The given mns has all the types of the input structure, regardless of the
   * fieldmask. When runtime fieldmasks are added then we must also be given
   * either the fieldmask and quickly compute the number of fields that are
   * set, or we have that number ready part of the runtime fieldmask structure,
   * as the nullmask width must be recomputed for every serialized value. *)
  let tup_rec_opn mn0 path mns p_stk =
    (* TODO: this must be revisited once runtime fieldmasks are in place: *)
    let nullmask_bits =
      if is_outermost path then
        Array.fold_left (fun c mn ->
          if T.is_nullable mn then c + 1 else c
        ) 0 mns
      else
        Array.length mns in
    enter_frame_const nullmask_bits mn0 path p_stk

  let tup_opn () mn0 path mns p_stk =
    tup_rec_opn mn0 path mns p_stk

  let tup_cls () _ _ p_stk =
    leave_frame p_stk

  let tup_sep _n () _ _ p_stk = p_stk

  let rec_opn () mn0 path mns p_stk =
    let mns = tuple_typs_of_record mns in
    tup_rec_opn mn0 path mns p_stk

  let rec_cls () _ _ p_stk =
    leave_frame p_stk

  let rec_sep _fname () _ _ p_stk = p_stk

  let sum_opn () mn0 path _mns lbl p =
    su16 () mn0 path lbl p

  let sum_cls () _ _ p = p

  let vec_opn () mn0 path dim mn p_stk =
    (* TODO: this must be revisited once runtime fieldmasks are in place: *)
    let nullmask_bits =
      if is_outermost path then
        if T.is_nullable mn then dim else 0
      else
        dim in
    enter_frame_const nullmask_bits mn0 path p_stk

  let vec_cls () _ _ p_stk =
    leave_frame p_stk

  let vec_sep _idx () _ _ p_stk = p_stk

  (* [n] is an u32 *)
  let list_opn () mn0 path mn n p_stk =
    let n = match n with
      | Some n -> n
      | None -> failwith "RamenRingBuffer.Ser needs list size upfront" in
    let nullmask_bits =
      if is_outermost path then
        if T.is_nullable mn then n else u32_of_int 0
      else
        n in
    let p_stk =
      with_data_ptr p_stk (fun p ->
        write_dword LittleEndian p (dword_of_u32 n)) in
    enter_frame_dyn nullmask_bits mn0 path p_stk

  let list_cls () _ _ p_stk =
    leave_frame p_stk

  let list_sep () _ _ p_stk = p_stk

  let nullable () _ _ p_stk = p_stk

  (* The nullmask has been zeroed already: *)
  let snull _t () mn0 path p_stk =
    may_skip_nullbit mn0 path p_stk

  (* nullbits are set when actual values are written: *)
  let snotnull _t () _ _ p_stk = p_stk

  type ssizer = T.maybe_nullable -> T.path -> E.t -> ssize

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
        | T.Nullable (TRec mns)
        | T.NotNullable (TRec mns) ->
            let name, mn = mns.(idx) in
            if is_private name then
              ConstSize 0
            else
              loop rest mn
        | T.Nullable (TTup mns)
        | T.NotNullable (TTup mns) ->
            loop rest mns.(idx)
        | T.Nullable (TVec (d, mn))
        | T.NotNullable (TVec (d, mn)) ->
            assert (idx < d) ;
            loop rest mn
        | T.Nullable (TList mn)
        | T.NotNullable (TList mn) ->
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
  let ssize_of_list mn path id =
    unless_private mn path (fun () ->
      let with_nullmask () =
        DynSize (add (size !ringbuf_word_size)
                     (round_up_dyn (size_of_u32 (list_length id))))
      and no_nullmask () =
        ConstSize !ringbuf_word_size in
      (* If its the outermost list and the items are not nullable then there is no
       * nullmask. In all other cases there is one nullbit per item. *)
      if path = [] then
        match mn with
        | NotNullable (TList vt) | Nullable (TList vt) ->
            if T.is_nullable vt then
              with_nullmask ()
            else
              no_nullmask ()
        | _ ->
            assert false
      else
        with_nullmask ())

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
        match T.type_of_path mn path with
        | Nullable (TTup typs)
        | NotNullable (TTup typs) ->
            typs
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost then
          Array.length typs
        else
          Array.fold_left (fun c typ ->
            if T.is_nullable typ then c + 1 else c
          ) 0 typs))

  let ssize_of_rec mn path _ =
    unless_private mn path (fun () ->
      (* Just the additional bitmask: *)
      let is_outermost = path = []
      and typs =
        match T.type_of_path mn path with
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
            if T.is_nullable typ then c + 1 else c
          ) 0 typs))

  (* Just the additional label: *)
  let ssize_of_sum _ _ _ =
    ConstSize !ringbuf_word_size

  let ssize_of_vec mn path _ =
    unless_private mn path (fun () ->
      let is_outermost = path = []
      and dim, typ =
        match T.type_of_path mn path with
        | Nullable (TVec (dim, typ))
        | NotNullable (TVec (dim, typ)) ->
            dim, typ
        | _ -> assert false in
      round_up_const_bits (
        if is_outermost || T.is_nullable typ then dim else 0))

  let ssize_of_null _mn _path = ConstSize 0
end

module Des : DES =
struct
  type state = unit

  (* To deserialize we need the same kind of pointer than to serialize: *)
  let ptr = Ser.ptr

  let skip_nullmask_const bits p =
    let sz = (bits + 7) / 8 in
    let p = data_ptr_add p (size sz) in
    align_const p sz

  let skip_nullmask_dyn bits p =
    let sz = right_shift (add (u32_of_int 7) bits) (u8 3) in
    let p = data_ptr_add p (size_of_u32 sz) in
    align_dyn p (size_of_u32 sz)

  (* Enter a new compound type by recording its location in the stack
   * and jumping over it, after having incremented the current nullbit
   * index: *)
  let enter_frame to_expr skip_nullmask nullmask_bits mn0 path p_stk =
    E.with_sploded_pair "enter_frame" p_stk (fun p stk ->
      let stk = may_set_nullbit false mn0 path stk in
      let new_frame = pair p (size 0) in
      seq [ debug (string "des: enter a new frame at ") ;
            debug (string_of_int_ (data_ptr_offset p)) ;
            debug (string " with ") ;
            debug (string_of_int_ (to_expr nullmask_bits)) ;
            debug (string " nullbits\n") ;
            pair
              (skip_nullmask nullmask_bits p)
              (cons new_frame stk) ])

  let enter_frame_const = enter_frame u8 skip_nullmask_const
  let enter_frame_dyn = enter_frame identity skip_nullmask_dyn

  let start mn p =
    assert (are_rec_fields_ordered mn) ;
    (), pair p (end_of_list t_frame)

  let stop () p_stk =
    (* TODO: assert tail stk = end_of_list *)
    first p_stk

  type des = state -> T.maybe_nullable -> T.path -> E.t -> E.t

  (* When we deserialize any value, we may have to increment the nullbit
   * pointer depending on the current type of position in the global type
   * [mn0]: *)
  let with_nullbit_done mn0 path p_stk f =
    E.with_sploded_pair "with_nullbit_done" p_stk (fun p stk ->
      let stk = may_set_nullbit false mn0 path stk in
      E.with_sploded_pair "with_nullbit_done" (f p) (fun v p ->
        pair v (pair p stk)))

  let dfloat () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      seq [ debug (string "desser a float from ") ;
            debug (string_of_int_ (data_ptr_offset p)) ;
            debug (char '\n') ;
            E.with_sploded_pair "dfloat" (read_qword LittleEndian p) (fun w p ->
              pair (float_of_qword w) p) ])

  let dstring () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      seq [ debug (string "deser a string from ") ;
            debug (string_of_int_ (data_ptr_offset p)) ;
            debug (char '\n') ;
            E.with_sploded_pair "dstring1" (read_dword LittleEndian p) (fun len p ->
              let len = size_of_dword len in
              E.with_sploded_pair "dstring2" (read_bytes p len) (fun bs p ->
                pair (string_of_bytes bs) (align_dyn p len))) ])

  let dbool () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "dbool" (read_byte p) (fun b p ->
        pair (bool_of_byte b) (align_const p 1)))

  let dchar () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "dchar" (read_byte p) (fun b p ->
        pair (char_of_byte b) (align_const p 1)))

  let du8 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du8" (read_byte p) (fun b p ->
        pair (u8_of_byte b) (align_const p 1)))

  let du16 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du16" (read_word LittleEndian p) (fun w p ->
        pair (u16_of_word w) (align_const p 2)))

  let du24 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du24" (read_dword LittleEndian p) (fun w p ->
        pair (to_u24 (u32_of_dword w)) (align_const p 4)))

  let du32 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du32" (read_dword LittleEndian p) (fun w p ->
        pair (u32_of_dword w) (align_const p 4)))

  let du40 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du40" (read_qword LittleEndian p) (fun w p ->
        pair (to_u40 (u64_of_qword w)) (align_const p 8)))

  let du48 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du48" (read_qword LittleEndian p) (fun w p ->
        pair (to_u48 (u64_of_qword w)) (align_const p 8)))

  let du56 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du56" (read_qword LittleEndian p) (fun w p ->
        pair (to_u56 (u64_of_qword w)) (align_const p 8)))

  let du64 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du64" (read_qword LittleEndian p) (fun w p ->
        pair (u64_of_qword w) (align_const p 8)))

  let du128 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du128" (read_oword LittleEndian p) (fun w p ->
        pair (u128_of_oword w) (align_const p 8)))

  let di8 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di8" (read_byte p) (fun b p ->
        pair (to_i8 (u8_of_byte b)) (align_const p 1)))

  let di16 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di16" (read_word LittleEndian p) (fun w p ->
        pair (to_i16 (u16_of_word w)) (align_const p 2)))

  let di24 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di24" (read_dword LittleEndian p) (fun w p ->
        pair (to_i24 (u32_of_dword w)) (align_const p 4)))

  let di32 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di32" (read_dword LittleEndian p) (fun w p ->
        pair (to_i32 (u32_of_dword w)) (align_const p 4)))

  let di40 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di40" (read_qword LittleEndian p) (fun w p ->
        pair (to_i40 (u64_of_qword w)) (align_const p 8)))

  let di48 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di48" (read_qword LittleEndian p) (fun w p ->
        pair (to_i48 (u64_of_qword w)) (align_const p 8)))

  let di56 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di56" (read_qword LittleEndian p) (fun w p ->
        pair (to_i56 (u64_of_qword w)) (align_const p 8)))

  let di64 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di64" (read_qword LittleEndian p) (fun w p ->
        pair (to_i64 (u64_of_qword w)) (align_const p 8)))

  let di128 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di128" (read_oword LittleEndian p) (fun w p ->
        pair (to_i128 (u128_of_oword w)) (align_const p 8)))

  let is_outermost = function
    | [] -> true
    | _ -> false

  let tup_rec_opn mn0 path mns p_stk =
    let nullmask_bits =
      if is_outermost path then
        Array.fold_left (fun c mn ->
          if T.is_nullable mn then c + 1 else c
        ) 0 mns
      else
        Array.length mns in
    enter_frame_const nullmask_bits mn0 path p_stk

  let tup_opn () mn0 path mns p_stk =
    tup_rec_opn mn0 path mns p_stk

  let tup_cls () _ _ p_stk =
    leave_frame p_stk

  let tup_sep _n () _ _ p_stk = p_stk

  let rec_opn () mn0 path mns p_stk =
    let mns = tuple_typs_of_record mns in
    tup_rec_opn mn0 path mns p_stk

  let rec_cls () _ _ p_stk =
    leave_frame p_stk

  let rec_sep _n () _ _ p_stk = p_stk

  let vec_opn () mn0 path dim mn p_stk =
    let nullmask_bits =
      if is_outermost path then
        if T.is_nullable mn then dim else 0
      else
        dim in
    enter_frame_const nullmask_bits mn0 path p_stk

  let vec_cls () _ _ p_stk =
    leave_frame p_stk

  let vec_sep _n () _ _ p_stk = p_stk

  (* Sums are encoded with a leading word for the label: *)
  let sum_opn () mn0 path _mns p =
    du16 () mn0 path p

  let sum_cls () _ _ p = p

  let list_opn = KnownSize
    (fun () mn0 path mn p_stk ->
      E.with_sploded_pair "list_opn1" p_stk (fun p stk ->
        E.with_sploded_pair "list_opn2" (read_dword LittleEndian p) (fun n p ->
          let n = u32_of_dword n in
          let nullmask_bits =
            if is_outermost path then
              if T.is_nullable mn then n else u32_of_int 0
            else
              n in
          (* TODO: change enter_frame_dyn signature to take p and stk *)
          let p_stk = pair p stk in
          let p_stk = enter_frame_dyn nullmask_bits mn0 path p_stk in
          pair n p_stk)))

  let list_cls () _ _ p_stk =
    leave_frame p_stk

  let list_sep () _ _ p_stk = p_stk

  (* Called only on nullable value, so there necessarily is a nullbit: *)
  let is_null () _ path p_stk =
    assert (path <> []) ;
    (* TODO: assert stk <> end_of_list *)
    (* Do not advance the nullbit index as it's already done on a per
     * value basis: *)
    E.with_sploded_pair "is_null2" (head (secnd p_stk)) (fun p bi ->
      E.let1 (not_ (bool_of_bit (get_bit p bi))) (fun b ->
        seq [ debug (string "des: get nullbit at ") ;
              debug (string_of_int_ bi) ;
              debug (string " -> ") ;
              debug (string_of_int_ (u8_of_bool b)) ;
              debug (char '\n') ;
              b ]))

  let dnull _t () mn0 path p_stk =
    may_skip_nullbit mn0 path p_stk

  let dnotnull _t () _ _ p_stk = p_stk
end
