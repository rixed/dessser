open Batteries
open Stdint

open Dessser
open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module Path = DessserPath
open E.Ops

(* Size of the word stored in the ringbuffer, in bytes. *)
let word_size = 4

let rec log2 n =
  assert (n >= 1) ;
  if n = 1 then 0 else
  1 + log2 (n lsr 1)

(*$= log2 & ~printer:string_of_int
  2 (log2 4)
*)

let bytes_of_const_bits n =
  (n + 7) asr 3

(* Return the minimum number of words to store [n] bytes: *)
let words_of_const_bytes n =
  (n + word_size - 1) asr (log2 word_size)

let words_of_const_bits =
  words_of_const_bytes % bytes_of_const_bits

(* Same as above, but [n] is now an u32 valued expression. *)
let words_of_dyn_bytes n =
  (right_shift
    (add n (u32_of_int (word_size - 1)))
    (u8_of_int (log2 word_size)))

(* Round up [n] bytes to fill ringbuf words: *)
let round_up_const_bytes n =
  ((n + word_size - 1) / word_size) * word_size

(* Same as above but [n] is given in bits: *)
let round_up_const_bits b =
  let n = bytes_of_const_bits b in
  round_up_const_bytes n

(* Round up [sz] bytes to fill ringbuf words.
 * [n] must be a size. Returns a size. *)
let round_up_dyn_bytes n =
  let mask = size (word_size - 1) in
  bit_and
    (add n mask)
    (bit_xor mask (size_of_u32 (u32 (Uint32.of_int64 0xFFFF_FFFFL))))

(* Same as above but [n] is given in bits, as a u32: *)
let round_up_dyn_bits n =
  let n = right_shift (add (u32_of_int 7) n) (u8_of_int 3) in
  round_up_dyn_bytes (size_of_u32 n)

(* Realign the pointer on a multiple of [word_size].
 * [extra_bytes] modulo [word_size] gives the number of bytes
 * that's been written after the last word boundary.
 * [extra_bytes] must be a size valued expression. *)
let align_dyn p extra_bytes =
  let wsize = size word_size in
  let extra_bytes =
    (* FIXME: Improve type-checking so that rem/div do not have to return
     * nullable types when used with constants *)
    size_of_u32 (force (rem (u32_of_size extra_bytes)
                            (u32_of_size wsize))) in
  let padding_len = sub wsize extra_bytes in
  let_ ~name:"align_ptr" p (fun p ->
    if_ (gt wsize padding_len)
      ~then_:(ptr_add p padding_len)
      ~else_:p)

let align_const p extra_bytes =
  assert (extra_bytes >= 0) ;
  let extra_bytes = extra_bytes mod word_size in
  if extra_bytes = 0 then p else
    let padding_len = word_size - extra_bytes in
    ptr_add p (size padding_len)

let tuple_typs_of_record mns =
  Array.map snd mns

(* We use a stack of "frames" of pointer to nullmask + nullbit index.
 * Our "data pointer" is therefore actually composed of the data pointer
 * itself (p) and a stack (stk): *)
let t_frame = T.(pair ptr size)

let leave_frame p_stk =
  E.with_sploded_pair "leave_frame" p_stk (fun p stk ->
    make_pair p (tail stk))

(* Set the next nullbit and return the new stack with increased nullbit
 * position: *)
let set_nullbit stk =
  E.with_sploded_pair "set_nullbit" (head stk) (fun p bi ->
    seq [ debug (string "set nullbit at ") ;
          debug (string_of_int_ bi) ;
          debug (char '\n') ;
          set_bit p bi (bit true) ;
          let frame = make_pair p (add bi (size 1)) in
          cons frame (tail stk) ])

let skip_nullbit stk =
  E.with_sploded_pair "skip_nullbit" (head stk) (fun p bi ->
    seq [ debug (string "skip nullbit at ") ;
          debug (string_of_int_ bi) ;
          debug (char '\n') ;
          let frame = make_pair p (add bi (size 1)) in
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
      assert (not mn0.T.nullable) ;
      stk
  | _ ->
      (* This is an item of some compound (top level or inner), which have a
       * nullbit if it is nullable: *)
      if (Path.type_of_path mn0 path).nullable then
        set_nullbit_to bit stk
      else
        stk

(* TODO: check a nullbit is present for this type before sploding *)
let may_skip_nullbit mn0 path p_stk =
  match path with
  | [] ->
      (* Cf above *)
      assert (not mn0.T.nullable) ;
      p_stk
  | _ ->
      if (Path.type_of_path mn0 path).nullable then
        E.with_sploded_pair "may_skip_nullbit" p_stk (fun p stk ->
          let stk = skip_nullbit stk in
          make_pair p stk)
      else
        p_stk

module NullMaskWidth =
struct
  let tup_bits mns =
    (* Even when there are no nullable fields in the tuple/record the reader
     * will expect a nullmask, as it could select also from another parent
     * with more fields, including nullable ones, and cannot tell in the
     * ringbuffer which parent a tuple is originating from. *)
    true,
    Array.count_matching (fun mn ->
      T.(develop_mn mn).nullable) mns

  let rec_bits mns =
    let mns = tuple_typs_of_record mns in
    tup_bits mns

  let vec_bits dim mn =
    let mn = T.develop_mn mn in
    mn.T.nullable,
    if mn.T.nullable then dim else 0

  let lst_bits mn n =
    let mn = T.develop_mn mn in
    mn.T.nullable,
    if mn.T.nullable then n else u32_of_int 0

  let rec of_type = function
    | T.TVec (dim, mn) ->
        vec_bits dim mn
    | TArr _ ->
        invalid_arg "NullMaskWidth.of_type for lists"
    | TTup mns ->
        tup_bits mns
    | TRec mns ->
        rec_bits mns
    | TSum _ ->
        true, 1 (* Although encoding also includes the label *)
    | TUsr { def ; _ } ->
        of_type def
    | _ ->
        false, 0

  (* Return the number of bytes required to encode the nullmask, including
   * its prefix length. 0 means: no nullmask necessary. *)
  let bytes_of_type typ =
    let has_nullmask, nullmask_bits = of_type typ in
    if not has_nullmask then 0 else round_up_const_bits (nullmask_bits + 8)

  (* Return the number of words required to encode the nullmask, including
   * its prefix length. 0 means: no nullmask necessary. *)
  let words_of_type =
    words_of_const_bytes % bytes_of_type
end

module Ser : SER with type config = unit =
struct
  let id = RingBuff

  type config = unit
  type state = unit

  let ptr _mn = T.(pair ptr (required (lst t_frame)))

  (* Few helper functions: *)

  (* Zero a nullmask which max width (ie. assuming all fields will be selected
   * by the fieldmask) is known at compile time, and advance the pointer.
   * Nullmasks occupy a given number of words, which count is given by the
   * first byte of the first nullmask word. Therefore, even for non nullable
   * compound types, there is a full word header which first byte is 1. *)
  let zero_nullmask_const bits p =
    let sz = (bits + 7) / 8 in
    let words = words_of_const_bytes (sz + 1) in
    let p = write_u8 p (u8_of_int words) in
    let p = blit_byte p (u8_of_int 0) (size sz) in
    align_const p (1 + sz)

  (* Zero the nullmask known only at runtime and advance the pointer *)
  let zero_nullmask_dyn bits p =
    let_ ~name:"sz_" (right_shift (add (u32_of_int 7) bits) (u8_of_int 3))
      (fun sz ->
        let words = words_of_dyn_bytes (add sz (u32_of_int 1)) in
        let p = write_u8 p (to_u8 words) in
        let p = blit_byte p (u8_of_int 0) (size_of_u32 sz) in
        align_dyn p (add (size 1) (size_of_u32 sz)))

  (* Enter a new compound type by zeroing a nullmask of the given max width
   * [nullmask_bits] and setting up a new frame for it, all this after having
   * set its own nullbit if needed. *)
  let enter_frame to_expr zero_nullmask
                  ~has_nullmask nullmask_bits mn0 path p_stk =
    E.with_sploded_pair "enter_frame" p_stk (fun p stk ->
      let stk = may_set_nullbit true mn0 path stk in
      let new_frame = make_pair p (size 8 (* width of the length prefix *)) in
      seq [ debug (string "ser: enter a new frame at ") ;
            debug (string_of_int_ (offset p)) ;
            debug (string " with ") ;
            debug (string_of_int_ (to_expr nullmask_bits)) ;
            debug (string " nullbits\n") ;
            make_pair
              (if has_nullmask then zero_nullmask nullmask_bits p else p)
              (cons new_frame stk) ])

  (* Enter a new compound type by zeroing a nullmask and setting up a new
   * frame for it, all this after having set its own nullbit if needed: *)
  let enter_frame_dyn = enter_frame identity zero_nullmask_dyn
  let enter_frame_const = enter_frame u8_of_int zero_nullmask_const

  let with_nullbit_done mn0 path p_stk f =
    E.with_sploded_pair "with_nullbit_done1" p_stk (fun p stk ->
      let stk = may_set_nullbit true mn0 path stk in
      let p = f p in
      make_pair p stk)

  let start ?(config=()) _mn p =
    config,
    make_pair p (end_of_list t_frame)

  let stop () p_stk =
    (* TODO: assert tail stk = end_of_list *)
    first p_stk

  type ser = state -> T.mn -> Path.t -> E.t -> E.t -> E.t

  let with_debug p what write =
    seq [ debug (string ("ser a "^ what ^" at ")) ;
          debug (string_of_int_ (offset p)) ;
          debug (char '\n') ;
          write ]

  let sfloat () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun  p ->
      with_debug p "float"
        (write_u64 LittleEndian p (u64_of_float v)))

  let sstring () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let len = string_length v in
      let p =
        seq [ debug (string "ser a string at ") ;
              debug (string_of_int_ (offset p)) ;
              debug (string " of length ") ;
              debug (string_of_int_ len) ;
              debug (char '\n') ;
              write_u32 LittleEndian p len ] in
      let bytes = bytes_of_string v in
      let p = write_bytes p bytes in
      align_dyn p (size_of_u32 len))

  let sbool () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "bool" (write_u8 p (u8_of_bool v)) in
      align_const p 1)

  let schar () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "char" (write_u8 p (u8_of_char v)) in
      align_const p 1)

  let si8 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "i8" (write_u8 p (to_u8 v)) in
      align_const p 1)

  let si16 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "i16"
                (write_u16 LittleEndian p (to_u16 v)) in
      align_const p 2)

  let si32 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "i24/32"
                (write_u32 LittleEndian p (to_u32 v)) in
      align_const p 4)

  let si24 = si32

  let si64 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "i40/48/56/64"
                (write_u64 LittleEndian p (to_u64 v)) in
      align_const p 8)

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "i128"
                (write_u128 LittleEndian p (to_u128 v)) in
      align_const p 16)

  let su8 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "u8" (write_u8 p v) in
      align_const p 1)

  let su16 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "u16"
                (write_u16 LittleEndian p v) in
      align_const p 2)

  let su32 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "u24/32"
                (write_u32 LittleEndian p v) in
      align_const p 4)

  let su24 () vt0 path v p = su32 () vt0 path (to_u32 v) p

  let su64 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "u40/48/56/64"
                (write_u64 LittleEndian p v) in
      align_const p 8)

  let su40 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su48 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su56 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su128 () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      let p = with_debug p "u128"
                (write_u128 LittleEndian p v) in
      align_const p 16)

  let sext f () mn0 path v p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      with_debug p "ext" (f p v))

  (* The given mns has all the types of the input structure, regardless of the
   * fieldmask. When runtime fieldmasks are added then we must also be given
   * either the fieldmask and quickly compute the number of fields that are
   * set, or we have that number ready part of the runtime fieldmask structure,
   * as the nullmask width must be recomputed for every serialized value.
   * Notice that if [nullmask_bits] is zero then an empty nullmask, occupying
   * a full word, will still be prepended. That's because the deserializer,
   * not knowing where any given tuple is coming from, might not be able to
   * ascertain that the original compound type had some null fields or not.
   * Remember, the children are feed a subset of the output of the parent,
   * selected with a fieldmask, not some arbitrary value construction. This is
   * different for vectors/lists. *)
  let tup_rec_opn mns mn0 path p_stk =
    (* Allocate one bit per nullable item. At most, the deserializer will look
     * for as many bits as there are nullable items, if all items are selected
     * by the field mask. *)
    let has_nullmask, nullmask_bits = NullMaskWidth.tup_bits mns in
    enter_frame_const ~has_nullmask nullmask_bits mn0 path p_stk

  let tup_opn mns () mn0 path p_stk =
    tup_rec_opn mns mn0 path p_stk

  let tup_cls () _ _ p_stk =
    leave_frame p_stk

  let tup_sep () _ _ p_stk = p_stk

  let rec_opn mns () mn0 path p_stk =
    let mns = tuple_typs_of_record mns in
    tup_rec_opn mns mn0 path p_stk

  let rec_cls () _ _ p_stk =
    leave_frame p_stk

  let rec_sep () _ _ p_stk = p_stk

  (* Sum types are encoded with a 1-dword header composed of:
   * - a 16bits nullmask, of which only bit 0 will ever be used
   *   (whether the constructed value is nullable or not, doesn't matter;
   *   we could reduce the amount of generated code by skipping the frame when
   *   that label is not nullable, though (TODO));
   * - the u16 of the label. *)
  let sum_opn _mns lbl () mn0 path p_stk =
    E.with_sploded_pair "sum_opn1" p_stk (fun p stk ->
      (* Set my own nulbit if needed: *)
      let stk = may_set_nullbit true mn0 path stk in
      (* Prepare the new frame: *)
      let new_frame = make_pair p (size 0) in
      let stk = cons new_frame stk in
      (* And zero that nullmask: *)
      let p = write_u16 LittleEndian p (u16_of_int 0) in
      (* Then the label: *)
      let p = write_u16 LittleEndian p lbl in
      let p = align_const p 4 in
      make_pair p stk)

  let sum_cls _lbl () _ _ p_stk =
    leave_frame p_stk

  (* For vectors/lists, children know that if the item is not nullable then
   * there can possibly be no nullmask, so in that case, unlike that of
   * tuple/record, [nullmask_bits = 0] means no nullmask. *)
  let vec_opn dim mn () mn0 path p_stk =
    let has_nullmask, nullmask_bits = NullMaskWidth.vec_bits dim mn in
    enter_frame_const ~has_nullmask nullmask_bits mn0 path p_stk

  let vec_cls () _ _ p_stk =
    leave_frame p_stk

  let vec_sep () _ _ p_stk = p_stk

  (* [n] is an u32 *)
  let arr_opn mn n () mn0 path p_stk =
    let n = match n with
      | Some n -> n
      | None -> failwith "RamenRingBuffer.Ser needs list size upfront" in
    let has_nullmask, nullmask_bits = NullMaskWidth.lst_bits mn n in
    let p_stk =
      E.with_sploded_pair "with_data_ptr" p_stk (fun p stk ->
        let p = write_u32 LittleEndian p n in
        make_pair p stk) in
    (* Nullmask must still be present for an empty list of nullable items: *)
    enter_frame_dyn ~has_nullmask nullmask_bits mn0 path p_stk

  let arr_cls () _ _ p_stk =
    leave_frame p_stk

  let arr_sep () _ _ p_stk = p_stk

  let nullable () _ _ p_stk = p_stk

  (* The nullmask has been zeroed already: *)
  let snull _t () mn0 path p_stk =
    may_skip_nullbit mn0 path p_stk

  (* nullbits are set when actual values are written: *)
  let snotnull _t () _ _ p_stk = p_stk

  type ssizer = T.mn -> Path.t -> E.t -> E.t

  (* SerSize of the whole string: *)
  let ssize_of_string _mn0 _path id =
    let sz = size_of_u32 (string_length id) in
    let headsz = size word_size in
    add headsz (round_up_dyn_bytes sz)

  (* SerSize of the list header: *)
  let ssize_of_arr mn0 path id =
    let with_nullmask () =
      let nullmask_bits_dyn = cardinality id in
      (* Add the nullmask length prefix: *)
      let nullmask_sz_bits = add nullmask_bits_dyn (u32_of_int 8) in
      (* Round up to ringbuf words: *)
      let nullmask_bytes = round_up_dyn_bits nullmask_sz_bits in
      add (size word_size) (* list length *)
          nullmask_bytes
    and without_nullmask () =
      size word_size in
    match (Path.type_of_path mn0 path).typ |> T.develop with
    | TArr mn ->
        (* If the items are not nullable then there is no nullmask. *)
        if mn.nullable then
          with_nullmask ()
        else
          without_nullmask ()
    | t ->
        Printf.eprintf "ERROR: List of type %a!?\n%!"
          T.print t ;
        assert false

  let ssize_of_float _mn0 _path _ =
    size (round_up_const_bytes 8)

  let ssize_of_bool _mn0 _path _ =
    size (round_up_const_bytes 1)

  let ssize_of_i8 _mn0 _path _ =
    size (round_up_const_bytes 1)

  let ssize_of_i16 _mn0 _path _ =
    size (round_up_const_bytes 2)

  let ssize_of_i24 _mn0 _path _ =
    size (round_up_const_bytes 3)

  let ssize_of_i32 _mn0 _path _ =
    size (round_up_const_bytes 4)

  let ssize_of_i40 _mn0 _path _ =
    size (round_up_const_bytes 5)

  let ssize_of_i48 _mn0 _path _ =
    size (round_up_const_bytes 6)

  let ssize_of_i56 _mn0 _path _ =
    size (round_up_const_bytes 7)

  let ssize_of_i64 _mn0 _path _ =
    size (round_up_const_bytes 8)

  let ssize_of_i128 _mn0 _path _ =
    size (round_up_const_bytes 16)

  let ssize_of_u8 _mn0 _path _ =
    size (round_up_const_bytes 1)

  let ssize_of_u16 _mn0 _path _ =
    size (round_up_const_bytes 2)

  let ssize_of_u24 _mn0 _path _ =
    size (round_up_const_bytes 3)

  let ssize_of_u32 _mn0 _path _ =
    size (round_up_const_bytes 4)

  let ssize_of_u40 _mn0 _path _ =
    size (round_up_const_bytes 5)

  let ssize_of_u48 _mn0 _path _ =
    size (round_up_const_bytes 6)

  let ssize_of_u56 _mn0 _path _ =
    size (round_up_const_bytes 7)

  let ssize_of_u64 _mn0 _path _ =
    size (round_up_const_bytes 8)

  let ssize_of_u128 _mn0 _path _ =
    size (round_up_const_bytes 16)

  let ssize_of_char _mn0 _path _ =
    size (round_up_const_bits 1)

  let ssize_of_tup mn0 path _ =
    (* Just the additional bitmask: *)
    let nullmask_words =
      NullMaskWidth.words_of_type (Path.type_of_path mn0 path).typ in
    size (nullmask_words * word_size)

  let ssize_of_rec mn0 path _ =
    (* Just the additional bitmask: *)
    let nullmask_words =
      NullMaskWidth.words_of_type (Path.type_of_path mn0 path).typ in
    size (nullmask_words * word_size)

  (* Just the additional label: *)
  let ssize_of_sum _ _ _ =
    size word_size

  let ssize_of_vec mn0 path _ =
    let nullmask_words =
      NullMaskWidth.words_of_type (Path.type_of_path mn0 path).typ in
    size (nullmask_words * word_size)

  let ssize_of_null _mn0 _path = size 0

  let ssize_start ?(config=()) _ =
    ignore config ;
    size 0
end

module Des : DES with type config = unit =
struct
  let id = RingBuff

  type config = unit
  type state = unit

  (* To deserialize we need the same kind of pointer than to serialize: *)
  let ptr = Ser.ptr

  (* Enter a new compound type by recording its location in the stack
   * and jumping over it, after having incremented the current nullbit
   * index.
   * The nullbit index of the new stack is set to 8 after the nullmask
   * length prefix (so sum deserializer cannot use this function) *)
  let enter_frame ~has_nullmask mn0 path p stk =
    let stk = may_set_nullbit false mn0 path stk in
    (* The following [8] is the size of the length prefix. Notice we could
     * always use 8 since bit offset should not be used when not nullable. *)
    let new_frame = make_pair p (size (if has_nullmask then 8 else 0)) in
    seq [ debug (string "des: enter a new frame at ") ;
          debug (string_of_int_ (offset p)) ;
          debug (string "\n") ;
          let p =
            if has_nullmask then
              let words = read_u8 p |> first in
              let bytes = left_shift (to_u32 words)
                                     (u8_of_int (log2 word_size)) in
              ptr_add p (size_of_u32 bytes)
            else p
          and stk = cons new_frame stk in
          make_pair p stk ]

  let start ?(config=()) _mn p =
    config,
    make_pair p (end_of_list t_frame)

  let stop () p_stk =
    (* TODO: assert tail stk = end_of_list *)
    first p_stk

  type des = state -> T.mn -> Path.t -> E.t -> E.t

  (* When we deserialize any value, we may have to increment the nullbit
   * pointer depending on the current type of position in the global type
   * [mn0]: *)
  let with_nullbit_done mn0 path p_stk f =
    E.with_sploded_pair "with_nullbit_done2" p_stk (fun p stk ->
      let stk = may_set_nullbit false mn0 path stk in
      E.with_sploded_pair "with_nullbit_done3" (f p) (fun v p ->
        make_pair v (make_pair p stk)))

  let dfloat () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      seq [ debug (string "desser a float from ") ;
            debug (string_of_int_ (offset p)) ;
            debug (char '\n') ;
            E.with_sploded_pair "dfloat" (read_u64 LittleEndian p) (fun w p ->
              make_pair (float_of_u64 w) p) ])

  let dstring () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      seq [ debug (string "deser a string from ") ;
            debug (string_of_int_ (offset p)) ;
            debug (char '\n') ;
            E.with_sploded_pair "dstring1" (read_u32 LittleEndian p) (fun len p ->
              let len = size_of_u32 len in
              E.with_sploded_pair "dstring2" (read_bytes p len) (fun bs p ->
                make_pair (string_of_bytes bs) (align_dyn p len))) ])

  let dbool () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "dbool" (read_u8 p) (fun b p ->
        make_pair (bool_of_u8 b) (align_const p 1)))

  let dchar () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "dchar" (read_u8 p) (fun b p ->
        make_pair (char_of_u8 b) (align_const p 1)))

  let du8 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du8" (read_u8 p) (fun b p ->
        make_pair b (align_const p 1)))

  let du16 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du16" (read_u16 LittleEndian p) (fun w p ->
        make_pair w (align_const p 2)))

  let du24 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du24" (read_u32 LittleEndian p) (fun w p ->
        make_pair (to_u24 w) (align_const p 4)))

  let du32 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du32" (read_u32 LittleEndian p) (fun w p ->
        make_pair w (align_const p 4)))

  let du40 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du40" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_u40 w) (align_const p 8)))

  let du48 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du48" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_u48 w) (align_const p 8)))

  let du56 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du56" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_u56 w) (align_const p 8)))

  let du64 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du64" (read_u64 LittleEndian p) (fun w p ->
        make_pair w (align_const p 8)))

  let du128 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "du128" (read_u128 LittleEndian p) (fun w p ->
        make_pair w (align_const p 8)))

  let di8 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di8" (read_u8 p) (fun b p ->
        make_pair (to_i8 b) (align_const p 1)))

  let di16 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di16" (read_u16 LittleEndian p) (fun w p ->
        make_pair (to_i16 w) (align_const p 2)))

  let di24 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di24" (read_u32 LittleEndian p) (fun w p ->
        make_pair (to_i24 w) (align_const p 4)))

  let di32 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di32" (read_u32 LittleEndian p) (fun w p ->
        make_pair (to_i32 w) (align_const p 4)))

  let di40 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di40" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_i40 w) (align_const p 8)))

  let di48 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di48" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_i48 w) (align_const p 8)))

  let di56 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di56" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_i56 w) (align_const p 8)))

  let di64 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di64" (read_u64 LittleEndian p) (fun w p ->
        make_pair (to_i64 w) (align_const p 8)))

  let di128 () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk (fun p ->
      E.with_sploded_pair "di128" (read_u128 LittleEndian p) (fun w p ->
        make_pair (to_i128 w) (align_const p 8)))

  let dext f () mn0 path p_stk =
    with_nullbit_done mn0 path p_stk f

  let tup_rec_opn _mns mn0 path p_stk =
    E.with_sploded_pair
      "tup_rec_opn" p_stk (enter_frame ~has_nullmask:true mn0 path)

  let tup_opn mns () mn0 path p_stk =
    tup_rec_opn mns mn0 path p_stk

  let tup_cls () _ _ p_stk =
    leave_frame p_stk

  let tup_sep () _ _ p_stk = p_stk

  let rec_opn mns () mn0 path p_stk =
    let mns = tuple_typs_of_record mns in
    tup_rec_opn mns mn0 path p_stk

  let rec_cls () _ _ p_stk =
    leave_frame p_stk

  let rec_sep () _ _ p_stk = p_stk

  let vec_opn _dim mn () mn0 path p_stk =
    let has_nullmask = mn.T.nullable in
    E.with_sploded_pair
      "vec_opn" p_stk (enter_frame ~has_nullmask mn0 path)

  let vec_cls () _ _ p_stk =
    leave_frame p_stk

  let vec_sep () _ _ p_stk = p_stk

  (* Sums are encoded with a leading word for the nullmask followed by
   * the label as a u16: *)
  let sum_opn _mns () mn0 path p_stk =
    E.with_sploded_pair "sum_opn2" p_stk (fun p stk ->
      (* Skip my own nullbit if needed: *)
      let stk = may_set_nullbit false mn0 path stk in
      (* Prepare the new frame: *)
      let new_frame = make_pair p (size 0) in
      let stk = cons new_frame stk in
      (* Skip that nullmask: *)
      let p = ptr_add p (size 2) in
      (* Read the label: *)
      let w_p = read_u16 LittleEndian p in
      E.with_sploded_pair "sum_opn3" w_p (fun w p ->
        let lbl = w in
        let p = align_const p 4 in
        make_pair lbl (make_pair p stk)))

  let sum_cls _lbl () _ _ p_stk =
    leave_frame p_stk

  let arr_opn () = KnownSize
    (fun mn mn0 path p_stk ->
      E.with_sploded_pair "list_opn1" p_stk (fun p stk ->
        E.with_sploded_pair "list_opn2" (read_u32 LittleEndian p) (fun n p ->
          let has_nullmask = mn.T.nullable in
          let p_stk = enter_frame ~has_nullmask mn0 path p stk in
          make_pair n p_stk)))

  let arr_cls () _ _ p_stk =
    leave_frame p_stk

  let arr_sep () _ _ p_stk = p_stk

  (* Called only on nullable value, so there necessarily is a nullbit: *)
  let is_null () _ path p_stk =
    assert (path <> []) ;
    (* TODO: assert stk <> end_of_list *)
    (* Do not advance the nullbit index as it's already done on a per
     * value basis: *)
    E.with_sploded_pair "is_null2" (head (secnd p_stk)) (fun p bi ->
      let_ (not_ (get_bit p bi)) (fun b ->
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
