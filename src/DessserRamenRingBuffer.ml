open Batteries
open Stdint

open Dessser
open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module Path = DessserPath
open E.Ops

let debug_flag = false

(* This is the encoding used to pass values in between Ramen workers.
 * Notice that there is a single bitmask for missing values (0=value is
 * present, 1=value is default).  Therefore, explicit default values of
 * nullable types must not be non-null, otherwise the logic implemented in
 * Dessser.ml will not work (it assumes that any absent value is the declared
 * default when deserializing and will also skip any default value when
 * encoding).
 * Size of this bitmask is always known at compile time but for variable
 * length TArr (TSet and TList are converted into TArr beforehand).
 * In those cases, a word prefixes the bitmask that gives the number of
 * items. *)

let is_serializable0 = function
  | T.{ nullable = true ; default = (None | Some (E0 (Null _))) ; _ } -> true
  | T.{ nullable = true ; _ } -> false
  | _ -> true

let is_serializable mn =
  try
    T.iter_mn (fun mn ->
      if not (is_serializable0 mn) then raise Exit
    ) mn ;
    true
  with Exit ->
    false

(* Take a maybe-nullable and make it serializable by nullifying its explicit
 * defaults for nullable fields: *)
let rec make_serializable mn =
  let mn =
    if not (is_serializable0 mn) then { mn with default = None } else mn in
  match mn.T.typ with
  | TThis _ ->
      todo "make_serializable for This"
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 ->
      mn
  | TUsr { def ; _ } ->
      let mn' = T.{ mn with typ = def } in
      make_serializable mn'
  | TVec (d, mn') ->
      { mn with typ = TVec (d, make_serializable mn') }
  | TArr mn' ->
      { mn with typ = TArr (make_serializable mn') }
  | TSet (st, mn') ->
      { mn with typ = TSet (st, make_serializable mn') }
  | TTup mns ->
      let mns = Array.map make_serializable mns in
      { mn with typ = TTup mns }
  | TRec mns ->
      let mns = Array.map (fun (n, mn) -> n, make_serializable mn) mns in
      { mn with typ = TRec mns }
  | TSum mns ->
      let mns = Array.map (fun (n, mn) -> n, make_serializable mn) mns in
      { mn with typ = TSum mns }
  | _ ->
      invalid_arg "make_serializable"

let make_serializable =
  if debug_flag then
    fun mn ->
      let mn = make_serializable mn in
      assert (is_serializable mn) ;
      mn
  else
    make_serializable

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

(* We use a stack of "frames" of pointer to bitmask + fieldbit index, where
 * the bitmask points at a bitmask with one bit per field, set if the field
 * value is actually present (non default).
 * Every bitmask starts with a byte giving ther length in words of this
 * bitmask (although it is only really necessary for arrays).
 * Our "data pointer" is therefore actually composed of the data pointer
 * itself (p) and a stack (stk): *)
let t_frame = T.(pair ptr size)

let leave_frame p_stk =
  E.with_sploded_pair "leave_frame" p_stk (fun p stk ->
    make_pair p (force ~what:"RingBuf.leave_frame" (tail stk)))

(* Set the next bit in the bitmask and return the new stack with increased
 * fieldbit position: *)
let set_fieldbit stk =
  let p_bi = force ~what:"RingBuf.set_fieldbit" (head stk) in
  E.with_sploded_pair "set_fieldbit" p_bi (fun p bi ->
    seq [ debug (string "set fieldbit ") ;
          debug (string_of_int_ bi) ;
          debug (string "@") ;
          debug (string_of_int_ (offset p)) ;
          debug (char '\n') ;
          set_bit p bi (bit true) ;
          let frame = make_pair p (add bi (size 1)) in
          cons frame (force ~what:"RingBuf.set_fieldbit2" (tail stk)) ])

(* Skip a bit in the bitmask, ie just increment the fieldbit (since the
 * bitmask is initialized to zero), and return as above the stack with
 * that new fieldbit. *)
let skip_fieldbit stk =
  let p_bi = force ~what:"RingBuf.skip_fieldbit" (head stk) in
  E.with_sploded_pair "skip_fieldbit" p_bi (fun p bi ->
    seq [ debug (string "skip fieldbit at ") ;
          debug (string_of_int_ bi) ;
          debug (char '\n') ;
          let frame = make_pair p (add bi (size 1)) in
          cons frame (force ~what:"RingBuf.skip_fieldbit" (tail stk)) ])

let skip_fieldbit_from_frame p_stk =
  E.with_sploded_pair "skip_fieldbit_from_frame" p_stk (fun p stk ->
    let stk = skip_fieldbit stk in
    make_pair p stk)

module BitMaskWidth =
struct
  let tup_bits mns =
    Array.length mns

  let rec_bits mns =
    Array.length mns

  let vec_bits dim =
    dim

  let lst_bits n =
    n

  let rec of_type = function
    | T.TVec (dim, _) ->
        vec_bits dim
    | TArr _ ->
        (* Arrays bitmask is dynamic and prefixed with a length *)
        invalid_arg "BitMaskWidth.of_type for lists"
    | TTup mns ->
        tup_bits mns
    | TRec mns ->
        rec_bits mns
    | TSum _ ->
        1
    | TUsr { def ; _ } ->
        of_type def
    | _ ->
        0

  (* Return the number of bytes required to encode the bitmask *)
  let bytes_of_type typ =
    let bits = of_type typ in
    round_up_const_bits bits

  (* Return the number of words required to encode the bitmask *)
  let words_of_type =
    words_of_const_bytes % bytes_of_type
end

module Ser : SER with type config = unit =
struct
  let id = RingBuff

  type config = unit
  type state = unit

  (* Few helper functions: *)

  (* Zero a bitmask which width is known at compile time, and advance the
   * pointer. *)
  let zero_bitmask_const bits p =
    let sz = (bits + 7) / 8 in
    let p = blit_byte p (u8_of_int 0) (size sz) in
    align_const p sz

  (* Zero the bitmask known only at runtime (which size in words a given
   * in the first byte of the mask) and advance the pointer *)
  let zero_bitmask_dyn bits p =
    let_ ~name:"sz_" (right_shift (add (u32_of_int 7) bits) (u8_of_int 3))
      (fun sz ->
        let p = blit_byte p (u8_of_int 0) (size_of_u32 sz) in
        align_dyn p (size_of_u32 sz))

  (* Enter a new compound type by zeroing a bitmask of the given [width]
   * and setting up a new frame for it, and increment the bitfield. *)
  let enter_frame to_expr zero_bitmask width p_stk =
    E.with_sploded_pair "enter_frame" p_stk (fun p stk ->
      let stk = skip_fieldbit stk in
      let new_frame = make_pair p (size 0) in
      seq [ debug (string "ser: enter a new frame at ") ;
            debug (string_of_int_ (offset p)) ;
            debug (string " with ") ;
            debug (string_of_int_ (to_expr width)) ;
            debug (string " fieldbits\n") ;
            make_pair
              (zero_bitmask width p)
              (cons new_frame stk) ])

  (* Enter a new compound type by zeroing a bitmask and setting up a new
   * frame for it: *)
  let enter_frame_dyn = enter_frame identity zero_bitmask_dyn
  let enter_frame_const = enter_frame u8_of_int zero_bitmask_const

  let with_fieldbit_done p_stk f =
    E.with_sploded_pair "with_fieldbit_done" p_stk (fun p stk ->
      let stk = skip_fieldbit stk in
      let_ ~name:"with_fieldbit_done_p" p (fun p ->
        let p = f p in
        make_pair p stk))

  let make_state ?(config=()) mn0 =
    if not (is_serializable mn0) then invalid_arg "not serializable" ;
    config

  let start _conf p =
    let_ ~name:"start_p" p (fun p ->
      let new_frame = make_pair p (size 0) in
      let stk = cons new_frame (end_of_list t_frame) in
      let p = zero_bitmask_const 1 p in
      make_pair p stk)

  let stop () p_stk =
    (* TODO: assert tail stk = end_of_list *)
    first p_stk

  type ser = state -> T.mn -> Path.t -> E.t -> E.t -> E.t

  let with_debug p what write =
    seq [ debug (string ("ser a "^ what ^" at ")) ;
          debug (string_of_int_ (offset p)) ;
          debug (char '\n') ;
          write ]

  let sfloat () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      with_debug p "float"
        (write_u64 LittleEndian p (u64_of_float v)))

  let sbytes () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let len = bytes_length v in
      let p =
        seq [ debug (string "ser bytes at ") ;
              debug (string_of_int_ (offset p)) ;
              debug (string " of length ") ;
              debug (string_of_int_ len) ;
              debug (char '\n') ;
              write_u32 LittleEndian p (u32_of_size len) ] in
      let p = write_bytes p v in
      align_dyn p len)

  let sstring () mn0 path v p_stk =
    let v = bytes_of_string v in
    sbytes () mn0 path v p_stk

  let sbool () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "bool" (write_u8 p (u8_of_bool v)) in
      align_const p 1)

  let schar () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "char" (write_u8 p (u8_of_char v)) in
      align_const p 1)

  let si8 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "i8" (write_u8 p (to_u8 v)) in
      align_const p 1)

  let si16 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "i16"
                (write_u16 LittleEndian p (to_u16 v)) in
      align_const p 2)

  let si32 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "i24/32"
                (write_u32 LittleEndian p (to_u32 v)) in
      align_const p 4)

  let si24 = si32

  let si64 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "i40/48/56/64"
                (write_u64 LittleEndian p (to_u64 v)) in
      align_const p 8)

  let si40 = si64

  let si48 = si64

  let si56 = si64

  let si128 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "i128"
                (write_u128 LittleEndian p (to_u128 v)) in
      align_const p 16)

  let su8 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "u8" (write_u8 p v) in
      align_const p 1)

  let su16 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "u16"
                (write_u16 LittleEndian p v) in
      align_const p 2)

  let su32 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "u24/32"
                (write_u32 LittleEndian p v) in
      align_const p 4)

  let su24 () vt0 path v p = su32 () vt0 path (to_u32 v) p

  let su64 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "u40/48/56/64"
                (write_u64 LittleEndian p v) in
      align_const p 8)

  let su40 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su48 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su56 () vt0 path v p = su64 () vt0 path (to_u64 v) p

  let su128 () _ _ v p_stk =
    with_fieldbit_done p_stk (fun p ->
      let p = with_debug p "u128"
                (write_u128 LittleEndian p v) in
      align_const p 16)

  let sext f () _ _ v p_stk =
    E.with_sploded_pair "sext" p_stk (fun p stk ->
      let stk = skip_fieldbit stk in
      let p_stk = make_pair p stk in
      f v p_stk)

  let tup_rec_opn mns _ _ p_stk =
    (* Allocate one bit per item. *)
    let bits = BitMaskWidth.tup_bits mns in
    enter_frame_const bits p_stk

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
   * - a 16bits bitmask, of which only bit 0 will ever be used
   * - the u16 of the label index. *)
  let sum_opn _mns lbl () _ _ p_stk =
    E.with_sploded_pair "sum_opn1" p_stk (fun p stk ->
      (* Increase bitfield: *)
      let stk = skip_fieldbit stk in
      (* Prepare the new frame: *)
      let new_frame = make_pair p (size 0) in
      let stk = cons new_frame stk in
      (* And zero that bitmask: *)
      let p = write_u16 LittleEndian p (u16_of_int 0) in
      (* Then the label index: *)
      let p = write_u16 LittleEndian p lbl in
      let p = align_const p 4 in
      make_pair p stk)

  let sum_cls _lbl () _ _ p_stk =
    leave_frame p_stk

  let vec_opn dim _mn () _ _ p_stk =
    let bits = BitMaskWidth.vec_bits dim in
    enter_frame_const bits p_stk

  let vec_cls () _ _ p_stk =
    leave_frame p_stk

  let vec_sep () _ _ p_stk = p_stk

  (* First prefix is the length (in items), then the bitmask.
   * [n] is an u32 *)
  let arr_opn _mn n () _ _ p_stk =
    let n = match n with
      | Some n -> n
      | None -> failwith "RamenRingBuffer.Ser needs list size upfront" in
    let p_stk =
      E.with_sploded_pair "with_data_ptr" p_stk (fun p stk ->
        let p = write_u32 LittleEndian p n in
        make_pair p stk) in
    let bits = BitMaskWidth.lst_bits n in
    enter_frame_dyn bits p_stk

  let arr_cls () _ _ p_stk =
    leave_frame p_stk

  let arr_sep () _ _ p_stk = p_stk

  let nullable () _ _ p_stk = p_stk

  (* That's the only default value that is set for now.
   * TODO: Dessser.ml should call an [sdefault] function in other cases: *)
  let snull _t () _ _ p_stk =
    E.with_sploded_pair "snull" p_stk (fun p stk ->
      let stk = set_fieldbit stk in
      make_pair p stk)

  (* The bitmask has been zeroed already; the fieldbit is increased when
   * actually serializing the value (for both nullable and non nullable): *)
  let snotnull _t () _ _ p_stk =
    p_stk

  (* SerSizes: Every value also has a bit for presence in some bitmask
   * somewhere. Every inner value is part of some compound type which
   * regroup these bits in a single bitmask. The outer value has its own
   * bitmask with a single bit in the beginning though. *)

  type ssizer = T.mn -> Path.t -> E.t -> E.t

  (* SerSize of the whole string: *)
  let ssize_of_string _ _ id =
    let sz = size_of_u32 (string_length id) in
    let headsz = size word_size in
    add headsz (round_up_dyn_bytes sz)

  let ssize_of_bytes _ _ id =
    let sz = bytes_length id in
    let headsz = size word_size in
    add headsz (round_up_dyn_bytes sz)

  (* SerSize of the list header: *)
  let ssize_of_arr _ _ id =
    let bitmask_bits_dyn = cardinality id in
    (* Round up to ringbuf words: *)
    let bitmask_bytes = round_up_dyn_bits bitmask_bits_dyn in
    add (size word_size) (* list length *)
        bitmask_bytes

  let ssize_of_float _ _ _ =
    size (round_up_const_bytes 8)

  let ssize_of_bool _ _ _ =
    size (round_up_const_bytes 1)

  let ssize_of_i8 _ _ _ =
    size (round_up_const_bytes 1)

  let ssize_of_i16 _ _ _ =
    size (round_up_const_bytes 2)

  let ssize_of_i24 _ _ _ =
    size (round_up_const_bytes 3)

  let ssize_of_i32 _ _ _ =
    size (round_up_const_bytes 4)

  let ssize_of_i40 _ _ _ =
    size (round_up_const_bytes 5)

  let ssize_of_i48 _ _ _ =
    size (round_up_const_bytes 6)

  let ssize_of_i56 _ _ _ =
    size (round_up_const_bytes 7)

  let ssize_of_i64 _ _ _ =
    size (round_up_const_bytes 8)

  let ssize_of_i128 _ _ _ =
    size (round_up_const_bytes 16)

  let ssize_of_u8 _ _ _ =
    size (round_up_const_bytes 1)

  let ssize_of_u16 _ _ _ =
    size (round_up_const_bytes 2)

  let ssize_of_u24 _ _ _ =
    size (round_up_const_bytes 3)

  let ssize_of_u32 _ _ _ =
    size (round_up_const_bytes 4)

  let ssize_of_u40 _ _ _ =
    size (round_up_const_bytes 5)

  let ssize_of_u48 _ _ _ =
    size (round_up_const_bytes 6)

  let ssize_of_u56 _ _ _ =
    size (round_up_const_bytes 7)

  let ssize_of_u64 _ _ _ =
    size (round_up_const_bytes 8)

  let ssize_of_u128 _ _ _ =
    size (round_up_const_bytes 16)

  let ssize_of_char _ _ _ =
    size (round_up_const_bits 1)

  let ssize_of_tup mn0 path  _ =
    (* Just the additional bitmask: *)
    let bitmask_words =
      BitMaskWidth.words_of_type (Path.type_of_path mn0 path).typ in
    size (bitmask_words * word_size)

  let ssize_of_rec mn0 path _ =
    (* Just the additional bitmask: *)
    let bitmask_words =
      BitMaskWidth.words_of_type (Path.type_of_path mn0 path).typ in
    size (bitmask_words * word_size)

  (* Just the additional label and "bitmask": *)
  let ssize_of_sum _ _ _ =
    size word_size

  let ssize_of_vec mn0 path _ =
    let bitmask_words =
      BitMaskWidth.words_of_type (Path.type_of_path mn0 path).typ in
    size (bitmask_words * word_size)

  let ssize_of_null _ _ = size 0

  let ssize_of_notnull _ _ = size 0

  let ssize_start ?(config=()) _ =
    ignore config ;
    size (round_up_const_bits 1)  (* The outer value "bitmask" *)
end

module Des : DES with type config = unit =
struct
  let id = RingBuff

  type config = unit
  type state = unit

  let make_state ?(config=()) mn0 =
    if not (is_serializable mn0) then invalid_arg "not serializable" ;
    config

  let start _conf p =
    let_ ~name:"start_p" p (fun p ->
      let new_frame = make_pair p (size 0) in
      let stk = cons new_frame (end_of_list t_frame) in
      (* Outer value always start with a one word "bitmask": *)
      let p = ptr_add p (size word_size) in
      make_pair p stk)

  let stop () p_stk =
    (* TODO: assert tail stk = end_of_list *)
    first p_stk

  type des = state -> T.mn -> Path.t -> E.t -> E.t

  (* Enter a new compound type by recording its location in the stack
   * and jumping over it. *)
  let enter_frame width p stk =
    let_ ~name:"enter_frame_p" p (fun p ->
      let new_frame = make_pair p (size 0) in
      let bytes = round_up_dyn_bits (to_u32 width) in
      seq [ debug (string "des: enter a new frame at ") ;
            debug (string_of_int_ (offset p)) ;
            debug (string " of size ") ;
            debug (string_of_int_ bytes) ;
            debug (string "\n") ;
            make_pair
              (ptr_add p bytes)
              (cons new_frame stk) ])

  let with_debug p_stk what read cont =
    E.with_sploded_pair "debug1" p_stk (fun p stk ->
      seq [ debug (string ("des a "^ what ^" at ")) ;
            debug (string_of_int_ (offset p)) ;
            debug (char '\n') ;
            E.with_sploded_pair ("d"^ what ^"0") (read p) (fun v p ->
              E.with_sploded_pair (what ^"1") (cont v p) (fun v p ->
                make_pair v (make_pair p stk))) ])

  let dfloat () _ _ p_stk =
    with_debug p_stk "float" (read_u64 LittleEndian) (fun w p ->
      make_pair (float_of_u64 w) p)

  let dbytes () _ _ p_stk =
    with_debug p_stk "bytes" (read_u32 LittleEndian) (fun len p ->
      let len = size_of_u32 len in
      E.with_sploded_pair "dbytes2" (read_bytes p len) (fun v p ->
        make_pair v (align_dyn p len)))

  let dstring () mn0 path p_stk =
    let_pair ~n1:"v" ~n2:"p" (dbytes () mn0 path p_stk) (fun v p ->
      make_pair (string_of_bytes v) p)

  let dbool () _ _ p_stk =
    with_debug p_stk "bool" read_u8 (fun b p ->
      make_pair (bool_of_u8 b) (align_const p 1))

  let dchar () _ _ p_stk =
    with_debug p_stk "char" read_u8 (fun b p ->
      make_pair (char_of_u8 b) (align_const p 1))

  let du8 () _ _ p_stk =
    with_debug p_stk "u8" read_u8 (fun b p ->
      make_pair b (align_const p 1))

  let du16 () _ _ p_stk =
    with_debug p_stk "u16" (read_u16 LittleEndian) (fun w p ->
      make_pair w (align_const p 2))

  let du24 () _ _ p_stk =
    with_debug p_stk "u24" (read_u32 LittleEndian) (fun w p ->
      make_pair (to_u24 w) (align_const p 4))

  let du32 () _ _ p_stk =
    with_debug p_stk "u32" (read_u32 LittleEndian) (fun w p ->
      make_pair w (align_const p 4))

  let du40 () _ _ p_stk =
    with_debug p_stk "u40" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_u40 w) (align_const p 8))

  let du48 () _ _ p_stk =
    with_debug p_stk "u48" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_u48 w) (align_const p 8))

  let du56 () _ _ p_stk =
    with_debug p_stk "u56" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_u56 w) (align_const p 8))

  let du64 () _ _ p_stk =
    with_debug p_stk "u64" (read_u64 LittleEndian) (fun w p ->
      make_pair w (align_const p 8))

  let du128 () _ _ p_stk =
    with_debug p_stk "u128" (read_u128 LittleEndian) (fun w p ->
      make_pair w (align_const p 8))

  let di8 () _ _ p_stk =
    with_debug p_stk "i8" read_u8 (fun b p ->
      make_pair (to_i8 b) (align_const p 1))

  let di16 () _ _ p_stk =
    with_debug p_stk "i16" (read_u16 LittleEndian) (fun w p ->
      make_pair (to_i16 w) (align_const p 2))

  let di24 () _ _ p_stk =
    with_debug p_stk "i24" (read_u32 LittleEndian) (fun w p ->
      make_pair (to_i24 w) (align_const p 4))

  let di32 () _ _ p_stk =
    with_debug p_stk "i32" (read_u32 LittleEndian) (fun w p ->
      make_pair (to_i32 w) (align_const p 4))

  let di40 () _ _ p_stk =
    with_debug p_stk "i40" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_i40 w) (align_const p 8))

  let di48 () _ _ p_stk =
    with_debug p_stk "i48" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_i48 w) (align_const p 8))

  let di56 () _ _ p_stk =
    with_debug p_stk "i56" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_i56 w) (align_const p 8))

  let di64 () _ _ p_stk =
    with_debug p_stk "i64" (read_u64 LittleEndian) (fun w p ->
      make_pair (to_i64 w) (align_const p 8))

  let di128 () _ _ p_stk =
    with_debug p_stk "i128" (read_u128 LittleEndian) (fun w p ->
      make_pair (to_i128 w) (align_const p 8))

  let dext f () _ _ p_stk =
    E.with_sploded_pair "dext" p_stk (fun p stk ->
      let_ ~name:"p_stk" (make_pair p stk) f)

  let tup_rec_opn mns _ _ p_stk =
    E.with_sploded_pair "tup_rec_opn" p_stk (enter_frame (size (Array.length mns)))

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

  let vec_opn dim _mn () _ _ p_stk =
    E.with_sploded_pair "vec_opn" p_stk (enter_frame (size dim))

  let vec_cls () _ _ p_stk =
    leave_frame p_stk

  let vec_sep () _ _ p_stk = p_stk

  (* Sums are encoded with a leading u16 for the bitmask followed by
   * the label as a u16: *)
  let sum_opn _mns () _ _ p_stk =
    E.with_sploded_pair "sum_opn2" p_stk (fun p stk ->
      (* Prepare the new frame: *)
      let new_frame = make_pair p (size 0) in
      let stk = cons new_frame stk in
      (* Skip that bitmask: *)
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
    (fun _mn _ _ p_stk ->
      E.with_sploded_pair "list_opn1" p_stk (fun p stk ->
        let n = read_u32 LittleEndian p in
        E.with_sploded_pair "list_opn2" n (fun n p ->
          let p_stk = enter_frame n p stk in
          make_pair n p_stk)))

  let arr_cls () _ _ p_stk =
    leave_frame p_stk

  let arr_sep () _ _ p_stk =
    p_stk

  let is_present () _ _ p_stk =
    (* TODO: assert stk <> end_of_list *)
    E.with_sploded_pair "is_present1" p_stk (fun p stk ->
      let fp_bi = force ~what:"Ringbuf.is_present" (head stk) in
      E.with_sploded_pair "is_present2" fp_bi (fun fp bi ->
        (* Increment bi in the stack: *)
        let frame = make_pair fp (add bi (size 1)) in
        let stk = cons frame (force ~what:"is_present3" (tail stk)) in
        let p_stk = make_pair p stk in
        let_ (not_ (get_bit fp bi)) (fun b ->
          seq [ debug (string ("des: get fieldbit ")) ;
                debug (string_of_int_ bi) ;
                debug (string "@") ;
                debug (string_of_int_ (offset fp)) ;
                debug (string " -> ") ;
                debug (string_of_int_ (u8_of_bool b)) ;
                debug (char '\n') ;
                make_pair b p_stk ])))

  let is_null () _ _ _ =
    false_ (* Always false because is_present is checked first *)

  let dnull _t () _ _ p_stk =
    p_stk

  let dnotnull _t () _ _ p_stk =
    p_stk
end

let () =
  let ptr = T.(pair ptr (required (lst t_frame))) in
  T.register_ptr_type RingBuff ptr ptr
