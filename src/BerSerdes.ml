(* BerSerde is a library that generates serializers and deserializers
 * between a choice of several external formats and several internal
 * representations. *)
(* Objectives:
 * 1. Be able to add serialization format easily
 * 2. Do not rely too much on large code values with OCaml code but rather
 *    try to combine smaller ones so that it's possible to lift them in C
 *    expressions instead of printing OCaml.
 * 3. Integrate nicely with Ramen types and type checker (ie: no need to ask
 *    to much to OCaml's typechecker since we are going to typecheck with z3)
 *)

(* Serialized data are read from / written to memory buffers accessed via this
 * interface.
 * You will find several implementations:
 * - One reading/writing into a Bytes.t;
 * - One reading/writing into a file;
 * - Other should be easy to add. *)
module type SERDATA =
sig
  type pointer
  type size
  val add : pointer code -> size code -> pointer code
  val sub : pointer code -> pointer code -> size code
  (*val print_pointer : 'a BatIO.output -> pointer -> unit
  val print_data : 'a BatIO.output -> pointer -> size -> unit*)

  type bit
  type byte
  type word
  type dword
  type qword
  type bytes
  val test_bit : pointer code -> int code -> bit code
  (* read functions: *)
  val read_byte : pointer code -> (byte * pointer) code
  val read_word : ?be:bool -> pointer code -> (word * pointer) code
  val read_dword : ?be:bool -> pointer code -> (dword * pointer) code
  val read_qword : ?be:bool -> pointer code -> (qword * pointer) code
  val read_bytes : pointer code -> size code -> (bytes * pointer) code
  val set_bit : pointer code -> int code -> bit code -> unit code
  (* write functions: *)
  val write_byte : pointer code -> byte code -> pointer code
  val write_word : ?be:bool -> pointer code -> word code -> pointer code
  val write_dword : ?be:bool -> pointer code -> dword code -> pointer code
  val write_qword : ?be:bool -> pointer code -> qword code -> pointer code
  val write_bytes : pointer code -> bytes code -> pointer code
  (* Those two do not move the pointer: *)
  val peek_byte : pointer code -> byte code
  val poke_byte : pointer code -> byte code -> unit code

  val size_of_const : int -> size code
  val make_buffer : size code -> pointer code
  val of_string : string code -> pointer code
end

(* Now when we generate code expressions we want to generate ocaml code (or
 * C code) without carrying along the types nor the value type tags. The
 * generator must either generate working code of fail in an assert false
 * (we do not rely on the type checker to avoid the asserts but on z3). *)

(* For instance, if we know that the 3rd item of a tuple is a i8 value,
 * then the generated code to write this 3rd item must be: `write x_3` not
 *   `write (match x_3 with VI8 v -> v)`.
 * How to do that?
 * if v is the value for the tuple, then we can do:
 *   (i8v_of_value v.(3))
 * to get the i8v (int code).
 * So the only thing that's missing to implement tuple_get et vec_get is the
 * vector and tuple *types*, that could be part of the value.
 *
 * Now what if the item we want is only known at runtime. For vectors that
 * must not be a problem as the type is still known at compile time. For
 * a tuple this is just not possible.
 *)

type typ =
  | TPointer
  | TSize
  | TBool
  | TI8
  | TI16
  | TVec of int * typ
  | TTup of typ array

module type INTREPR_TYPES =
sig
  module SerData : SERDATA

  type value =
    | VPointer of SerData.pointer code
    | VSize of SerData.size code
    | VBool of boolv code
    | VI8 of i8v code
    | VI16 of i16v code
    (* There is no such thing as a stage0 representation of the stage1 tuple
     * because it is impossible and useless. What stage0 needs is a way to
     * get the code to ser/deser i-th field. *)
    | VTup of value array
    | VVec of value array

  and boolv
  and i8v
  and i16v

  type value_pointer =
    | VPBool of (boolv * SerData.pointer) code
    | VPI8 of (i8v * SerData.pointer) code
    | VPI16 of (i16v * SerData.pointer) code
    (* When deserializing a tuple, we ought to retrieve the array of [value]s,
     * Note that we deser only to ser again (in another format), not to ever
     * read the actual values in stage0. *)
    | VPTup of value_pointer array
    | VPVec of value_pointer array
end

module type INTREPR =
sig
  include INTREPR_TYPES

  val value_of_pointer : SerData.pointer code -> value
  val pointer_of_value : value -> SerData.pointer code
  val value_of_size : SerData.size code -> value
  val size_of_value : value -> SerData.size code
  val value_of_boolv : boolv code -> value
  val boolv_of_value : value -> boolv code
  val value_of_i8v : i8v code -> value
  val i8v_of_value : value -> i8v code
  val value_of_i16v : i16v code -> value
  val i16v_of_value : value -> i16v code
  val value_of_vecv : value array -> value
  val vecv_of_value : value -> value array
  val value_of_tupv : value array -> value
  val tupv_of_value : value -> value array
  (* To be continued... *)

  val byte_of_i8v : i8v code -> SerData.byte code
  val i8v_of_byte : SerData.byte code -> i8v code
  val word_of_i16v : i16v code -> SerData.word code
  val i16v_of_word : SerData.word code -> i16v code

  val choose : boolv code -> 'a code -> 'a code -> 'a code
  val fst : ('a * 'b) code -> 'a code
  val snd : ('a * 'b) code -> 'b code
  val map_fst : ('a -> 'c) code -> ('a * 'b) code -> ('c * 'b) code

  val boolv_of_const : bool -> boolv code
  val boolv_and : boolv code -> boolv code -> boolv code
  val i8v_of_const : int -> i8v code
  val i8v_eq : i8v code -> i8v code -> boolv code
  val i8v_ne : i8v code -> i8v code -> boolv code
  val i8v_ge : i8v code -> i8v code -> boolv code
  val i8v_gt : i8v code -> i8v code -> boolv code
  val i8v_add : i8v code -> i8v code -> i8v code
  val i8v_sub : i8v code -> i8v code -> i8v code
  val i8v_mul : i8v code -> i8v code -> i8v code
  val i8v_mod : i8v code -> i8v code -> i8v code
  val i8v_div : i8v code -> i8v code -> i8v code
  val i16v_of_const : int -> i16v code
  val i16v_gt : i16v code -> i16v code -> boolv code

  (* Peek next byte and if cond is true then accumulate it into the value. *)
  val read_while :
    cond:(SerData.byte -> boolv) code ->
    reduce:('a -> SerData.byte -> 'a) code ->
    ('a * SerData.pointer) code ->
      ('a * SerData.pointer) code

  (* Process a value of type ['a] in a loop while a condition on the loop
   * counter (of type ['b]) is true. The condition is tested initially. *)
  val do_while :
    cond:('a -> 'b -> boolv) code ->
    loop:('a -> 'b -> 'a * 'b) code ->
    'b code -> 'a code -> 'a code
end

module MakeCasts (B : INTREPR_TYPES) =
struct
  let value_of_pointer c = B.VPointer c
  let pointer_of_value = function B.VPointer c -> c | _ -> assert false
  let value_of_size c = B.VSize c
  let size_of_value = function B.VSize c -> c | _ -> assert false
  let value_of_boolv c = B.VBool c
  let boolv_of_value = function B.VBool c -> c | _ -> assert false
  let value_of_i8v c = B.VI8 c
  let i8v_of_value = function B.VI8 c -> c | _ -> assert false
  let value_of_i16v c = B.VI16 c
  let i16v_of_value = function B.VI16 c -> c | _ -> assert false
  let value_of_vecv c = B.VVec c
  let vecv_of_value = function B.VVec c -> c | _ -> assert false
  let value_of_tupv c = B.VTup c
  let tupv_of_value = function B.VTup c -> c | _ -> assert false
end

(* Implementations must (de)serialize using only the functions above.
 * Possible implementations include:
 * - One for some user-friendly s-expression format;
 * - One for some user-friendly CSV format;
 * - One for some more efficient yet still simple binary encoding. *)
module type SERDES =
sig
  module IntRepr : INTREPR

  val ser : IntRepr.value_pointer -> IntRepr.SerData.pointer code

  (* The result pair will match [typ] of course, but OCaml's type checker
   * won't help with that. *)
  val des : typ -> IntRepr.SerData.pointer code -> IntRepr.value_pointer
end
