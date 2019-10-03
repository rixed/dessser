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
 * - One reading/writing directly (ie stage 0) into a Bytes.t;
 * - One reading/writing directly into a file;
 * - One that return the code to read/write into a stage 1 Bytes.t buffer. *)
module type SERDATA =
sig
  type pointer
  type size
  type nop
  val nop : nop
  val and_then : nop -> nop -> nop
  val skip : pointer -> size -> nop
  val sub : pointer -> pointer -> size
  (*val print_pointer : 'a BatIO.output -> pointer -> unit
  val print_data : 'a BatIO.output -> pointer -> size -> unit*)

  type bit
  type byte
  type word
  type dword
  type qword
  type bytes
  val test_bit : pointer -> int -> bit
  (* read functions also advance pointer! *)
  val read_byte : pointer -> byte
  val read_word : ?be:bool -> pointer -> word
  val read_dword : ?be:bool -> pointer -> dword
  val read_qword : ?be:bool -> pointer -> qword
  val read_bytes : pointer -> size -> bytes
  val set_bit : pointer -> int -> bit -> nop
  (* write functions also advance pointer! *)
  val write_byte : pointer -> byte -> nop
  val write_word : ?be:bool -> pointer -> word -> nop
  val write_dword : ?be:bool -> pointer -> dword -> nop
  val write_qword : ?be:bool -> pointer -> qword -> nop
  val write_bytes : pointer -> bytes -> nop
  (* Those two do not move the pointer: *)
  val peek_byte : pointer -> byte
  val poke_byte : pointer -> byte -> nop

  val size_of_const : int -> size
  val make_buffer : size -> pointer
  val print : pointer -> nop
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
  | TTuple of typ array

module type INTREPR_TYPES =
sig
  module SerData : SERDATA

  type value =
    | VPointer of SerData.pointer
    | VSize of SerData.size
    | VBool of boolv
    | VI8 of i8v
    | VI16 of i16v
    | VVec of vecv
    | VTuple of tupv

  and boolv
  and i8v
  and i16v
  and vecv
  and tupv
end

module type INTREPR =
sig
  include INTREPR_TYPES

  val value_of_pointer : SerData.pointer -> value
  val pointer_of_value : value -> SerData.pointer
  val value_of_size : SerData.size -> value
  val size_of_value : value -> SerData.size
  val value_of_boolv : boolv -> value
  val boolv_of_value : value -> boolv
  val value_of_i8v : i8v -> value
  val i8v_of_value : value -> i8v
  val value_of_i16v : i16v -> value
  val i16v_of_value : value -> i16v
  val value_of_vecv : vecv -> value
  val vecv_of_value : value -> vecv
  val value_of_tupv : tupv -> value
  val tupv_of_value : value -> tupv
  (* To be continued... *)

  val byte_of_i8v : i8v -> SerData.byte
  val i8v_of_byte : SerData.byte -> i8v
  val word_of_i16v : i16v -> SerData.word
  val i16v_of_word : SerData.word -> i16v

  val choose : boolv -> value -> value -> value
  val vec_get : vecv -> int -> value
  (* Lengths are always known at compile time: *)
  val vec_length : vecv -> int
  val tup_length : tupv -> int
  val tup_get : tupv -> int -> value

  val vecv_of_const : value list -> vecv
  val tupv_of_const : value list -> tupv
  val boolv_of_const : bool -> boolv
  val boolv_and : boolv -> boolv -> boolv
  val i8v_of_const : int -> i8v
  val i8v_eq : i8v -> i8v -> boolv
  val i8v_ge : i8v -> i8v -> boolv
  val i8v_add : i8v -> i8v -> i8v
  val i8v_sub : i8v -> i8v -> i8v
  val i8v_mul : i8v -> i8v -> i8v
  val i8v_mod : i8v -> i8v -> i8v
  val i8v_div : i8v -> i8v -> i8v
  val i16v_of_const : int -> i16v
  val i16v_gt : i16v -> i16v -> boolv

  (* Peek next byte and if cond is true then accumulate it into the value *)
  val read_while :
    SerData.pointer ->
    (SerData.byte -> boolv) ->
    (value -> SerData.byte -> value) ->
    value ->
      value
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
  let value_of_tupv c = B.VTuple c
  let tupv_of_value = function B.VTuple c -> c | _ -> assert false
end

(* Implementations must (de)serialize using only the functions above.
 * Possible implementations include:
 * - One for some user-friendly s-expression format;
 * - One for some user-friendly CSV format;
 * - One for some more efficient yet still simple binary encoding. *)
module type SERDES =
sig
  module IntRepr : INTREPR

  val ser : IntRepr.SerData.pointer -> IntRepr.value -> IntRepr.SerData.nop
  val des : typ -> IntRepr.SerData.pointer -> IntRepr.value
end
