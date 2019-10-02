(* BerSerde is a library that generates serializers and deserializers
 * between a choice of several external formats and several internal
 * representations. *)
open Batteries

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
  val add : pointer -> size -> pointer
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
  val read_byte : pointer -> byte
  val read_word : ?be:bool -> pointer -> word
  val read_dword : ?be:bool -> pointer -> dword
  val read_qword : ?be:bool -> pointer -> qword
  val read_bytes : pointer -> size -> bytes
  type nop
  val set_bit : pointer -> int -> bit -> nop
  val write_byte : pointer -> byte -> pointer
  val write_word : ?be:bool -> pointer -> word -> pointer
  val write_dword : ?be:bool -> pointer -> dword -> pointer
  val write_qword : ?be:bool -> pointer -> qword -> pointer
  val write_bytes : pointer -> size -> bytes -> pointer
end

(* Any SERDATA can be turned into a staged SERDATA: *)

module SerDataStaged (M : SERDATA) :
  SERDATA with type pointer = M.pointer code
           and type size = M.size code
           and type bit = M.bit code
           and type byte = M.byte code
           and type word = M.word code
           and type dword = M.dword code
           and type qword = M.qword code
           and type bytes = M.bytes code
           and type nop = M.nop code =
struct
  type pointer = M.pointer code
  type size = M.size code

  let add pc sc =
    .< M.add .~pc .~sc >.
  let sub p1c p2c =
    .< M.sub .~p1c .~p2c >.

  type bit = M.bit code
  type byte = M.byte code
  type word = M.word code
  type dword = M.dword code
  type qword = M.qword code
  type bytes = M.bytes code
  type nop = M.nop code

  let test_bit pc o =
    .< M.test_bit .~pc o >.

  let read_byte pc =
    .< M.read_byte .~pc >.

  let read_word ?be pc =
    .< M.read_word ?be .~pc >.

  let read_dword ?be pc =
    .< M.read_dword ?be .~pc >.

  let read_qword ?be pc =
    .< M.read_qword ?be .~pc >.

  let read_bytes pc sc =
    .< M.read_bytes .~pc .~sc >.

  let set_bit pc o bc =
    .< M.set_bit .~pc o .~bc >.

  let write_byte pc vc =
    .< M.write_byte .~pc .~vc >.

  let write_word ?be pc vc =
    .< M.write_word ?be .~pc .~vc >.

  let write_dword ?be pc vc =
    .< M.write_dword ?be .~pc .~vc >.

  let write_qword ?be pc vc =
    .< M.write_qword ?be .~pc .~vc >.

  let write_bytes pc sc vc =
    .< M.write_bytes .~pc .~sc .~vc >.
end

(* Internal representation of values:
 * how to convert from/to serialized bytes and operations to manipulate
 * them: *)

type typ =
  | TBool
  | TI8
  | TI16
  | TVec of int * typ
  | TTuple of typ list

module type INTREPR_BASE =
sig
  module SerData : SERDATA

  type boolv
  type i8v
  type i16v
  (* length is its own type with the hope that in many cases it will be known
   * at stage0: *)
  type length
  type 'a arr

  val byte_of_i8v : i8v -> SerData.byte
  val i8v_of_byte : SerData.byte -> i8v
  val word_of_i16v : i16v -> SerData.word
  val i16v_of_word : SerData.word -> i16v

  val choose : boolv -> 'a -> 'a -> 'a
  val loop : length -> 'a -> (i16v -> 'a -> 'a) -> 'a
  val arr_len : 'a arr -> length
  val arr_get : 'a arr -> i16v -> 'a

  val boolv_of_const : bool -> boolv
  val i8v_of_const : int -> i8v
  val arr_of_const : 'a list -> 'a arr
  val i8v_add : i8v -> i8v -> i8v
  val i8v_sub : i8v -> i8v -> i8v
  val i8v_mul : i8v -> i8v -> i8v
  val i8v_mod : i8v -> i8v -> i8v
  val i8v_div : i8v -> i8v -> i8v
  val i16v_of_const : int -> i16v
  val i16v_gt : i16v -> i16v -> boolv
end

module type INTREPR =
sig
  include INTREPR_BASE

  type value =
    | VBool of boolv
    | VI8 of i8v
    | VI16 of i16v
    | VVec of value arr
    | VTuple of value list

  and tuplev = value list
end

module MakeIntRepr (B : INTREPR_BASE) : INTREPR with module SerData = B.SerData =
struct
  include B

  type value =
    | VBool of boolv
    | VI8 of i8v
    | VI16 of i16v
    | VVec of value arr
    | VTuple of value list

  and tuplev = value list
end

(* Implementations must (de)serialize using only the functions above.
 * Possible implementations include:
 * - One for some user-friendly s-expression format;
 * - One for some user-friendly CSV format;
 * - One for some more efficient yet still simple binary encoding. *)
module type SERDES =
sig
  module IntRepr : INTREPR

  val ser : IntRepr.SerData.pointer -> IntRepr.value -> IntRepr.SerData.pointer
(*  val des : SerData.pointer -> IntRepr.value * SerData.pointer*)
end
