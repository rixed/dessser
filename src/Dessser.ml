open Batteries
open Stdint

module Types =
struct
  type t = { nullable : bool ; structure : structure }

  and structure =
    | TFloat
    | TString
    | TBool
    | TChar (* Exact same values as U8 but different typing rules *)
    | TU8 | TU16 | TU32 | TU64 | TU128
    | TI8 | TI16 | TI32 | TI64 | TI128
    | TVec of int * t
    | TList of t
    | TTup of t array
    (* Exact same as a tuple, but with field names that can be used as
     * accessors (also used to name actual fields in generated code): *)
    | TRec of (string * t) array
    (* Special purpose for serialization: *)
    | TPointer
    | TSize
    (* Data access, may be just pointer to the actual serialized object: *)
    | TBit | TByte | TWord | TDWord | TQWord | TOWord | TBytes
    (* Used only at the meta-level. Probably won't work for anything but
     * non-null scalars: *)
    | TPair of t * t

  let make ?(nullable=false) structure = { nullable ; structure }

  let rec print_structure oc = function
    | TFloat -> String.print oc "Float"
    | TString -> String.print oc "String"
    | TBool -> String.print oc "Bool"
    | TChar -> String.print oc "Char"
    | TU8 -> String.print oc "U8"
    | TU16 -> String.print oc "U16"
    | TU32 -> String.print oc "U32"
    | TU64 -> String.print oc "U64"
    | TU128 -> String.print oc "U128"
    | TI8 -> String.print oc "I8"
    | TI16 -> String.print oc "I16"
    | TI32 -> String.print oc "I32"
    | TI64 -> String.print oc "I64"
    | TI128 -> String.print oc "I128"
    | TVec (dim, typ) ->
        Printf.fprintf oc "%a[%d]" print typ dim
    | TList typ ->
        Printf.fprintf oc "%a[]" print typ
    | TTup typs ->
        Printf.fprintf oc "%a"
          (Array.print ~first:"(" ~last:")" ~sep:";" print) typs
    | TRec typs ->
        Printf.fprintf oc "%a"
          (Array.print ~first:"{" ~last:"}" ~sep:";"
            (fun oc (n, t) ->
              Printf.fprintf oc "%s: %a" n print t)
          ) typs
    | TPointer -> String.print oc "Pointer"
    | TSize -> String.print oc "Size"
    | TBit -> String.print oc "Bit"
    | TByte -> String.print oc "Byte"
    | TWord -> String.print oc "Word"
    | TDWord -> String.print oc "DWord"
    | TQWord -> String.print oc "QWord"
    | TOWord -> String.print oc "OWord"
    | TBytes -> String.print oc "Bytes"
    | TPair (t1, t2) ->
        Printf.fprintf oc "Pair(%a, %a)"
          print t1
          print t2

  and print oc t =
    Printf.fprintf oc "%a%s"
      print_structure t.structure
      (if t.nullable then "?" else "")

  (* Many return values have the type of a pair or src*dst pointers: *)
  let pair_ptrs = make (TPair (make TPointer, make TPointer))
  let bool = make TBool
  let u32 = make TU32
  let i32 = make TI32
end

(* Every expression is bound to an identifier, and only identifiers are
 * passed around, thus limiting module recursion and linearising the
 * generated code (FWIW). *)
module Identifier :
  sig
    type +'a t = private string
    val print : 'a IO.output -> 'b t -> unit

    val float : unit -> [`Float] t
    val string : unit -> [`String] t
    val bool : unit -> [`Bool] t
    val char : unit -> [`Char] t
    val i8 : unit -> [`I8] t
    val u8 : unit -> [`U8] t
    val i16 : unit -> [`I16] t
    val u16 : unit -> [`U16] t
    val u32 : unit -> [`U32] t
    val i32 : unit -> [`I32] t
    val u64 : unit -> [`U64] t
    val i64 : unit -> [`I64] t
    val u128 : unit -> [`U128] t
    val i128 : unit -> [`I128] t
    val tuple : unit -> [`Tup] t
    val record : unit -> [`Rec] t
    val vector : unit -> [`Vec] t
    val list : unit -> [`List] t
    val pointer : unit -> [`Pointer] t
    val size : unit -> [`Size] t
    val bit : unit -> [`Bit] t
    val byte : unit -> [`Byte] t
    val word : unit -> [`Word] t
    val dword : unit -> [`DWord] t
    val qword : unit -> [`QWord] t
    val oword : unit -> [`OWord] t
    val bytes : unit -> [`Bytes] t
    val pair : unit -> ([`Pair] * 'a * 'b) t
    val auto : unit -> 'a t
    (* FunctionX * param1 * param2 * ... * returned type *)
    val func0 : unit -> ([`Function0] * 'a) t
    val func1 : 'a t -> 'b t -> ([`Function1] * 'a * 'b) t
    val func2 : unit -> ([`Function2] * 'a * 'b * 'c) t
    val func3 : unit -> ([`Function3] * 'a * 'b * 'c * 'd) t
    val param : int -> unit -> 'a t

    val any : Types.structure -> 'a t

    val to_float : 'a t -> [`Float] t
    val to_string : 'a t -> [`String] t
    val to_bool : 'a t -> [`Bool] t
    val to_char : 'a t -> [`Char] t
    val to_u8 : 'a t -> [`U8] t
    val to_u16 : 'a t -> [`U16] t
    val to_u32 : 'a t -> [`U32] t
    val to_u64 : 'a t -> [`U64] t
    val to_u128 : 'a t -> [`U128] t
    val to_i8 : 'a t -> [`I8] t
    val to_i16 : 'a t -> [`I16] t
    val to_i32 : 'a t -> [`I32] t
    val to_i64 : 'a t -> [`I64] t
    val to_i128 : 'a t -> [`I128] t
    val to_vec : 'a t -> [`Vec] t
    val to_list : 'a t -> [`List] t
    val to_tup : 'a t -> [`Tup] t
    val to_rec : 'a t -> [`Rec] t
    val to_pointer : 'a t -> [`Pointer] t
    val to_size : 'a t -> [`Size] t
    val to_bit : 'a t -> [`Bit] t
    val to_byte : 'a t -> [`Byte] t
    val to_word : 'a t -> [`Word] t
    val to_dWord : 'a t -> [`DWord] t
    val to_qWord : 'a t -> [`QWord] t
    val to_oWord : 'a t -> [`OWord] t
    val to_bytes : 'a t -> [`Bytes] t

    val to_any : 'a t -> [`Any] t
    val of_any : Types.structure -> 'a t -> 'b t

    val of_string : string -> [`Any] t

    val modify : string -> 'a t -> string -> 'a t
  end =
struct
  type 'a t = string

  let make pref =
    let seq = ref (-1) in
    fun () ->
      incr seq ;
      pref ^ "_" ^ string_of_int !seq

  let float = make "flt"
  let string = make "str"
  let bool = make "bool"
  let char = make "char"
  let i8 = make "i8"
  let u8 = make "u8"
  let i16 = make "i16"
  let u16 = make "u16"
  let u32 = make "u32"
  let i32 = make "i32"
  let u64 = make "u64"
  let i64 = make "i64"
  let u128 = make "u128"
  let i128 = make "i128"
  let tuple = make "tup"
  let record = make "rec"
  let vector = make "vec"
  let list = make "list"
  let pointer = make "ptr"
  let size = make "sz"
  let bit = make "bit"
  let byte = make "byte"
  let word = make "word"
  let dword = make "dword"
  let qword = make "qword"
  let oword = make "oword"
  let bytes = make "bytes"
  let pair = make "pair"
  let auto = make "auto"
  let func0 = make "func0"
  let func1 : 'a t -> 'a t -> ([`Function1] * 'a * 'b) t =
    let maker = make "func1" in
    fun _ _ -> maker ()
  let func2 = make "func2"
  let func3 = make "func3"
  let param n = make ("param"^ string_of_int n)
  let any structure =
    match structure with
    | Types.TFloat -> float ()
    | Types.TString -> string ()
    | Types.TBool -> bool ()
    | Types.TChar -> char ()
    | Types.TU8 -> u8 ()
    | Types.TU16 -> u16 ()
    | Types.TU32 -> u32 ()
    | Types.TU64 -> u64 ()
    | Types.TU128 -> u128 ()
    | Types.TI8 -> i8 ()
    | Types.TI16 -> i16 ()
    | Types.TI32 -> i32 ()
    | Types.TI64 -> i64 ()
    | Types.TI128 -> i128 ()
    | Types.TVec _ -> vector ()
    | Types.TList _ -> list ()
    | Types.TTup _ -> tuple ()
    | Types.TRec _ -> record ()
    | Types.TPointer -> pointer ()
    | Types.TSize -> size ()
    | Types.TBit -> bit ()
    | Types.TByte -> byte ()
    | Types.TWord -> word ()
    | Types.TDWord -> dword ()
    | Types.TQWord -> qword ()
    | Types.TOWord -> oword ()
    | Types.TBytes -> bytes ()
    | Types.TPair _ -> pair ()

  let to_float s = s
  let to_string s = s
  let to_bool s = s
  let to_char s = s
  let to_u8 s = s
  let to_u16 s = s
  let to_u32 s = s
  let to_u64 s = s
  let to_u128 s = s
  let to_i8 s = s
  let to_i16 s = s
  let to_i32 s = s
  let to_i64 s = s
  let to_i128 s = s
  let to_vec s = s
  let to_list s = s
  let to_tup s = s
  let to_rec s = s
  let to_pointer s = s
  let to_size s = s
  let to_bit s = s
  let to_byte s = s
  let to_word s = s
  let to_dWord s = s
  let to_qWord s = s
  let to_oWord s = s
  let to_bytes s = s

  let to_any s = s
  let of_any structure s =
    match structure with
    | Types.TFloat -> to_float s
    | Types.TString -> to_string s
    | Types.TBool -> to_bool s
    | Types.TChar -> to_char s
    | Types.TU8 -> to_u8 s
    | Types.TU16 -> to_u16 s
    | Types.TU32 -> to_u32 s
    | Types.TU64 -> to_u64 s
    | Types.TU128 -> to_u128 s
    | Types.TI8 -> to_i8 s
    | Types.TI16 -> to_i16 s
    | Types.TI32 -> to_i32 s
    | Types.TI64 -> to_i64 s
    | Types.TI128 -> to_i128 s
    | Types.TVec _ -> to_vec s
    | Types.TList _ -> to_list s
    | Types.TTup _ -> to_tup s
    | Types.TRec _ -> to_rec s
    | Types.TPointer -> to_pointer s
    | Types.TSize -> to_size s
    | Types.TBit -> to_bit s
    | Types.TByte -> to_byte s
    | Types.TWord -> to_word s
    | Types.TDWord -> to_dWord s
    | Types.TQWord -> to_qWord s
    | Types.TOWord -> to_oWord s
    | Types.TBytes -> to_bytes s
    | Types.TPair _ -> assert false (* get rid of Any! *)

  let of_string s = s

  let print : 'a BatIO.output -> 'b t -> unit = String.print

  let modify p id s = p ^ id ^ s
end

type 'a id = 'a Identifier.t

module type NUMERIC =
sig
  type output
  type mid
  val eq : output -> mid -> mid -> [`Bool] id
  val ne : output -> mid -> mid -> [`Bool] id
  val gt : output -> mid -> mid -> [`Bool] id
  val ge : output -> mid -> mid -> [`Bool] id
  val add : output -> mid -> mid -> mid
  val sub : output -> mid -> mid -> mid
  val mul : output -> mid -> mid -> mid
  val div : output -> mid -> mid -> mid

  val of_const_int : output -> int -> mid
  val of_byte : output -> [`Byte] id -> mid
  val to_byte : output -> mid -> [`Byte] id
  val of_word : output -> [`Word] id -> mid
  val to_word : output -> mid -> [`Word] id
  val of_dword : output -> [`DWord] id -> mid
  val to_dword : output -> mid -> [`DWord] id
  val of_qword : output -> [`QWord] id -> mid
  val to_qword : output -> mid -> [`QWord] id
  val of_oword : output -> [`OWord] id -> mid
  val to_oword : output -> mid -> [`OWord] id
  val to_string : output -> mid -> [`String] id
end

module type INTEGER =
sig
  include NUMERIC

  val rem : output -> mid -> mid -> mid
  val log_and : output -> mid -> mid -> mid
  val log_or : output -> mid -> mid -> mid
  val log_xor : output -> mid -> mid -> mid
  val log_not : output -> mid -> mid
  val shift_left : output -> mid -> [`U8] id -> mid
  val shift_right : output -> mid -> [`U8] id -> mid
  val of_string : output -> [`String] id -> mid
  val of_u8 : output -> [`U8] id -> mid
  val to_u8 : output -> mid -> [`U8] id
end

(* Dessser provides the SER/DES with a frame stack (esp. useful when
 * (de)constructing heap values but could help with some SER/DES as well: *)
type frame =
  { (* The type of the topmost value: *)
    typ : Types.t ;
    (* The index of this value within the parent compound type (or 0
     * if root) *)
    index : int ;
    (* Optionally, if the parent type is a record: *)
    name : string }

module type BACKEND =
sig
  val preferred_file_extension : string
  (* Depending on the back-end, one might want to write in several sections
   * at the same time and combine them together at the end. For instance,
   * a section for global definitions, for local definitions, and for the
   * code proper. *)
  type output
  val make_output : unit -> output
  val print_output : 'a IO.output -> output -> unit
  val function0 : output -> Types.t -> (output -> 'a id) -> ([`Function0] * 'a)  id
  val function1 : output -> Types.t -> Types.t -> (output -> 'a id -> 'b id) -> ([`Function1] * 'a * 'b) id
  val function2 : output -> Types.t -> Types.t -> Types.t -> (output -> 'a id -> 'b id -> 'c id) -> ([`Function2] * 'a * 'b * 'c) id

  val ignore : output -> 'a id -> unit
  val comment : output -> ('a, string BatIO.output, unit, unit, unit, unit) format6 -> 'a

  val dump : output -> [`String] id list -> unit

  val dword_eq : output -> [`DWord] id -> [`DWord] id -> [`Bool] id
  val size_ge : output -> [`Size] id -> [`Size] id -> [`Bool] id
  val bytes_append : output -> [`Bytes] id -> [`Bytes] id -> [`Bytes] id
  val u8_of_byte : output -> [`Byte] id -> [`U8] id
  val byte_of_u8 : output -> [`U8] id -> [`Byte] id
  val test_bit : output -> [`Pointer] id -> [`U32] id -> [`Bit] id
  (* Also works for bits located further away from the pointed byte (when >= 8) *)
  val set_bit : output -> [`Pointer] id -> [`U32] id -> [`Bit] id -> unit
  val read_byte : output -> [`Pointer] id -> ([`Byte] id * [`Pointer] id)
  val read_word : output -> ?be:bool -> [`Pointer] id -> ([`Word] id * [`Pointer] id)
  val read_dword : output -> ?be:bool -> [`Pointer] id -> ([`DWord] id * [`Pointer] id)
  val read_qword : output -> ?be:bool -> [`Pointer] id -> ([`QWord] id * [`Pointer] id)
  val read_oword : output -> ?be:bool -> [`Pointer] id -> ([`OWord] id * [`Pointer] id)
  val read_bytes : output -> [`Pointer] id -> [`Size] id -> ([`Bytes] id * [`Pointer] id)
  val write_byte : output -> [`Pointer] id -> [`Byte] id -> [`Pointer] id
  val write_word : output -> ?be:bool -> [`Pointer] id -> [`Word] id -> [`Pointer] id
  val write_dword : output -> ?be:bool -> [`Pointer] id -> [`DWord] id -> [`Pointer] id
  val write_qword : output -> ?be:bool -> [`Pointer] id -> [`QWord] id -> [`Pointer] id
  val write_oword : output -> ?be:bool -> [`Pointer] id -> [`OWord] id -> [`Pointer] id
  val write_bytes : output -> [`Pointer] id -> [`Bytes] id -> [`Pointer] id
  val blit_bytes : output -> [`Pointer] id -> [`Byte] id -> [`Size] id -> [`Pointer] id
  val peek_byte : output -> ?at:[`Size] id -> [`Pointer] id -> [`Byte] id
  val peek_word : output -> ?be:bool -> ?at:[`Size] id -> [`Pointer] id -> [`Word] id
  val peek_dword : output -> ?be:bool -> ?at:[`Size] id -> [`Pointer] id -> [`DWord] id
  val peek_qword : output -> ?be:bool -> ?at:[`Size] id -> [`Pointer] id -> [`QWord] id
  val peek_oword : output -> ?be:bool -> ?at:[`Size] id -> [`Pointer] id -> [`OWord] id
  val poke_byte : output -> [`Pointer] id -> [`Byte] id -> unit

  val pointer_add : output -> [`Pointer] id -> [`Size] id -> [`Pointer] id
  val pointer_sub : output -> [`Pointer] id -> [`Pointer] id -> [`Size] id
  val size_add : output -> [`Size] id -> [`Size] id -> [`Size] id
  val size_to_string : output -> [`Size] id -> [`String] id
  val rem_size : output -> [`Pointer] id -> [`Size] id
  val bit_of_const : output -> bool -> [`Bit] id
  val byte_of_const : output -> int -> [`Byte] id
  val word_of_const : output -> int -> [`Word] id
  val dword_of_const : output -> Uint32.t -> [`DWord] id
  val qword_of_const : output -> Uint64.t -> [`QWord] id
  val oword_of_const : output -> Uint128.t -> [`OWord] id
  val size_of_const : output -> int -> [`Size] id
  val size_of_u32 : output -> [`U32] id -> [`Size] id
  val u32_of_size : output -> [`Size] id -> [`U32] id
  (* Build a pointer from a const string *)
  val pointer_of_string : output -> string -> [`Pointer] id
  val float_of_i8 : output -> [`I8] id -> [`Float] id
  val float_of_qword : output -> [`QWord] id -> [`Float] id
  val qword_of_float : output -> [`Float] id -> [`QWord] id
  val string_of_float : output -> [`Float] id -> [`String] id (* human readable *)
  val cat_string : output -> [`String] id -> [`String] id -> [`String] id
  val byte_of_char : output -> [`Char] id -> [`Byte] id
  val char_of_byte : output -> [`Byte] id -> [`Char] id

  (* Cast those from/into a common TupItem type? *)
  val make_pair : output -> Types.t -> 'a id -> 'b id -> ([`Pair] * 'a * 'b) id
  val pair_fst : output -> ([`Pair] * 'a * 'b) id -> 'a id
  val pair_snd : output -> ([`Pair] * 'a * 'b) id -> 'b id

  val length_of_string : output -> [`String] id -> [`Size] id
  val string_of_bytes : output -> [`Bytes] id -> [`String] id
  val bytes_of_string : output -> [`String] id -> [`Bytes] id

  (* Returns the number of elements in the given list *)
  val length_of_list : output -> [`List] id -> [`U32] id

  val string_of_const : output -> string -> [`String] id
  val bool_of_const : output -> bool -> [`Bool] id
  val float_of_const : output -> float -> [`Float] id
  val u8_of_bool : output -> [`Bool] id -> [`U8] id
  val bool_and : output -> [`Bool] id -> [`Bool] id -> [`Bool] id
  val bool_or : output -> [`Bool] id -> [`Bool] id -> [`Bool] id
  val bool_not : output -> [`Bool] id -> [`Bool] id

  val i32_of_u32 : output -> [`U32] id -> [`I32] id
  val u32_of_i32 : output -> [`I32] id -> [`U32] id

  val u8_of_const : output -> Uint8.t -> [`U8] id
  val u16_of_const : output -> Uint16.t -> [`U16] id
  val u32_of_const : output -> Uint32.t -> [`U32] id
  val u64_of_const : output -> Uint64.t -> [`U64] id
  val u128_of_const : output -> Uint128.t -> [`U128] id
  val i8_of_const : output -> Int8.t -> [`I8] id
  val i16_of_const : output -> Int16.t -> [`I16] id
  val i32_of_const : output -> Int32.t -> [`I32] id
  val i64_of_const : output -> Int64.t -> [`I64] id
  val i128_of_const : output -> Int128.t -> [`I128] id

  val choose :
    output -> cond:[`Bool] id -> (output -> 'a id) -> (output -> 'a id) -> 'a id

  (* [cond] must be a function from byte to bool.
   * [reduce] must be a function from any value and byte into any value. *)
  (* TODO: an id type for pair of 'a * 'b *)
  val read_while :
    output ->
    cond:([`Function1] * [`Byte] * [`Bool]) id ->
    reduce:([`Function2] * 'a * [`Byte] * 'a) id ->
    'a id ->
    [`Pointer] id ->
      'a id * [`Pointer] id

  val loop_while :
    output ->
    cond:([`Function1] * 'res * [`Bool]) id ->
    loop:([`Function1] * 'res * 'res) id ->
    'res id ->
      'res id

  val loop_until :
    output ->
    loop:([`Function1] * 'res * 'res) id ->
    cond:([`Function1] * 'res * [`Bool]) id ->
    'res id ->
      'res id

  val loop_repeat :
    output ->
    from:[`I32] id ->
    to_:[`I32] id ->
    loop:([`Function2] * [`I32] * 'res * 'res) id ->
    'res id ->
      'res id

  module Float : NUMERIC with type output = output and type mid = [`Float] id
  module U8 : INTEGER with type output = output and type mid = [`U8] id
  module I8 : INTEGER with type output = output and type mid = [`I8] id
  module U16 : INTEGER with type output = output and type mid = [`U16] id
  module I16 : INTEGER with type output = output and type mid = [`I16] id
  module U32 : INTEGER with type output = output and type mid = [`U32] id
  module I32 : INTEGER with type output = output and type mid = [`I32] id
  module U64 : INTEGER with type output = output and type mid = [`U64] id
  module I64 : INTEGER with type output = output and type mid = [`I64] id
  module U128 : INTEGER with type output = output and type mid = [`U128] id
  module I128 : INTEGER with type output = output and type mid = [`I128] id

  (* Special needs for des/ser into/from a heap allocated value *)
  val alloc_value : output -> Types.t -> [`Pointer] id
  val set_field : output -> frame list -> [`Pointer] id -> 'a id -> unit
  val set_nullable_field : output -> frame list -> [`Pointer] id -> 'a id option -> unit

  (* Return an id of the field pointed at the frame stack, from the given
   * "pointer" identifier (that actually identify a value on the heap rather
   * than a proper pointer object) *)
  val get_field : output -> frame list -> [`Pointer] id -> [`Any] id
  val get_nullable_field : output -> frame list -> [`Pointer] id -> [`Any] id
  val field_is_set : output -> frame list -> [`Pointer] id -> [`Bool] id
end

module type DES =
sig
  module BE : BACKEND

  (* RW state passed to every deserialization operations *)
  type state
  val start : Types.t -> BE.output -> [`Pointer] id -> state * [`Pointer] id
  val stop : BE.output -> state -> [`Pointer] id -> [`Pointer] id

  type 'a des = BE.output -> frame list -> state -> [`Pointer] id -> 'a * [`Pointer] id

  val dfloat : [`Float] id des
  val dstring : [`String] id des
  val dbool : [`Bool] id des
  val dchar : [`Char] id des
  val di8 : [`I8] id des
  val di16 : [`I16] id des
  val di32 : [`I32] id des
  val di64 : [`I64] id des
  val di128 : [`I128] id des
  val du8 : [`U8] id des
  val du16 : [`U16] id des
  val du32 : [`U32] id des
  val du64 : [`U64] id des
  val du128 : [`U128] id des

  val tup_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val tup_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val tup_sep : int (* before *) -> BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val rec_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val rec_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val rec_sep : string (* before *) -> BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val vec_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val vec_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val vec_sep : int (* before *) -> BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val list_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`U32] id * [`Pointer] id
  val list_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val list_sep : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id

  val is_null : BE.output -> frame list -> state -> [`Pointer] id -> [`Bool] id
  val dnull : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val dnotnull : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
end

type ssize = ConstSize of int | DynSize of [`Size] id

module type SER =
sig
  module BE : BACKEND

  (* RW state passed to every serialization operations *)
  type state
  val start : Types.t -> BE.output -> [`Pointer] id -> state * [`Pointer] id
  val stop : BE.output -> state -> [`Pointer] id -> [`Pointer] id

  (* FIXME: make this type "private": *)
  type 'a ser = BE.output -> frame list -> state -> 'a -> [`Pointer] id -> [`Pointer] id

  val sfloat : [`Float] id ser
  val sstring : [`String] id ser
  val sbool : [`Bool] id ser
  val schar : [`Char] id ser
  val si8 : [`I8] id ser
  val si16 : [`I16] id ser
  val si32 : [`I32] id ser
  val si64 : [`I64] id ser
  val si128 : [`I128] id ser
  val su8 : [`U8] id ser
  val su16 : [`U16] id ser
  val su32 : [`U32] id ser
  val su64 : [`U64] id ser
  val su128 : [`U128] id ser

  val tup_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val tup_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val tup_sep : int (* before *) -> BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val rec_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val rec_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val rec_sep : string (* before *) -> BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val vec_opn : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val vec_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val vec_sep : int (* before *) -> BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val list_opn : BE.output -> frame list -> state -> [`U32] id -> [`Pointer] id -> [`Pointer] id
  val list_cls : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val list_sep : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id

  val nullable : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val snull : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id
  val snotnull : BE.output -> frame list -> state -> [`Pointer] id -> [`Pointer] id

  (* Sometimes, we'd like to know in advance how large a serialized value is
   * going to be. Value must have been deserialized already. *)
  type 'a ssizer = BE.output -> frame list -> 'a -> ssize
  val ssize_of_float : [`Float] id ssizer
  val ssize_of_string : [`String] id ssizer
  val ssize_of_bool : [`Bool] id ssizer
  val ssize_of_char : [`Char] id ssizer
  val ssize_of_i8 : [`I8] id ssizer
  val ssize_of_i16 : [`I16] id ssizer
  val ssize_of_i32 : [`I32] id ssizer
  val ssize_of_i64 : [`I64] id ssizer
  val ssize_of_i128 : [`I128] id ssizer
  val ssize_of_u8 : [`U8] id ssizer
  val ssize_of_u16 : [`U16] id ssizer
  val ssize_of_u32 : [`U32] id ssizer
  val ssize_of_u64 : [`U64] id ssizer
  val ssize_of_u128 : [`U128] id ssizer
  (* Specifically for the compound, excluding the size of the parts: *)
  val ssize_of_tup : [`Tup] id ssizer
  val ssize_of_rec : [`Rec] id ssizer
  val ssize_of_vec : [`Vec] id ssizer
  val ssize_of_list : [`List] id ssizer
  val ssize_of_null : BE.output -> frame list -> ssize
end

module DesSer (Des : DES) (Ser : SER with module BE = Des.BE) =
struct
  module BE = Des.BE

  let ds ser des transform oc frames sstate dstate src dst =
    let typ = (List.hd frames).typ in
    let what = IO.to_string Types.print typ in
    BE.comment oc "Desserialize a %s" what ;
    let v, src = des oc frames dstate src in
    let v = transform oc frames v in
    BE.comment oc "Serialize a %s" what ;
    let dst = ser oc frames sstate v dst in
    BE.make_pair oc Types.pair_ptrs src dst

  let dsfloat = ds Ser.sfloat Des.dfloat
  let dsstring = ds Ser.sstring Des.dstring
  let dsbool = ds Ser.sbool Des.dbool
  let dschar = ds Ser.schar Des.dchar
  let dsi8 = ds Ser.si8 Des.di8
  let dsi16 = ds Ser.si16 Des.di16
  let dsi32 = ds Ser.si32 Des.di32
  let dsi64 = ds Ser.si64 Des.di64
  let dsi128 = ds Ser.si128 Des.di128
  let dsu8 = ds Ser.su8 Des.du8
  let dsu16 = ds Ser.su16 Des.du16
  let dsu32 = ds Ser.su32 Des.du32
  let dsu64 = ds Ser.su64 Des.du64
  let dsu128 = ds Ser.su128 Des.du128

  let dsnull oc frames sstate dstate src dst =
    BE.comment oc "NULL" ;
    Des.dnull oc frames dstate src,
    Ser.snull oc frames sstate dst

  let dsnotnull oc frames sstate dstate src dst =
    BE.comment oc "NOT NULL" ;
    Des.dnotnull oc frames dstate src,
    Ser.snotnull oc frames sstate dst

  let rec dstup typs transform oc frames sstate dstate src dst =
    BE.comment oc "DesSer a Tuple" ;
    let src = Des.tup_opn oc frames dstate src
    and dst = Ser.tup_opn oc frames sstate dst in
    let src, dst =
      BatArray.fold_lefti (fun (src, dst) i typ ->
        let subframes = { typ ; index = i ; name = "" } :: frames in
        BE.comment oc "DesSer tuple field %d" i ;
        if i = 0 then
          desser_ transform oc subframes sstate dstate src dst
        else
          let src = Des.tup_sep i oc frames dstate src
          and dst = Ser.tup_sep i oc frames sstate dst in
          desser_ transform oc subframes sstate dstate src dst
      ) (src, dst) typs in
    BE.make_pair oc Types.pair_ptrs
      (Des.tup_cls oc frames dstate src)
      (Ser.tup_cls oc frames sstate dst)

  and dsrec typs transform oc frames sstate dstate src dst =
    BE.comment oc "DesSer a Record" ;
    let src = Des.rec_opn oc frames dstate src
    and dst = Ser.rec_opn oc frames sstate dst in
    let src, dst =
      BatArray.fold_lefti (fun (src, dst) i (name, typ) ->
        let subframes = { typ ; index = i ; name } :: frames in
        BE.comment oc "DesSer record field %s" name ;
        if i = 0 then
          desser_ transform oc subframes sstate dstate src dst
        else
          let src = Des.rec_sep name oc frames dstate src
          and dst = Ser.rec_sep name oc frames sstate dst in
          desser_ transform oc subframes sstate dstate src dst
      ) (src, dst) typs in
    BE.make_pair oc Types.pair_ptrs
      (Des.rec_cls oc frames dstate src)
      (Ser.rec_cls oc frames sstate dst)

  (* This will generates a long linear code with one block per array
   * item. Maybe have a BE.loop instead? *)
  and dsvec dim typ transform oc frames sstate dstate src dst =
    BE.comment oc "DesSer a Vector" ;
    let src = Des.vec_opn oc frames dstate src
    and dst = Ser.vec_opn oc frames sstate dst in
    let rec loop src dst i =
      if i >= dim then
        BE.make_pair oc Types.pair_ptrs
          (Des.vec_cls oc frames dstate src)
          (Ser.vec_cls oc frames sstate dst)
      else (
        BE.comment oc "DesSer vector field %d" i ;
        let subframes = { typ ; index = i ; name = "" } :: frames in
        if i = 0 then
          let src, dst = desser_ transform oc subframes sstate dstate src dst in
          loop src dst (i + 1)
        else
          let src = Des.vec_sep i oc frames dstate src
          and dst = Ser.vec_sep i oc frames sstate dst in
          let src, dst = desser_ transform oc subframes sstate dstate src dst in
          loop src dst (i + 1)
      )
    in
    loop src dst 0

  and dslist typ transform oc frames sstate dstate src dst =
    BE.comment oc "DesSer a List" ;
    let dim, src = Des.list_opn oc frames dstate src in
    let dst = Ser.list_opn oc frames sstate dim dst in
    let src_dst = BE.make_pair oc Types.pair_ptrs src dst in
    let loop =
      BE.function2 oc Types.i32 Types.pair_ptrs Types.bool (fun oc n src_dst ->
        BE.comment oc "DesSer a list item" ;
        let subframes = { typ ; index = -1 ; name = "" } :: frames in
        let src_dst =
          BE.choose oc ~cond:(BE.I32.(eq oc n (of_const_int oc 0)))
            (fun _oc -> src_dst)
            (fun oc ->
              let src = Des.list_sep oc frames dstate (BE.pair_fst oc src_dst)
              and dst = Ser.list_sep oc frames sstate (BE.pair_snd oc src_dst) in
              BE.make_pair oc Types.pair_ptrs src dst) in
        let src, dst =
          desser_ transform oc subframes sstate dstate
                  (BE.pair_fst oc src_dst)
                  (BE.pair_snd oc src_dst) in
        BE.make_pair oc Types.pair_ptrs src dst) in
    let src_dst =
      let from = BE.I32.of_const_int oc 0
      and to_ = BE.i32_of_u32 oc dim in
      BE.loop_repeat oc ~from ~to_ ~loop src_dst in
    BE.make_pair oc Types.pair_ptrs
      (Des.vec_cls oc frames dstate (BE.pair_fst oc src_dst))
      (Ser.vec_cls oc frames sstate (BE.pair_snd oc src_dst))

  and desser_ transform oc =
    let spec_transf of_any =
      fun oc frames v -> of_any (transform oc frames (Identifier.to_any v)) in
    let desser_structure = function
      | Types.TFloat -> dsfloat (spec_transf Identifier.to_float)
      | Types.TString -> dsstring (spec_transf Identifier.to_string)
      | Types.TBool -> dsbool (spec_transf Identifier.to_bool)
      | Types.TChar -> dschar (spec_transf Identifier.to_char)
      | Types.TI8 -> dsi8 (spec_transf Identifier.to_i8)
      | Types.TI16 -> dsi16 (spec_transf Identifier.to_i16)
      | Types.TI32 -> dsi32 (spec_transf Identifier.to_i32)
      | Types.TI64 -> dsi64 (spec_transf Identifier.to_i64)
      | Types.TI128 -> dsi128 (spec_transf Identifier.to_i128)
      | Types.TU8 -> dsu8 (spec_transf Identifier.to_u8)
      | Types.TU16 -> dsu16 (spec_transf Identifier.to_u16)
      | Types.TU32 -> dsu32 (spec_transf Identifier.to_u32)
      | Types.TU64 -> dsu64 (spec_transf Identifier.to_u64)
      | Types.TU128 -> dsu128 (spec_transf Identifier.to_u128)
      | Types.TTup typs -> dstup typs transform
      | Types.TRec typs -> dsrec typs transform
      | Types.TVec (dim, typ) -> dsvec dim typ transform
      | Types.TList typ -> dslist typ transform
      | _ -> assert false
    in
    fun frames sstate dstate src dst ->
      let typ = (List.hd frames).typ in
      let pair =
        if typ.nullable then
          let cond = Des.is_null oc frames dstate src in
          (* Des can us [is_null] to prepare for a nullable, but Ser might also
           * have some work to do: *)
          let dst = Ser.nullable oc frames sstate dst in
          BE.choose oc ~cond
            (fun oc ->
              let s, d = dsnull oc frames sstate dstate src dst in
              BE.make_pair oc Types.pair_ptrs s d)
            (fun oc ->
              let s, d = dsnotnull oc frames sstate dstate src dst in
              desser_structure typ.structure oc frames sstate dstate s d)
        else
          desser_structure typ.structure oc frames sstate dstate src dst in
      (* Returns src and dest: *)
      (BE.pair_fst oc pair),
      (BE.pair_snd oc pair)

  let desser typ ?transform oc =
    let no_transform _oc _frames v = v in
    let transform = transform |? no_transform in
    let desser_ = desser_ transform oc in
    fun src dst ->
      let sstate, dst = Ser.start typ oc dst
      and dstate, src = Des.start typ oc src in
      let src, dst =
        desser_ [ { typ ; index = 0 ; name = "" } ] sstate dstate src dst in
      let dst = Ser.stop oc sstate dst
      and src = Des.stop oc dstate src in
      src, dst
end
