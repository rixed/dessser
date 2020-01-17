open Batteries
open Stdint

let pp = Printf.fprintf

(* Those types describing values that can be (de)serialized.
 * All of them can possibly be nullable.
 * In what follows, only the types used by the code generator
 * itself are type-checked.
 * There is no type-checking whatsoever for value-types: it is
 * assumed that all operations involving values have been type
 * checked already. So that we avoid GADT in this module. *)
module ValueType =
struct
  (*$< ValueType *)

  type not_nullable =
    | Float
    | String
    | Bool
    | Char (* Exact same values as U8 but different typing rules *)
    | U8
    | U16
    | U24
    | U32
    | U40
    | U48
    | U56
    | U64
    | U128
    | I8
    | I16
    | I24
    | I32
    | I40
    | I48
    | I56
    | I64
    | I128
    | Vec of int * t
    | List of t
    | Tup of t array
    (* Exact same as a tuple, but with field names that can be used as
     * accessors (also used to name actual fields in generated code): *)
    | Rec of (string * t) array
    (* The type for maps exist because there will be some operations using
     * that type indirectly (such as fetching from a DB by key, or describing
     * a key->value mapping in a type expression). But there is no value of
     * that type, ever. From a (de)serialized point of view, maps are
     * equivalent to association lists. *)
    | Map of t * t

  and t =
    | Nullable of nullable
    | NotNullable of not_nullable

  and nullable = not_nullable

  let is_nullable = function
    | Nullable _ -> true
    | NotNullable _ -> false

  let to_not_nullable = function
    | Nullable t -> t
    | NotNullable t -> t

  let rec print_not_nullable oc =
    let sp = String.print oc in
    function
    | Float -> sp "Float"
    | String -> sp "String"
    | Bool -> sp "Bool"
    | Char -> sp "Char"
    | U8 -> sp "U8"
    | U16 -> sp "U16"
    | U24 -> sp "U24"
    | U32 -> sp "U32"
    | U40 -> sp "U40"
    | U48 -> sp "U48"
    | U56 -> sp "U56"
    | U64 -> sp "U64"
    | U128 -> sp "U128"
    | I8 -> sp "I8"
    | I16 -> sp "I16"
    | I24 -> sp "I24"
    | I32 -> sp "I32"
    | I40 -> sp "I40"
    | I48 -> sp "I48"
    | I56 -> sp "I56"
    | I64 -> sp "I64"
    | I128 -> sp "I128"
    | Vec (dim, vt) ->
        pp oc "%a[%d]" print vt dim
    | List vt ->
        pp oc "%a[]" print vt
    | Tup vts ->
        pp oc "%a"
          (Array.print ~first:"(" ~last:")" ~sep:";" print) vts
    | Rec vts ->
        pp oc "%a"
          (Array.print ~first:"{" ~last:"}" ~sep:";"
            (fun oc (n, t) ->
              pp oc "%s: %a" n print t)
          ) vts
    | Map (k, v) ->
        pp oc "%a{%a}" print v print k

  and print_nullable oc t =
    pp oc "%a?" print_not_nullable t

  and print oc = function
    | Nullable t ->
        print_nullable oc t
    | NotNullable t ->
        print_not_nullable oc t

  (* Locate subfields within a compound ValueType.t: *)
  type path = int list

  let print_path oc p =
    List.print ~first:"" ~last:"" ~sep:"/" Int.print oc p

  let to_nullable = function
    | NotNullable t -> Nullable t
    | Nullable _ as x -> x

  let rec type_of_path t path =
    match path with
    | [] -> t
    | i :: path ->
        let rec type_of_not_nullable = function
          | NotNullable (
              Float | String | Bool | Char | Map _ |
              U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
              I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128) ->
              assert false
          | NotNullable (Vec (dim, vt)) ->
              assert (i < dim) ;
              type_of_path vt path
          | NotNullable (List vt) ->
              type_of_path vt path
          | NotNullable (Tup vts) ->
              assert (i < Array.length vts) ;
              type_of_path vts.(i) path
          | NotNullable (Rec vts) ->
              assert (i < Array.length vts) ;
              type_of_path (snd vts.(i)) path
          | Nullable x ->
              type_of_not_nullable (NotNullable x) |>
              to_nullable in
        type_of_not_nullable t

  (*$inject
     let test_t = NotNullable (Tup [| NotNullable U8 ; Nullable String |])
  *)

  (*$= type_of_path & ~printer:(BatIO.to_string print)
    test_t (type_of_path test_t [])
    (NotNullable U8) (type_of_path test_t [0])
    (Nullable String) (type_of_path test_t [1])
  *)
  (*$>*)
end

(* All types we can generate code for.
 * Include the types for serialized values and a few additional types used
 * to manipulate them. *)
module Type =
struct
  type t =
    | Value of ValueType.t
    | Void
    (* DataPtr are used to point into the stream of bytes that's being
     * serialized into / deserialized from. The type of the value that's
     * being (de)serialized is kept nonetheless. *)
    | DataPtr
    (* ValuePtr are used to point at heap allocated values of a given type.
     * The "offset" is then the location in that data structure, and it is
     * "advanced" by hopping from subfield to subfields, traversing the
     * structure depth first. The path is thus merely an integer, but the
     * backend has to know how to locate each addressable leaves. *)
    | ValuePtr of ValueType.t
    (* A size in byte. *)
    | Size
    (* Data access, may be just pointer to the actual serialized object: *)
    | Bit
    | Byte
    | Word
    | DWord
    | QWord
    | OWord
    | Bytes
    | Pair of t * t
    | Function of t array * (* result: *) t

  let rec print oc =
    let sp = String.print oc in
    function
    | Value vt ->
        ValueType.print oc vt
    | Void -> sp "Void"
    | DataPtr -> sp "DataPtr"
    | ValuePtr t ->
        pp oc "ValuePtr(%a)"
          ValueType.print t
    | Size -> sp "Size"
    | Bit -> sp "Bit"
    | Byte -> sp "Byte"
    | Word -> sp "Word"
    | DWord -> sp "DWord"
    | QWord -> sp "QWord"
    | OWord -> sp "OWord"
    | Bytes -> sp "Bytes"
    | Pair (t1, t2) ->
        pp oc "Pair(%a, %a)"
          print t1
          print t2
    | Function ([||], t1) ->
        pp oc "(unit->%a)" print t1
    | Function (ts, t2) ->
        pp oc "(%a->%a)"
          (Array.print ~first:"" ~last:"" ~sep:"->" print) ts
          print t2

  let to_nullable = function
    | Value ValueType.(NotNullable t) -> Value ValueType.(Nullable t)
    | t ->
        Printf.eprintf "Cannot turn type %a into nullable\n%!"
          print t ;
        assert false

  let to_not_nullable = function
    | Value ValueType.(Nullable t) -> Value ValueType.(NotNullable t)
    | t ->
        Printf.eprintf "Cannot turn type %a into not-nullable\n%!"
          print t ;
        assert false
end

(* Some short cuts for often used types: *)

type typ = Type.t
type vtyp = ValueType.t
type path = ValueType.path

let bool = Type.Value ValueType.(NotNullable Bool)
let char = Type.Value ValueType.(NotNullable Char)
let nstring = Type.Value ValueType.(Nullable String)
let string = Type.Value ValueType.(NotNullable String)
let float = Type.Value ValueType.(NotNullable Float)
let u8 = Type.Value ValueType.(NotNullable U8)
let u16 = Type.Value ValueType.(NotNullable U16)
let u24 = Type.Value ValueType.(NotNullable U24)
let u32 = Type.Value ValueType.(NotNullable U32)
let u40 = Type.Value ValueType.(NotNullable U40)
let u48 = Type.Value ValueType.(NotNullable U48)
let u56 = Type.Value ValueType.(NotNullable U56)
let u64 = Type.Value ValueType.(NotNullable U64)
let u128 = Type.Value ValueType.(NotNullable U128)
let i8 = Type.Value ValueType.(NotNullable I8)
let i16 = Type.Value ValueType.(NotNullable I16)
let i24 = Type.Value ValueType.(NotNullable I24)
let i32 = Type.Value ValueType.(NotNullable I32)
let i40 = Type.Value ValueType.(NotNullable I40)
let i48 = Type.Value ValueType.(NotNullable I48)
let i56 = Type.Value ValueType.(NotNullable I56)
let i64 = Type.Value ValueType.(NotNullable I64)
let i128 = Type.Value ValueType.(NotNullable I128)
let void = Type.Void
let bit = Type.Bit
let byte = Type.Byte
let size = Type.Size
let word = Type.Word
let dword = Type.DWord
let qword = Type.QWord
let oword = Type.OWord
let bytes = Type.Bytes
let dataptr = Type.DataPtr
let valueptr t = Type.ValuePtr t
let pair t1 t2 = Type.Pair (t1, t2)

module Expression =
struct
  (*$< Expression *)
  type endianness = LittleEndian | BigEndian

  let print_endianness oc = function
    | LittleEndian -> String.print oc "little-endian"
    | BigEndian -> String.print oc "big-endian"

  type e =
    | Comment of string * e
    | Dump of e
    | Seq of e list
    | IsNull of e
    | Coalesce of e * e
    (* Turn e into a nullable: *)
    | Nullable of e
    (* Turn e into a not-nullable: *)
    | NotNullable of e
    | Null of ValueType.not_nullable
    | Float of float
    | String of string
    | Bool of bool
    | Char of char
    | U8 of int
    | U16 of int
    | U24 of int
    | U32 of Uint32.t
    | U40 of Uint40.t
    | U48 of Uint48.t
    | U56 of Uint56.t
    | U64 of Uint64.t
    | U128 of Uint128.t
    | I8 of int
    | I16 of int
    | I24 of int
    | I32 of Int32.t
    | I40 of Int64.t
    | I48 of Int64.t
    | I56 of Int64.t
    | I64 of Int64.t
    | I128 of Int128.t
    | Bit of bool
    | Size of int
    | Byte of int
    | Word of int
    | DWord of Uint32.t
    | QWord of Uint64.t
    | OWord of Uint128.t
    | Gt of e * e
    (* Convert from/to string for all base value types: *)
    | StringOfFloat of e
    | StringOfChar of e
    | StringOfInt of e
    | FloatOfString of e
    | CharOfString of e
    | U8OfString of e
    | U16OfString of e
    | U24OfString of e
    | U32OfString of e
    | U40OfString of e
    | U48OfString of e
    | U56OfString of e
    | U64OfString of e
    | U128OfString of e
    | I8OfString of e
    | I16OfString of e
    | I24OfString of e
    | I32OfString of e
    | I40OfString of e
    | I48OfString of e
    | I56OfString of e
    | I64OfString of e
    | I128OfString of e
    (* Integers can be casted upon others regardless of sign and width: *)
    | ToU8 of e
    | ToU16 of e
    | ToU24 of e
    | ToU32 of e
    | ToU40 of e
    | ToU48 of e
    | ToU56 of e
    | ToU64 of e
    | ToU128 of e
    | ToI8 of e
    | ToI16 of e
    | ToI24 of e
    | ToI32 of e
    | ToI40 of e
    | ToI48 of e
    | ToI56 of e
    | ToI64 of e
    | ToI128 of e
    (* Comparators: *)
    | Ge of e * e
    | Eq of e * e
    | Ne of e * e
    | Add of e * e
    | Sub of e * e
    | Mul of e * e
    | Div of e * e
    | Rem of e * e
    | LogAnd of e * e
    | LogOr of e * e
    | LogXor of e * e
    | LogNot of e
    | LeftShift of e * e
    | RightShift of e * e
    | FloatOfQWord of e
    | QWordOfFloat of e
    | U8OfByte of e
    | ByteOfU8 of e
    | U16OfWord of e
    | WordOfU16 of e
    | U32OfDWord of e
    | DWordOfU32 of e
    | U64OfQWord of e
    | QWordOfU64 of e
    | U128OfOWord of e
    | OWordOfU128 of e
    | U8OfChar of e
    | CharOfU8 of e
    | SizeOfU32 of e
    | U32OfSize of e
    | BitOfBool of e
    | BoolOfBit of e
    (* à la C: *)
    | U8OfBool of e
    | BoolOfU8 of e
    | AppendBytes of e * e
    | AppendString of e * e
    | StringLength of e
    | StringOfBytes of e
    | BytesOfString of e
    | ListLength of e
    | DataPtrOfString of string
    | TestBit of e * e
    | SetBit of e * e * e
    | ReadByte of e
    | ReadWord of endianness * e
    | ReadDWord of endianness * e
    | ReadQWord of endianness * e
    | ReadOWord of endianness * e
    | ReadBytes of e * e
    | PeekByte of e * e
    | PeekWord of endianness * e * e
    | PeekDWord of endianness * e * e
    | PeekQWord of endianness * e * e
    | PeekOWord of endianness * e * e
    | WriteByte of e * e
    | WriteWord of endianness * e * e
    | WriteDWord of endianness * e * e
    | WriteQWord of endianness * e * e
    | WriteOWord of endianness * e * e
    | WriteBytes of e * e
    | PokeByte of e * e
    | BlitBytes of e * e * e
    | DataPtrAdd of e * e
    | DataPtrSub of e * e
    | RemSize of e
    | And of e * e
    | Or of e * e
    | Not of e
    (* To build heap values: *)
    (* Allocate a default values value.
     * For backends that can alloc uninitialized values:
     *   Will define the type, then alloc an uninitialized value of that
     *   type and return an identifier for it. Fields will be initialized
     *   as serialization progresses.
     * For backends that can not alloc uninitialized values:
     *   Will define the type, then make up a name and return it. Construction
     *   and allocation will happen as the serialize progresses. *)
    | AllocValue of vtyp
    (* Get the value pointed by a valueptr: *)
    | DerefValuePtr of e
    (* Set a field. There is no control that the field type match the type at
     * this location. *)
    | SetField of path * e * e
    | FieldIsNull of path * e
    | GetField of path * e
    | Pair of e * e
    (* WARNING: never use Fst and Snd on the same expression or that expression
     * will be computed twice!
     * Instead, use MapPair or Let *)
    | Fst of e
    | Snd of e
    | MapPair of (* the pair: *) e * (* the function2: *) e
    (* Identifier are set with `Let` expressions, or obtained from the code
     * generators in exchange for an expression: *)
    | Identifier of string
    | Let of string * e * e
    | Function of (*function id*) int * (*args*) typ array * (* body: *) e
    | Param of (*function id*) int * (*param no*) int
    | Choose :
        (* Condition: *) e * (* Consequent: *) e * (* Alternative: *) e -> e
    | ReadWhile :
        (* Condition (byte->bool): *) e *
        (* Reducer ('a->byte->'a): *) e *
        (* Initial value: *) e *
        (* Starting position: *) e ->
        (* Result ('a * ptr): *) e
    | LoopWhile :
        (* Condition ('a->bool): *) e *
        (* Loop body ('a->'a): *) e *
        (* Initial value: *) e -> e
    | LoopUntil :
        (* Loop body ('a->'a): *) e *
        (* Condition ('a->bool): *) e *
        (* Initial value: *) e -> e
    | Repeat :
        (* From: *) e * (* To: *) e *
        (* Loop body (idx->'a->'a): *) e *
        (* Initial value: *) e -> e

  (* Create a function expression and return its id: *)
  let func =
    let next_id = ref (0) in
    fun typs f ->
      let id = !next_id in
      incr next_id ;
      Function (id, typs, f id)

  let rec print ?max_depth oc e =
    if Option.map_default (fun m -> m <= 0) false max_depth then
      pp oc "…"
    else
      let max_depth = Option.map pred max_depth in
      let p = print ?max_depth in
      match e with
      | Comment (str, e) ->
          pp oc "(Comment %S %a)" str p e
      | Dump e1 ->
          pp oc "(Dump %a)" p e1
      | Seq es ->
          pp oc "(Seq %a)" (List.print ~first:"" ~last:"" ~sep:" " p) es
      | IsNull e ->
          pp oc "(IsNull %a)" p e
      | Coalesce (e1, e2) ->
          pp oc "(Coalesce %a %a)" p e1 p e2
      | Nullable e ->
          pp oc "(Nullable %a)" p e
      | NotNullable e ->
          pp oc "(NotNullable %a)" p e
      | Null t ->
          pp oc "(Null %a)" ValueType.print_not_nullable t
      | Float f ->
          pp oc "(Float %f)" f
      | String s ->
          pp oc "(String %S)" s
      | Bool b ->
          pp oc "(Bool %b)" b
      | Char c ->
          pp oc "(Char %c)" c
      | U8 i ->
          pp oc "(U8 %d)" i
      | U16 i ->
          pp oc "(U16 %d)" i
      | U24 i ->
          pp oc "(U24 %d)" i
      | U32 u ->
          pp oc "(U32 %s)" (Uint32.to_string u)
      | U40 u ->
          pp oc "(U40 %s)" (Uint40.to_string u)
      | U48 u ->
          pp oc "(U48 %s)" (Uint48.to_string u)
      | U56 u ->
          pp oc "(U56 %s)" (Uint56.to_string u)
      | U64 u ->
          pp oc "(U64 %s)" (Uint64.to_string u)
      | U128 u ->
          pp oc "(U128 %s)" (Uint128.to_string u)
      | I8 i ->
          pp oc "(I8 %d)" i
      | I16 i ->
          pp oc "(I16 %d)" i
      | I24 i ->
          pp oc "(I24 %d)" i
      | I32 i ->
          pp oc "(I32 %ld)" i
      | I40 i ->
          pp oc "(I40 %Ld)" i
      | I48 i ->
          pp oc "(I48 %Ld)" i
      | I56 i ->
          pp oc "(I56 %Ld)" i
      | I64 i ->
          pp oc "(I64 %Ld)" i
      | I128 u ->
          pp oc "(I128 %s)" (Int128.to_string u)
      | Bit b ->
          pp oc "(Bit %b)" b
      | Size i ->
          pp oc "(Size %d)" i
      | Byte i ->
          pp oc "(Byte %d)" i
      | Word i ->
          pp oc "(Word %d)" i
      | DWord u ->
          pp oc "(DWord %s)" (Uint32.to_string u)
      | QWord u ->
          pp oc "(QWord %s)" (Uint64.to_string u)
      | OWord u ->
          pp oc "(OWord %s)" (Uint128.to_string u)
      | Gt (e1, e2) ->
          pp oc "(Gt %a %a)" p e1 p e2
      | Ge (e1, e2) ->
          pp oc "(Ge %a %a)" p e1 p e2
      | Eq (e1, e2) ->
          pp oc "(Eq %a %a)" p e1 p e2
      | Ne (e1, e2) ->
          pp oc "(Ne %a %a)" p e1 p e2
      | Add (e1, e2) ->
          pp oc "(Add %a %a)" p e1 p e2
      | Sub (e1, e2) ->
          pp oc "(Sub %a %a)" p e1 p e2
      | Mul (e1, e2) ->
          pp oc "(Mul %a %a)" p e1 p e2
      | Div (e1, e2) ->
          pp oc "(Div %a %a)" p e1 p e2
      | Rem (e1, e2) ->
          pp oc "(Rem %a %a)" p e1 p e2
      | LogAnd (e1, e2) ->
          pp oc "(LogAnd %a %a)" p e1 p e2
      | LogOr (e1, e2) ->
          pp oc "(LogOr %a %a)" p e1 p e2
      | LogXor (e1, e2) ->
          pp oc "(LogXor %a %a)" p e1 p e2
      | LeftShift (e1, e2) ->
          pp oc "(LeftShift %a %a)" p e1 p e2
      | RightShift (e1, e2) ->
          pp oc "(RightShift %a %a)" p e1 p e2
      | LogNot e ->
          pp oc "(LogNot %a)" p e
      | StringOfInt e ->
          pp oc "(StringOfInt %a)" p e
      | StringOfChar e ->
          pp oc "(StringOfChar %a)" p e
      | StringOfFloat e ->
          pp oc "(StringOfFloat %a)" p e
      | FloatOfQWord e ->
          pp oc "(FloatOfQWord %a)" p e
      | QWordOfFloat e ->
          pp oc "(QWordOfFloat %a)" p e
      | FloatOfString e ->
          pp oc "(FloatOfString %a)" p e
      | CharOfString e ->
          pp oc "(CharOfString %a)" p e
      | U8OfString e ->
          pp oc "(U8OfString %a)" p e
      | U16OfString e ->
          pp oc "(U16OfString %a)" p e
      | U24OfString e ->
          pp oc "(U24OfString %a)" p e
      | U32OfString e ->
          pp oc "(U32OfString %a)" p e
      | U40OfString e ->
          pp oc "(U40OfString %a)" p e
      | U48OfString e ->
          pp oc "(U48OfString %a)" p e
      | U56OfString e ->
          pp oc "(U56OfString %a)" p e
      | U64OfString e ->
          pp oc "(U64OfString %a)" p e
      | U128OfString e ->
          pp oc "(U128OfString %a)" p e
      | I8OfString e ->
          pp oc "(I8OfString %a)" p e
      | I16OfString e ->
          pp oc "(I16OfString %a)" p e
      | I24OfString e ->
          pp oc "(I24OfString %a)" p e
      | I32OfString e ->
          pp oc "(I32OfString %a)" p e
      | I40OfString e ->
          pp oc "(I40OfString %a)" p e
      | I48OfString e ->
          pp oc "(I48OfString %a)" p e
      | I56OfString e ->
          pp oc "(I56OfString %a)" p e
      | I64OfString e ->
          pp oc "(I64OfString %a)" p e
      | I128OfString e ->
          pp oc "(I128OfString %a)" p e
      | U8OfByte e ->
          pp oc "(U8OfByte %a)" p e
      | ByteOfU8 e ->
          pp oc "(ByteOfU8 %a)" p e
      | U16OfWord e ->
          pp oc "(U16OfWord %a)" p e
      | WordOfU16 e ->
          pp oc "(WordOfU16 %a)" p e
      | U32OfDWord e ->
          pp oc "(U32OfDWord %a)" p e
      | DWordOfU32 e ->
          pp oc "(DWordOfU32 %a)" p e
      | U64OfQWord e ->
          pp oc "(U64OfQWord %a)" p e
      | QWordOfU64 e ->
          pp oc "(QWordOfU64 %a)" p e
      | U128OfOWord e ->
          pp oc "(U128OfOWord %a)" p e
      | OWordOfU128 e ->
          pp oc "(OWordOfU128 %a)" p e
      | U8OfChar e ->
          pp oc "(U8OfChar %a)" p e
      | CharOfU8 e ->
          pp oc "(CharOfU8 %a)" p e
      | SizeOfU32 e ->
          pp oc "(SizeOfU32 %a)" p e
      | U32OfSize e ->
          pp oc "(U32OfSize %a)" p e
      | BitOfBool e ->
          pp oc "(BitOfBool %a)" p e
      | BoolOfBit e ->
          pp oc "(BoolOfBit %a)" p e
      | U8OfBool e ->
          pp oc "(U8OfBool %a)" p e
      | BoolOfU8 e ->
          pp oc "(BoolOfU8 %a)" p e
      | AppendBytes (e1, e2) ->
          pp oc "(AppendBytes %a %a)" p e1 p e2
      | AppendString (e1, e2) ->
          pp oc "(AppendString %a %a)" p e1 p e2
      | StringLength e ->
          pp oc "(StringLength %a)" p e
      | StringOfBytes e ->
          pp oc "(StringOfBytes %a)" p e
      | BytesOfString e ->
          pp oc "(BytesOfString %a)" p e
      | ListLength e ->
          pp oc "(ListLength %a)" p e
      | DataPtrOfString s ->
          pp oc "(DataPtrOfString %S)" s
      | TestBit (e1, e2) ->
          pp oc "(TestBit %a %a)" p e1 p e2
      | SetBit (e1, e2, e3) ->
          pp oc "(SetBit %a %a %a)" p e1 p e2 p e3
      | ReadByte e ->
          pp oc "(ReadByte %a)" p e
      | ReadWord (e1, e2) ->
          pp oc "(ReadWord %a %a)" print_endianness e1 p e2
      | ReadDWord (e1, e2) ->
          pp oc "(ReadDWord %a %a)" print_endianness e1 p e2
      | ReadQWord (e1, e2) ->
          pp oc "(ReadQWord %a %a)" print_endianness e1 p e2
      | ReadOWord (e1, e2) ->
          pp oc "(ReadOWord %a %a)" print_endianness e1 p e2
      | ReadBytes (e1, e2) ->
          pp oc "(ReadBytes %a %a)" p e1 p e2
      | PeekByte (e1, e2) ->
          pp oc "(PeekByte %a %a)" p e1 p e2
      | PeekWord (e1, e2, e3) ->
          pp oc "(PeekWord %a %a %a)" print_endianness e1 p e2 p e3
      | PeekDWord (e1, e2, e3) ->
          pp oc "(PeekDWord %a %a %a)" print_endianness e1 p e2 p e3
      | PeekQWord (e1, e2, e3) ->
          pp oc "(PeekQWord %a %a %a)" print_endianness e1 p e2 p e3
      | PeekOWord (e1, e2, e3) ->
          pp oc "(PeekOWord %a %a %a)" print_endianness e1 p e2 p e3
      | WriteByte (e1, e2) ->
          pp oc "(WriteByte %a %a)" p e1 p e2
      | WriteWord (e1, e2, e3) ->
          pp oc "(WriteWord %a %a %a)" print_endianness e1 p e2 p e3
      | WriteDWord (e1, e2, e3) ->
          pp oc "(WriteDWord %a %a %a)" print_endianness e1 p e2 p e3
      | WriteQWord (e1, e2, e3) ->
          pp oc "(WriteQWord %a %a %a)" print_endianness e1 p e2 p e3
      | WriteOWord (e1, e2, e3) ->
          pp oc "(WriteOWord %a %a %a)" print_endianness e1 p e2 p e3
      | WriteBytes (e1, e2) ->
          pp oc "(WriteBytes %a %a)" p e1 p e2
      | PokeByte (e1, e2) ->
          pp oc "(PokeByte %a %a)" p e1 p e2
      | BlitBytes (e1, e2, e3) ->
          pp oc "(BlitBytes %a %a %a)" p e1 p e2 p e3
      | DataPtrAdd (e1, e2) ->
          pp oc "(DataPtrAdd %a %a)" p e1 p e2
      | DataPtrSub (e1, e2) ->
          pp oc "(DataPtrSub %a %a)" p e1 p e2
      | RemSize e ->
          pp oc "(RemSize %a)" p e
      | And (e1, e2) ->
          pp oc "(And %a %a)" p e1 p e2
      | Or (e1, e2) ->
          pp oc "(Or %a %a)" p e1 p e2
      | Not e ->
          pp oc "(Not %a)" p e
      | ToU8 e ->
          pp oc "(ToU8%a)" p e
      | ToI8 e ->
          pp oc "(ToI8 %a)" p e
      | ToU16 e ->
          pp oc "(ToU16 %a)" p e
      | ToI16 e ->
          pp oc "(ToI16 %a)" p e
      | ToU24 e ->
          pp oc "(ToU24 %a)" p e
      | ToI24 e ->
          pp oc "(ToI24 %a)" p e
      | ToU32 e ->
          pp oc "(ToU32 %a)" p e
      | ToI32 e ->
          pp oc "(ToI32 %a)" p e
      | ToU40 e ->
          pp oc "(ToU40 %a)" p e
      | ToI40 e ->
          pp oc "(ToI40 %a)" p e
      | ToU48 e ->
          pp oc "(ToU48 %a)" p e
      | ToI48 e ->
          pp oc "(ToI48 %a)" p e
      | ToU56 e ->
          pp oc "(ToU56 %a)" p e
      | ToI56 e ->
          pp oc "(ToI56 %a)" p e
      | ToU64 e ->
          pp oc "(ToU64 %a)" p e
      | ToI64 e ->
          pp oc "(ToI64 %a)" p e
      | ToU128 e ->
          pp oc "(ToU128 %a)" p e
      | ToI128 e ->
          pp oc "(ToI128 %a)" p e
      | AllocValue vtyp ->
          pp oc "(AllocValue %a)" ValueType.print vtyp
      | DerefValuePtr e ->
          pp oc "(DerefValuePtr %a)" p e
      | SetField (path, e1, e2) ->
          pp oc "(SetField %a %a %a)" ValueType.print_path path p e1 p e2
      | FieldIsNull (path, e) ->
          pp oc "(FieldIsNull %a %a)" ValueType.print_path path p e
      | GetField (path, e) ->
          pp oc "(GetField %a %a)" ValueType.print_path path p e
      | Pair (e1, e2) ->
          pp oc "(Pair %a %a)" p e1 p e2
      | Fst e ->
          pp oc "(Fst %a)" p e
      | Snd e ->
          pp oc "(Snd %a)" p e
      | MapPair (e1, e2) ->
          pp oc "(MapPair %a %a)" p e1 p e2
      | Identifier n ->
          (* Do not repeat the expression, that we keep only for knowing the type of this expression: *)
          pp oc "(Identifier %s)" n
      | Let (n, e1, e2) ->
          pp oc "(Let %s %a %a)" n p e1 p e2
      | Function (id, ts, e) ->
          pp oc "(Function %d %a %a)"
            id (Array.print ~first:"" ~last:"" ~sep:" " Type.print) ts p e
      | Param (fid, n) ->
          pp oc "(Param %d %d)" fid n
      | Choose (e1, e2, e3) ->
          pp oc "(Choose %a %a %a)" p e1 p e2 p e3
      | ReadWhile (e1, e2, e3, e4) ->
          pp oc "(ReadWord %a %a %a %a)" p e1 p e2 p e3 p e4
      | LoopWhile (e1, e2, e3) ->
          pp oc "(LoopWhile %a %a %a)" p e1 p e2 p e3
      | LoopUntil (e1, e2, e3) ->
          pp oc "(LoopUntil %a %a %a)" p e1 p e2 p e3
      | Repeat (e1, e2, e3, e4) ->
          pp oc "(Repeat %a %a %a %a)" p e1 p e2 p e3 p e4

  exception Type_error of e * e * typ * string
  exception Type_error_param of e * e * int * typ * string
  exception Type_error_path of e * e * path * string

  let rec vtype_of_valueptr e0 l e =
    match type_of l e with
    | Type.ValuePtr vt -> vt
    | t -> raise (Type_error (e0, e, t, "be a ValuePtr"))

  (* [e] must have been type checked already: *)
  and type_of l e0 =
    match e0 with
    | Dump _
    | Seq [] ->
        void
    | Seq es ->
        type_of l (List.last es)
    | Comment (_, e)
    | Coalesce (_, e)
    | Add (e, _)
    | Sub (e, _)
    | Mul (e, _)
    | Div (e, _)
    | Rem (e, _)
    | LogAnd (e, _)
    | LogOr (e, _)
    | LogXor (e, _)
    | LeftShift (e, _)
    | RightShift (e, _)
    | LogNot e ->
        type_of l e
    | Nullable e ->
        Type.to_nullable (type_of l e)
    | NotNullable e ->
        Type.to_not_nullable (type_of l e)
    | IsNull _ -> bool
    | Null t -> Value (Nullable t)
    | Float _ -> float
    | String _ -> string
    | Bool _ -> bool
    | Char _ -> char
    | U8 _ -> u8
    | U16 _ -> u16
    | U24 _ -> u24
    | U32 _ -> u32
    | U40 _ -> u40
    | U48 _ -> u48
    | U56 _ -> u56
    | U64 _ -> u64
    | U128 _ -> u128
    | I8 _ -> i8
    | I16 _ -> i16
    | I24 _ -> i24
    | I32 _ -> i32
    | I40 _ -> i40
    | I48 _ -> i48
    | I56 _ -> i56
    | I64 _ -> i64
    | I128 _ -> i128
    | Bit _ -> bit
    | Size _ -> size
    | Byte _ -> byte
    | Word _ -> word
    | DWord _ -> dword
    | QWord _ -> qword
    | OWord _ -> oword
    | Gt _ -> bool
    | Ge _ -> bool
    | Eq _ -> bool
    | Ne _ -> bool
    | StringOfFloat _
    | StringOfChar _
    | StringOfInt _ -> string
    | CharOfString _ -> char
    | FloatOfString _ -> float
    | U8OfString _ -> u8
    | U16OfString _ -> u16
    | U24OfString _ -> u24
    | U32OfString _ -> u32
    | U40OfString _ -> u40
    | U48OfString _ -> u48
    | U56OfString _ -> u56
    | U64OfString _ -> u64
    | U128OfString _ -> u128
    | I8OfString _ -> i8
    | I16OfString _ -> i16
    | I24OfString _ -> i24
    | I32OfString _ -> i32
    | I40OfString _ -> i40
    | I48OfString _ -> i48
    | I56OfString _ -> i56
    | I64OfString _ -> i64
    | I128OfString _ -> i128
    | FloatOfQWord _ -> float
    | QWordOfFloat _ -> qword
    | U8OfByte _ -> u8
    | ByteOfU8 _ -> byte
    | U16OfWord _ -> u16
    | WordOfU16 _ -> word
    | U32OfDWord _ -> u32
    | DWordOfU32 _ -> dword
    | U64OfQWord _ -> u64
    | QWordOfU64 _ -> qword
    | U128OfOWord _ -> u128
    | OWordOfU128 _ -> oword
    | U8OfChar _ -> u8
    | CharOfU8 _ -> char
    | SizeOfU32 _ -> size
    | U32OfSize _ -> u32
    | BitOfBool _ -> bit
    | BoolOfBit _ -> bool
    | U8OfBool _ -> u8
    | BoolOfU8 _ -> bool
    | AppendBytes _ -> bytes
    | AppendString _ -> string
    | StringLength _ -> u32
    | StringOfBytes _ -> string
    | BytesOfString _ -> bytes
    | ListLength _ -> u32
    | DataPtrOfString _ -> dataptr
    | TestBit _ -> bit
    | SetBit _ -> dataptr
    | ReadByte _ -> pair byte dataptr
    | ReadWord _ -> pair word dataptr
    | ReadDWord _ -> pair dword dataptr
    | ReadQWord _ -> pair qword dataptr
    | ReadOWord _ -> pair oword dataptr
    | ReadBytes _ -> pair bytes dataptr
    | PeekByte _ -> byte
    | PeekWord _ -> word
    | PeekDWord _ -> dword
    | PeekQWord _ -> qword
    | PeekOWord _ -> oword
    | WriteByte _ -> dataptr
    | WriteWord _ -> dataptr
    | WriteDWord _ -> dataptr
    | WriteQWord _ -> dataptr
    | WriteOWord _ -> dataptr
    | WriteBytes _ -> dataptr
    | PokeByte _ -> dataptr
    | BlitBytes _ -> dataptr
    | DataPtrAdd _ -> dataptr
    | DataPtrSub _ -> size
    | RemSize _ -> size
    | And _ -> bool
    | Or _ -> bool
    | Not _ -> bool
    | ToU8 _ -> i8
    | ToI8 _ -> u8
    | ToU16 _ -> u16
    | ToI16 _ -> i16
    | ToU24 _ -> u24
    | ToI24 _ -> i24
    | ToU32 _ -> u32
    | ToI32 _ -> i32
    | ToU40 _ -> u40
    | ToI40 _ -> i40
    | ToU48 _ -> u48
    | ToI48 _ -> i48
    | ToU56 _ -> u56
    | ToI56 _ -> i56
    | ToU64 _ -> u64
    | ToI64 _ -> i64
    | ToU128 _ -> u128
    | ToI128 _ -> i128
    | AllocValue vtyp -> valueptr vtyp
    | DerefValuePtr e ->
        Type.Value (vtype_of_valueptr e0 l e)
    | SetField (_, e1, _) -> type_of l e1
    | FieldIsNull _ -> bool
    | GetField (path, e) ->
        let vt = vtype_of_valueptr e0 l e in
        Type.Value (ValueType.type_of_path vt path)
    | Pair (e1, e2) ->
        pair (type_of l e1) (type_of l e2)
    | Fst e ->
        (match type_of l e with
        | Type.Pair (t, _) -> t
        | t -> raise (Type_error (e0, e, t, "be a pair")))
    | Snd e ->
        (match type_of l e with
        | Type.Pair (_, t) -> t
        | t -> raise (Type_error (e0, e, t, "be a pair")))
    | MapPair (_, e) ->
        (match type_of l e with
        | Function (_, t) -> t
        | t -> raise (Type_error (e0, e, t, "be a function")))
    | Identifier n as e ->
        (try List.assoc e l
        with Not_found ->
            Printf.eprintf "Cannot find identifier %S in %a\n%!"
              n (List.print (fun oc (e, _) -> print oc e)) l ;
            raise Not_found)
    | Let (n, e1, e2) ->
        type_of ((Identifier n, (type_of l e1))::l) e2
    | Function (fid, ts, e) ->
        let l = Array.fold_lefti (fun l i t -> (Param (fid, i), t)::l) l ts in
        Type.Function (ts, type_of l e)
    | Param (_, n) as e ->
        (try List.assoc e l
        with Not_found ->
            Printf.eprintf "Cannot find parameter %d in %a\n%!"
              n (List.print (fun oc (e, _) -> print oc e)) l ;
            raise Not_found)
    | Choose (_, e, _) -> type_of l e
    | ReadWhile (_, _, e, _) -> pair (type_of l e) dataptr
    | LoopWhile (_, _, e) -> type_of l e
    | LoopUntil (_, _, e) -> type_of l e
    | Repeat (_, _, _, e) -> type_of l e

  (* depth last, pass the list of bound identifiers along the way: *)
  let rec fold u l f e =
    let u = f u l e in
    match e with
    | Null _
    | Float _
    | String _
    | Bool _
    | Char _
    | U8 _
    | U24 _
    | U16 _
    | U32 _
    | U40 _
    | U48 _
    | U56 _
    | U64 _
    | U128 _
    | I8 _
    | I16 _
    | I24 _
    | I32 _
    | I40 _
    | I48 _
    | I56 _
    | I64 _
    | I128 _
    | Bit _
    | Size _
    | Byte _
    | Word _
    | DWord _
    | QWord _
    | OWord _
    | Param _
    | DataPtrOfString _
    | AllocValue _
    | Identifier _ ->
        u
    | Dump e
    | DerefValuePtr e
    | Comment (_, e)
    | IsNull e
    | Nullable e
    | NotNullable e
    | LogNot e
    | StringOfInt e
    | StringOfChar e
    | FloatOfQWord e
    | QWordOfFloat e
    | StringOfFloat e
    | FloatOfString e
    | CharOfString e
    | U8OfString e
    | U16OfString e
    | U24OfString e
    | U32OfString e
    | U40OfString e
    | U48OfString e
    | U56OfString e
    | U64OfString e
    | U128OfString e
    | I8OfString e
    | I16OfString e
    | I24OfString e
    | I32OfString e
    | I40OfString e
    | I48OfString e
    | I56OfString e
    | I64OfString e
    | I128OfString e
    | U8OfByte e
    | ByteOfU8 e
    | U16OfWord e
    | WordOfU16 e
    | U32OfDWord e
    | DWordOfU32 e
    | U64OfQWord e
    | QWordOfU64 e
    | U128OfOWord e
    | OWordOfU128 e
    | U8OfChar e
    | CharOfU8 e
    | SizeOfU32 e
    | U32OfSize e
    | BitOfBool e
    | BoolOfBit e
    | U8OfBool e
    | BoolOfU8 e
    | StringLength e
    | StringOfBytes e
    | BytesOfString e
    | ListLength e
    | ReadByte e
    | ReadWord (_, e)
    | ReadDWord (_, e)
    | ReadQWord (_, e)
    | ReadOWord (_, e)
    | RemSize e
    | Not e
    | ToU8 e
    | ToI8 e
    | ToU16 e
    | ToI16 e
    | ToU24 e
    | ToI24 e
    | ToU32 e
    | ToI32 e
    | ToU40 e
    | ToI40 e
    | ToU48 e
    | ToI48 e
    | ToU56 e
    | ToI56 e
    | ToU64 e
    | ToI64 e
    | ToU128 e
    | ToI128 e
    | FieldIsNull (_, e)
    | GetField (_, e)
    | Fst e
    | Snd e ->
        fold u l f e
    | Coalesce (e1, e2)
    | Gt (e1, e2)
    | Ge (e1, e2)
    | Eq (e1, e2)
    | Ne (e1, e2)
    | Add (e1, e2)
    | Sub (e1, e2)
    | Mul (e1, e2)
    | Div (e1, e2)
    | Rem (e1, e2)
    | LogAnd (e1, e2)
    | LogOr (e1, e2)
    | LogXor (e1, e2)
    | LeftShift (e1, e2)
    | RightShift (e1, e2)
    | AppendBytes (e1, e2)
    | AppendString (e1, e2)
    | TestBit (e1, e2)
    | ReadBytes (e1, e2)
    | PeekByte (e1, e2)
    | WriteByte (e1, e2)
    | PeekWord (_, e1, e2)
    | PeekDWord (_, e1, e2)
    | PeekQWord (_, e1, e2)
    | PeekOWord (_, e1, e2)
    | WriteWord (_, e1, e2)
    | WriteDWord (_, e1, e2)
    | WriteQWord (_, e1, e2)
    | WriteOWord (_, e1, e2)
    | WriteBytes (e1, e2)
    | PokeByte (e1, e2)
    | DataPtrAdd (e1, e2)
    | DataPtrSub (e1, e2)
    | And (e1, e2)
    | Or (e1, e2)
    | SetField (_, e1, e2)
    | Pair (e1, e2)
    | MapPair (e1, e2) ->
        fold (fold u l f e1) l f e2
    | Let (s, e1, e2) ->
        fold (fold u l f e1) ((Identifier s, type_of l e1)::l) f e2
    | SetBit (e1, e2, e3)
    | BlitBytes (e1, e2, e3)
    | Choose (e1, e2, e3)
    | LoopWhile (e1, e2, e3)
    | LoopUntil (e1, e2, e3) ->
        fold (fold (fold u l f e1) l f e2) l f e3
    | ReadWhile (e1, e2, e3, e4)
    | Repeat (e1, e2, e3, e4) ->
        fold (fold (fold (fold u l f e1) l f e2) l f e3) l f e4
    | Seq es ->
        List.fold_left (fun u e -> fold u l f e) u es
    | Function (id, ts, e) ->
        let l = Array.fold_lefti (fun l i t -> (Param (id, i), t)::l) l ts in
        fold u l f e

  let rec type_check l e =
    fold () l (fun () l e0 ->
      let check_void l e =
        match type_of l e with
        | Type.Void -> ()
        | t -> raise (Type_error (e0, e, t, "be Void")) in
      let check_nullable l e =
        match type_of l e with
        | Type.Value ValueType.(Nullable _) -> ()
        | t -> raise (Type_error (e0, e, t, "be nullable")) in
      let check_not_nullable l e =
        match type_of l e with
        | Type.Value ValueType.(NotNullable _) -> ()
        | t -> raise (Type_error (e0, e, t, "not be nullable")) in
      let check_comparable l e =
        match type_of l e with
        | Type.Size | Byte | Word | DWord | QWord | OWord
        | Value ValueType.(NotNullable (Float | String | Char |
            U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128)) -> ()
        | t -> raise (Type_error (e0, e, t, "be comparable")) in
      let check_numeric l e =
        match type_of l e with
        | Type.Size | Byte | Word | DWord | QWord | OWord
        | Value ValueType.(NotNullable (Float | Char |
            U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128)) -> ()
        | t -> raise (Type_error (e0, e, t, "be numeric")) in
      let check_integer l e =
        match type_of l e with
        | Type.Size | Byte | Word | DWord | QWord | OWord
        | Value ValueType.(NotNullable (
            U8 | U16 | U32 | U64 | U128 | I8 | I16 | I32 | I64 | I128)) -> ()
        | t -> raise (Type_error (e0, e, t, "be an integer")) in
      let check_param fe n act exp =
        if act <> exp then
          let expected = IO.to_string Type.print act in
          raise (Type_error_param (e0, fe, n, act, "be a "^ expected)) in
      let check_eq l e exp =
        let act = type_of l e in
        if act <> exp then
          let expected = IO.to_string Type.print exp in
          raise (Type_error (e0, e, act, "be a "^ expected)) in
      let check_same_types l e1 e2 =
        let t1 = type_of l e1 in
        check_eq l e2 t1 in
      let check_list l e =
        match type_of l e with
        | Type.Value ValueType.(NotNullable List _) -> ()
        | t -> raise (Type_error (e0, e, t, "be a list")) in
      let check_pair l e =
        match type_of l e with
        | Type.Pair _ -> ()
        | t -> raise (Type_error (e0, e, t, "be a pair")) in
      let bad_arity expected e t =
        let s = Printf.sprintf "be a function of %d parameters" expected in
        raise (Type_error (e0, e, t, s)) in
      let check_function arity l e =
        match type_of l e with
        | Type.Function (ts, _) as t ->
            if Array.length ts <> arity then bad_arity arity e t
        | t -> raise (Type_error (e0, e, t, "be a function")) in
      let check_params1 l e f =
        match type_of l e with
        | Type.Function ([|t1|], t2) -> f t1 t2
        | t -> bad_arity 1 e t in
      let check_params2 l e f =
        match type_of l e with
        | Type.Function ([|t1; t2|], t3) -> f t1 t2 t3
        | t -> bad_arity 2 e t in
      let check_valueptr l e  =
        match type_of l e with
        | Type.ValuePtr _ -> ()
        | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
      let check_valueptr_path_same_types l e1 path e2 =
        match type_of l e1 with
        | Type.ValuePtr vt ->
            let exp = Type.Value (ValueType.type_of_path vt path) in
            check_eq l e2 exp
        | t -> raise (Type_error (e0, e1, t, "be a ValuePtr")) in
      let check_valueptr_path l e path =
        match type_of l e with
        | Type.ValuePtr vt ->
            (try ignore (ValueType.type_of_path vt path)
            with _ ->
              let s = IO.to_string ValueType.print vt in
              raise (Type_error_path (e0, e, path, "stay within "^ s)))
        | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
      let check_valueptr_path_nullable l e path nullable =
        match type_of l e with
        | Type.ValuePtr vt ->
            let vt = ValueType.type_of_path vt path in
            let act = ValueType.is_nullable vt in
            if act <> nullable then
              let expected = (if nullable then "" else "not") ^" nullable" in
              raise (Type_error_path (e0, e, path, "be "^ expected))
        | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
      match e0 with
      | Comment _
      | Dump _
      | Null _
      | Float _
      | String _
      | Bool _
      | Char _
      | U8 _
      | U16 _
      | U24 _
      | U32 _
      | U40 _
      | U48 _
      | U56 _
      | U64 _
      | U128 _
      | I8 _
      | I16 _
      | I24 _
      | I32 _
      | I40 _
      | I48 _
      | I56 _
      | I64 _
      | I128 _
      | Bit _
      | Size _
      | Byte _
      | Word _
      | DWord _
      | QWord _
      | OWord _
      | DataPtrOfString _
      | AllocValue _
      | Pair _
      | Identifier _
      | Let _
      | Param _
      | Function _ ->
          ()
      | Seq es ->
          let rec loop = function
            | [] | [_] -> ()
            | e::es -> check_void l e ; loop es in
          loop es
      | IsNull e ->
          check_nullable l e
      | Coalesce (e1, e2) ->
          check_nullable l e1 ;
          check_not_nullable l e2
      | Nullable e ->
          check_not_nullable l e
      | NotNullable e ->
          check_nullable l e
      | Gt (e1, e2)
      | Ge (e1, e2)
      | Eq (e1, e2)
      | Ne (e1, e2) ->
          check_comparable l e1 ;
          check_same_types l e1 e2
      | Add (e1, e2)
      | Sub (e1, e2)
      | Mul (e1, e2)
      | Div (e1, e2)
      | Rem (e1, e2) ->
          check_numeric l e1 ;
          check_same_types l e1 e2
      | LogAnd (e1, e2)
      | LogOr (e1, e2)
      | LogXor (e1, e2) ->
          check_integer l e1 ;
          check_same_types l e1 e2
      | LeftShift (e1, e2)
      | RightShift (e1, e2) ->
          check_integer l e1 ;
          check_eq l e2 u8
      | LogNot e
      | StringOfInt e ->
          check_integer l e
      | StringOfChar e
      | U8OfChar e ->
          check_eq l e char
      | FloatOfString e
      | CharOfString e
      | U8OfString e
      | U16OfString e
      | U24OfString e
      | U32OfString e
      | U40OfString e
      | U48OfString e
      | U56OfString e
      | U64OfString e
      | U128OfString e
      | I8OfString e
      | I16OfString e
      | I24OfString e
      | I32OfString e
      | I40OfString e
      | I48OfString e
      | I56OfString e
      | I64OfString e
      | I128OfString e
      | StringLength e
      | BytesOfString e ->
          check_eq l e string
      | FloatOfQWord e
      | U64OfQWord e ->
          check_eq l e qword
      | QWordOfFloat e
      | StringOfFloat e ->
          check_eq l e float
      | U8OfByte e ->
          check_eq l e byte
      | CharOfU8 e
      | ByteOfU8 e
      | BoolOfU8 e ->
          check_eq l e u8
      | ToU8 e
      | ToI8 e
      | ToI16 e
      | ToU16 e
      | ToI24 e
      | ToU24 e
      | ToI32 e
      | ToU32 e
      | ToI40 e
      | ToU40 e
      | ToI48 e
      | ToU48 e
      | ToI56 e
      | ToU56 e
      | ToI64 e
      | ToU64 e
      | ToI128 e
      | ToU128 e ->
          check_integer l e
      | U16OfWord e ->
          check_eq l e word
      | WordOfU16 e ->
          check_eq l e u16
      | U32OfDWord e ->
          check_eq l e dword
      | DWordOfU32 e
      | SizeOfU32 e ->
          check_eq l e u32
      | QWordOfU64 e ->
          check_eq l e i64
      | OWordOfU128 e ->
          check_eq l e u128
      | U128OfOWord e ->
          check_eq l e oword
      | U32OfSize e ->
          check_eq l e size
      | BitOfBool e
      | U8OfBool e
      | Not e ->
          check_eq l e bool
      | BoolOfBit e ->
          check_eq l e bit
      | AppendBytes (e1, e2) ->
          check_eq l e1 bytes ;
          check_eq l e2 bytes
      | AppendString (e1, e2) ->
          check_eq l e1 string ;
          check_eq l e2 string
      | StringOfBytes e ->
          check_eq l e bytes
      | ListLength e ->
          check_list l e
      | TestBit (e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 size
      | SetBit (e1, e2, e3) ->
          check_eq l e1 dataptr ;
          check_eq l e2 u32 ;
          check_eq l e3 bit
      | ReadByte e
      | ReadWord (_, e)
      | ReadDWord (_, e)
      | ReadQWord (_, e)
      | ReadOWord (_, e) ->
          check_eq l e dataptr
      | ReadBytes (e1, e2)
      | PeekByte (e1, e2)
      | PeekWord (_, e1, e2)
      | PeekDWord (_, e1, e2)
      | PeekQWord (_, e1, e2)
      | PeekOWord (_, e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 size
      | WriteByte (e1, e2)
      | PokeByte (e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 byte
      | WriteWord (_, e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 word
      | WriteDWord (_, e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 dword
      | WriteQWord (_, e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 qword
      | WriteOWord (_, e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 oword
      | WriteBytes (e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 bytes
      | BlitBytes (e1, e2, e3) ->
          check_eq l e1 dataptr ;
          check_eq l e2 bytes ;
          check_eq l e3 size
      | DataPtrAdd (e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 size
      | DataPtrSub (e1, e2) ->
          check_eq l e1 dataptr ;
          check_eq l e2 dataptr
      | RemSize e ->
          check_eq l e dataptr ;
      | And (e1, e2)
      | Or (e1, e2) ->
          check_eq l e1 bool ;
          check_eq l e2 bool
      | DerefValuePtr e1 ->
          check_valueptr l e1
      | SetField (path, e1, e2) ->
          check_valueptr_path_same_types l e1 path e2
      | FieldIsNull (path, e) ->
          check_valueptr_path_nullable l e path true
      | GetField (path, e) ->
          check_valueptr_path l e path
      | Fst e ->
          check_pair l e
      | Snd e ->
          check_pair l e
      | MapPair (e1, e2) ->
          check_pair l e1 ;
          check_function 2 l e2
      | Choose (e1, e2, e3) ->
          check_eq l e1 bool ;
          check_same_types l e2 e3
      | ReadWhile (e1, e2, e3, e4) ->
          check_params1 l e1 (fun t1 t2 ->
            check_param e1 0 t1 byte ;
            check_param e1 1 t2 bool) ;
          check_params2 l e2 (fun t1 t2 t3 ->
            check_eq l e3 t1 ;
            check_param e2 1 t2 byte ;
            check_eq l e3 t3) ;
          check_eq l e4 dataptr
      | LoopWhile (e1, e2, e3) ->
          check_params1 l e1 (fun t1 t2 ->
            check_eq l e3 t1 ;
            check_param e1 1 t2 bool) ;
          check_params1 l e2 (fun t1 t2 ->
            check_eq l e3 t1 ;
            check_eq l e3 t2)
      | LoopUntil (e1, e2, e3) ->
          check_params1 l e1 (fun t1 t2 ->
            check_eq l e3 t1 ;
            check_eq l e3 t2) ;
          check_params1 l e2 (fun t1 t2 ->
            check_eq l e3 t1 ;
            check_param e2 1 t2 bool) ;
      | Repeat (e1, e2, e3, e4) ->
          check_eq l e1 i32 ;
          check_eq l e2 i32 ;
          check_params2 l e3 (fun t1 t2 t3 ->
            check_param e3 0 t1 i32 ;
            check_eq l e4 t2 ;
            check_eq l e4 t3)
    ) e

  let () =
    let max_depth = 3 in
    Printexc.register_printer (function
      | Type_error (e0, e, t, s) ->
          Some (
            Printf.sprintf2
              "Type Error: In expression %a, expression %a should %s but is a %a"
              (print ~max_depth) e0 (print ~max_depth) e s Type.print t)
      | Type_error_param (e0, e, n, t, s) ->
          Some (
            Printf.sprintf2
              "Type Error: In expression %a, parameter %d of expression %a \
               should %s but is a %a"
              (print ~max_depth) e0 n (print ~max_depth) e s Type.print t)
      | Type_error_path (e0, e, path, s) ->
          Some (
            Printf.sprintf2
              "Type Error: In expression %a, path %a of expression %a should %s"
              (print ~max_depth) e0 ValueType.print_path path (print ~max_depth) e s)
      | _ ->
          None)

  (*
   * Some helpers to deal with expressions:
   *)

  let gen_id =
    let seq = ref (-1) in
    fun () ->
      incr seq ;
      "gen"^ string_of_int !seq

  let with_sploded_pair what e f =
    let pair_id = gen_id () ^"_"^ what in
    let n1 = pair_id ^"_0"
    and n2 = pair_id ^"_1" in
    Let (pair_id, e,
      Let (n1, Fst (Identifier pair_id),
        Let (n2, Snd (Identifier pair_id),
          f (Identifier n1) (Identifier n2))))

  (*$inject
    let vptr = Type.ValuePtr ValueType.(Nullable String)
    let func2 =
      Function (14, [|vptr; DataPtr|],
        Let ("gen9_ds", Pair (GetField ([], Param (14, 0)),
                              Param (14, 0)),
          Let ("gen9_ds_0", Fst (Identifier "gen9_ds"),
            Let ("gen9_ds_1", Snd (Identifier "gen9_ds"),
              Pair (Identifier "gen9_ds_1",
                    Comment ("Serialize a String",
                      WriteByte (
                        WriteBytes (
                          WriteByte (Param (14, 1), ByteOfU8 (U8OfChar (Char '"'))),
                          BytesOfString (NotNullable (Identifier "gen9_ds_0"))),
                        ByteOfU8 (U8OfChar (Char '"')))))))))
  *)
  (*$= type_of & ~printer:(BatIO.to_string Type.print)
    (Function ([|vptr; DataPtr|], Pair (vptr, DataPtr))) (type_of [] func2)
  *)

  (*$>*)
end

type e = Expression.e

(* Given the above we can built interesting expressions.
 * A DES(serializer) is a module that implements a particular serialization
 * format in such a way that it provides simple expressions for the basic
 * types that can then be assembled to build either a deserializer from
 * DataPtr to a heap value, or a converter between two formats. *)
module type DES =
sig
  (* No need for a backend (BE) since we merely compute expressions *)
  (* RW state passed to every deserialization operations *)
  type state
  val ptr : ValueType.t -> Type.t (* either dataptr or valueptr *)

  val start : vtyp -> (*ptr*) e -> state * (*ptr*) e
  val stop : state -> (*ptr*) e -> (*ptr*) e

  (* A basic value deserializer takes a state, an expression
   * yielding a pointer (either a CodePtr pointing at a byte stream or a
   * ValuePtr pointing at a heap value of type ['b]), and returns two
   * expressions: one yielding the advanced pointer (of the exact same type) and
   * one yielding the value that's been deserialized from the given location: *)
  (* FIXME: make this type "private": *)
  type des = state -> (*ptr*) e -> (* (nn * ptr) *) e

  val dfloat : des
  val dstring : des
  val dbool : des
  val dchar : des
  val di8 : des
  val di16 : des
  val di24 : des
  val di32 : des
  val di40 : des
  val di48 : des
  val di56 : des
  val di64 : des
  val di128 : des
  val du8 : des
  val du16 : des
  val du24 : des
  val du32 : des
  val du40 : des
  val du48 : des
  val du56 : des
  val du64 : des
  val du128 : des

  val tup_opn : state -> vtyp array -> (*ptr*) e -> (*ptr*) e
  val tup_cls : state -> (*ptr*) e -> (*ptr*) e
  val tup_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val rec_opn : state -> (string * vtyp) array -> (*ptr*) e -> (*ptr*) e
  val rec_cls : state -> (*ptr*) e -> (*ptr*) e
  val rec_sep : string (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val vec_opn : state -> (*dim*) int -> vtyp -> (*ptr*) e -> (*ptr*) e
  val vec_cls : state -> (*ptr*) e -> (*ptr*) e
  val vec_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val list_opn : state -> vtyp -> (*ptr*) e -> (* (nn * ptr) *) e
  val list_cls : state -> (*ptr*) e -> (*ptr*) e
  val list_sep : state -> (*ptr*) e -> (*ptr*) e

  val is_null : state -> (*ptr*) e -> (*bool*) e
  val dnull : ValueType.not_nullable -> state -> (*ptr*) e -> (*ptr*) e
  val dnotnull : ValueType.not_nullable -> state -> (*ptr*) e -> (*ptr*) e
end

(* Same goes for SER(rializers), with the addition that it is also possible to
 * "serialize" into a heap value instead of a data stream:
 * (note: "ssize" stands for "serialized size") *)
type ssize = ConstSize of int | DynSize of (*size*) e

module type SER =
sig
  (* RW state passed to every serialization operations *)
  type state
  val ptr : ValueType.t -> Type.t (* either dataptr or valueptr *)

  val start : vtyp -> (*ptr*) e -> state * (*ptr*) e
  val stop : state -> (*ptr*) e -> (*ptr*) e

  (* FIXME: make this type "private": *)
  type ser = state -> (*nn*) e -> (*ptr*) e -> (*ptr*) e

  val sfloat : ser
  val sstring : ser
  val sbool : ser
  val schar : ser
  val si8 : ser
  val si16 : ser
  val si24 : ser
  val si32 : ser
  val si40 : ser
  val si48 : ser
  val si56 : ser
  val si64 : ser
  val si128 : ser
  val su8 : ser
  val su16 : ser
  val su24 : ser
  val su32 : ser
  val su40 : ser
  val su48 : ser
  val su56 : ser
  val su64 : ser
  val su128 : ser

  val tup_opn : state -> vtyp array -> (*ptr*) e -> (*ptr*) e
  val tup_cls : state -> (*ptr*) e -> (*ptr*) e
  val tup_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val rec_opn : state -> (string * vtyp) array -> (*ptr*) e -> (*ptr*) e
  val rec_cls : state -> (*ptr*) e -> (*ptr*) e
  val rec_sep : string (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val vec_opn : state -> (*dim*) int -> vtyp -> (*ptr*) e -> (*ptr*) e
  val vec_cls : state -> (*ptr*) e -> (*ptr*) e
  val vec_sep : int (* before *) -> state -> (*ptr*) e -> (*ptr*) e
  val list_opn : state -> vtyp -> (*ptr*) e -> (*nn*) e -> (*ptr*) e
  val list_cls : state -> (*ptr*) e -> (*ptr*) e
  val list_sep : state -> (*ptr*) e -> (*ptr*) e

  val nullable : state -> (*ptr*) e -> (*ptr*) e
  val snull : ValueType.not_nullable -> state -> (*ptr*) e -> (*ptr*) e
  val snotnull : ValueType.not_nullable -> state -> (*ptr*) e -> (*ptr*) e

  (* Sometimes, we'd like to know in advance how large a serialized value is
   * going to be. Value must have been deserialized into a heap value. *)
  type ssizer = vtyp -> path -> (*value*) e -> ssize
  val ssize_of_float : ssizer
  val ssize_of_string : ssizer
  val ssize_of_bool : ssizer
  val ssize_of_char : ssizer
  val ssize_of_i8 : ssizer
  val ssize_of_i16 : ssizer
  val ssize_of_i24 : ssizer
  val ssize_of_i32 : ssizer
  val ssize_of_i40 : ssizer
  val ssize_of_i48 : ssizer
  val ssize_of_i56 : ssizer
  val ssize_of_i64 : ssizer
  val ssize_of_i128 : ssizer
  val ssize_of_u8 : ssizer
  val ssize_of_u16 : ssizer
  val ssize_of_u24 : ssizer
  val ssize_of_u32 : ssizer
  val ssize_of_u40 : ssizer
  val ssize_of_u48 : ssizer
  val ssize_of_u56 : ssizer
  val ssize_of_u64 : ssizer
  val ssize_of_u128 : ssizer
  (* Specifically for the compound, excluding the size of the parts: *)
  val ssize_of_tup : ssizer
  val ssize_of_rec : ssizer
  val ssize_of_vec : ssizer
  val ssize_of_list : ssizer
  val ssize_of_null : vtyp -> path -> ssize
end

(* Now we can combine a DES and a SER to create a converter from one format
 * into another (including heap values, with some trickery) *)
module DesSer (Des : DES) (Ser : SER) =
struct
  open Expression

  (* Most of the functions below return the src and dst pointers advanced to
   * point to the next value to read/write.
   * [vtyp] denotes the ValueType.t of the current subfields, whereas
   * [vtyp0] denotes the ValueType.t of the whole value. *)
  let ds ser des typ sstate dstate vtyp0 src_dst =
    let what = IO.to_string Type.print typ in
    let src_dst = Comment ("Desserialize a "^ what, src_dst) in
    MapPair (src_dst,
      func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid ->
        let v_src = des dstate (Param (fid, 0)) in
        let dst = Param (fid, 1) in
        with_sploded_pair "ds" v_src (fun v src ->
          (* dessser handle nulls itself, so that DES/SER implementations
           * do not have to care for nullability. NotNullable is just a cast
           * and has no other effect outside of type_check. *)
          Pair (
            src,
            Comment ("Serialize a "^ what, ser sstate v dst)))))

  let dsfloat = ds Ser.sfloat Des.dfloat float
  let dsstring = ds Ser.sstring Des.dstring string
  let dsbool = ds Ser.sbool Des.dbool bool
  let dschar = ds Ser.schar Des.dchar char
  let dsi8 = ds Ser.si8 Des.di8 i8
  let dsi16 = ds Ser.si16 Des.di16 i16
  let dsi24 = ds Ser.si24 Des.di24 i24
  let dsi32 = ds Ser.si32 Des.di32 i32
  let dsi40 = ds Ser.si40 Des.di40 i40
  let dsi48 = ds Ser.si48 Des.di48 i48
  let dsi56 = ds Ser.si56 Des.di56 i56
  let dsi64 = ds Ser.si64 Des.di64 i64
  let dsi128 = ds Ser.si128 Des.di128 i128
  let dsu8 = ds Ser.su8 Des.du8 u8
  let dsu16 = ds Ser.su16 Des.du16 u16
  let dsu24 = ds Ser.su24 Des.du24 u24
  let dsu32 = ds Ser.su32 Des.du32 u32
  let dsu40 = ds Ser.su40 Des.du40 u40
  let dsu48 = ds Ser.su48 Des.du48 u48
  let dsu56 = ds Ser.su56 Des.du56 u56
  let dsu64 = ds Ser.su64 Des.du64 u64
  let dsu128 = ds Ser.su128 Des.du128 u128

  let dsnull t sstate dstate vtyp0 src_dst =
    MapPair (src_dst,
      func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
        Comment ("Desserialize NULL", Des.dnull t dstate (Param (fid, 0))),
        Comment ("Serialize NULL", Ser.snull t sstate (Param (fid, 1))))))

  let dsnotnull t sstate dstate vtyp0 src_dst =
    MapPair (src_dst,
      func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
        Comment ("Desserialize NonNull", Des.dnotnull t dstate (Param (fid, 0))),
        Comment ("Serialize NonNull", Ser.snotnull t sstate (Param (fid, 1))))))

  let rec dstup vtyps sstate dstate vtyp0 src_dst =
    let src_dst = Comment ("Convert a Tuple",
      MapPair (src_dst,
        func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
          Des.tup_opn dstate vtyps (Param (fid, 0)),
          Ser.tup_opn sstate vtyps (Param (fid, 1)))))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i vtyp ->
        let src_dst = Comment ("Convert tuple field "^ string_of_int i, src_dst) in
        if i = 0 then
          desser_ vtyp sstate dstate vtyp0 src_dst
        else
          let src_dst = MapPair (src_dst,
            func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid ->
              Pair (
                Des.tup_sep i dstate (Param (fid, 0)),
                Ser.tup_sep i sstate (Param (fid, 1))))) in
          desser_ vtyp sstate dstate vtyp0 src_dst
      ) src_dst vtyps in
    MapPair (src_dst,
      func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
        Des.tup_cls dstate (Param (fid, 0)),
        Ser.tup_cls sstate (Param (fid, 1)))))

  and dsrec vtyps sstate dstate vtyp0 src_dst =
    let src_dst = Comment ("Convert a Record",
      MapPair (src_dst,
        func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
          Des.rec_opn dstate vtyps (Param (fid, 0)),
          Ser.rec_opn sstate vtyps (Param (fid, 1)))))) in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i (name, vtyp) ->
        let src_dst = Comment ("Convert record field "^ name, src_dst) in
        if i = 0 then
          desser_ vtyp sstate dstate vtyp0 src_dst
        else
          let src_dst = MapPair (src_dst,
            func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid ->
              Pair (
                Des.rec_sep name dstate (Param (fid, 0)),
                Ser.rec_sep name sstate (Param (fid, 1))))) in
          desser_ vtyp sstate dstate vtyp0 src_dst
      ) src_dst vtyps in
    MapPair (src_dst,
      func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
        Des.rec_cls dstate (Param (fid, 0)),
        Ser.rec_cls sstate (Param (fid, 1)))))

  (* This will generates a long linear code with one block per array
   * item, which should be ok since vector dimension is expected to be small.
   * TODO: use one of the loop expressions instead if the dimension is large *)
  and dsvec dim vtyp sstate dstate vtyp0 src_dst =
    let src_dst = Comment ("Convert a Vector",
      MapPair (src_dst,
        func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
          Des.vec_opn dstate dim vtyp (Param (fid, 0)),
          Ser.vec_opn sstate dim vtyp (Param (fid, 1)))))) in
    let rec loop src_dst i =
      if i >= dim then
        MapPair (src_dst,
          func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
            Des.vec_cls dstate (Param (fid, 0)),
            Ser.vec_cls sstate (Param (fid, 1)))))
      else (
        let src_dst = Comment ("Convert vector field "^ string_of_int i, src_dst) in
        if i = 0 then (
          let src_dst = desser_ vtyp sstate dstate vtyp0 src_dst in
          loop src_dst (i + 1)
        ) else (
          let src_dst = MapPair (src_dst,
            func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
              Des.vec_sep i dstate (Param (fid, 0)),
              Ser.vec_sep i sstate (Param (fid, 1))))) in
          let src_dst = desser_ vtyp sstate dstate vtyp0 src_dst in
          loop src_dst (i + 1)
        )
      )
    in
    loop src_dst 0

  and dslist vtyp sstate dstate vtyp0 src_dst =
    let pair_ptrs = Type.Pair (Des.ptr vtyp0, Ser.ptr vtyp0) in
    Comment ("Convert a List",
      with_sploded_pair "dslist1" src_dst (fun src dst ->
        let dim_src = Des.list_opn dstate vtyp src in
        with_sploded_pair "dslist2" dim_src (fun dim src ->
          let dst = Ser.list_opn sstate vtyp dst dim in
          let src_dst =
            Repeat (I32 0l, ToI32 dim,
              Comment ("Convert a list item",
                func [|i32; pair_ptrs|] (fun fid -> (
                  let param_n = Param (fid, 0) (*i32*) in
                  let param_src_dst = Param (fid, 1) (*pair_ptrs*) in
                  let src_dst =
                    Choose (Eq (param_n, I32 0l),
                      param_src_dst,
                      MapPair (param_src_dst,
                        func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
                          Des.list_sep dstate (Param (fid, 0)),
                          Ser.list_sep sstate (Param (fid, 1)))))) in
                  desser_ vtyp sstate dstate vtyp0 src_dst))),
              Pair (src, dst)) in
          MapPair (src_dst,
            func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
              Des.vec_cls dstate (Param (fid, 0)),
              Ser.vec_cls sstate (Param (fid, 1))))))))

  and desser_not_nullable = function
    | ValueType.Float -> dsfloat
    | String -> dsstring
    | Bool -> dsbool
    | Char -> dschar
    | I8 -> dsi8
    | I16 -> dsi16
    | I24 -> dsi24
    | I32 -> dsi32
    | I40 -> dsi40
    | I48 -> dsi48
    | I56 -> dsi56
    | I64 -> dsi64
    | I128 -> dsi128
    | U8 -> dsu8
    | U16 -> dsu16
    | U24 -> dsu24
    | U32 -> dsu32
    | U40 -> dsu40
    | U48 -> dsu48
    | U56 -> dsu56
    | U64 -> dsu64
    | U128 -> dsu128
    | Tup vtyps -> dstup vtyps
    | Rec vtyps -> dsrec vtyps
    | Vec (dim, vtyp) -> dsvec dim vtyp
    | List vtyp -> dslist vtyp
    | Map _ -> assert false (* No value of map type *)

  and desser_ vtyp sstate dstate vtyp0 src_dst =
    match vtyp with
    | Nullable t ->
        with_sploded_pair "desser_" src_dst (fun src dst ->
          let cond = Des.is_null dstate src in
          (* Des can use [is_null] to prepare for a nullable, but Ser might also
           * have some work to do: *)
          let dst = Ser.nullable sstate dst in
          let src_dst = Pair (src, dst) in
          (* XXX WARNING XXX
           * if any of dnull/snull/snotnull/etc update the state, they will
           * do so in both branches of this alternative. *)
          Choose (cond,
            dsnull t sstate dstate vtyp0 src_dst,
            (
              let src_dst = dsnotnull t sstate dstate vtyp0 src_dst in
              desser_not_nullable t sstate dstate vtyp0 src_dst
            )))
    | NotNullable t ->
        desser_not_nullable t sstate dstate vtyp0 src_dst

  let desser vtyp0 src dst =
    let sstate, dst = Ser.start vtyp0 dst
    and dstate, src = Des.start vtyp0 src in
    let src_dst = Pair (src, dst) in
    let src_dst = desser_ vtyp0 sstate dstate vtyp0 src_dst in
    MapPair (src_dst,
      func [|Des.ptr vtyp0; Ser.ptr vtyp0|] (fun fid -> Pair (
        Des.stop dstate (Param (fid, 0)),
        Ser.stop sstate (Param (fid, 1)))))
end

(*
 * Now let's move on to code generators.
 *
 * The idea is that a code generator receives its state, an expression and an
 * optional name for it, and returns an identifier alongside a new state.
 *
 * This state has all defined identifiers.
 *
 * Eventually, the state can be turned into a source file. Unused identifiers
 * may not be included unless they are non-anonymous functions.
 *)

module type BACKEND =
sig
  type state
  val make_state : unit -> state
  val print_source : state -> 'a IO.output -> unit
  (* Returns the new state, the Identifier expression to use in new expressions,
   * and the identifier name in the source code: *)
  val identifier_of_expression : state -> ?name:string -> e -> (state * e * string)
  val preferred_file_extension : string
end
