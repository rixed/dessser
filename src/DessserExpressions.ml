open Batteries
open Stdint
open DessserTypes

type endianness = LittleEndian | BigEndian

let print_endianness oc = function
  | LittleEndian -> String.print oc "little-endian"
  | BigEndian -> String.print oc "big-endian"

type e0 =
  | Param of (*function id*) int * (*param no*) int
  | Null of value_type
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
  | DataPtrOfString of string
  (* To build heap values: *)
  (* Allocate a default values value.
   * For backends that can alloc uninitialized values:
   *   Will define the type, then alloc an uninitialized value of that
   *   type and return an identifier for it. Fields will be initialized
   *   as serialization progresses.
   * For backends that can not alloc uninitialized values:
   *   Will define the type, then make up a name and return it. Construction
   *   and allocation will happen as the serialize progresses. *)
  | AllocValue of maybe_nullable
  (* Identifier are set with `Let` expressions, or obtained from the code
   * generators in exchange for an expression: *)
  | Identifier of string

type e1 =
  | Function of (*function id*) int * (*args*) typ array
  | Comment of string
  | FieldIsNull of path
  | GetField of path
  | Dump
  | Ignore
  | IsNull
  (* Turn e into a nullable: *)
  | ToNullable
  (* Turn e into a not-nullable: *)
  | ToNotNullable
  (* Convert from/to string for all base value types: *)
  | StringOfFloat
  | StringOfChar
  | StringOfInt
  | FloatOfString
  | CharOfString
  | U8OfString
  | U16OfString
  | U24OfString
  | U32OfString
  | U40OfString
  | U48OfString
  | U56OfString
  | U64OfString
  | U128OfString
  | I8OfString
  | I16OfString
  | I24OfString
  | I32OfString
  | I40OfString
  | I48OfString
  | I56OfString
  | I64OfString
  | I128OfString
  (* Integers can be casted upon others regardless of sign and width: *)
  | ToU8
  | ToU16
  | ToU24
  | ToU32
  | ToU40
  | ToU48
  | ToU56
  | ToU64
  | ToU128
  | ToI8
  | ToI16
  | ToI24
  | ToI32
  | ToI40
  | ToI48
  | ToI56
  | ToI64
  | ToI128
  | LogNot
  | FloatOfQWord
  | QWordOfFloat
  | U8OfByte
  | ByteOfU8
  | U16OfWord
  | WordOfU16
  | U32OfDWord
  | DWordOfU32
  | U64OfQWord
  | QWordOfU64
  | U128OfOWord
  | OWordOfU128
  | U8OfChar
  | CharOfU8
  | SizeOfU32
  | U32OfSize
  | BitOfBool
  | BoolOfBit
  (* à la C: *)
  | U8OfBool
  | BoolOfU8
  | StringLength
  | StringOfBytes
  | BytesOfString
  | ListLength
  | ReadByte
  | DataPtrPush
  | DataPtrPop
  | RemSize
  | Not
  (* Get the value pointed by a valueptr: *)
  | DerefValuePtr
  (* WARNING: never use Fst and Snd on the same expression or that expression
   * will be computed twice!
   * Instead, use MapPair or Let *)
  | Fst
  | Snd
  | ReadWord of endianness
  | ReadDWord of endianness
  | ReadQWord of endianness
  | ReadOWord of endianness

type e2 =
  | Let of string
  (* Set a field. There is no control that the field type match the type at
   * this location. *)
  | SetField of path
  | Coalesce
  (* Comparators: *)
  | Gt
  | Ge
  | Eq
  | Ne
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | LogAnd
  | LogOr
  | LogXor
  | LeftShift
  | RightShift
  | AppendBytes
  | AppendString
  | TestBit
  | ReadBytes
  | PeekByte
  | WriteByte
  | WriteBytes
  | PokeByte
  | DataPtrAdd
  | DataPtrSub
  | And
  | Or
  | Pair
  | MapPair (* the pair * the function2 *)
  | PeekWord of endianness
  | PeekDWord of endianness
  | PeekQWord of endianness
  | PeekOWord of endianness
  | WriteWord of endianness
  | WriteDWord of endianness
  | WriteQWord of endianness
  | WriteOWord of endianness

type e3 =
  | SetBit
  | BlitByte
  | Choose (* Condition * Consequent * Alternative *)
  | LoopWhile (* Condition ('a->bool) * Loop body ('a->'a) * Initial value *)
  | LoopUntil (* Loop body ('a->'a) * Condition ('a->bool) * Initial value *)

type e4 =
  | ReadWhile (* Cond (byte->bool) * Reducer ('a->byte->'a) * Init * Start pos ->
                   Result ('a*ptr) *)
  | Repeat (* From * To * body (idx->'a->'a) * Init value *)

type e =
  | Seq of e list
  | E0 of e0
  | E1 of e1 * e
  | E2 of e2 * e * e
  | E3 of e3 * e * e * e
  | E4 of e4 * e * e * e * e

(* Create a function expression and return its id: *)
let func =
  let next_id = ref 0 in
  fun typs f ->
    let id = !next_id in
    incr next_id ;
    E1 (Function (id, typs), f id)

let rec print_expr ?max_depth oc e =
  if Option.map_default (fun m -> m <= 0) false max_depth then
    pp oc "…"
  else
    let max_depth = Option.map pred max_depth in
    let p = print_expr ?max_depth in
    match e with
    | E1 (Comment str, e) ->
        pp oc "(Comment %S %a)" str p e
    | E1 (Dump, e) ->
        pp oc "(Dump %a)" p e
    | Seq es ->
        pp oc "(Seq %a)" (List.print ~first:"" ~last:"" ~sep:" " p) es
    | E1 (Ignore, e) ->
        pp oc "(Ignore %a)" p e
    | E1 (IsNull, e) ->
        pp oc "(IsNull %a)" p e
    | E2 (Coalesce, e1, e2) ->
        pp oc "(Coalesce %a %a)" p e1 p e2
    | E1 (ToNullable, e) ->
        pp oc "(ToNullable %a)" p e
    | E1 (ToNotNullable, e) ->
        pp oc "(ToNotNullable %a)" p e
    | E0 (Null t) ->
        pp oc "(Null %a)" print_value_type t
    | E0 (Float f) ->
        pp oc "(Float %f)" f
    | E0 (String s) ->
        pp oc "(String %S)" s
    | E0 (Bool b) ->
        pp oc "(Bool %b)" b
    | E0 (Char c) ->
        pp oc "(Char %C)" c
    | E0 (U8 i) ->
        pp oc "(U8 %d)" i
    | E0 (U16 i) ->
        pp oc "(U16 %d)" i
    | E0 (U24 i) ->
        pp oc "(U24 %d)" i
    | E0 (U32 u) ->
        pp oc "(U32 %s)" (Uint32.to_string u)
    | E0 (U40 u) ->
        pp oc "(U40 %s)" (Uint40.to_string u)
    | E0 (U48 u) ->
        pp oc "(U48 %s)" (Uint48.to_string u)
    | E0 (U56 u) ->
        pp oc "(U56 %s)" (Uint56.to_string u)
    | E0 (U64 u) ->
        pp oc "(U64 %s)" (Uint64.to_string u)
    | E0 (U128 u) ->
        pp oc "(U128 %s)" (Uint128.to_string u)
    | E0 (I8 i) ->
        pp oc "(I8 %d)" i
    | E0 (I16 i) ->
        pp oc "(I16 %d)" i
    | E0 (I24 i) ->
        pp oc "(I24 %d)" i
    | E0 (I32 i) ->
        pp oc "(I32 %ld)" i
    | E0 (I40 i) ->
        pp oc "(I40 %Ld)" i
    | E0 (I48 i) ->
        pp oc "(I48 %Ld)" i
    | E0 (I56 i) ->
        pp oc "(I56 %Ld)" i
    | E0 (I64 i) ->
        pp oc "(I64 %Ld)" i
    | E0 (I128 u) ->
        pp oc "(I128 %s)" (Int128.to_string u)
    | E0 (Bit b) ->
        pp oc "(Bit %b)" b
    | E0 (Size i) ->
        pp oc "(Size %d)" i
    | E0 (Byte i) ->
        pp oc "(Byte %d)" i
    | E0 (Word i) ->
        pp oc "(Word %d)" i
    | E0 (DWord u) ->
        pp oc "(DWord %s)" (Uint32.to_string u)
    | E0 (QWord u) ->
        pp oc "(QWord %s)" (Uint64.to_string u)
    | E0 (OWord u) ->
        pp oc "(OWord %s)" (Uint128.to_string u)
    | E2 (Gt, e1, e2) ->
        pp oc "(Gt %a %a)" p e1 p e2
    | E2 (Ge, e1, e2) ->
        pp oc "(Ge %a %a)" p e1 p e2
    | E2 (Eq, e1, e2) ->
        pp oc "(Eq %a %a)" p e1 p e2
    | E2 (Ne, e1, e2) ->
        pp oc "(Ne %a %a)" p e1 p e2
    | E2 (Add, e1, e2) ->
        pp oc "(Add %a %a)" p e1 p e2
    | E2 (Sub, e1, e2) ->
        pp oc "(Sub %a %a)" p e1 p e2
    | E2 (Mul, e1, e2) ->
        pp oc "(Mul %a %a)" p e1 p e2
    | E2 (Div, e1, e2) ->
        pp oc "(Div %a %a)" p e1 p e2
    | E2 (Rem, e1, e2) ->
        pp oc "(Rem %a %a)" p e1 p e2
    | E2 (LogAnd, e1, e2) ->
        pp oc "(LogAnd %a %a)" p e1 p e2
    | E2 (LogOr, e1, e2) ->
        pp oc "(LogOr %a %a)" p e1 p e2
    | E2 (LogXor, e1, e2) ->
        pp oc "(LogXor %a %a)" p e1 p e2
    | E2 (LeftShift, e1, e2) ->
        pp oc "(LeftShift %a %a)" p e1 p e2
    | E2 (RightShift, e1, e2) ->
        pp oc "(RightShift %a %a)" p e1 p e2
    | E1 (LogNot, e) ->
        pp oc "(LogNot %a)" p e
    | E1 (StringOfInt, e) ->
        pp oc "(StringOfInt %a)" p e
    | E1 (StringOfChar, e) ->
        pp oc "(StringOfChar %a)" p e
    | E1 (StringOfFloat, e) ->
        pp oc "(StringOfFloat %a)" p e
    | E1 (FloatOfQWord, e) ->
        pp oc "(FloatOfQWord %a)" p e
    | E1 (QWordOfFloat, e) ->
        pp oc "(QWordOfFloat %a)" p e
    | E1 (FloatOfString, e) ->
        pp oc "(FloatOfString %a)" p e
    | E1 (CharOfString, e) ->
        pp oc "(CharOfString %a)" p e
    | E1 (U8OfString, e) ->
        pp oc "(U8OfString %a)" p e
    | E1 (U16OfString, e) ->
        pp oc "(U16OfString %a)" p e
    | E1 (U24OfString, e) ->
        pp oc "(U24OfString %a)" p e
    | E1 (U32OfString, e) ->
        pp oc "(U32OfString %a)" p e
    | E1 (U40OfString, e) ->
        pp oc "(U40OfString %a)" p e
    | E1 (U48OfString, e) ->
        pp oc "(U48OfString %a)" p e
    | E1 (U56OfString, e) ->
        pp oc "(U56OfString %a)" p e
    | E1 (U64OfString, e) ->
        pp oc "(U64OfString %a)" p e
    | E1 (U128OfString, e) ->
        pp oc "(U128OfString %a)" p e
    | E1 (I8OfString, e) ->
        pp oc "(I8OfString %a)" p e
    | E1 (I16OfString, e) ->
        pp oc "(I16OfString %a)" p e
    | E1 (I24OfString, e) ->
        pp oc "(I24OfString %a)" p e
    | E1 (I32OfString, e) ->
        pp oc "(I32OfString %a)" p e
    | E1 (I40OfString, e) ->
        pp oc "(I40OfString %a)" p e
    | E1 (I48OfString, e) ->
        pp oc "(I48OfString %a)" p e
    | E1 (I56OfString, e) ->
        pp oc "(I56OfString %a)" p e
    | E1 (I64OfString, e) ->
        pp oc "(I64OfString %a)" p e
    | E1 (I128OfString, e) ->
        pp oc "(I128OfString %a)" p e
    | E1 (U8OfByte, e) ->
        pp oc "(U8OfByte %a)" p e
    | E1 (ByteOfU8, e) ->
        pp oc "(ByteOfU8 %a)" p e
    | E1 (U16OfWord, e) ->
        pp oc "(U16OfWord %a)" p e
    | E1 (WordOfU16, e) ->
        pp oc "(WordOfU16 %a)" p e
    | E1 (U32OfDWord, e) ->
        pp oc "(U32OfDWord %a)" p e
    | E1 (DWordOfU32, e) ->
        pp oc "(DWordOfU32 %a)" p e
    | E1 (U64OfQWord, e) ->
        pp oc "(U64OfQWord %a)" p e
    | E1 (QWordOfU64, e) ->
        pp oc "(QWordOfU64 %a)" p e
    | E1 (U128OfOWord, e) ->
        pp oc "(U128OfOWord %a)" p e
    | E1 (OWordOfU128, e) ->
        pp oc "(OWordOfU128 %a)" p e
    | E1 (U8OfChar, e) ->
        pp oc "(U8OfChar %a)" p e
    | E1 (CharOfU8, e) ->
        pp oc "(CharOfU8 %a)" p e
    | E1 (SizeOfU32, e) ->
        pp oc "(SizeOfU32 %a)" p e
    | E1 (U32OfSize, e) ->
        pp oc "(U32OfSize %a)" p e
    | E1 (BitOfBool, e) ->
        pp oc "(BitOfBool %a)" p e
    | E1 (BoolOfBit, e) ->
        pp oc "(BoolOfBit %a)" p e
    | E1 (U8OfBool, e) ->
        pp oc "(U8OfBool %a)" p e
    | E1 (BoolOfU8, e) ->
        pp oc "(BoolOfU8 %a)" p e
    | E2 (AppendBytes, e1, e2) ->
        pp oc "(AppendBytes %a %a)" p e1 p e2
    | E2 (AppendString, e1, e2) ->
        pp oc "(AppendString %a %a)" p e1 p e2
    | E1 (StringLength, e) ->
        pp oc "(StringLength %a)" p e
    | E1 (StringOfBytes, e) ->
        pp oc "(StringOfBytes %a)" p e
    | E1 (BytesOfString, e) ->
        pp oc "(BytesOfString %a)" p e
    | E1 (ListLength, e) ->
        pp oc "(ListLength %a)" p e
    | E0 (DataPtrOfString s) ->
        pp oc "(DataPtrOfString %S)" s
    | E2 (TestBit, e1, e2) ->
        pp oc "(TestBit %a %a)" p e1 p e2
    | E3 (SetBit, e1, e2, e3) ->
        pp oc "(SetBit %a %a %a)" p e1 p e2 p e3
    | E1 (ReadByte, e) ->
        pp oc "(ReadByte %a)" p e
    | E1 (ReadWord en, e1) ->
        pp oc "(ReadWord %a %a)" print_endianness en p e1
    | E1 (ReadDWord en, e1) ->
        pp oc "(ReadDWord %a %a)" print_endianness en p e1
    | E1 (ReadQWord en, e1) ->
        pp oc "(ReadQWord %a %a)" print_endianness en p e1
    | E1 (ReadOWord en, e1) ->
        pp oc "(ReadOWord %a %a)" print_endianness en p e1
    | E2 (ReadBytes, e1, e2) ->
        pp oc "(ReadBytes %a %a)" p e1 p e2
    | E2 (PeekByte, e1, e2) ->
        pp oc "(PeekByte %a %a)" p e1 p e2
    | E2 (PeekWord en, e1, e2) ->
        pp oc "(PeekWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (PeekDWord en, e1, e2) ->
        pp oc "(PeekDWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (PeekQWord en, e1, e2) ->
        pp oc "(PeekQWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (PeekOWord en, e1, e2) ->
        pp oc "(PeekOWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (WriteByte, e1, e2) ->
        pp oc "(WriteByte %a %a)" p e1 p e2
    | E2 (WriteWord en, e1, e2) ->
        pp oc "(WriteWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (WriteDWord en, e1, e2) ->
        pp oc "(WriteDWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (WriteQWord en, e1, e2) ->
        pp oc "(WriteQWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (WriteOWord en, e1, e2) ->
        pp oc "(WriteOWord %a %a %a)" print_endianness en p e1 p e2
    | E2 (WriteBytes, e1, e2) ->
        pp oc "(WriteBytes %a %a)" p e1 p e2
    | E2 (PokeByte, e1, e2) ->
        pp oc "(PokeByte %a %a)" p e1 p e2
    | E3 (BlitByte, e1, e2, e3) ->
        pp oc "(BlitByte %a %a %a)" p e1 p e2 p e3
    | E2 (DataPtrAdd, e1, e2) ->
        pp oc "(DataPtrAdd %a %a)" p e1 p e2
    | E2 (DataPtrSub, e1, e2) ->
        pp oc "(DataPtrSub %a %a)" p e1 p e2
    | E1 (DataPtrPush, e1) ->
        pp oc "(DataPtrPush %a)" p e1
    | E1 (DataPtrPop, e1) ->
        pp oc "(DataPtrPop %a)" p e1
    | E1 (RemSize, e) ->
        pp oc "(RemSize %a)" p e
    | E2 (And, e1, e2) ->
        pp oc "(And %a %a)" p e1 p e2
    | E2 (Or, e1, e2) ->
        pp oc "(Or %a %a)" p e1 p e2
    | E1 (Not, e) ->
        pp oc "(Not %a)" p e
    | E1 (ToU8, e) ->
        pp oc "(ToU8%a)" p e
    | E1 (ToI8, e) ->
        pp oc "(ToI8 %a)" p e
    | E1 (ToU16, e) ->
        pp oc "(ToU16 %a)" p e
    | E1 (ToI16, e) ->
        pp oc "(ToI16 %a)" p e
    | E1 (ToU24, e) ->
        pp oc "(ToU24 %a)" p e
    | E1 (ToI24, e) ->
        pp oc "(ToI24 %a)" p e
    | E1 (ToU32, e) ->
        pp oc "(ToU32 %a)" p e
    | E1 (ToI32, e) ->
        pp oc "(ToI32 %a)" p e
    | E1 (ToU40, e) ->
        pp oc "(ToU40 %a)" p e
    | E1 (ToI40, e) ->
        pp oc "(ToI40 %a)" p e
    | E1 (ToU48, e) ->
        pp oc "(ToU48 %a)" p e
    | E1 (ToI48, e) ->
        pp oc "(ToI48 %a)" p e
    | E1 (ToU56, e) ->
        pp oc "(ToU56 %a)" p e
    | E1 (ToI56, e) ->
        pp oc "(ToI56 %a)" p e
    | E1 (ToU64, e) ->
        pp oc "(ToU64 %a)" p e
    | E1 (ToI64, e) ->
        pp oc "(ToI64 %a)" p e
    | E1 (ToU128, e) ->
        pp oc "(ToU128 %a)" p e
    | E1 (ToI128, e) ->
        pp oc "(ToI128 %a)" p e
    | E0 (AllocValue vtyp) ->
        pp oc "(AllocValue %a)" print_maybe_nullable vtyp
    | E1 (DerefValuePtr, e) ->
        pp oc "(DerefValuePtr %a)" p e
    | E2 (SetField path, e1, e2) ->
        pp oc "(SetField %a %a %a)" print_path path p e1 p e2
    | E1 (FieldIsNull path, e) ->
        pp oc "(FieldIsNull %a %a)" print_path path p e
    | E1 (GetField path, e) ->
        pp oc "(GetField %a %a)" print_path path p e
    | E2 (Pair, e1, e2) ->
        pp oc "(Pair %a %a)" p e1 p e2
    | E1 (Fst, e) ->
        pp oc "(Fst %a)" p e
    | E1 (Snd, e) ->
        pp oc "(Snd %a)" p e
    | E2 (MapPair, e1, e2) ->
        pp oc "(MapPair %a %a)" p e1 p e2
    | E0 (Identifier n) ->
        (* Do not repeat the expression, that we keep only for knowing the type of this expression: *)
        pp oc "(Identifier %s)" n
    | E2 (Let n, e1, e2) ->
        pp oc "(Let %s %a %a)" n p e1 p e2
    | E1 (Function (id, ts), e) ->
        pp oc "(Function %d %a %a)"
          id (Array.print ~first:"" ~last:"" ~sep:" " print_typ) ts p e
    | E0 (Param (fid, n)) ->
        pp oc "(Param %d %d)" fid n
    | E3 (Choose, e1, e2, e3) ->
        pp oc "(Choose %a %a %a)" p e1 p e2 p e3
    | E4 (ReadWhile, e1, e2, e3, e4) ->
        pp oc "(ReadWhile %a %a %a %a)" p e1 p e2 p e3 p e4
    | E3 (LoopWhile, e1, e2, e3) ->
        pp oc "(LoopWhile %a %a %a)" p e1 p e2 p e3
    | E3 (LoopUntil, e1, e2, e3) ->
        pp oc "(LoopUntil %a %a %a)" p e1 p e2 p e3
    | E4 (Repeat, e1, e2, e3, e4) ->
        pp oc "(Repeat %a %a %a %a)" p e1 p e2 p e3 p e4

exception Type_error of e * e * typ * string
exception Type_error_param of e * e * int * typ * string
exception Type_error_path of e * e * path * string

let rec vtype_of_valueptr e0 l e =
  match type_of l e with
  | TValuePtr vt -> vt
  | t -> raise (Type_error (e0, e, t, "be a ValuePtr"))

(* [e] must have been type checked already: *)
and type_of l e0 =
  match e0 with
  | E1 (Dump, _)
  | Seq []
  | E1 (Ignore, _) ->
      void
  | Seq es ->
      type_of l (List.last es)
  | E1 (Comment _, e)
  | E2 (Coalesce, _, e)
  | E2 (Add, e, _)
  | E2 (Sub, e, _)
  | E2 (Mul, e, _)
  | E2 (Div, e, _)
  | E2 (Rem, e, _)
  | E2 (LogAnd, e, _)
  | E2 (LogOr, e, _)
  | E2 (LogXor, e, _)
  | E2 (LeftShift, e, _)
  | E2 (RightShift, e, _)
  | E1 (LogNot, e) ->
      type_of l e
  | E1 (ToNullable, e) ->
      typ_to_nullable (type_of l e)
  | E1 (ToNotNullable, e) ->
      typ_to_not_nullable (type_of l e)
  | E1 (IsNull, _) -> bool
  | E0 (Null t) -> TValue (Nullable t)
  | E0 (Float _) -> float
  | E0 (String _) -> string
  | E0 (Bool _) -> bool
  | E0 (Char _) -> char
  | E0 (U8 _) -> u8
  | E0 (U16 _) -> u16
  | E0 (U24 _) -> u24
  | E0 (U32 _) -> u32
  | E0 (U40 _) -> u40
  | E0 (U48 _) -> u48
  | E0 (U56 _) -> u56
  | E0 (U64 _) -> u64
  | E0 (U128 _) -> u128
  | E0 (I8 _) -> i8
  | E0 (I16 _) -> i16
  | E0 (I24 _) -> i24
  | E0 (I32 _) -> i32
  | E0 (I40 _) -> i40
  | E0 (I48 _) -> i48
  | E0 (I56 _) -> i56
  | E0 (I64 _) -> i64
  | E0 (I128 _) -> i128
  | E0 (Bit _) -> bit
  | E0 (Size _) -> size
  | E0 (Byte _) -> byte
  | E0 (Word _) -> word
  | E0 (DWord _) -> dword
  | E0 (QWord _) -> qword
  | E0 (OWord _) -> oword
  | E2 (Gt, _, _) -> bool
  | E2 (Ge, _, _) -> bool
  | E2 (Eq, _, _) -> bool
  | E2 (Ne, _, _) -> bool
  | E1 (StringOfFloat, _)
  | E1 (StringOfChar, _)
  | E1 (StringOfInt, _) -> string
  | E1 (CharOfString, _) -> char
  | E1 (FloatOfString, _) -> float
  | E1 (U8OfString, _) -> u8
  | E1 (U16OfString, _) -> u16
  | E1 (U24OfString, _) -> u24
  | E1 (U32OfString, _) -> u32
  | E1 (U40OfString, _) -> u40
  | E1 (U48OfString, _) -> u48
  | E1 (U56OfString, _) -> u56
  | E1 (U64OfString, _) -> u64
  | E1 (U128OfString, _) -> u128
  | E1 (I8OfString, _) -> i8
  | E1 (I16OfString, _) -> i16
  | E1 (I24OfString, _) -> i24
  | E1 (I32OfString, _) -> i32
  | E1 (I40OfString, _) -> i40
  | E1 (I48OfString, _) -> i48
  | E1 (I56OfString, _) -> i56
  | E1 (I64OfString, _) -> i64
  | E1 (I128OfString, _) -> i128
  | E1 (FloatOfQWord, _) -> float
  | E1 (QWordOfFloat, _) -> qword
  | E1 (U8OfByte, _) -> u8
  | E1 (ByteOfU8, _) -> byte
  | E1 (U16OfWord, _) -> u16
  | E1 (WordOfU16, _) -> word
  | E1 (U32OfDWord, _) -> u32
  | E1 (DWordOfU32, _) -> dword
  | E1 (U64OfQWord, _) -> u64
  | E1 (QWordOfU64, _) -> qword
  | E1 (U128OfOWord, _) -> u128
  | E1 (OWordOfU128, _) -> oword
  | E1 (U8OfChar, _) -> u8
  | E1 (CharOfU8, _) -> char
  | E1 (SizeOfU32, _) -> size
  | E1 (U32OfSize, _) -> u32
  | E1 (BitOfBool, _) -> bit
  | E1 (BoolOfBit, _) -> bool
  | E1 (U8OfBool, _) -> u8
  | E1 (BoolOfU8, _) -> bool
  | E2 (AppendBytes, _, _) -> bytes
  | E2 (AppendString, _, _) -> string
  | E1 (StringLength, _) -> u32
  | E1 (StringOfBytes, _) -> string
  | E1 (BytesOfString, _) -> bytes
  | E1 (ListLength, _) -> u32
  | E0 (DataPtrOfString _) -> dataptr
  | E2 (TestBit, _, _) -> bit
  | E3 (SetBit, _, _, _) -> dataptr
  | E1 (ReadByte, _) -> pair byte dataptr
  | E1 (ReadWord _, _) -> pair word dataptr
  | E1 (ReadDWord _, _) -> pair dword dataptr
  | E1 (ReadQWord _, _) -> pair qword dataptr
  | E1 (ReadOWord _, _) -> pair oword dataptr
  | E2 (ReadBytes, _, _) -> pair bytes dataptr
  | E2 (PeekByte, _, _) -> byte
  | E2 (PeekWord _, _, _) -> word
  | E2 (PeekDWord _ , _, _)-> dword
  | E2 (PeekQWord _, _, _) -> qword
  | E2 (PeekOWord _, _, _) -> oword
  | E2 (WriteByte, _, _) -> dataptr
  | E2 (WriteWord _, _, _) -> dataptr
  | E2 (WriteDWord _, _, _) -> dataptr
  | E2 (WriteQWord _, _, _) -> dataptr
  | E2 (WriteOWord _, _, _) -> dataptr
  | E2 (WriteBytes, _, _) -> dataptr
  | E2 (PokeByte, _, _) -> dataptr
  | E3 (BlitByte, _, _, _) -> dataptr
  | E2 (DataPtrAdd, _, _) -> dataptr
  | E2 (DataPtrSub, _, _) -> size
  | E1 (DataPtrPush, _) -> dataptr
  | E1 (DataPtrPop, _) -> dataptr
  | E1 (RemSize, _) -> size
  | E2 (And, _, _) -> bool
  | E2 (Or, _, _) -> bool
  | E1 (Not, _) -> bool
  | E1 (ToU8, _) -> i8
  | E1 (ToI8, _) -> u8
  | E1 (ToU16, _) -> u16
  | E1 (ToI16, _) -> i16
  | E1 (ToU24, _) -> u24
  | E1 (ToI24, _) -> i24
  | E1 (ToU32, _) -> u32
  | E1 (ToI32, _) -> i32
  | E1 (ToU40, _) -> u40
  | E1 (ToI40, _) -> i40
  | E1 (ToU48, _) -> u48
  | E1 (ToI48, _) -> i48
  | E1 (ToU56, _) -> u56
  | E1 (ToI56, _) -> i56
  | E1 (ToU64, _) -> u64
  | E1 (ToI64, _) -> i64
  | E1 (ToU128, _) -> u128
  | E1 (ToI128, _) -> i128
  | E0 (AllocValue vtyp) -> valueptr vtyp
  | E1 (DerefValuePtr, e) ->
      TValue (vtype_of_valueptr e0 l e)
  | E2 (SetField _, e1, _) -> type_of l e1
  | E1 (FieldIsNull _, _) -> bool
  | E1 (GetField path, e) ->
      let vt = vtype_of_valueptr e0 l e in
      TValue (type_of_path vt path)
  | E2 (Pair, e1, e2) ->
      pair (type_of l e1) (type_of l e2)
  | E1 (Fst, e) ->
      (match type_of l e with
      | TPair (t, _) -> t
      | t -> raise (Type_error (e0, e, t, "be a pair")))
  | E1 (Snd, e) ->
      (match type_of l e with
      | TPair (_, t) -> t
      | t -> raise (Type_error (e0, e, t, "be a pair")))
  | E2 (MapPair, _, e) ->
      (match type_of l e with
      | TFunction (_, t) -> t
      | t -> raise (Type_error (e0, e, t, "be a function")))
  | E0 (Identifier n) as e ->
      (try List.assoc e l
      with Not_found ->
          Printf.eprintf "Cannot find identifier %S in %a\n%!"
            n (List.print (fun oc (e, _) -> print_expr oc e)) l ;
          raise Not_found)
  | E2 (Let n, e1, e2) ->
      type_of ((E0 (Identifier n), type_of l e1) :: l) e2
  | E1 (Function (fid, ts), e) ->
      let l = Array.fold_lefti (fun l i t ->
        (E0 (Param (fid, i)), t) :: l
      ) l ts in
      TFunction (ts, type_of l e)
  | E0 (Param (fid, n)) as e ->
      (try List.assoc e l
      with Not_found ->
          Printf.eprintf "Cannot find parameter #%d of function %d in %a\n%!"
            n fid (List.print (fun oc (e, _) -> print_expr oc e)) l ;
          raise Not_found)
  | E3 (Choose, _, e, _) -> type_of l e
  | E4 (ReadWhile, _, _, e, _) -> pair (type_of l e) dataptr
  | E3 (LoopWhile, _, _, e) -> type_of l e
  | E3 (LoopUntil, _, _, e) -> type_of l e
  | E4 (Repeat, _, _, _, e) -> type_of l e

(* depth last, pass the list of bound identifiers along the way: *)
let rec fold_expr u l f e =
  let u = f u l e in
  match e with
  | Seq es ->
      List.fold_left (fun u e1 -> fold_expr u l f e1) u es
  | E0 _ ->
      u
  | E1 (Function (id, ts), e1) ->
      let l = Array.fold_lefti (fun l i t ->
        (E0 (Param (id, i)), t) :: l
      ) l ts in
      fold_expr u l f e1
  | E1 (_, e1) ->
      fold_expr u l f e1
  | E2 (Let s, e1, e2) ->
      let l' = (E0 (Identifier s), type_of l e1) :: l in
      fold_expr (fold_expr u l f e1) l' f e2
  | E2 (_, e1, e2) ->
      fold_expr (fold_expr u l f e1) l f e2
  | E3 (_, e1, e2, e3) ->
      fold_expr (fold_expr (fold_expr u l f e1) l f e2) l f e3
  | E4 (_, e1, e2, e3, e4) ->
      fold_expr (fold_expr (fold_expr (fold_expr u l f e1) l f e2) l f e3) l f e4

let type_check l e =
  fold_expr () l (fun () l e0 ->
    let check_void l e =
      match type_of l e with
      | TVoid -> ()
      | t -> raise (Type_error (e0, e, t, "be Void")) in
    let check_nullable l e =
      match type_of l e with
      | TValue (Nullable _) -> ()
      | t -> raise (Type_error (e0, e, t, "be nullable")) in
    let check_not_nullable l e =
      match type_of l e with
      | TValue (NotNullable _) -> ()
      | t -> raise (Type_error (e0, e, t, "not be nullable")) in
    let check_comparable l e =
      match type_of l e with
      | TSize | TByte | TWord | TDWord | TQWord | TOWord
      | TValue (NotNullable (Mac (
          TFloat | TString | TChar |
          TU8 | TU16 | TU32 | TU64 | TU128 |
          TI8 | TI16 | TI32 | TI64 | TI128))) -> ()
      | t -> raise (Type_error (e0, e, t, "be comparable")) in
    let check_numeric l e =
      match type_of l e with
      | TSize | TByte | TWord | TDWord | TQWord | TOWord
      | TValue (NotNullable (Mac (
          TFloat | TChar |
          TU8 | TU16 | TU32 | TU64 | TU128 |
          TI8 | TI16 | TI32 | TI64 | TI128))) -> ()
      | t -> raise (Type_error (e0, e, t, "be numeric")) in
    let check_integer l e =
      match type_of l e with
      | TSize | TByte | TWord | TDWord | TQWord | TOWord
      | TValue (NotNullable (Mac (
          TU8 | TU16 | TU32 | TU64 | TU128 |
          TI8 | TI16 | TI32 | TI64 | TI128))) -> ()
      | t -> raise (Type_error (e0, e, t, "be an integer")) in
    let check_param fe n act exp =
      if act <> exp then
        let expected = IO.to_string print_typ act in
        raise (Type_error_param (e0, fe, n, act, "be a "^ expected)) in
    let check_eq l e exp =
      let act = type_of l e in
      if act <> exp then
        let expected = IO.to_string print_typ exp in
        raise (Type_error (e0, e, act, "be a "^ expected)) in
    let check_same_types l e1 e2 =
      let t1 = type_of l e1 in
      check_eq l e2 t1 in
    let check_list l e =
      match type_of l e with
      | TValue (NotNullable TList _) -> ()
      | t -> raise (Type_error (e0, e, t, "be a list")) in
    let check_pair l e =
      match type_of l e with
      | TPair _ -> ()
      | t -> raise (Type_error (e0, e, t, "be a pair")) in
    let bad_arity expected e t =
      let s = Printf.sprintf "be a function of %d parameters" expected in
      raise (Type_error (e0, e, t, s)) in
    let check_function arity l e =
      match type_of l e with
      | TFunction (ts, _) as t ->
          if Array.length ts <> arity then bad_arity arity e t
      | t -> raise (Type_error (e0, e, t, "be a function")) in
    let check_params1 l e f =
      match type_of l e with
      | TFunction ([|t1|], t2) -> f t1 t2
      | t -> bad_arity 1 e t in
    let check_params2 l e f =
      match type_of l e with
      | TFunction ([|t1; t2|], t3) -> f t1 t2 t3
      | t -> bad_arity 2 e t in
    let check_valueptr l e  =
      match type_of l e with
      | TValuePtr _ -> ()
      | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
    let check_valueptr_path_same_types l e1 path e2 =
      match type_of l e1 with
      | TValuePtr vt ->
          let exp = TValue (type_of_path vt path) in
          check_eq l e2 exp
      | t -> raise (Type_error (e0, e1, t, "be a ValuePtr")) in
    let check_valueptr_path l e path =
      match type_of l e with
      | TValuePtr vt ->
          (try ignore (type_of_path vt path)
          with _ ->
            let s = IO.to_string print_maybe_nullable vt in
            raise (Type_error_path (e0, e, path, "stay within "^ s)))
      | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
    let check_valueptr_path_nullable l e path nullable =
      match type_of l e with
      | TValuePtr vt ->
          let vt = type_of_path vt path in
          let act = is_nullable vt in
          if act <> nullable then
            let expected = (if nullable then "" else "not") ^" nullable" in
            raise (Type_error_path (e0, e, path, "be "^ expected))
      | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
    match e0 with
    | E0 (Null _ | Float _ | String _ | Bool _ | Char _
         | U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _
         | I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _
         | Bit _ | Size _ | Byte _ | Word _ | DWord _ | QWord _ | OWord _
         | DataPtrOfString _ | AllocValue _
         | Identifier _| Param _)
    | E1 ((Comment _ | Dump | Ignore | Function _), _)
    | E2 ((Pair | Let _), _, _) ->
        ()
    | Seq es ->
        let rec loop = function
          | [] | [_] -> ()
          | e::es -> check_void l e ; loop es in
        loop es
    | E1 (IsNull, e) ->
        check_nullable l e
    | E2 (Coalesce, e1, e2) ->
        check_nullable l e1 ;
        check_not_nullable l e2
    | E1 (ToNullable, e) ->
        check_not_nullable l e
    | E1 (ToNotNullable, e) ->
        check_nullable l e
    | E2 ((Gt | Ge | Eq | Ne), e1, e2) ->
        check_comparable l e1 ;
        check_same_types l e1 e2
    | E2 ((Add | Sub | Mul | Div | Rem), e1, e2) ->
        check_numeric l e1 ;
        check_same_types l e1 e2
    | E2 ((LogAnd | LogOr | LogXor), e1, e2) ->
        check_integer l e1 ;
        check_same_types l e1 e2
    | E2 ((LeftShift | RightShift), e1, e2) ->
        check_integer l e1 ;
        check_eq l e2 u8
    | E1 ((LogNot | StringOfInt), e) ->
        check_integer l e
    | E1 ((StringOfChar | U8OfChar), e) ->
        check_eq l e char
    | E1 ((FloatOfString | CharOfString | U8OfString | U16OfString
         | U24OfString | U32OfString | U40OfString | U48OfString
         | U56OfString | U64OfString | U128OfString | I8OfString
         | I16OfString | I24OfString | I32OfString | I40OfString
         | I48OfString | I56OfString | I64OfString | I128OfString
         | StringLength | BytesOfString), e) ->
        check_eq l e string
    | E1 ((FloatOfQWord | U64OfQWord), e) ->
        check_eq l e qword
    | E1 ((QWordOfFloat | StringOfFloat), e) ->
        check_eq l e float
    | E1 (U8OfByte, e) ->
        check_eq l e byte
    | E1 ((CharOfU8 | ByteOfU8 | BoolOfU8), e) ->
        check_eq l e u8
    | E1 ((ToU8 | ToI8 | ToI16 | ToU16 | ToI24 | ToU24 | ToI32 | ToU32
         | ToI40 | ToU40 | ToI48 | ToU48 | ToI56 | ToU56 | ToI64 | ToU64
         | ToI128 | ToU128), e) ->
        check_integer l e
    | E1 (U16OfWord, e) ->
        check_eq l e word
    | E1 (WordOfU16, e) ->
        check_eq l e u16
    | E1 (U32OfDWord, e) ->
        check_eq l e dword
    | E1 ((DWordOfU32 | SizeOfU32), e) ->
        check_eq l e u32
    | E1 (QWordOfU64, e) ->
        check_eq l e u64
    | E1 (OWordOfU128, e) ->
        check_eq l e u128
    | E1 (U128OfOWord, e) ->
        check_eq l e oword
    | E1 (U32OfSize, e) ->
        check_eq l e size
    | E1 ((BitOfBool | U8OfBool | Not), e) ->
        check_eq l e bool
    | E1 (BoolOfBit, e) ->
        check_eq l e bit
    | E2 (AppendBytes, e1, e2) ->
        check_eq l e1 bytes ;
        check_eq l e2 bytes
    | E2 (AppendString, e1, e2) ->
        check_eq l e1 string ;
        check_eq l e2 string
    | E1 (StringOfBytes, e) ->
        check_eq l e bytes
    | E1 (ListLength, e) ->
        check_list l e
    | E2 (TestBit, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 size
    | E3 (SetBit, e1, e2, e3) ->
        check_eq l e1 dataptr ;
        check_eq l e2 u32 ;
        check_eq l e3 bit
    | E1 ((ReadByte | ReadWord _ | ReadDWord _ | ReadQWord _ | ReadOWord _), e) ->
        check_eq l e dataptr
    | E2 ((ReadBytes | PeekByte | PeekWord _ | PeekDWord _ | PeekQWord _
         | PeekOWord _), e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 size
    | E2 ((WriteByte | PokeByte), e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 byte
    | E2 (WriteWord _, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 word
    | E2 (WriteDWord _, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 dword
    | E2 (WriteQWord _, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 qword
    | E2 (WriteOWord _, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 oword
    | E2 (WriteBytes, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 bytes
    | E3 (BlitByte, e1, e2, e3) ->
        check_eq l e1 dataptr ;
        check_eq l e2 byte ;
        check_eq l e3 size
    | E2 (DataPtrAdd, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 size
    | E2 (DataPtrSub, e1, e2) ->
        check_eq l e1 dataptr ;
        check_eq l e2 dataptr
    | E1 ((DataPtrPush | DataPtrPop), e1) ->
        check_eq l e1 dataptr
    | E1 (RemSize, e) ->
        check_eq l e dataptr ;
    | E2 ((And | Or), e1, e2) ->
        check_eq l e1 bool ;
        check_eq l e2 bool
    | E1 (DerefValuePtr, e1) ->
        check_valueptr l e1
    | E2 (SetField path, e1, e2) ->
        check_valueptr_path_same_types l e1 path e2
    | E1 (FieldIsNull path, e) ->
        check_valueptr_path_nullable l e path true
    | E1 (GetField path, e) ->
        check_valueptr_path l e path
    | E1 (Fst, e) ->
        check_pair l e
    | E1 (Snd, e) ->
        check_pair l e
    | E2 (MapPair, e1, e2) ->
        check_pair l e1 ;
        check_function 2 l e2
    | E3 (Choose, e1, e2, e3) ->
        check_eq l e1 bool ;
        check_same_types l e2 e3
    | E4 (ReadWhile, e1, e2, e3, e4) ->
        check_params1 l e1 (fun t1 t2 ->
          check_param e1 0 t1 byte ;
          check_param e1 1 t2 bool) ;
        check_params2 l e2 (fun t1 t2 t3 ->
          check_eq l e3 t1 ;
          check_param e2 1 t2 byte ;
          check_eq l e3 t3) ;
        check_eq l e4 dataptr
    | E3 (LoopWhile, e1, e2, e3) ->
        check_params1 l e1 (fun t1 t2 ->
          check_eq l e3 t1 ;
          check_param e1 1 t2 bool) ;
        check_params1 l e2 (fun t1 t2 ->
          check_eq l e3 t1 ;
          check_eq l e3 t2)
    | E3 (LoopUntil, e1, e2, e3) ->
        check_params1 l e1 (fun t1 t2 ->
          check_eq l e3 t1 ;
          check_eq l e3 t2) ;
        check_params1 l e2 (fun t1 t2 ->
          check_eq l e3 t1 ;
          check_param e2 1 t2 bool) ;
    | E4 (Repeat, e1, e2, e3, e4) ->
        check_eq l e1 i32 ;
        check_eq l e2 i32 ;
        check_params2 l e3 (fun t1 t2 t3 ->
          check_param e3 0 t1 i32 ;
          check_eq l e4 t2 ;
          check_eq l e4 t3)
  ) e

let size_of_expr l e =
  fold_expr 0 l (fun n _l _e0 -> n + 1) e

let () =
  let max_depth = 3 in
  Printexc.register_printer (function
    | Type_error (e0, e, t, s) ->
        Some (
          Printf.sprintf2
            "Type Error: In expression %a, expression %a should %s but is a %a"
            (print_expr ~max_depth) e0 (print_expr ~max_depth) e s print_typ t)
    | Type_error_param (e0, e, n, t, s) ->
        Some (
          Printf.sprintf2
            "Type Error: In expression %a, parameter %d of expression %a \
             should %s but is a %a"
            (print_expr ~max_depth) e0 n (print_expr ~max_depth) e s print_typ t)
    | Type_error_path (e0, e, path, s) ->
        Some (
          Printf.sprintf2
            "Type Error: In expression %a, path %a of expression %a should %s"
            (print_expr ~max_depth) e0 print_path path (print_expr ~max_depth) e s)
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

(* Do not use a function (thus not MapPair) to avoid leaking function parameters *)
let with_sploded_pair what e f =
  let pair_id = gen_id () ^"_"^ what in
  let n1 = pair_id ^"_0"
  and n2 = pair_id ^"_1" in
  E2 (Let pair_id, e,
    E2 (Let n1, E1 (Fst, E0 (Identifier pair_id)),
      E2 (Let n2, E1 (Snd, E0 (Identifier pair_id)),
        f (E0 (Identifier n1)) (E0 (Identifier n2)))))

(*$< DessserTypes *)
(*$inject
  let vptr = TValuePtr (Nullable (Mac TString))
  let func2 =
    let open Ops in
    E1 (Function (14, [|vptr; TDataPtr|]),
      let_ "gen9_ds" (pair (get_field [] (param 14 0))
                           (param 14 0))
        (let_ "gen9_ds_0" (fst (identifier "gen9_ds"))
          (let_ "gen9_ds_1" (snd (identifier "gen9_ds"))
            (pair (identifier "gen9_ds_1")
                  (comment "Serialize a String"
                    (write_byte
                      (write_bytes
                        (write_byte (param 14 1) (byte_of_const_char '"'))
                        (bytes_of_string (to_not_nullable (identifier "gen9_ds_0"))))
                      (byte_of_const_char '"')))))))
*)
(*$= type_of & ~printer:(BatIO.to_string print_typ)
  (TFunction ([|vptr; TDataPtr|], TPair (vptr, TDataPtr))) (type_of [] func2)
*)

(* Users can define additional expressions, defined in terms of the above
 * expressions with a specific name, type checker, pretty printer and
 * parser. *)

type user_expr =
  { name : string ;
    def : e ;
    type_check : (e * typ) list -> unit ;
    type_of : (e * typ) list -> typ ;
    print : unit IO.output -> unit ;
    (* parse *) }

let user_expressions = Hashtbl.create 50

let register_user_expr name ?check ?type_ ?print def =
  Hashtbl.modify_opt name (function
    | None ->
        let type_check = check |?
          fun l -> type_check l def
        and type_of =
          Option.default_delayed (fun () ->
            fun l -> type_of l def
          ) type_
        and print = print |?
          fun oc -> print_expr oc def in
        Some { name ; def ; type_check ; type_of ; print }
    | Some _ ->
        invalid_arg "register_user_expr"
  ) user_expressions

(*
 * Simplified notation:
 *)

module Ops =
struct
  let ignore_ e1 = E1 (Ignore, e1)
  let dump e1 = E1 (Dump, e1)
  let is_null e1 = E1 (IsNull, e1)
  let coalesce e1 e2 = E2 (Coalesce, e1, e2)
  let read_byte e1 = E1 (ReadByte, e1)
  let read_word en e1 = E1 (ReadWord en, e1)
  let read_dword en e1 = E1 (ReadDWord en, e1)
  let read_qword en e1 = E1 (ReadQWord en, e1)
  let read_oword en e1 = E1 (ReadOWord en, e1)
  let peek_word en e1 e2 = E2 (PeekWord en, e1, e2)
  let peek_dword en e1 e2 = E2 (PeekDWord en, e1, e2)
  let peek_qword en e1 e2 = E2 (PeekQWord en, e1, e2)
  let peek_oword en e1 e2 = E2 (PeekOWord en, e1, e2)
  let read_bytes e1 e2 = E2 (ReadBytes, e1, e2)
  let peek_byte e1 e2 = E2 (PeekByte, e1, e2)
  let write_bytes e1 e2 = E2 (WriteBytes, e1, e2)
  let write_byte e1 e2 = E2 (WriteByte, e1, e2)
  let write_word en e1 e2 = E2 (WriteWord en, e1, e2)
  let write_dword en e1 e2 = E2 (WriteDWord en, e1, e2)
  let write_qword en e1 e2 = E2 (WriteQWord en, e1, e2)
  let write_oword en e1 e2 = E2 (WriteOWord en, e1, e2)
  let bytes_of_string e1 = E1 (BytesOfString, e1)
  let string_of_int e1 = E1 (StringOfInt, e1)
  let string_of_float e1 = E1 (StringOfFloat, e1)
  let byte_of_u8 e1 = E1 (ByteOfU8, e1)
  let bool_of_u8 e1 = E1 (BoolOfU8, e1)
  let word_of_u16 e1 = E1 (WordOfU16, e1)
  let dword_of_u32 e1 = E1 (DWordOfU32, e1)
  let qword_of_u64 e1 = E1 (QWordOfU64, e1)
  let oword_of_u128 e1 = E1 (OWordOfU128, e1)
  let u8_of_byte e1 = E1 (U8OfByte, e1)
  let u8_of_char e1 = E1 (U8OfChar, e1)
  let u8_of_bool e1 = E1 (U8OfBool, e1)
  let char_of_u8 e1 = E1 (CharOfU8, e1)
  let u32_of_size e1 = E1 (U32OfSize, e1)
  let size_of_u32 e1 = E1 (SizeOfU32, e1)
  let null t = E0 (Null t)
  let bit n = E0 (Bit n)
  let bool n = E0 (Bool n)
  let i8 n = E0 (I8 n)
  let u8 n = E0 (U8 n)
  let i16 n = E0 (I16 n)
  let u16 n = E0 (U16 n)
  let i24 n = E0 (I24 n)
  let u24 n = E0 (U24 n)
  let i32 n = E0 (I32 n)
  let u32 n = E0 (U32 n)
  let i40 n = E0 (I40 n)
  let u40 n = E0 (U40 n)
  let i48 n = E0 (I48 n)
  let u48 n = E0 (U48 n)
  let i56 n = E0 (I56 n)
  let u56 n = E0 (U56 n)
  let i64 n = E0 (I64 n)
  let u64 n = E0 (U64 n)
  let i128 n = E0 (I128 n)
  let u128 n = E0 (U128 n)
  let char n = E0 (Char n)
  let float n = E0 (Float n)
  let string n = E0 (String n)
  let byte n = E0 (Byte n)
  let size n = E0 (Size n)
  let word n = E0 (Word n)
  let dword n = E0 (DWord n)
  let qword n = E0 (QWord n)
  let oword n = E0 (OWord n)
  let byte_of_char e1 = byte_of_u8 (u8_of_char e1)
  let byte_of_const_char e1 = byte_of_char (char e1)
  let choose ~cond e2 e3 =  E3 (Choose, cond, e2, e3)
  let read_while ~cond ~reduce ~init ~pos = E4 (ReadWhile, cond, reduce, init, pos)
  let pair e1 e2 = E2 (Pair, e1, e2)
  let float_of_qword e1 = E1 (FloatOfQWord, e1)
  let qword_of_float e1 = E1 (QWordOfFloat, e1)
  let let_ n e1 e2 = E2 (Let n, e1, e2)
  let comment n e1 = E1 (Comment n, e1)
  let ge e1 e2 = E2 (Ge, e1, e2)
  let gt e1 e2 = E2 (Gt, e1, e2)
  let eq e1 e2 = E2 (Eq, e1, e2)
  let param fid n = E0 (Param (fid, n))
  let add e1 e2 = E2 (Add, e1, e2)
  let sub e1 e2 = E2 (Sub, e1, e2)
  let mul e1 e2 = E2 (Mul, e1, e2)
  let div e1 e2 = E2 (Div, e1, e2)
  let rem e1 e2 = E2 (Rem, e1, e2)
  let left_shift e1 e2 = E2 (LeftShift, e1, e2)
  let right_shift e1 e2 = E2 (RightShift, e1, e2)
  let log_and e1 e2 = E2 (LogAnd, e1, e2)
  let log_or e1 e2 = E2 (LogOr, e1, e2)
  let log_xor e1 e2 = E2 (LogXor, e1, e2)
  let identifier n = E0 (Identifier n)
  let to_i8 e1 = E1 (ToI8, e1)
  let to_u8 e1 = E1 (ToU8, e1)
  let to_i16 e1 = E1 (ToI16, e1)
  let to_u16 e1 = E1 (ToU16, e1)
  let to_i24 e1 = E1 (ToI24, e1)
  let to_u24 e1 = E1 (ToU24, e1)
  let to_i32 e1 = E1 (ToI32, e1)
  let to_u32 e1 = E1 (ToU32, e1)
  let to_i40 e1 = E1 (ToI40, e1)
  let to_u40 e1 = E1 (ToU40, e1)
  let to_i48 e1 = E1 (ToI48, e1)
  let to_u48 e1 = E1 (ToU48, e1)
  let to_i56 e1 = E1 (ToI56, e1)
  let to_u56 e1 = E1 (ToU56, e1)
  let to_i64 e1 = E1 (ToI64, e1)
  let to_u64 e1 = E1 (ToU64, e1)
  let to_i128 e1 = E1 (ToI128, e1)
  let to_u128 e1 = E1 (ToU128, e1)
  let repeat ~from ~to_ ~body ~init = E4 (Repeat, from, to_, body, init)
  let loop_until ~body ~cond ~init = E3 (LoopUntil, body, cond, init)
  let loop_while ~cond ~body ~init = E3 (LoopWhile, cond, body, init)
  let fst e1 = E1 (Fst, e1)
  let snd e1 = E1 (Snd, e1)
  let size_of_u32 e1 = E1 (SizeOfU32, e1)
  let string_of_bytes e1 = E1 (StringOfBytes, e1)
  let not_ e1 = E1 (Not, e1)
  let u16_of_word e1 = E1 (U16OfWord, e1)
  let u32_of_dword e1 = E1 (U32OfDWord, e1)
  let u64_of_qword e1 = E1 (U64OfQWord, e1)
  let u128_of_oword e1 = E1 (U128OfOWord, e1)
  let data_ptr_add e1 e2 = E2 (DataPtrAdd, e1, e2)
  let data_ptr_push e1 = E1 (DataPtrPush, e1)
  let data_ptr_pop e1 = E1 (DataPtrPop, e1)
  let data_ptr_of_string s = E0 (DataPtrOfString s)
  let string_length e1 = E1 (StringLength, e1)
  let list_length e1 = E1 (ListLength, e1)
  let blit_byte e1 e2 e3 = E3 (BlitByte, e1, e2, e3)
  let set_bit e1 e2 e3 = E3 (SetBit, e1, e2, e3)
  let to_nullable e1 = E1 (ToNullable, e1)
  let to_not_nullable e1 = E1 (ToNotNullable, e1)
  let set_field p e1 e2 = E2 (SetField p, e1, e2)
  let get_field p e1 = E1 (GetField p, e1)
  let field_is_null p e1 = E1 (FieldIsNull p, e1)
  let map_pair e1 e2 = E2 (MapPair, e1, e2)
  let seq es = Seq es
  let alloc_value mn = E0 (AllocValue mn)
end
