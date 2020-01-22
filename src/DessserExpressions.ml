open Batteries
open Stdint
open DessserTypes

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
  | ToNullable of e
  (* Turn e into a not-nullable: *)
  | ToNotNullable of e
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
  | BlitByte of e * e * e
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
  | AllocValue of maybe_nullable
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

let rec print_expr ?max_depth oc e =
  if Option.map_default (fun m -> m <= 0) false max_depth then
    pp oc "…"
  else
    let max_depth = Option.map pred max_depth in
    let p = print_expr ?max_depth in
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
    | ToNullable e ->
        pp oc "(ToNullable %a)" p e
    | ToNotNullable e ->
        pp oc "(ToNotNullable %a)" p e
    | Null t ->
        pp oc "(Null %a)" print_value_type t
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
    | BlitByte (e1, e2, e3) ->
        pp oc "(BlitByte %a %a %a)" p e1 p e2 p e3
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
    | AllocValue mtyp ->
        pp oc "(AllocValue %a)" print_maybe_nullable mtyp
    | DerefValuePtr e ->
        pp oc "(DerefValuePtr %a)" p e
    | SetField (path, e1, e2) ->
        pp oc "(SetField %a %a %a)" print_path path p e1 p e2
    | FieldIsNull (path, e) ->
        pp oc "(FieldIsNull %a %a)" print_path path p e
    | GetField (path, e) ->
        pp oc "(GetField %a %a)" print_path path p e
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
          id (Array.print ~first:"" ~last:"" ~sep:" " print_typ) ts p e
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

let rec mtype_of_valueptr e0 l e =
  match type_of l e with
  | TValuePtr mt -> mt
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
  | ToNullable e ->
      typ_to_nullable (type_of l e)
  | ToNotNullable e ->
      typ_to_not_nullable (type_of l e)
  | IsNull _ -> bool
  | Null t -> TValue (Nullable t)
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
  | BlitByte _ -> dataptr
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
  | AllocValue mtyp -> valueptr mtyp
  | DerefValuePtr e ->
      TValue (mtype_of_valueptr e0 l e)
  | SetField (_, e1, _) -> type_of l e1
  | FieldIsNull _ -> bool
  | GetField (path, e) ->
      let mt = mtype_of_valueptr e0 l e in
      TValue (type_of_path mt path)
  | Pair (e1, e2) ->
      pair (type_of l e1) (type_of l e2)
  | Fst e ->
      (match type_of l e with
      | TPair (t, _) -> t
      | t -> raise (Type_error (e0, e, t, "be a pair")))
  | Snd e ->
      (match type_of l e with
      | TPair (_, t) -> t
      | t -> raise (Type_error (e0, e, t, "be a pair")))
  | MapPair (_, e) ->
      (match type_of l e with
      | TFunction (_, t) -> t
      | t -> raise (Type_error (e0, e, t, "be a function")))
  | Identifier n as e ->
      (try List.assoc e l
      with Not_found ->
          Printf.eprintf "Cannot find identifier %S in %a\n%!"
            n (List.print (fun oc (e, _) -> print_expr oc e)) l ;
          raise Not_found)
  | Let (n, e1, e2) ->
      type_of ((Identifier n, (type_of l e1))::l) e2
  | Function (fid, ts, e) ->
      let l = Array.fold_lefti (fun l i t -> (Param (fid, i), t)::l) l ts in
      TFunction (ts, type_of l e)
  | Param (fid, n) as e ->
      (try List.assoc e l
      with Not_found ->
          Printf.eprintf "Cannot find parameter #%d of function %d in %a\n%!"
            n fid (List.print (fun oc (e, _) -> print_expr oc e)) l ;
          raise Not_found)
  | Choose (_, e, _) -> type_of l e
  | ReadWhile (_, _, e, _) -> pair (type_of l e) dataptr
  | LoopWhile (_, _, e) -> type_of l e
  | LoopUntil (_, _, e) -> type_of l e
  | Repeat (_, _, _, e) -> type_of l e

(* depth last, pass the list of bound identifiers along the way: *)
let rec fold_expr u l f e =
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
  | Dump e1
  | DerefValuePtr e1
  | Comment (_, e1)
  | IsNull e1
  | ToNullable e1
  | ToNotNullable e1
  | LogNot e1
  | StringOfInt e1
  | StringOfChar e1
  | FloatOfQWord e1
  | QWordOfFloat e1
  | StringOfFloat e1
  | FloatOfString e1
  | CharOfString e1
  | U8OfString e1
  | U16OfString e1
  | U24OfString e1
  | U32OfString e1
  | U40OfString e1
  | U48OfString e1
  | U56OfString e1
  | U64OfString e1
  | U128OfString e1
  | I8OfString e1
  | I16OfString e1
  | I24OfString e1
  | I32OfString e1
  | I40OfString e1
  | I48OfString e1
  | I56OfString e1
  | I64OfString e1
  | I128OfString e1
  | U8OfByte e1
  | ByteOfU8 e1
  | U16OfWord e1
  | WordOfU16 e1
  | U32OfDWord e1
  | DWordOfU32 e1
  | U64OfQWord e1
  | QWordOfU64 e1
  | U128OfOWord e1
  | OWordOfU128 e1
  | U8OfChar e1
  | CharOfU8 e1
  | SizeOfU32 e1
  | U32OfSize e1
  | BitOfBool e1
  | BoolOfBit e1
  | U8OfBool e1
  | BoolOfU8 e1
  | StringLength e1
  | StringOfBytes e1
  | BytesOfString e1
  | ListLength e1
  | ReadByte e1
  | ReadWord (_, e1)
  | ReadDWord (_, e1)
  | ReadQWord (_, e1)
  | ReadOWord (_, e1)
  | RemSize e1
  | Not e1
  | ToU8 e1
  | ToI8 e1
  | ToU16 e1
  | ToI16 e1
  | ToU24 e1
  | ToI24 e1
  | ToU32 e1
  | ToI32 e1
  | ToU40 e1
  | ToI40 e1
  | ToU48 e1
  | ToI48 e1
  | ToU56 e1
  | ToI56 e1
  | ToU64 e1
  | ToI64 e1
  | ToU128 e1
  | ToI128 e1
  | FieldIsNull (_, e1)
  | GetField (_, e1)
  | Fst e1
  | Snd e1 ->
      fold_expr u l f e1
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
      fold_expr (fold_expr u l f e1) l f e2
  | Let (s, e1, e2) ->
      fold_expr (fold_expr u l f e1) ((Identifier s, type_of l e1)::l) f e2
  | SetBit (e1, e2, e3)
  | BlitByte (e1, e2, e3)
  | Choose (e1, e2, e3)
  | LoopWhile (e1, e2, e3)
  | LoopUntil (e1, e2, e3) ->
      fold_expr (fold_expr (fold_expr u l f e1) l f e2) l f e3
  | ReadWhile (e1, e2, e3, e4)
  | Repeat (e1, e2, e3, e4) ->
      fold_expr (fold_expr (fold_expr (fold_expr u l f e1) l f e2) l f e3) l f e4
  | Seq es ->
      List.fold_left (fun u e1 -> fold_expr u l f e1) u es
  | Function (id, ts, e1) ->
      let l = Array.fold_lefti (fun l i t -> (Param (id, i), t)::l) l ts in
      fold_expr u l f e1

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
      | TValuePtr mt ->
          let exp = TValue (type_of_path mt path) in
          check_eq l e2 exp
      | t -> raise (Type_error (e0, e1, t, "be a ValuePtr")) in
    let check_valueptr_path l e path =
      match type_of l e with
      | TValuePtr mt ->
          (try ignore (type_of_path mt path)
          with _ ->
            let s = IO.to_string print_maybe_nullable mt in
            raise (Type_error_path (e0, e, path, "stay within "^ s)))
      | t -> raise (Type_error (e0, e, t, "be a ValuePtr")) in
    let check_valueptr_path_nullable l e path nullable =
      match type_of l e with
      | TValuePtr mt ->
          let mt = type_of_path mt path in
          let act = is_nullable mt in
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
    | ToNullable e ->
        check_not_nullable l e
    | ToNotNullable e ->
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
        check_eq l e u64
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
    | BlitByte (e1, e2, e3) ->
        check_eq l e1 dataptr ;
        check_eq l e2 byte ;
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
  Let (pair_id, e,
    Let (n1, Fst (Identifier pair_id),
      Let (n2, Snd (Identifier pair_id),
        f (Identifier n1) (Identifier n2))))

(*$< DessserTypes *)
(*$inject
  let vptr = TValuePtr (Nullable (Mac TString))
  let func2 =
    Function (14, [|vptr; TDataPtr|],
      Let ("gen9_ds", Pair (GetField ([], Param (14, 0)),
                            Param (14, 0)),
        Let ("gen9_ds_0", Fst (Identifier "gen9_ds"),
          Let ("gen9_ds_1", Snd (Identifier "gen9_ds"),
            Pair (Identifier "gen9_ds_1",
                  Comment ("Serialize a String",
                    WriteByte (
                      WriteBytes (
                        WriteByte (Param (14, 1), ByteOfU8 (U8OfChar (Char '"'))),
                        BytesOfString (ToNotNullable (Identifier "gen9_ds_0"))),
                      ByteOfU8 (U8OfChar (Char '"')))))))))
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
