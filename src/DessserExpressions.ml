open Batteries
open Stdint
open DessserTypes
open DessserTools
open DessserFloatTools

(*$inject
  open Batteries
  open Stdint
  module T = DessserTypes *)

type endianness = LittleEndian | BigEndian

let string_of_endianness = function
  | LittleEndian -> "little-endian"
  | BigEndian -> "big-endian"

let print_endianness oc en =
  string_of_endianness en |> String.print oc

let endianness_of_string = function
  | "little-endian" -> LittleEndian
  | "big-endian" -> BigEndian
  | en -> invalid_arg ("endianness_of_string "^ en)

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
  | AppendByte
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
  | ReadWhile
      (* Cond (byte->bool) * Reducer ('a->byte->'a) * Init ('a) * Start pos ->
           Result ('a*ptr)
        Read whenever cond returns true, or the input stream is exhausted *)
  | Repeat (* From * To * body (idx->'a->'a) * Init value *)

type e =
  | Seq of e list
  | E0 of e0
  | E1 of e1 * e
  | E2 of e2 * e * e
  | E3 of e3 * e * e * e
  | E4 of e4 * e * e * e * e

let rec e0_eq e1 e2 =
  match e1, e2 with
  | Null vt1, Null vt2 ->
      value_type_eq vt1 vt2
  | AllocValue mn1, AllocValue mn2 ->
      maybe_nullable_eq mn1 mn2
  | e1, e2 ->
      (* Assuming here and below that when the constructors are different
       * the generic equality does not look t the fields and therefore won't
       * encounter functional values: *)
      e1 = e2

and e1_eq e1 e2 =
  match e1, e2 with
  | Function (fid1, typ1), Function (fid2, typ2) ->
      fid1 = fid2 && Array.for_all2 typ_eq typ1 typ2
  | e1, e2 -> e1 = e2

and e2_eq e1 e2 = e1 = e2

and e3_eq e1 e2 = e1 = e2

and e4_eq e1 e2 = e1 = e2

and expr_eq e1 e2 =
  match e1, e2 with
  | Seq e1s, Seq e2s ->
      List.for_all2 expr_eq e1s e2s
  | E0 op1, E0 op2 ->
      e0_eq op1 op2
  | E1 (op1, e11), E1 (op2, e21) ->
      e1_eq op1 op2 && expr_eq e11 e21
  | E2 (op1, e11, e12), E2 (op2, e21, e22) ->
      e2_eq op1 op2 && expr_eq e11 e21 && expr_eq e12 e22
  | E3 (op1, e11, e12, e13), E3 (op2, e21, e22, e23) ->
      e3_eq op1 op2 && expr_eq e11 e21 && expr_eq e12 e22 && expr_eq e13 e23
  | E4 (op1, e11, e12, e13, e14), E4 (op2, e21, e22, e23, e24) ->
      e4_eq op1 op2 && expr_eq e11 e21 && expr_eq e12 e22 && expr_eq e13 e23 && expr_eq e14 e24
  | _ -> false

(* Create a function expression and return its id: *)
let func =
  let next_id = ref 0 in
  fun typs f ->
    let id = !next_id in
    incr next_id ;
    E1 (Function (id, typs), f id)

(* Specialized to a given arity: *)
let func1 t1 f =
  func [| t1 |] (fun fid ->
    let p1 = E0 (Param (fid, 0)) in
    f p1)

let func2 t1 t2 f =
  func [| t1 ; t2 |] (fun fid ->
    let p1 = E0 (Param (fid, 0))
    and p2 = E0 (Param (fid, 1)) in
    f p1 p2)

let func3 t1 t2 t3 f =
  func [| t1 ; t2 ; t3 |] (fun fid ->
    let p1 = E0 (Param (fid, 0))
    and p2 = E0 (Param (fid, 1))
    and p3 = E0 (Param (fid, 2)) in
    f p1 p2 p3)

let string_of_e0 = function
  | Param (fid, n) -> "param "^ string_of_int fid ^" "^ string_of_int n
  | Null vt -> "null "^ String.quote (IO.to_string print_value_type vt)
  | Float f -> "float "^ hexstring_of_float f
  | String s -> "string "^ String.quote s
  | Bool b -> "bool "^ Bool.to_string b
  | Char c -> "char "^ String.quote (String.of_char c)
  | U8 n -> "u8 "^ string_of_int n
  | U16 n -> "u16 "^ string_of_int n
  | U24 n -> "u24 "^ string_of_int n
  | U32 n -> "u32 "^ Uint32.to_string n
  | U40 n -> "u40 "^ Uint40.to_string n
  | U48 n -> "u48 "^ Uint48.to_string n
  | U56 n -> "u56 "^ Uint56.to_string n
  | U64 n -> "u64 "^ Uint64.to_string n
  | U128 n -> "u128 "^ Uint128.to_string n
  | I8 n -> "i8 "^ string_of_int n
  | I16 n -> "i16 "^ string_of_int n
  | I24 n -> "i24 "^ string_of_int n
  | I32 n -> "i32 "^ Int32.to_string n
  | I40 n -> "i40 "^ Int64.to_string n
  | I48 n -> "i48 "^ Int64.to_string n
  | I56 n -> "i56 "^ Int64.to_string n
  | I64 n -> "i64 "^ Int64.to_string n
  | I128 n -> "i128 "^ Int128.to_string n
  | Bit b -> "bit "^ Bool.to_string b
  | Size n -> "size "^ string_of_int n
  | Byte n -> "byte "^ string_of_int n
  | Word n -> "word "^ string_of_int n
  | DWord n -> "dword "^ Uint32.to_string n
  | QWord n -> "qword "^ Uint64.to_string n
  | OWord n -> "oword "^ Uint128.to_string n
  | DataPtrOfString s -> "data-ptr-of-string "^ String.quote s
  | AllocValue mn ->
      "alloc-value "^ String.quote (IO.to_string print_maybe_nullable mn)
  | Identifier s -> "identifier "^ String.quote s

let string_of_path = IO.to_string print_path

let string_of_e1 = function
  | Function (fid, typs) ->
      "function "^ string_of_int fid ^
      IO.to_string (Array.print ~first:" " ~sep:" " ~last:"" (fun oc t ->
        Printf.fprintf oc "%S" (IO.to_string print_typ t))) typs
  | Comment s -> "comment "^ String.quote s
  | FieldIsNull p -> "field-is-null "^ string_of_path p
  | GetField p -> "get-field "^ string_of_path p
  | Dump -> "dump"
  | Ignore -> "ignore"
  | IsNull -> "is-null"
  | ToNullable -> "to-nullable"
  | ToNotNullable -> "to-not-nullable"
  | StringOfFloat -> "string-of-float"
  | StringOfChar -> "string-of-char"
  | StringOfInt -> "string-of-int"
  | FloatOfString -> "float-of-string"
  | CharOfString -> "char-of-string"
  | U8OfString -> "u8-of-string"
  | U16OfString -> "u16-of-string"
  | U24OfString -> "u24-of-string"
  | U32OfString -> "u32-of-string"
  | U40OfString -> "u40-of-string"
  | U48OfString -> "u48-of-string"
  | U56OfString -> "u56-of-string"
  | U64OfString -> "u64-of-string"
  | U128OfString -> "u128-of-string"
  | I8OfString -> "i8-of-string"
  | I16OfString -> "i16-of-string"
  | I24OfString -> "i24-of-string"
  | I32OfString -> "i32-of-string"
  | I40OfString -> "i40-of-string"
  | I48OfString -> "i48-of-string"
  | I56OfString -> "i56-of-string"
  | I64OfString -> "i64-of-string"
  | I128OfString -> "i128-of-string"
  | ToU8 -> "to-u8"
  | ToU16 -> "to-u16"
  | ToU24 -> "to-u24"
  | ToU32 -> "to-u32"
  | ToU40 -> "to-u40"
  | ToU48 -> "to-u48"
  | ToU56 -> "to-u56"
  | ToU64 -> "to-u64"
  | ToU128 -> "to-u128"
  | ToI8 -> "to-i8"
  | ToI16 -> "to-i16"
  | ToI24 -> "to-i24"
  | ToI32 -> "to-i32"
  | ToI40 -> "to-i40"
  | ToI48 -> "to-i48"
  | ToI56 -> "to-i56"
  | ToI64 -> "to-i64"
  | ToI128 -> "to-i128"
  | LogNot -> "log-not"
  | FloatOfQWord -> "float-of-qword"
  | QWordOfFloat -> "qword-of-float"
  | U8OfByte -> "u8-of-byte"
  | ByteOfU8 -> "byte-of-u8"
  | U16OfWord -> "u16-of-word"
  | WordOfU16 -> "word-of-u16"
  | U32OfDWord -> "u32-of-dword"
  | DWordOfU32 -> "dword-of-u32"
  | U64OfQWord -> "u64-of-qword"
  | QWordOfU64 -> "qword-of-u64"
  | U128OfOWord -> "u128-of-oword"
  | OWordOfU128 -> "oword-of-u128"
  | U8OfChar -> "u8-of-char"
  | CharOfU8 -> "char-of-u8"
  | SizeOfU32 -> "size-of-u32"
  | U32OfSize -> "u32-of-size"
  | BitOfBool -> "bit-of-bool"
  | BoolOfBit -> "bool-of-bit"
  | U8OfBool -> "u8-of-bool"
  | BoolOfU8 -> "bool-of-u8"
  | StringLength -> "string-length"
  | StringOfBytes -> "string-of-bytes"
  | BytesOfString -> "bytes-of-string"
  | ListLength -> "list-length"
  | ReadByte -> "read-byte"
  | DataPtrPush -> "data-ptr-push"
  | DataPtrPop -> "data-ptr-pop"
  | RemSize -> "rem-size"
  | Not -> "not"
  | DerefValuePtr -> "deref-value-ptr"
  | Fst -> "fst"
  | Snd -> "snd"
  | ReadWord en -> "read-word "^ string_of_endianness en
  | ReadDWord en -> "read-dword "^ string_of_endianness en
  | ReadQWord en -> "read-qword "^ string_of_endianness en
  | ReadOWord en -> "read-oword "^ string_of_endianness en

let string_of_e2 = function
  | Let s -> "let "^ String.quote s
  | SetField p -> "set-field "^ string_of_path p
  | Coalesce -> "coalesce"
  | Gt -> "gt"
  | Ge -> "ge"
  | Eq -> "eq"
  | Ne -> "ne"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Rem -> "rem"
  | LogAnd -> "log-and"
  | LogOr -> "log-or"
  | LogXor -> "log-xor"
  | LeftShift -> "left-shift"
  | RightShift -> "right-shift"
  | AppendByte -> "append-byte"
  | AppendBytes -> "append-bytes"
  | AppendString -> "append-string"
  | TestBit -> "test-bit"
  | ReadBytes -> "read-bytes"
  | PeekByte -> "peek-byte"
  | WriteByte -> "write-byte"
  | WriteBytes -> "write-bytes"
  | PokeByte -> "poke-byte"
  | DataPtrAdd -> "data-ptr-add"
  | DataPtrSub -> "data-ptr-sub"
  | And -> "and"
  | Or -> "or"
  | Pair -> "pair"
  | MapPair -> "map-pair"
  | PeekWord en -> "peek-word "^ string_of_endianness en
  | PeekDWord en -> "peek-dword "^ string_of_endianness en
  | PeekQWord en -> "peek-qword "^ string_of_endianness en
  | PeekOWord en -> "peek-oword "^ string_of_endianness en
  | WriteWord en -> "write-word "^ string_of_endianness en
  | WriteDWord en -> "write-dword "^ string_of_endianness en
  | WriteQWord en -> "write-qword "^ string_of_endianness en
  | WriteOWord en -> "write-oword "^ string_of_endianness en

let string_of_e3 = function
  | SetBit -> "set-bit"
  | BlitByte -> "blit-byte"
  | Choose -> "choose"
  | LoopWhile -> "loop-while"
  | LoopUntil -> "loop-until"

let string_of_e4 = function
  | ReadWhile -> "read-while"
  | Repeat -> "repeat"

let rec print_expr ?max_depth oc e =
  if Option.map_default (fun m -> m <= 0) false max_depth then
    pp oc "…"
  else
    let max_depth = Option.map pred max_depth in
    let p = print_expr ?max_depth in
    match e with
    | Seq es ->
        pp oc "(seq %a)" (List.print ~first:"" ~last:"" ~sep:" " p) es
    | E0 op -> pp oc "(%s)" (string_of_e0 op)
    | E1 (op, e1) ->
        pp oc "(%s %a)" (string_of_e1 op) p e1
    | E2 (op, e1, e2) ->
        pp oc "(%s %a %a)" (string_of_e2 op) p e1 p e2
    | E3 (op, e1, e2, e3) ->
        pp oc "(%s %a %a %a)" (string_of_e3 op) p e1 p e2 p e3
    | E4 (op, e1, e2, e3, e4) ->
        pp oc "(%s %a %a %a %a)" (string_of_e4 op) p e1 p e2 p e3 p e4

module Parser =
struct
  (* String representation of expressions are mere s-expressions.
   * strings are represented as OCaml quoted strings. *)
  type context = Blank | Enter | Leave | Symbol | String

  let rec tok str res i =
    let ctx = match res with (ctx, _)::_ -> ctx | [] -> Blank in
    if i >= String.length str then List.rev res
    else if Char.is_whitespace str.[i] then
      let res =
        if ctx = Blank || ctx = String then res
        else (Blank, i) :: res in
      tok str res (i + 1)
    else if str.[i] = '(' then
      let res =
        if ctx = String then res
        else (Enter, i) :: res in
      tok str res (i + 1)
    else if str.[i] = ')' then
      let res =
        if ctx = String then res
        else (Leave, i) :: res in
      tok str res (i + 1)
    else if str.[i] = '"' then
      let res =
        (* String start and stop with the double quotes, included: *)
        if ctx = String then (Blank, i + 1) :: res
        else (String, i) :: res in
      tok str res (i + 1)
    else if str.[i] = '\\' && ctx = String && i < String.length str - 1 then
      tok str res (i + 2)
    else
      let res =
        if ctx = Symbol || ctx = String then res
        else (Symbol, i) :: res in
      tok str res (i + 1)

  type sexpr =
    | Sym of string
    | Str of string
    | Lst of sexpr list

  let print oc =
    let rec loop indent sep oc = function
      | Sym s ->
          Printf.fprintf oc "%s%s" sep s
      | Str s ->
          Printf.fprintf oc "%s%S" sep s
      | Lst lst ->
          let indent = indent ^"  " in
          Printf.fprintf oc "\n%s(" indent ;
          List.iteri (fun i x ->
            loop indent (if i > 0 then " " else "") oc x ;
          ) lst ;
          Printf.fprintf oc ")" in
    loop "" "" oc

  exception Invalid_expression of sexpr
  exception Extraneous_expressions of int

  let () =
    Printexc.register_printer (function
      | Invalid_expression x ->
          Some (Printf.sprintf2 "Invalid S-Expression: %a" print x)
      | Extraneous_expressions i ->
          Some ("Extraneous expressions at offset "^ string_of_int i)
      | _ ->
          None)

  let sexpr_of_string str =
    let toks = tok str [] 0 in
    let add_sym sta sto lst =
      Sym (String.sub str sta (sto-sta)) :: lst in
    let add_str sta sto lst =
      Str (Scanf.sscanf (String.sub str sta (sto-sta)) "%S" identity) :: lst in
    let add_blk _sta _sto lst = lst in
    let rec loop lst adder = function
      | [] ->
          let sto = String.length str in
          List.rev (adder sto lst),
          []
      | (Symbol, i) :: rest -> loop (adder i lst) (add_sym i) rest
      | (String, i) :: rest -> loop (adder i lst) (add_str i) rest
      | (Blank, i) :: rest -> loop (adder i lst) (add_blk i) rest
      | (Enter, i) :: rest ->
          let lst = adder i lst in
          let sublst, rest = loop [] (add_blk i) rest in
          loop (Lst sublst :: lst) (add_blk i) rest
      | (Leave, i) :: rest ->
          List.rev (adder i lst),
          rest
    in
    let sublst, rest = loop [] (add_blk 0) toks in
    (match rest with
    | (_, i) :: _ -> raise (Extraneous_expressions i)
    | [] -> ()) ;
    sublst

  (*$< Parser *)

  (*$= sexpr_of_string & ~printer:(IO.to_string (List.print print))
    [ Sym "glop" ] (sexpr_of_string "glop")
    [ Str "glop" ] (sexpr_of_string "\"glop\"")
    [ Lst [ Sym "pas" ; Sym "glop" ] ] (sexpr_of_string "(pas glop)")
    [ Lst [ Sym "pas" ; Sym "glop" ] ] (sexpr_of_string " (pas   glop ) ")
    [ Lst [ Lst [ Sym "pas" ; Str "glop" ] ; Lst [ Sym "glop" ] ] ] \
      (sexpr_of_string "((pas \"glop\") (glop))")
  *)

  let expr str =
    let rec e = function
      | Lst (Sym "seq" :: xs) -> Seq (List.map e xs)
      (* e0 *)
      | Lst [ Sym "param" ; Sym fid ; Sym n ] ->
          E0 (Param (int_of_string fid, int_of_string n))
      | Lst [ Sym "null" ; Str vt ] ->
          E0 (Null (Parser.maybe_nullable_of_string vt |> to_value_type))
      | Lst [ Sym "float" ; Sym f ] -> E0 (Float (float_of_anystring f))
      | Lst [ Sym "string" ; Str s ] -> E0 (String s)
      | Lst [ Sym "bool" ; Sym b ] -> E0 (Bool (Bool.of_string b))
      | Lst [ Sym "char" ; Str c ] -> assert (String.length c = 1) ; E0 (Char c.[0])
      | Lst [ Sym "u8" ; Sym n ] -> E0 (U8 (int_of_string n))
      | Lst [ Sym "u16" ; Sym n ] -> E0 (U16 (int_of_string n))
      | Lst [ Sym "u24" ; Sym n ] -> E0 (U24 (int_of_string n))
      | Lst [ Sym "u32" ; Sym n ] -> E0 (U32 (Uint32.of_string n))
      | Lst [ Sym "u40" ; Sym n ] -> E0 (U40 (Uint40.of_string n))
      | Lst [ Sym "u48" ; Sym n ] -> E0 (U48 (Uint48.of_string n))
      | Lst [ Sym "u56" ; Sym n ] -> E0 (U56 (Uint56.of_string n))
      | Lst [ Sym "u64" ; Sym n ] -> E0 (U64 (Uint64.of_string n))
      | Lst [ Sym "u128" ; Sym n ] -> E0 (U128 (Uint128.of_string n))
      | Lst [ Sym "i8" ; Sym n ] -> E0 (I8 (int_of_string n))
      | Lst [ Sym "i16" ; Sym n ] -> E0 (I16 (int_of_string n))
      | Lst [ Sym "i24" ; Sym n ] -> E0 (I24 (int_of_string n))
      | Lst [ Sym "i32" ; Sym n ] -> E0 (I32 (Int32.of_string n))
      | Lst [ Sym "i40" ; Sym n ] -> E0 (I40 (Int64.of_string n))
      | Lst [ Sym "i48" ; Sym n ] -> E0 (I48 (Int64.of_string n))
      | Lst [ Sym "i56" ; Sym n ] -> E0 (I56 (Int64.of_string n))
      | Lst [ Sym "i64" ; Sym n ] -> E0 (I64 (Int64.of_string n))
      | Lst [ Sym "i128" ; Sym n ] -> E0 (I128 (Int128.of_string n))
      | Lst [ Sym "bit" ; Sym b ] -> E0 (Bit (Bool.of_string b))
      | Lst [ Sym "size" ; Sym n ] -> E0 (Size (int_of_string n))
      | Lst [ Sym "byte" ; Sym n ] -> E0 (Byte (int_of_string n))
      | Lst [ Sym "word" ; Sym n ] -> E0 (Word (int_of_string n))
      | Lst [ Sym "dword" ; Sym n ] -> E0 (DWord (Uint32.of_string n))
      | Lst [ Sym "qword" ; Sym n ] -> E0 (QWord (Uint64.of_string n))
      | Lst [ Sym "oword" ; Sym n ] -> E0 (OWord (Uint128.of_string n))
      | Lst [ Sym "data-ptr-of-string" ; Str s ] -> E0 (DataPtrOfString s)
      | Lst [ Sym "alloc-value" ; Str mn ] ->
          E0 (AllocValue (Parser.maybe_nullable_of_string mn))
      | Lst [ Sym "identifier" ; Str s ] -> E0 (Identifier s)
      (* e1 *)
      | Lst (Sym "function" :: Sym fid :: (_ :: _ :: _ as tail)) ->
          let typs, x = list_split_last tail in
          let typs =
            Array.of_list typs |>
            Array.map (function
              | Str s -> Parser.typ_of_string s
              | x ->
                  Printf.sprintf2 "Need a type (in string) not %a" print x |>
                  failwith
            ) in
          E1 (Function (int_of_string fid, typs), e x)
      | Lst [ Sym "comment" ; Str s ; x ] ->
          E1 (Comment s, e x)
      | Lst [ Sym "field-is-null" ; Sym p ; x ] ->
          E1 (FieldIsNull (path_of_string p), e x)
      | Lst [ Sym "get-field" ; Sym p ; x ] ->
          E1 (GetField (path_of_string p), e x)
      | Lst [ Sym "dump" ; x ] -> E1 (Dump, e x)
      | Lst [ Sym "ignore" ; x ] -> E1 (Ignore, e x)
      | Lst [ Sym "is-null" ; x ] -> E1 (IsNull, e x)
      | Lst [ Sym "to-nullable" ; x ] -> E1 (ToNullable, e x)
      | Lst [ Sym "to-not-nullable" ; x ] -> E1 (ToNotNullable, e x)
      | Lst [ Sym "string-of-float" ; x ] -> E1 (StringOfFloat, e x)
      | Lst [ Sym "string-of-char" ; x ] -> E1 (StringOfChar, e x)
      | Lst [ Sym "string-of-int" ; x ] -> E1 (StringOfInt, e x)
      | Lst [ Sym "float-of-string" ; x ] -> E1 (FloatOfString, e x)
      | Lst [ Sym "char-of-string" ; x ] -> E1 (CharOfString, e x)
      | Lst [ Sym "u8-of-string" ; x ] -> E1 (U8OfString, e x)
      | Lst [ Sym "u16-of-string" ; x ] -> E1 (U16OfString, e x)
      | Lst [ Sym "u24-of-string" ; x ] -> E1 (U24OfString, e x)
      | Lst [ Sym "u32-of-string" ; x ] -> E1 (U32OfString, e x)
      | Lst [ Sym "u40-of-string" ; x ] -> E1 (U40OfString, e x)
      | Lst [ Sym "u48-of-string" ; x ] -> E1 (U48OfString, e x)
      | Lst [ Sym "u56-of-string" ; x ] -> E1 (U56OfString, e x)
      | Lst [ Sym "u64-of-string" ; x ] -> E1 (U64OfString, e x)
      | Lst [ Sym "u128-of-string" ; x ] -> E1 (U128OfString, e x)
      | Lst [ Sym "i8-of-string" ; x ] -> E1 (I8OfString, e x)
      | Lst [ Sym "i16-of-string" ; x ] -> E1 (I16OfString, e x)
      | Lst [ Sym "i24-of-string" ; x ] -> E1 (I24OfString, e x)
      | Lst [ Sym "i32-of-string" ; x ] -> E1 (I32OfString, e x)
      | Lst [ Sym "i40-of-string" ; x ] -> E1 (I40OfString, e x)
      | Lst [ Sym "i48-of-string" ; x ] -> E1 (I48OfString, e x)
      | Lst [ Sym "i56-of-string" ; x ] -> E1 (I56OfString, e x)
      | Lst [ Sym "i64-of-string" ; x ] -> E1 (I64OfString, e x)
      | Lst [ Sym "i128-of-string" ; x ] -> E1 (I128OfString, e x)
      | Lst [ Sym "to-u8" ; x ] -> E1 (ToU8, e x)
      | Lst [ Sym "to-u16" ; x ] -> E1 (ToU16, e x)
      | Lst [ Sym "to-u24" ; x ] -> E1 (ToU24, e x)
      | Lst [ Sym "to-u32" ; x ] -> E1 (ToU32, e x)
      | Lst [ Sym "to-u40" ; x ] -> E1 (ToU40, e x)
      | Lst [ Sym "to-u48" ; x ] -> E1 (ToU48, e x)
      | Lst [ Sym "to-u56" ; x ] -> E1 (ToU56, e x)
      | Lst [ Sym "to-u64" ; x ] -> E1 (ToU64, e x)
      | Lst [ Sym "to-u128" ; x ] -> E1 (ToU128, e x)
      | Lst [ Sym "to-i8" ; x ] -> E1 (ToI8, e x)
      | Lst [ Sym "to-i16" ; x ] -> E1 (ToI16, e x)
      | Lst [ Sym "to-i24" ; x ] -> E1 (ToI24, e x)
      | Lst [ Sym "to-i32" ; x ] -> E1 (ToI32, e x)
      | Lst [ Sym "to-i40" ; x ] -> E1 (ToI40, e x)
      | Lst [ Sym "to-i48" ; x ] -> E1 (ToI48, e x)
      | Lst [ Sym "to-i56" ; x ] -> E1 (ToI56, e x)
      | Lst [ Sym "to-i64" ; x ] -> E1 (ToI64, e x)
      | Lst [ Sym "to-i128" ; x ] -> E1 (ToI128, e x)
      | Lst [ Sym "log-not" ; x ] -> E1 (LogNot, e x)
      | Lst [ Sym "float-of-qword" ; x ] -> E1 (FloatOfQWord, e x)
      | Lst [ Sym "qword-of-float" ; x ] -> E1 (QWordOfFloat, e x)
      | Lst [ Sym "u8-of-byte" ; x ] -> E1 (U8OfByte, e x)
      | Lst [ Sym "byte-of-u8" ; x ] -> E1 (ByteOfU8, e x)
      | Lst [ Sym "u16-of-word" ; x ] -> E1 (U16OfWord, e x)
      | Lst [ Sym "word-of-u16" ; x ] -> E1 (WordOfU16, e x)
      | Lst [ Sym "u32-of-dword" ; x ] -> E1 (U32OfDWord, e x)
      | Lst [ Sym "dword-of-u32" ; x ] -> E1 (DWordOfU32, e x)
      | Lst [ Sym "u64-of-qword" ; x ] -> E1 (U64OfQWord, e x)
      | Lst [ Sym "qword-of-u64" ; x ] -> E1 (QWordOfU64, e x)
      | Lst [ Sym "u128-of-oword" ; x ] -> E1 (U128OfOWord, e x)
      | Lst [ Sym "oword-of-u128" ; x ] -> E1 (OWordOfU128, e x)
      | Lst [ Sym "u8-of-char" ; x ] -> E1 (U8OfChar, e x)
      | Lst [ Sym "char-of-u8" ; x ] -> E1 (CharOfU8, e x)
      | Lst [ Sym "size-of-u32" ; x ] -> E1 (SizeOfU32, e x)
      | Lst [ Sym "u32-of-size" ; x ] -> E1 (U32OfSize, e x)
      | Lst [ Sym "bit-of-bool" ; x ] -> E1 (BitOfBool, e x)
      | Lst [ Sym "bool-of-bit" ; x ] -> E1 (BoolOfBit, e x)
      | Lst [ Sym "u8-of-bool" ; x ] -> E1 (U8OfBool, e x)
      | Lst [ Sym "bool-of-u8" ; x ] -> E1 (BoolOfU8, e x)
      | Lst [ Sym "string-length" ; x ] -> E1 (StringLength, e x)
      | Lst [ Sym "string-of-bytes" ; x ] -> E1 (StringOfBytes, e x)
      | Lst [ Sym "bytes-of-string" ; x ] -> E1 (BytesOfString, e x)
      | Lst [ Sym "list-length" ; x ] -> E1 (ListLength, e x)
      | Lst [ Sym "read-byte" ; x ] -> E1 (ReadByte, e x)
      | Lst [ Sym "data-ptr-push" ; x ] -> E1 (DataPtrPush, e x)
      | Lst [ Sym "data-ptr-pop" ; x ] -> E1 (DataPtrPop, e x)
      | Lst [ Sym "rem-size" ; x ] -> E1 (RemSize, e x)
      | Lst [ Sym "not" ; x ] -> E1 (Not, e x)
      | Lst [ Sym "deref-value-ptr" ; x ] -> E1 (DerefValuePtr, e x)
      | Lst [ Sym "fst" ; x ] -> E1 (Fst, e x)
      | Lst [ Sym "snd" ; x ] -> E1 (Snd, e x)
      | Lst [ Sym "read-word" ; Sym en ; x ] ->
          E1 (ReadWord (endianness_of_string en), e x)
      | Lst [ Sym "read-dword" ; Sym en ; x ] ->
          E1 (ReadDWord (endianness_of_string en), e x)
      | Lst [ Sym "read-qword" ; Sym en ; x ] ->
          E1 (ReadQWord (endianness_of_string en), e x)
      | Lst [ Sym "read-oword" ; Sym en ; x ] ->
          E1 (ReadOWord (endianness_of_string en), e x)
      (* e2 *)
      | Lst [ Sym "let" ; Str s ; x1 ; x2 ] -> E2 (Let s, e x1, e x2)
      | Lst [ Sym "set-field" ; Sym p ; x1 ; x2 ] ->
          E2 (SetField (path_of_string p), e x1, e x2)
      | Lst [ Sym "coalesce" ; x1 ; x2 ] -> E2 (Coalesce, e x1, e x2)
      | Lst [ Sym "gt" ; x1 ; x2 ] -> E2 (Gt, e x1, e x2)
      | Lst [ Sym "ge" ; x1 ; x2 ] -> E2 (Ge, e x1, e x2)
      | Lst [ Sym "eq" ; x1 ; x2 ] -> E2 (Eq, e x1, e x2)
      | Lst [ Sym "ne" ; x1 ; x2 ] -> E2 (Ne, e x1, e x2)
      | Lst [ Sym "add" ; x1 ; x2 ] -> E2 (Add, e x1, e x2)
      | Lst [ Sym "sub" ; x1 ; x2 ] -> E2 (Sub, e x1, e x2)
      | Lst [ Sym "mul" ; x1 ; x2 ] -> E2 (Mul, e x1, e x2)
      | Lst [ Sym "div" ; x1 ; x2 ] -> E2 (Div, e x1, e x2)
      | Lst [ Sym "rem" ; x1 ; x2 ] -> E2 (Rem, e x1, e x2)
      | Lst [ Sym "log-and" ; x1 ; x2 ] -> E2 (LogAnd, e x1, e x2)
      | Lst [ Sym "log-or" ; x1 ; x2 ] -> E2 (LogOr, e x1, e x2)
      | Lst [ Sym "log-xor" ; x1 ; x2 ] -> E2 (LogXor, e x1, e x2)
      | Lst [ Sym "left-shift" ; x1 ; x2 ] -> E2 (LeftShift, e x1, e x2)
      | Lst [ Sym "right-shift" ; x1 ; x2 ] -> E2 (RightShift, e x1, e x2)
      | Lst [ Sym "append-byte" ; x1 ; x2 ] -> E2 (AppendByte, e x1, e x2)
      | Lst [ Sym "append-bytes" ; x1 ; x2 ] -> E2 (AppendBytes, e x1, e x2)
      | Lst [ Sym "append-string" ; x1 ; x2 ] -> E2 (AppendString, e x1, e x2)
      | Lst [ Sym "test-bit" ; x1 ; x2 ] -> E2 (TestBit, e x1, e x2)
      | Lst [ Sym "read-bytes" ; x1 ; x2 ] -> E2 (ReadBytes, e x1, e x2)
      | Lst [ Sym "peek-byte" ; x1 ; x2 ] -> E2 (PeekByte, e x1, e x2)
      | Lst [ Sym "write-byte" ; x1 ; x2 ] -> E2 (WriteByte, e x1, e x2)
      | Lst [ Sym "write-bytes" ; x1 ; x2 ] -> E2 (WriteBytes, e x1, e x2)
      | Lst [ Sym "poke-byte" ; x1 ; x2 ] -> E2 (PokeByte, e x1, e x2)
      | Lst [ Sym "data-ptr-add" ; x1 ; x2 ] -> E2 (DataPtrAdd, e x1, e x2)
      | Lst [ Sym "data-ptr-sub" ; x1 ; x2 ] -> E2 (DataPtrSub, e x1, e x2)
      | Lst [ Sym "and" ; x1 ; x2 ] -> E2 (And, e x1, e x2)
      | Lst [ Sym "or" ; x1 ; x2 ] -> E2 (Or, e x1, e x2)
      | Lst [ Sym "pair" ; x1 ; x2 ] -> E2 (Pair, e x1, e x2)
      | Lst [ Sym "map-pair" ; x1 ; x2 ] -> E2 (MapPair, e x1, e x2)
      | Lst [ Sym "peek-word" ; Sym en ; x1 ; x2 ] ->
          E2 (PeekWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "peek-dword" ; Sym en ; x1 ; x2 ] ->
          E2 (PeekDWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "peek-qword" ; Sym en ; x1 ; x2 ] ->
          E2 (PeekQWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "peek-oword" ; Sym en ; x1 ; x2 ] ->
          E2 (PeekOWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "write-word" ; Sym en ; x1 ; x2 ] ->
          E2 (WriteWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "write-dword" ; Sym en ; x1 ; x2 ] ->
          E2 (WriteDWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "write-qword" ; Sym en ; x1 ; x2 ] ->
          E2 (WriteQWord (endianness_of_string en), e x1, e x2)
      | Lst [ Sym "write-oword" ; Sym en ; x1 ; x2 ] ->
          E2 (WriteOWord (endianness_of_string en), e x1, e x2)
      (* e3 *)
      | Lst [ Sym "set-bit" ; x1 ; x2 ; x3 ] -> E3 (SetBit, e x1, e x2, e x3)
      | Lst [ Sym "blit-byte" ; x1 ; x2 ; x3 ] -> E3 (BlitByte, e x1, e x2, e x3)
      | Lst [ Sym "choose" ; x1 ; x2 ; x3 ] -> E3 (Choose, e x1, e x2, e x3)
      | Lst [ Sym "loop-while" ; x1 ; x2 ; x3 ] -> E3 (LoopWhile, e x1, e x2, e x3)
      | Lst [ Sym "loop-until" ; x1 ; x2 ; x3 ] -> E3 (LoopUntil, e x1, e x2, e x3)
      (* e4 *)
      | Lst [ Sym "read-while" ; x1 ; x2 ; x3 ; x4 ] ->
          E4 (ReadWhile, e x1, e x2, e x3, e x4)
      | Lst [ Sym "repeat" ; x1 ; x2 ; x3 ; x4 ] ->
          E4 (Repeat, e x1, e x2, e x3, e x4)


      | x -> raise (Invalid_expression x)
    in
    List.map e (sexpr_of_string str)

  (*$= expr & ~printer:(IO.to_string (List.print print_expr))
    [ Ops.u8 42 ] (expr "(u8 42)")
    [ Ops.float 1. ] (expr "(float 1.0)")
    [ Ops.char '\019' ] (expr "(char \"\\019\")")
    [ Ops.null T.(Mac TString) ] (expr "(null \"string\")")
    [ Ops.alloc_value T.(Nullable (Mac TU24)) ] (expr "(alloc-value \"U24?\")")
    [ Ops.i56 (-1567305629568954678L) ] (expr "(i56 -1567305629568954678)")
    [ Ops.i128 (Int128.of_string "-1213949874624120272") ] \
      (expr "(i128 -1213949874624120272)")
    [ Ops.bool false ] (expr "(bool false)")
    [ Ops.u64 (Uint64.of_int 8) ] (expr "(u64 8)")
    [ Ops.seq [ Ops.u16 45134 ; Ops.u64 (Uint64.of_int 6)] ] \
      (expr "(seq (u16 45134) (u64 6))")
    [ Ops.comment "foo" (Ops.u32 (Uint32.of_int 2)) ] \
      (expr "(comment \"foo\" (u32 2))")
  *)

  (*$>*)
end

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
  | E2 (AppendByte, _, _) -> bytes
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
  | E1 (ToU8, _) -> u8
  | E1 (ToI8, _) -> i8
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
          TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 |
          TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128))) -> ()
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
    | E2 (AppendByte, e1, e2) ->
        check_eq l e1 bytes ;
        check_eq l e2 byte
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
    | E4 (ReadWhile, e1 (*byte->bool*), e2 (*'a->byte->'a*), e3 (*'a*), e4 (*ptr*)) ->
        check_params1 l e1 (fun t1 t2 ->
          check_param e1 0 t1 byte ;
          check_param e1 1 t2 bool) ;
        check_params2 l e2 (fun t1 t2 t3 ->
          check_eq l e3 t1 ;
          check_param e2 1 t2 byte ;
          check_eq l e3 t3) ;
        check_eq l e4 dataptr
    | E3 (LoopWhile, e1 (*'a->bool*), e2 (*'a->'a*), e3 (*'a*)) ->
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
(*$= type_of & ~printer:(IO.to_string print_typ)
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
  let u8_of_string e1 = E1 (U8OfString, e1)
  let u16_of_string e1 = E1 (U16OfString, e1)
  let u24_of_string e1 = E1 (U24OfString, e1)
  let u32_of_string e1 = E1 (U32OfString, e1)
  let u40_of_string e1 = E1 (U40OfString, e1)
  let u48_of_string e1 = E1 (U48OfString, e1)
  let u56_of_string e1 = E1 (U56OfString, e1)
  let u64_of_string e1 = E1 (U64OfString, e1)
  let u128_of_string e1 = E1 (U128OfString, e1)
  let i8_of_string e1 = E1 (I8OfString, e1)
  let i16_of_string e1 = E1 (I16OfString, e1)
  let i24_of_string e1 = E1 (I24OfString, e1)
  let i32_of_string e1 = E1 (I32OfString, e1)
  let i40_of_string e1 = E1 (I40OfString, e1)
  let i48_of_string e1 = E1 (I48OfString, e1)
  let i56_of_string e1 = E1 (I56OfString, e1)
  let i64_of_string e1 = E1 (I64OfString, e1)
  let i128_of_string e1 = E1 (I128OfString, e1)
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
  let and_ e1 e2 = E2 (And, e1, e2)
  let or_ e1 e2 = E2 (Or, e1, e2)
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
  let append_byte e1 e2 = E2 (AppendByte, e1, e2)
  let append_bytes e1 e2 = E2 (AppendBytes, e1, e2)
  let append_string e1 e2 = E2 (AppendString, e1, e2)
  let float_of_string e1 = E1 (FloatOfString, e1)
end
