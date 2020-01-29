open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open BackEndCLike
open DessserTools

module Config =
struct
  let preferred_def_extension = "cc"
  let preferred_decl_extension = "h"
  let compile_cmd ~optim ~link src dst =
    let optim = cap 0 3 optim in
    Printf.sprintf "g++ -std=c++17 -g -O%d -W -Wall -I src %s %S -o %S"
      optim (if link then "" else "-c") src dst

  let tuple_field_name i = "field_"^ string_of_int i

  let rec print_struct p oc id vts =
    let id = valid_identifier id in
    pp oc "%sstruct %s {\n" p.indent id ;
    indent_more p (fun () ->
      Array.iter (fun (field_name, vt) ->
        let typ_id = type_identifier p (TValue vt) in
        pp oc "%s%s %s;\n" p.indent typ_id field_name
      ) vts
    ) ;
    pp oc "%s};\n\n" p.indent

  and type_identifier p = function
    | TValue (Nullable t) ->
        "std::optional<"^ type_identifier p (TValue (NotNullable t)) ^">"
    | TValue (NotNullable (Mac TFloat)) -> "double"
    | TValue (NotNullable (Mac TString)) -> "std::string"
    | TValue (NotNullable (Mac TBool)) -> "bool"
    | TValue (NotNullable (Mac TChar)) -> "char"
    | TValue (NotNullable (Mac TI8)) -> "int8_t"
    | TValue (NotNullable (Mac TU8)) -> "uint8_t"
    | TValue (NotNullable (Mac TI16)) -> "int16_t"
    | TValue (NotNullable (Mac TU16)) -> "uint16_t"
    | TValue (NotNullable (Mac TI24)) -> "int32_t"
    | TValue (NotNullable (Mac TU24)) -> "uint32_t"
    | TValue (NotNullable (Mac TI32)) -> "int32_t"
    | TValue (NotNullable (Mac TU32)) -> "uint32_t"
    | TValue (NotNullable (Mac TI40)) -> "int64_t"
    | TValue (NotNullable (Mac TU40)) -> "uint64_t"
    | TValue (NotNullable (Mac TI48)) -> "int64_t"
    | TValue (NotNullable (Mac TU48)) -> "uint64_t"
    | TValue (NotNullable (Mac TI56)) -> "int64_t"
    | TValue (NotNullable (Mac TU56)) -> "uint64_t"
    | TValue (NotNullable (Mac TI64)) -> "int64_t"
    | TValue (NotNullable (Mac TU64)) -> "uint64_t"
    | TValue (NotNullable (Mac TI128)) -> "int128_t"
    | TValue (NotNullable (Mac TU128)) -> "uint128_t"
    | TValue (NotNullable (Usr t)) ->
        type_identifier p (TValue (NotNullable t.def))
    | TValue (NotNullable (TTup vts)) as t ->
        let vts = Array.mapi (fun i vt -> tuple_field_name i, vt) vts in
        declared_type p t (fun oc type_id -> print_struct p oc type_id vts) |>
        valid_identifier
    | TValue (NotNullable (TRec vts)) as t ->
        declared_type p t (fun oc type_id -> print_struct p oc type_id vts) |>
        valid_identifier
    | TValue (NotNullable (TVec (dim, typ))) ->
        Printf.sprintf "Vec<%d, %s>" dim (type_identifier p (TValue typ))
    | TValue (NotNullable (TList typ)) ->
        Printf.sprintf "List<%s>" (type_identifier p (TValue typ))
    | TValue (NotNullable (TMap _)) ->
        assert false (* No value of map type *)
    | TPair (t1, t2) ->
        "std::pair<"^ type_identifier p t1 ^", "^ type_identifier p t2 ^">"
    | TFunction (args, ret) ->
        "std::function<"^ type_identifier p ret ^
          IO.to_string (
            Array.print ~first:"(" ~last:")" ~sep:"," (fun oc t ->
              String.print oc (type_identifier p t))
          ) args ^">"
    | TVoid -> "void"
    | TDataPtr -> "Pointer"
    | TValuePtr vt -> type_identifier p (TValue vt) ^"*"
    | TSize -> "Size"
    | TBit -> "bool"
    | TByte -> "uint8_t"
    | TWord -> "uint16_t"
    | TDWord -> "uint32_t"
    | TQWord -> "uint64_t"
    | TOWord -> "uint128_t"
    | TBytes -> "Bytes"

  (* Identifiers used for function parameters: *)
  let param fid n = "p_"^ string_of_int fid ^"_"^ string_of_int n

  let print_binding n tn f oc =
    pp oc "%s %s(%t);" tn n f

  let print_comment oc s =
    pp oc "/* %s */" s

  let rec deref_path v vt = function
    | [] -> v
    | i :: path ->
        let rec deref_not_nullable v = function
          | NotNullable (Mac _ | TMap _) ->
              assert false
          | NotNullable (Usr t) ->
              deref_not_nullable v (NotNullable t.def)
          | NotNullable (TVec (_, vt))
          | NotNullable (TList vt) ->
              deref_path (v ^"["^ string_of_int i ^"]") vt path
          | NotNullable (TTup vts) ->
              deref_path (v ^"."^ tuple_field_name i) vts.(i) path
          | NotNullable (TRec vts) ->
              let name = valid_identifier (fst vts.(i)) in
              deref_path (v ^"."^ name) (snd vts.(i)) path
          | Nullable x ->
              deref_not_nullable (v ^".value()") (NotNullable x) in
        deref_not_nullable v vt

  let print_float_literal v oc =
    if v = infinity then
      String.print oc "std::numeric_limits<double>::infinity"
    else if v = neg_infinity then
      String.print oc "-std::numeric_limits<double>::infinity"
    else
      Legacy.Printf.sprintf "%h" v |> String.print oc

  let gen_sym pref = valid_identifier (gen_sym pref)

  let c_char_of c =
    match Char.code c with
    | 0x07 -> "\\a"
    | 0x08 -> "\\b"
    | 0x09 -> "\\t"
    | 0x0a -> "\\n"
    | 0x0b -> "\\v"
    | 0x0c -> "\\f"
    | 0x0d -> "\\r"
    | 0x22 -> "\\\""
    | 0x27 -> "\\'"
    | 0x3f -> "\\?"
    | 0x5c -> "\\\\"
    | n ->
        if char_is_printable c then String.of_char c
        else Printf.sprintf "\\%03o" n

  let rec print ?name emit p l e =
    let gen_sym ?name pref =
      match name with Some n -> n | None -> gen_sym pref in
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.indent in
    let unary_op op e1 =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s %s" op n1) in
    let binary_infix_op e1 op e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2 in
      emit ?name p l e (fun oc -> pp oc "%s %s %s" n1 op n2) in
    let method_call e1 m args =
      let n1 = print emit p l e1
      and ns = List.map (print emit p l) args in
      emit ?name p l e (fun oc ->
        pp oc "%s.%s%a" n1 m
          (List.print ~first:"(" ~last:")" ~sep:", " String.print) ns) in
    let member e1 m =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s.%s" n1 m) in
    match e with
    | E1 (Comment c, e1) ->
        pp p.def "%s/* %s */\n" p.indent c ;
        print ?name emit p l e1
    | Seq es ->
        List.fold_left (fun _ e -> print emit p l e) "must_not_be_used1" es
    | E1 (Ignore, e1) ->
        let n = print emit p l e1 in
        pp p.def "%s(void)%s;\n" p.indent n ;
        "must_not_be_used3"
    | E1 (Dump, e1) ->
        let n = print emit p l e1 in
        pp p.def "%sstd::cout << %s;\n" p.indent n ;
        "must_not_be_used2"
    | E1 (IsNull, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s.has_value ()" n)
    | E2 (Coalesce, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s.has_value () |? %s : %s" n1 n1 n2)
    | E1 (ToNullable, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> String.print oc n1)
    | E1 (ToNotNullable, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> Printf.fprintf oc "%s.value()" n1)
    | E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "std::nullopt")
    | E0 (Float f) ->
        emit ?name p l e (print_float_literal f)
    | E0 (String s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc s)
    | E0 (Bit b) | E0 (Bool b) ->
        emit ?name p l e (fun oc -> Bool.print oc b)
    | E0 (Char c) ->
        emit ?name p l e (fun oc -> pp oc "'%s'" (c_char_of c))
    | E0 (Byte i) | E0 (U8 i) ->
        emit ?name p l e (fun oc -> pp oc "%d" i)
    | E0 (Word i) | E0 (U16 i) ->
        emit ?name p l e (fun oc -> pp oc "%d" i)
    | E0 (U24 u) ->
        emit ?name p l e (fun oc -> pp oc "%dU" u)
    | E0 (DWord u) | E0 (U32 u) ->
        emit ?name p l e (fun oc -> pp oc "%sU" (Uint32.to_string u))
    | E0 (U40 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint40.to_string u))
    | E0 (U48 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint48.to_string u))
    | E0 (U56 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint56.to_string u))
    | E0 (QWord u) | E0 (U64 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint64.to_string u))
    | E0 (OWord u) | E0 (U128 u) ->
        emit ?name p l e (fun oc ->
          let lo = Uint128.to_uint64 u
          and hi = Uint128.(to_uint64 (shift_right_logical u 64)) in
          pp oc "((((uint128_t)%sULL) << 64U) | %sULL)"
            (Uint64.to_string hi)
            (Uint64.to_string lo))
    | E0 (I8 i) ->
        emit ?name p l e (fun oc -> pp oc "%d" i)
    | E0 (I16 i) ->
        emit ?name p l e (fun oc -> pp oc "%d" i)
    | E0 (I24 i) ->
        emit ?name p l e (fun oc -> pp oc "%dL" i)
    | E0 (I32 i) ->
        emit ?name p l e (fun oc -> pp oc "%sL" (Int32.to_string i))
    | E0 (I40 i) ->
        emit ?name p l e (fun oc -> pp oc "%LdLL" i)
    | E0 (I48 i) ->
        emit ?name p l e (fun oc -> pp oc "%LdLL" i)
    | E0 (I56 i) ->
        emit ?name p l e (fun oc -> pp oc "%LdLL" i)
    | E0 (I64 i) ->
        emit ?name p l e (fun oc -> pp oc "%LdLL" i)
    | E0 (I128 i) ->
        emit ?name p l e (fun oc ->
          let lo = Int128.to_int64 i
          and hi = Int128.(to_int64 (shift_right_logical i 64)) in
          pp oc "((((int128_t)%sLL) << 64) | %sLL)"
            (Int64.to_string hi)
            (Int64.to_string lo))
    | E0 (Size s) ->
        emit ?name p l e (fun oc -> pp oc "%dUL" s)
    | E2 (Gt, e1, e2) ->
        binary_infix_op e1 ">" e2
    | E2 (Ge, e1, e2) ->
        binary_infix_op e1 ">=" e2
    | E2 (Eq, e1, e2) ->
        binary_infix_op e1 "==" e2
    | E2 (Ne, e1, e2) ->
        binary_infix_op e1 "!=" e2
    | E2 (Add, e1, e2) ->
        binary_infix_op e1 "+" e2
    | E2 (Sub, e1, e2) ->
        binary_infix_op e1 "-" e2
    | E2 (Mul, e1, e2) ->
        binary_infix_op e1 "*" e2
    | E2 (Div, e1, e2) ->
        binary_infix_op e1 "/" e2
    | E2 (Rem, e1, e2) ->
        binary_infix_op e1 "%" e2
    | E2 (LogAnd, e1, e2) ->
        binary_infix_op e1 "&" e2
    | E2 (LogOr, e1, e2) ->
        binary_infix_op e1 "|" e2
    | E2 (LogXor, e1, e2) ->
        binary_infix_op e1 "^" e2
    | E1 (LogNot, e1) ->
        unary_op "~" e1
    | E2 (LeftShift, e1, e2) ->
        binary_infix_op e1 "<<" e2
    | E2 (RightShift, e1, e2) ->
        binary_infix_op e1 ">>" e2
    | E1 (StringOfInt, e1) ->
        let n1 = print emit p l e1 in
        (match type_of l e1 with
        | TValue (Nullable (Mac TU128) | NotNullable (Mac TU128)) ->
            emit ?name p l e (fun oc -> pp oc "string_of_u128(%s)" n1)
        | TValue (Nullable (Mac TI128) | NotNullable (Mac TI128)) ->
            emit ?name p l e (fun oc -> pp oc "string_of_i128(%s)" n1)
        | _ ->
            emit ?name p l e (fun oc -> pp oc "std::to_string(%s)" n1))
    | E1 (U8OfString, e1)
    | E1 (I8OfString, e1)
    | E1 (U16OfString, e1)
    | E1 (I16OfString, e1)
    | E1 (U24OfString, e1)
    | E1 (I24OfString, e1)
    | E1 (U32OfString, e1)
    | E1 (I32OfString, e1)
    | E1 (U40OfString, e1)
    | E1 (I40OfString, e1)
    | E1 (U48OfString, e1)
    | E1 (I48OfString, e1)
    | E1 (U56OfString, e1)
    | E1 (I56OfString, e1)
    | E1 (U64OfString, e1)
    | E1 (I64OfString, e1)
    | E1 (U128OfString, e1)
    | E1 (I128OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stoll(%s)" n)
    | E1 (FloatOfQWord, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "floatOfQword(%s)" n)
    | E1 (QWordOfFloat, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "qwordOfFloat(%s)" n)
    | E1 (StringOfFloat, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::to_string(%s)" n)
    | E1 (StringOfChar, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::string(%s)" n)
    | E1 (CharOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s[0]" n)
    | E1 (FloatOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stod(%s)" n)
    | E1 (ByteOfU8, e1) | E1 (U8OfByte, e1)
    | E1 (WordOfU16, e1) | E1 (U16OfWord, e1)
    | E1 (U32OfDWord, e1) | E1 (DWordOfU32, e1)
    | E1 (U64OfQWord, e1) | E1 (QWordOfU64, e1)
    | E1 (U128OfOWord, e1) | E1 (OWordOfU128, e1)
    | E1 (BitOfBool, e1) | E1 (BoolOfBit, e1)
    | E1 (U8OfChar, e1) | E1 (CharOfU8, e1)
    | E1 (SizeOfU32, e1) | E1 (U32OfSize, e1)
    | E1 (ToU8, e1) | E1 (ToI8, e1)
    | E1 (ToU16, e1) | E1 (ToI16, e1)
    | E1 (ToU24, e1) | E1 (ToI24, e1)
    | E1 (ToU32, e1) | E1 (ToI32, e1)
    | E1 (ToU40, e1) | E1 (ToI40, e1)
    | E1 (ToU48, e1) | E1 (ToI48, e1)
    | E1 (ToU56, e1) | E1 (ToI56, e1)
    | E1 (ToU64, e1) | E1 (ToI64, e1)
    | E1 (ToU128, e1) | E1 (ToI128, e1)
    | E1 (U8OfBool, e1) | E1 (BoolOfU8, e1) ->
        print ?name emit p l e1
    | E2 (AppendBytes, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E2 (AppendString, e1, e2) ->
        binary_infix_op e1 "+" e2
    | E1 (StringLength, e1)
    | E1 (ListLength, e1) ->
        method_call e1 "length" []
    | E1 (StringOfBytes, e1) ->
        method_call e1 "toString" []
    | E1 (BytesOfString, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E0 (DataPtrOfString s) ->
        emit ?name p l e (fun oc -> pp oc "%S" s)
    | E2 (TestBit, e1, e2) ->
        method_call e1 "getBit" [ e2 ]
    | E3 (SetBit, e1, e2, e3) ->
        method_call e1 "setBit" [ e2 ; e3 ]
    | E1 (ReadByte, e1) ->
        method_call e1 "readByte" []
    | E1 (ReadWord LittleEndian, e1) ->
        method_call e1 "readWordLe" []
    | E1 (ReadWord BigEndian, e1) ->
        method_call e1 "readWordBe" []
    | E1 (ReadDWord LittleEndian, e1) ->
        method_call e1 "readDWordLe" []
    | E1 (ReadDWord BigEndian, e1) ->
        method_call e1 "readDWordBe" []
    | E1 (ReadQWord LittleEndian, e1) ->
        method_call e1 "readQWordLe" []
    | E1 (ReadQWord BigEndian, e1) ->
        method_call e1 "readQWordBe" []
    | E1 (ReadOWord LittleEndian, e1) ->
        method_call e1 "readOWordLe" []
    | E1 (ReadOWord BigEndian, e1) ->
        method_call e1 "readOWordBe" []
    | E2 (ReadBytes, e1, e2) ->
        method_call e1 "readBytes" [ e2 ]
    | E2 (PeekByte, e1, e2) ->
        method_call e1 "peekByte" [ e2 ]
    | E2 (PeekWord LittleEndian, e1, e2) ->
        method_call e1 "peekWorkLe" [ e2 ]
    | E2 (PeekWord BigEndian, e1, e2) ->
        method_call e1 "peekWorkBe" [ e2 ]
    | E2 (PeekDWord LittleEndian, e1, e2) ->
        method_call e1 "peekDWorkLe" [ e2 ]
    | E2 (PeekDWord BigEndian, e1, e2) ->
        method_call e1 "peekDWorkBe" [ e2 ]
    | E2 (PeekQWord LittleEndian, e1, e2) ->
        method_call e1 "peekQWorkLe" [ e2 ]
    | E2 (PeekQWord BigEndian, e1, e2) ->
        method_call e1 "peekQWorkBe" [ e2 ]
    | E2 (PeekOWord LittleEndian, e1, e2) ->
        method_call e1 "peekOWorkLe" [ e2 ]
    | E2 (PeekOWord BigEndian, e1, e2) ->
        method_call e1 "peekOWorkBe" [ e2 ]
    | E2 (WriteByte, e1, e2) ->
        method_call e1 "writeByte" [ e2 ]
    | E2 (WriteWord LittleEndian, e1, e2) ->
        method_call e1 "writeWordLe" [ e2 ]
    | E2 (WriteWord BigEndian, e1, e2) ->
        method_call e1 "writeWordBe" [ e2 ]
    | E2 (WriteDWord LittleEndian, e1, e2) ->
        method_call e1 "writeDWordLe" [ e2 ]
    | E2 (WriteDWord BigEndian, e1, e2) ->
        method_call e1 "writeDWordBe" [ e2 ]
    | E2 (WriteQWord LittleEndian, e1, e2) ->
        method_call e1 "writeQWordLe" [ e2 ]
    | E2 (WriteQWord BigEndian, e1, e2) ->
        method_call e1 "writeQWordBe" [ e2 ]
    | E2 (WriteOWord LittleEndian, e1, e2) ->
        method_call e1 "writeOWordLe" [ e2 ]
    | E2 (WriteOWord BigEndian, e1, e2) ->
        method_call e1 "writeOWordBe" [ e2 ]
    | E2 (WriteBytes, e1, e2) ->
        method_call e1 "writeBytes" [ e2 ]
    | E2 (PokeByte, e1, e2) ->
        method_call e1 "pokeByte" [ e2 ]
    | E3 (BlitByte, e1, e2, e3) ->
        method_call e1 "blitBytes" [ e2 ; e3 ]
    | E2 (DataPtrAdd, e1, e2) ->
        method_call e1 "skip" [ e2 ]
    | E2 (DataPtrSub, e1, e2) ->
        method_call e1 "sub" [ e2 ]
    | E1 (DataPtrPush, e1) ->
        method_call e1 "push" []
    | E1 (DataPtrPop, e1) ->
        method_call e1 "pop" []
    | E1 (RemSize, e1) ->
        method_call e1 "remSize" []
    | E2 (And, e1, e2) ->
        binary_infix_op e1 "&&" e2
    | E2 (Or, e1, e2) ->
        binary_infix_op e1 "||" e2
    | E1 (Not, e1) ->
        unary_op "!" e1
    | E0 (AllocValue vtyp) ->
        let tn = type_identifier p (TValue vtyp) in
        emit ?name p l e (fun oc -> Printf.fprintf oc "new %s" tn)
    | E1 (DerefValuePtr, e1) ->
        print ?name emit p l e1
    | E2 (Pair, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E1 (Fst, e1) ->
        member e1 "first"
    | E1 (Snd, e1) ->
        member e1 "second"
    | E2 (MapPair, e1, e2) ->
        let pair = print emit p l e1
        and func = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s(%s.first, %s.second)" func pair pair)
    | E0 (Identifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            valid_identifier s)
    | E2 (Let n, e1, e2) ->
        let n1 = print emit p l e1 in
        let t = type_of l e1 in
        let tn = type_identifier p t in
        let res = gen_sym ?name "let_res_" in
        let l = (E0 (Identifier n), t) :: l in
        let t2 = type_of l e2 in
        ppi p.def "%s %s;" (type_identifier p t2) res ;
        ppi p.def "{" ;
        indent_more p (fun () ->
          ppi p.def "%s %s(%s);" tn (valid_identifier n) n1 ;
          let tmp = print emit p l e2 in
          ppi p.def "%s = %s;" res tmp) ;
        ppi p.def "}" ;
        res
    | E1 (Function (fid, ts), e1) ->
        emit ?name p l e (fun oc ->
          array_print_i ~first:"[&](" ~last:") {\n" ~sep:", "
            (fun i oc t -> Printf.fprintf oc "%s %s"
              (type_identifier p t) (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (E0 (Param (fid, i)), t) :: l
            ) l ts in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            ppi oc "return %s; }" n) ;
          pp oc "%s" p.indent)
    | E0 (Param (fid, n)) ->
        param fid n
    | E3 (Choose, e1, e2, e3) ->
        let cond = print emit p l e1 in
        let res = gen_sym ?name "choose_res_" in
        let t2 = type_of l e2 in
        ppi p.def "%s %s;" (type_identifier p t2) res ;
        ppi p.def "if (%s) {" cond ;
        indent_more p (fun () ->
          let n = print emit p l e2 in
          ppi p.def "%s = %s;" res n) ;
        ppi p.def "} else {" ;
        indent_more p (fun () ->
          let n = print emit p l e3 in
          ppi p.def "%s = %s;" res n) ;
        ppi p.def "}" ;
        res
    | E4 (ReadWhile, e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr0 = print emit p l e4 in
        let res = gen_sym ?name "read_while_res_" in
        let t3 = type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        let ptr = gen_sym "read_while_ptr_" in
        let t4 = type_of l e4 in
        ppi p.def "%s %s(%s);" (type_identifier p t4) ptr ptr0 ;
        ppi p.def "while (true) {" ;
        indent_more p (fun () ->
          ppi p.def "uint8_t const next_byte_(%s.peekByte(0));" ptr ;
          ppi p.def "if (%s(next_byte_)) {" cond ;
          indent_more p (fun () ->
            ppi p.def "%s = %s(%s, next_byte_);" res reduce res ;
            ppi p.def "%s = %s.skip(1);" ptr ptr) ;
          ppi p.def "} else break;") ;
        ppi p.def "}" ;
        emit ?name p l e (fun oc -> pp oc "%s, %s" res ptr)
    | E3 (LoopWhile, e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym ?name "while_res_" in
        let t3 = type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "while (%s(%s)) {" cond res ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(%s);" res body res) ;
        ppi p.def "}" ;
        res
    | E3 (LoopUntil, e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym ?name "until_res_" in
        let t3 = type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "do {" ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(%s);" res body res) ;
        ppi p.def "} while (%s(%s));" cond res ;
        res
    | E4 (Repeat, e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        let res = gen_sym ?name "repeat_res_" in
        let t3 = type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "for (int32_t idx_ = %s; idx != %s; idx++) {" from to_ ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(idx_, %s);" res body res) ;
        ppi p.def "}" ;
        res
    | E2 (SetField path, e1, e2) ->
        let ptr = print ?name emit p l e1
        and v = print emit p l e2 in
        (match type_of l e1 with
        | TValuePtr vt ->
            let a = deref_path ("(*"^ ptr ^")") vt path in
            ppi p.def "%s = %s;" a v ;
            ptr
        | _ -> assert false)
    | E1 (FieldIsNull path, e1) ->
        let ptr = print emit p l e1 in
        (match type_of l e1 with
        | TValuePtr vt ->
            let a = deref_path ("(*"^ ptr ^")") vt path in
            emit ?name p l e (fun oc -> pp oc "!%s.has_value()" a)
        | _ -> assert false)
    | E1 (GetField path, e1) ->
        let ptr = print emit p l e1 in
        (match type_of l e1 with
        | TValuePtr vt ->
            let a = deref_path ("(*"^ ptr ^")") vt path in
            emit ?name p l e (fun oc -> pp oc "%s" a)
        | _ -> assert false)

  let print_binding_toplevel emit n p l e =
    (* In C++ toplevel expressions cannot be initialized with arbitrary code so we
     * must rely on a static function to produce the value: *)
    let t = type_of l e in
    let tn = type_identifier p t in
    pp p.def "%sstatic %s %s_init()\n" p.indent tn n ;
    pp p.def "%s{\n" p.indent ;
    indent_more p (fun () ->
      (* TODO: add other predefined globals in the environment: *)
      let l = [] in
      let n = print emit p l e in
      pp p.def "%sreturn %s;\n" p.indent n) ;
    pp p.def "%s}\n" p.indent ;
    pp p.def "%s%s %s(%s_init());\n" p.indent tn n n

  let print_identifier_declaration n p l e =
    let t = type_of l e in
    let tn = type_identifier p t in
    pp p.def "%s%s %s;\n" p.indent tn n

  let source_intro =
    "#include <iostream>\n\
     #include <fstream>\n\
     #include <functional>\n\
     #include <optional>\n\
     #include <utility>\n\
     #include <vector>\n\
     #include \"dessser/runtime.h\"\n"
end

include BackEndCLike.Make (Config)
