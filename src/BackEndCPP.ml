open Batteries
open Stdint
open Dessser
open BackEndCLike
open DessserTools
module T = DessserTypes
module E = DessserExpressions

module Config =
struct
  let valid_identifier = BackEndCLike.valid_identifier
  let valid_source_name n = n
  let preferred_def_extension = "cc"
  let preferred_decl_extension = "h"
  let compile_cmd ~optim ~link src dst =
    let optim = cap 0 3 optim in
    Printf.sprintf
      "g++ -std=c++17 -g -O%d -W -Wall \
           -Wno-unused-parameter -Wno-unused-variable \
           -Wno-shift-negative-value -I src %s %S -o %S"
      optim (if link then "" else "-c") src dst

  let tuple_field_name i = "field_"^ string_of_int i

  let rec print_struct p oc id mns =
    let id = valid_identifier id in
    pp oc "%sstruct %s {\n" p.indent id ;
    indent_more p (fun () ->
      Array.iter (fun (field_name, vt) ->
        let typ_id = type_identifier p (T.TValue vt) in
        pp oc "%s%s %s;\n" p.indent typ_id (valid_identifier field_name)
      ) mns
    ) ;
    pp oc "%s};\n\n" p.indent

  and print_variant p oc id mns =
    let id = valid_identifier id in
    pp oc "%stypedef std::variant<\n" p.indent ;
    indent_more p (fun () ->
      Array.iteri (fun i (_, mn) ->
        let typ_id = type_identifier p (T.TValue mn) in
        pp oc "%s%s%s\n"
          p.indent typ_id (if i < Array.length mns - 1 then "," else "")
      ) mns
    ) ;
    pp oc "%s> %s;\n\n" p.indent id

  and type_identifier p = function
    | T.TValue { vtyp ; nullable = true } ->
        "std::optional<"^
          type_identifier p (TValue { vtyp ; nullable = false })
        ^">"
    | T.TValue { vtyp = Unknown ; _ } -> invalid_arg "type_identifier"
    | T.TValue { vtyp = Mac TFloat ; _ } -> "double"
    | T.TValue { vtyp = Mac TString ; _ } -> "std::string"
    | T.TValue { vtyp = Mac TBool ; _ } -> "bool"
    | T.TValue { vtyp = Mac TChar ; _ } -> "char"
    | T.TValue { vtyp = Mac TI8 ; _ } -> "int8_t"
    | T.TValue { vtyp = Mac TU8 ; _ } -> "uint8_t"
    | T.TValue { vtyp = Mac TI16 ; _ } -> "int16_t"
    | T.TValue { vtyp = Mac TU16 ; _ } -> "uint16_t"
    | T.TValue { vtyp = Mac TI24 ; _ } -> "int32_t"
    | T.TValue { vtyp = Mac TU24 ; _ } -> "uint32_t"
    | T.TValue { vtyp = Mac TI32 ; _ } -> "int32_t"
    | T.TValue { vtyp = Mac TU32 ; _ } -> "uint32_t"
    | T.TValue { vtyp = Mac TI40 ; _ } -> "int64_t"
    | T.TValue { vtyp = Mac TU40 ; _ } -> "uint64_t"
    | T.TValue { vtyp = Mac TI48 ; _ } -> "int64_t"
    | T.TValue { vtyp = Mac TU48 ; _ } -> "uint64_t"
    | T.TValue { vtyp = Mac TI56 ; _ } -> "int64_t"
    | T.TValue { vtyp = Mac TU56 ; _ } -> "uint64_t"
    | T.TValue { vtyp = Mac TI64 ; _ } -> "int64_t"
    | T.TValue { vtyp = Mac TU64 ; _ } -> "uint64_t"
    | T.TValue { vtyp = Mac TI128 ; _ } -> "int128_t"
    | T.TValue { vtyp = Mac TU128 ; _ } -> "uint128_t"
    | T.TValue { vtyp = Usr t ; _ } ->
        type_identifier p (TValue { vtyp = t.def ; nullable = false })
    | T.TValue { vtyp = TTup mns ; _ } as t ->
        let mns = Array.mapi (fun i vt -> tuple_field_name i, vt) mns in
        declared_type p t (fun oc type_id -> print_struct p oc type_id mns) |>
        valid_identifier
    | T.TValue { vtyp = TRec mns ; _ } as t ->
        declared_type p t (fun oc type_id -> print_struct p oc type_id mns) |>
        valid_identifier
    | T.TValue { vtyp = TSum mns ; _ } as t ->
        declared_type p t (fun oc type_id -> print_variant p oc type_id mns) |>
        valid_identifier
    | T.TValue { vtyp = TVec (dim, typ) ; _ } ->
        Printf.sprintf "Vec<%d, %s>" dim (type_identifier p (TValue typ))
    | T.TValue { vtyp = TList typ ; _ } ->
        Printf.sprintf "List<%s>" (type_identifier p (TValue typ))
    | T.TValue { vtyp = TMap _ ; _ } ->
        assert false (* No value of map type *)
    | T.TPair (t1, t2) ->
        "Pair<"^ type_identifier p t1 ^", "^ type_identifier p t2 ^">"
    | T.TSList t1 ->
        "SList<"^ type_identifier p t1 ^">"
    | T.TFunction (args, ret) ->
        "std::function<"^ type_identifier p ret ^
          IO.to_string (
            Array.print ~first:"(" ~last:")" ~sep:"," (fun oc t ->
              String.print oc (type_identifier p t))
          ) args ^">"
    | T.TVoid -> "void"
    | T.TDataPtr -> "Pointer"
    | T.TSize -> "Size"
    | T.TBit -> "bool"
    | T.TByte -> "uint8_t"
    | T.TWord -> "uint16_t"
    | T.TDWord -> "uint32_t"
    | T.TQWord -> "uint64_t"
    | T.TOWord -> "uint128_t"
    | T.TBytes -> "Bytes"
    | T.TMask -> "Mask"
    | T.TMaskAction -> "MaskAction"

  (* Identifiers used for function parameters: *)
  let param fid n = "p_"^ string_of_int fid ^"_"^ string_of_int n

  let print_binding n tn f oc =
    if tn = "void" then (
      pp oc "%t;" f
    ) else
      pp oc "%s %s(%t);" tn n f

  let print_comment oc fmt =
    pp oc ("/* "^^ fmt ^^" */\n")

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
    let shortcutting_binary_infix_op e1 e2 short_cond_on_e1 =
      let n1 = print emit p l e1 in
      let res = gen_sym ?name "shortcut_res_" in
      let t1 = E.type_of l e1 in
      ppi p.def "%s %s;" (type_identifier p t1) res ;
      ppi p.def "if (%s == %b) {" n1 short_cond_on_e1 ;
      indent_more p (fun () ->
        ppi p.def "%s = %s;" res n1) ;
      ppi p.def "} else {" ;
      indent_more p (fun () ->
        let n2 = print emit p l e2 in
        ppi p.def "%s = %s;" res n2) ;
      ppi p.def "}" ;
      res in
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
    | E.E1 (Comment c, e1) ->
        pp p.def "%s/* %s */\n" p.indent c ;
        print ?name emit p l e1
    | E.E0S (Seq, es) ->
        List.fold_left (fun _ e -> print emit p l e) "" es
    | E.E0S ((MakeVec | MakeList | MakeTup), es) ->
        let inits = List.map (print emit p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:"{ " ~last:" }" ~sep:", " String.print oc inits)
    | E.E0S (MakeRec, es) ->
        let _, inits =
          List.fold_left (fun (prev_name, inits) e ->
            match prev_name with
            | None ->
                Some (E.field_name_of_expr e), inits
            | Some name ->
                let n = print emit p l e in
                None, (valid_identifier name, n) :: inits
          ) (None, []) es in
        let inits = List.rev inits in
        emit ?name p l e (fun oc ->
          List.print ~first:"{ " ~last:" }" ~sep:", "
            (fun oc (name, n) ->
              Printf.fprintf oc ".%s = %s" name n) oc inits)
    | E.E1 (Ignore, e1) ->
        let n = print emit p l e1 in
        pp p.def "%s(void)%s;\n" p.indent n ;
        ""
    | E.E1 (Dump, e1) ->
        let n = print emit p l e1 in
        pp p.def "%sstd::cout << %s;\n" p.indent n ;
        ""
    | E.E1 (Debug, e1) ->
        print ?name emit p l (E1 ((if !E.dump_debug then Dump else Ignore), e1))
    | E.E1 (IsNull, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "!(%s.has_value ())" n)
    | E.E2 (Coalesce, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s.has_value () |? %s : %s" n1 n1 n2)
    | E.E2 (Nth, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s[%s]" n2 n1)
    | E.E1 (ToNullable, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> String.print oc n1)
    | E.E1 (ToNotNullable, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> Printf.fprintf oc "%s.value()" n1)
    | E.E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "std::nullopt")
    | E.E0 (Float f) ->
        emit ?name p l e (print_float_literal f)
    | E.E0 (String s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc s)
    | E.E0 (Bit b) | E.E0 (Bool b) ->
        emit ?name p l e (fun oc -> Bool.print oc b)
    | E.E0 (Char c) ->
        emit ?name p l e (fun oc -> pp oc "'%s'" (c_char_of c))
    | E.E0 (Byte i) | E.E0 (U8 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint8.to_string i))
    | E.E0 (Word i) | E.E0 (U16 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint16.to_string i))
    | E.E0 (U24 u) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint24.to_string u))
    | E.E0 (DWord u) | E.E0 (U32 u) ->
        emit ?name p l e (fun oc -> pp oc "%sU" (Uint32.to_string u))
    | E.E0 (U40 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint40.to_string u))
    | E.E0 (U48 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint48.to_string u))
    | E.E0 (U56 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint56.to_string u))
    | E.E0 (QWord u) | E.E0 (U64 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint64.to_string u))
    | E.E0 (OWord u) | E.E0 (U128 u) ->
        emit ?name p l e (fun oc ->
          let lo = Uint128.to_uint64 u
          and hi = Uint128.(to_uint64 (shift_right_logical u 64)) in
          pp oc "((((uint128_t)%sULL) << 64U) | %sULL)"
            (Uint64.to_string hi)
            (Uint64.to_string lo))
    | E.E0 (Bytes s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc (Bytes.to_string s))
    | E.E0 (I8 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Int8.to_string i))
    | E.E0 (I16 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Int16.to_string i))
    | E.E0 (I24 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Int24.to_string i))
    | E.E0 (I32 i) ->
        emit ?name p l e (fun oc -> pp oc "%sL" (Int32.to_string i))
    | E.E0 (I40 i) ->
        emit ?name p l e (fun oc -> pp oc "%sLL" (Int40.to_string i))
    | E.E0 (I48 i) ->
        emit ?name p l e (fun oc -> pp oc "%sLL" (Int48.to_string i))
    | E.E0 (I56 i) ->
        emit ?name p l e (fun oc -> pp oc "%sLL" (Int56.to_string i))
    | E.E0 (I64 i) ->
        emit ?name p l e (fun oc -> pp oc "%LdLL" i)
    | E.E0 (I128 i) ->
        emit ?name p l e (fun oc ->
          let lo = Int128.to_int64 i
          and hi = Int128.(to_int64 (shift_right_logical i 64)) in
          pp oc "((((int128_t)%sLL) << 64) | %sLL)"
            (Int64.to_string hi)
            (Int64.to_string lo))
    | E.E0 (Size s) ->
        emit ?name p l e (fun oc -> pp oc "%dUL" s)
    | E.E2 (Gt, e1, e2) ->
        binary_infix_op e1 ">" e2
    | E.E2 (Ge, e1, e2) ->
        binary_infix_op e1 ">=" e2
    | E.E2 (Eq, e1, e2) ->
        binary_infix_op e1 "==" e2
    | E.E2 (Ne, e1, e2) ->
        binary_infix_op e1 "!=" e2
    | E.E2 (Add, e1, e2) ->
        binary_infix_op e1 "+" e2
    | E.E2 (Sub, e1, e2) ->
        binary_infix_op e1 "-" e2
    | E.E2 (Mul, e1, e2) ->
        binary_infix_op e1 "*" e2
    | E.E2 (Div, e1, e2) ->
        binary_infix_op e1 "/" e2
    | E.E2 (Rem, e1, e2) ->
        binary_infix_op e1 "%" e2
    | E.E2 (LogAnd, e1, e2) ->
        binary_infix_op e1 "&" e2
    | E.E2 (LogOr, e1, e2) ->
        binary_infix_op e1 "|" e2
    | E.E2 (LogXor, e1, e2) ->
        binary_infix_op e1 "^" e2
    | E.E1 (LogNot, e1) ->
        unary_op "~" e1
    | E.E2 (LeftShift, e1, e2) ->
        binary_infix_op e1 "<<" e2
    | E.E2 (RightShift, e1, e2) ->
        binary_infix_op e1 ">>" e2
    | E.E1 (StringOfInt, e1) ->
        let n1 = print emit p l e1 in
        (match T.develop_user_types (E.type_of l e1) with
        | TValue { vtyp = Mac TU128 ; _ } ->
            emit ?name p l e (fun oc -> pp oc "string_of_u128(%s)" n1)
        | TValue { vtyp = Mac TI128 ; _ } ->
            emit ?name p l e (fun oc -> pp oc "string_of_i128(%s)" n1)
        | _ ->
            emit ?name p l e (fun oc -> pp oc "std::to_string(%s)" n1))
    | E.E1 (U8OfString, e1)
    | E.E1 (U16OfString, e1)
    | E.E1 (U24OfString, e1)
    | E.E1 (U32OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stoul(%s)" n)
    | E.E1 (U40OfString, e1)
    | E.E1 (U48OfString, e1)
    | E.E1 (U56OfString, e1)
    | E.E1 (U64OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stoull(%s)" n)
    | E.E1 (I8OfString, e1)
    | E.E1 (I16OfString, e1)
    | E.E1 (I24OfString, e1)
    | E.E1 (I32OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stol(%s)" n)
    | E.E1 (I40OfString, e1)
    | E.E1 (I48OfString, e1)
    | E.E1 (I56OfString, e1)
    | E.E1 (I64OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stoll(%s)" n)
    | E.E1 (U128OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "i128_of_string(%s)" n)
    | E.E1 (I128OfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "i128_of_string(%s)" n)
    | E.E1 (FloatOfQWord, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "floatOfQword(%s)" n)
    | E.E1 (QWordOfFloat, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "qwordOfFloat(%s)" n)
    | E.E1 (StringOfFloat, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "hexStringOfFloat(%s)" n)
    | E.E1 (StringOfChar, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "1, %s" n)
    | E.E1 (CharOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s[0]" n)
    | E.E1 (FloatOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "std::stod(%s)" n)
    | E.E1 (ByteOfU8, e1) | E.E1 (U8OfByte, e1)
    | E.E1 (WordOfU16, e1) | E.E1 (U16OfWord, e1)
    | E.E1 (U32OfDWord, e1) | E.E1 (DWordOfU32, e1)
    | E.E1 (U64OfQWord, e1) | E.E1 (QWordOfU64, e1)
    | E.E1 (U128OfOWord, e1) | E.E1 (OWordOfU128, e1)
    | E.E1 (BitOfBool, e1) | E.E1 (BoolOfBit, e1)
    | E.E1 (U8OfChar, e1) | E.E1 (CharOfU8, e1)
    | E.E1 (SizeOfU32, e1) | E.E1 (U32OfSize, e1)
    | E.E1 (ToU8, e1) | E.E1 (ToI8, e1)
    | E.E1 (ToU16, e1) | E.E1 (ToI16, e1)
    | E.E1 (ToU24, e1) | E.E1 (ToI24, e1)
    | E.E1 (ToU32, e1) | E.E1 (ToI32, e1)
    | E.E1 (ToU40, e1) | E.E1 (ToI40, e1)
    | E.E1 (ToU48, e1) | E.E1 (ToI48, e1)
    | E.E1 (ToU56, e1) | E.E1 (ToI56, e1)
    | E.E1 (ToU64, e1) | E.E1 (ToI64, e1)
    | E.E1 (ToU128, e1) | E.E1 (ToI128, e1)
    | E.E1 (U8OfBool, e1) | E.E1 (BoolOfU8, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n)
    | E.E1 (ListOfSList, e1) ->
        method_call e1 "toList" []
    | E.E1 (ListOfSListRev, e1) ->
        method_call e1 "toListRev" []
    | E.E2 (AppendByte, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E2 (AppendBytes, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E2 (AppendString, e1, e2) ->
        binary_infix_op e1 "+" e2
    | E.E1 (StringLength, e1)
    | E.E1 (ListLength, e1) ->
        method_call e1 "size" []
    | E.E1 (StringOfBytes, e1) ->
        method_call e1 "toString" []
    | E.E1 (BytesOfString, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E.E0 (DataPtrOfString s) ->
        emit ?name p l e (fun oc -> pp oc "%S" s)
    | E.E0 (DataPtrOfBuffer n) ->
        emit ?name p l e (fun oc -> pp oc "%d" n)
    | E.E3 (DataPtrOfPtr, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc -> pp oc "%s, %s, %s" n1 n2 n3)
    | E.E2 (GetBit, e1, e2) ->
        method_call e1 "getBit" [ e2 ]
    | E.E3 (SetBit, e1, e2, e3) ->
        method_call e1 "setBit" [ e2 ; e3 ]
    | E.E1 (ReadByte, e1) ->
        method_call e1 "readByte" []
    | E.E1 (ReadWord LittleEndian, e1) ->
        method_call e1 "readWordLe" []
    | E.E1 (ReadWord BigEndian, e1) ->
        method_call e1 "readWordBe" []
    | E.E1 (ReadDWord LittleEndian, e1) ->
        method_call e1 "readDWordLe" []
    | E.E1 (ReadDWord BigEndian, e1) ->
        method_call e1 "readDWordBe" []
    | E.E1 (ReadQWord LittleEndian, e1) ->
        method_call e1 "readQWordLe" []
    | E.E1 (ReadQWord BigEndian, e1) ->
        method_call e1 "readQWordBe" []
    | E.E1 (ReadOWord LittleEndian, e1) ->
        method_call e1 "readOWordLe" []
    | E.E1 (ReadOWord BigEndian, e1) ->
        method_call e1 "readOWordBe" []
    | E.E2 (ReadBytes, e1, e2) ->
        method_call e1 "readBytes" [ e2 ]
    | E.E2 (PeekByte, e1, e2) ->
        method_call e1 "peekByte" [ e2 ]
    | E.E2 (PeekWord LittleEndian, e1, e2) ->
        method_call e1 "peekWorkLe" [ e2 ]
    | E.E2 (PeekWord BigEndian, e1, e2) ->
        method_call e1 "peekWorkBe" [ e2 ]
    | E.E2 (PeekDWord LittleEndian, e1, e2) ->
        method_call e1 "peekDWorkLe" [ e2 ]
    | E.E2 (PeekDWord BigEndian, e1, e2) ->
        method_call e1 "peekDWorkBe" [ e2 ]
    | E.E2 (PeekQWord LittleEndian, e1, e2) ->
        method_call e1 "peekQWorkLe" [ e2 ]
    | E.E2 (PeekQWord BigEndian, e1, e2) ->
        method_call e1 "peekQWorkBe" [ e2 ]
    | E.E2 (PeekOWord LittleEndian, e1, e2) ->
        method_call e1 "peekOWorkLe" [ e2 ]
    | E.E2 (PeekOWord BigEndian, e1, e2) ->
        method_call e1 "peekOWorkBe" [ e2 ]
    | E.E2 (WriteByte, e1, e2) ->
        method_call e1 "writeByte" [ e2 ]
    | E.E2 (WriteWord LittleEndian, e1, e2) ->
        method_call e1 "writeWordLe" [ e2 ]
    | E.E2 (WriteWord BigEndian, e1, e2) ->
        method_call e1 "writeWordBe" [ e2 ]
    | E.E2 (WriteDWord LittleEndian, e1, e2) ->
        method_call e1 "writeDWordLe" [ e2 ]
    | E.E2 (WriteDWord BigEndian, e1, e2) ->
        method_call e1 "writeDWordBe" [ e2 ]
    | E.E2 (WriteQWord LittleEndian, e1, e2) ->
        method_call e1 "writeQWordLe" [ e2 ]
    | E.E2 (WriteQWord BigEndian, e1, e2) ->
        method_call e1 "writeQWordBe" [ e2 ]
    | E.E2 (WriteOWord LittleEndian, e1, e2) ->
        method_call e1 "writeOWordLe" [ e2 ]
    | E.E2 (WriteOWord BigEndian, e1, e2) ->
        method_call e1 "writeOWordBe" [ e2 ]
    | E.E2 (WriteBytes, e1, e2) ->
        method_call e1 "writeBytes" [ e2 ]
    | E.E2 (PokeByte, e1, e2) ->
        method_call e1 "pokeByte" [ e2 ]
    | E.E3 (BlitByte, e1, e2, e3) ->
        method_call e1 "blitBytes" [ e2 ; e3 ]
    | E.E2 (DataPtrAdd, e1, e2) ->
        method_call e1 "skip" [ e2 ]
    | E.E2 (DataPtrSub, e1, e2) ->
        binary_infix_op e1 "-" e2
    | E.E1 (DataPtrPush, e1) ->
        method_call e1 "push" []
    | E.E1 (DataPtrPop, e1) ->
        method_call e1 "pop" []
    | E.E1 (RemSize, e1) ->
        method_call e1 "remSize" []
    | E.E1 (DataPtrOffset, e1) ->
        method_call e1 "getOffset" []
    | E.E2 (And, e1, e2) ->
        shortcutting_binary_infix_op e1 e2 false
    | E.E2 (Or, e1, e2) ->
        shortcutting_binary_infix_op e1 e2 true
    | E.E1 (Not, e1) ->
        unary_op "!" e1
    | E.E1 (Neg, e1) ->
        unary_op "-" e1
    | E.E0 (EndOfList _) ->
        (* Default constructor cannot be called with no-args as that would
         * not be C++ish enough: *)
        let res = gen_sym ?name "endoflist_" in
        let t = E.type_of l e in
        let tn = type_identifier p t in
        ppi p.def "%s %s;" tn res ;
        res
    | E.E2 (Cons, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E1 (Head, e1) ->
        method_call e1 "head" []
    | E.E1 (Tail, e1) ->
        method_call e1 "tail" []
    | E.E2 (Pair, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E1 (Fst, e1) ->
        member e1 "v1"
    | E.E1 (Snd, e1) ->
        member e1 "v2"
    | E.E2 (MapPair, e1, e2) ->
        let pair = print emit p l e1
        and func = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s(%s.v1, %s.v2)" func pair pair)
    | E.E0 (Identifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            valid_identifier s)
    | E.E2 (Let n, e1, e2) ->
        let n1 = print emit p l e1 in
        let t = E.type_of l e1 in
        let tn = type_identifier p t in
        let res = gen_sym ?name "let_res_" in
        let l = (E.E0 (Identifier n), t) :: l in
        let t2 = E.type_of l e2 in
        ppi p.def "%s %s;" (type_identifier p t2) res ;
        ppi p.def "{" ;
        indent_more p (fun () ->
          ppi p.def "%s %s(%s);" tn (valid_identifier n) n1 ;
          let tmp = print emit p l e2 in
          ppi p.def "%s = %s;" res tmp) ;
        ppi p.def "}" ;
        res
    | E.E1 (Function (fid, ts), e1) ->
        emit ?name p l e (fun oc ->
          array_print_i ~first:"[&](" ~last:") {\n" ~sep:", "
            (fun i oc t -> Printf.fprintf oc "%s %s"
              (type_identifier p t) (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (E.E0 (Param (fid, i)), t) :: l
            ) l ts in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            ppi oc "return %s; }" n) ;
          pp oc "%s" p.indent)
    | E.E0 (Param (fid, n)) ->
        param fid n
    | E.E3 (Choose, e1, e2, e3) ->
        let cond = print emit p l e1 in
        let res = gen_sym ?name "choose_res_" in
        let t2 = E.type_of l e2 in
        ppi p.def "%s %s;" (type_identifier p t2) res ;
        ppi p.def "if (%s) {" cond ;
        indent_more p (fun () ->
          let n = print emit p l e2 in
          ppi p.def "%s = %s;" res n) ;
        ppi p.def "} else {" ;
        indent_more p (fun () ->
          let n = print emit p l e3 in
          ppi p.def "%s = %s;" res n) ;
        pp p.def "%s}\n" p.indent ;
        res
    | E.E4 (ReadWhile, e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr0 = print emit p l e4 in
        let res = gen_sym ?name "read_while_res_" in
        let t3 = E.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        let ptr = gen_sym "read_while_ptr_" in
        let t4 = E.type_of l e4 in
        ppi p.def "%s %s(%s);" (type_identifier p t4) ptr ptr0 ;
        ppi p.def "while (true) {" ;
        indent_more p (fun () ->
          ppi p.def "if (%s.rem() <= 0) { break; } else {" ptr ;
          indent_more p (fun () ->
            ppi p.def "uint8_t const next_byte_(%s.peekByte(0));" ptr ;
            ppi p.def "if (%s(next_byte_)) {" cond ;
            indent_more p (fun () ->
              ppi p.def "%s = %s(%s, next_byte_);" res reduce res ;
              ppi p.def "%s = %s.skip(1);" ptr ptr) ;
            ppi p.def "} else break;") ;
          ppi p.def "}") ;
        pp p.def "%s}\n" p.indent ;
        emit ?name p l e (fun oc -> pp oc "%s, %s" res ptr)
    | E.E3 (LoopWhile, e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym ?name "while_res_" in
        let t3 = E.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "while (%s(%s)) {" cond res ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(%s);" res body res) ;
        pp p.def "%s}\n" p.indent ;
        res
    | E.E3 (LoopUntil, e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym ?name "until_res_" in
        let t3 = E.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "do {" ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(%s);" res body res) ;
        pp p.def "%s} while (%s(%s));\n" p.indent cond res ;
        res
    | E.E4 (Repeat, e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        let res = gen_sym ?name "repeat_res_" in
        let t4 = E.type_of l e4 in
        ppi p.def "%s %s(%s);" (type_identifier p t4) res accum ;
        ppi p.def "for (int32_t idx_ = %s; idx_ != %s; idx_++) {" from to_ ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(idx_, %s);" res body res) ;
        pp p.def "%s}\n" p.indent ;
        res
    | E.E1 (GetItem n, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 (tuple_field_name n))
    | E.E1 (GetField s, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 s)
    | E.E1 (GetAlt s, e1) ->
        (match E.type_of l e1 with
        | T.TValue { vtyp = TSum mns ; nullable = false } ->
            let n1 = print emit p l e1 in
            let lbl = Array.findi (fun (n, _) -> n = s) mns in
            emit ?name p l e (fun oc ->
              Printf.fprintf oc "std::get<%d>(%s)" lbl n1)
        | _ ->
            assert false)
    | E.E1 (Construct (_, i), e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "std::in_place_index<%d>, %s" i n1)
    | E.E1 (Assert, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "assert(%s)" n)
    | E.E1 (MaskGet i, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s.get(%d)" n1 i)
    | E.E1 (MaskEnter d, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "Mask::enter_action(%s, %d)" n1 d)
    | E.E1 (LabelOf, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "uint16_t(%s.index())" n1)
    | E.E0 CopyField ->
        emit ?name p l e (fun oc -> pp oc "MaskAction::COPY")
    | E.E0 SkipField ->
        emit ?name p l e (fun oc -> pp oc "MaskAction::SKIP")
    | E.E0 SetFieldNull ->
        emit ?name p l e (fun oc -> pp oc "MaskAction::SET_NULL")

  let print_binding_toplevel emit n p l e =
    (* In C++ toplevel expressions cannot be initialized with arbitrary code so we
     * must rely on a static function to produce the value: *)
    let t = E.type_of l e in
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
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.def "%s%s %s;\n" p.indent tn n

  let source_intro =
    "#include <iostream>\n\
     #include <fstream>\n\
     #include <functional>\n\
     #include <optional>\n\
     #include <utility>\n\
     #include <variant>\n\
     #include <vector>\n\
     #include \"dessser/runtime.h\"\n"
end

include BackEndCLike.Make (Config)
