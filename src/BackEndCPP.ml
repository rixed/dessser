open Batteries
open Stdint
open Dessser
open BackEndCLike

module Config =
struct
  let preferred_file_extension = "cc"

  let tuple_field_name i = "field_"^ string_of_int i

  let rec print_struct p oc id vts =
    let id = valid_identifier id in
    pp oc "%sstruct %s {\n" p.indent id ;
    indent_more p (fun () ->
      Array.iter (fun (field_name, vt) ->
        let typ_id = type_identifier p (Type.Value vt) in
        pp oc "%s%s %s;\n" p.indent typ_id field_name
      ) vts
    ) ;
    pp oc "%s};\n\n" p.indent

  and type_identifier p = function
    | Type.Value (ValueType.Nullable t) ->
        "std::optional<"^ type_identifier p (Value (NotNullable t)) ^">"
    | Type.Value ValueType.(NotNullable Float) -> "double"
    | Value (NotNullable String) -> "std::string"
    | Value (NotNullable Bool) -> "bool"
    | Value (NotNullable Char) -> "char"
    | Value (NotNullable I8) -> "int8_t"
    | Value (NotNullable U8) -> "uint8_t"
    | Value (NotNullable I16) -> "int16_t"
    | Value (NotNullable U16) -> "uint16_t"
    | Value (NotNullable I32) -> "int32_t"
    | Value (NotNullable U32) -> "uint32_t"
    | Value (NotNullable I64) -> "int64_t"
    | Value (NotNullable U64) -> "uint64_t"
    | Value (NotNullable I128) -> "int128_t"
    | Value (NotNullable U128) -> "uint128_t"
    | Value (NotNullable (Tup vts)) as t ->
        let vts = Array.mapi (fun i vt -> tuple_field_name i, vt) vts in
        declared_type p t (fun oc type_id -> print_struct p oc type_id vts) |>
        valid_identifier
    | Value (NotNullable (Rec vts)) as t ->
        declared_type p t (fun oc type_id -> print_struct p oc type_id vts) |>
        valid_identifier
    | Value (NotNullable Vec (dim, typ)) ->
        Printf.sprintf "Vec<%d, %s>" dim (type_identifier p (Value typ))
    | Value (NotNullable List typ) ->
        Printf.sprintf "List<%s>" (type_identifier p (Value typ))
    | Pair (t1, t2) ->
        "std::pair<"^ type_identifier p t1 ^", "^ type_identifier p t2 ^">"
    | Function0 t ->
        "std::function<"^ type_identifier p t ^"()>"
    | Function1 (t1, t2) ->
        "std::function<"^ type_identifier p t2 ^"("^ type_identifier p t1 ^")>"
    | Function2 (t1, t2, t3) ->
        "std::function<"^ type_identifier p t3 ^"("^ type_identifier p t1 ^","^ type_identifier p t2 ^")>"
    | Void -> "void"
    | DataPtr -> "Pointer"
    | ValuePtr vt -> type_identifier p (Value vt) ^"*"
    | Size -> "Size"
    | Bit -> "bool"
    | Byte -> "uint8_t"
    | Word -> "uint16_t"
    | DWord -> "uint32_t"
    | QWord -> "uint64_t"
    | OWord -> "uint128_t"
    | Bytes -> "Bytes"

  (* Identifiers used for function parameters: *)
  let param fid n = "_"^ string_of_int fid ^"_"^ string_of_int n

  let print_binding n tn f oc =
    pp oc "%s %s(%t);" tn n f

  let print_comment oc s =
    pp oc "/* %s */" s

  let accessor_of_path vt path =
    let open ValueType in
    let rec loop a field_accessor vt = function
      | [] -> a
      | i :: path ->
          let rec accessor_of_not_nullable = function
            | NotNullable (Float | String | Bool | Char |
                           U8 | U16 | U32 | U64 | U128 |
                           I8 | I16 | I32 | I64 | I128) ->
                assert false
            | NotNullable (Vec (_, vt))
            | NotNullable (List vt) ->
                loop ("["^ string_of_int i ^"]") "." vt path
            | NotNullable (Tup vts) ->
                loop (field_accessor ^ tuple_field_name i) "." vts.(i) path
            | NotNullable (Rec vts) ->
                loop (field_accessor ^ (fst vts.(i))) "." (snd vts.(i)) path
            | Nullable x -> accessor_of_not_nullable (NotNullable x) in
          accessor_of_not_nullable vt in
    loop "" "->" vt path

  let print_float_literal v oc =
    if v = infinity then
      String.print oc "std::numeric_limits<double>::infinity"
    else if v = neg_infinity then
      String.print oc "-std::numeric_limits<double>::infinity"
    else
      Legacy.Printf.sprintf "%h" v |> String.print oc

  let gen_sym pref = valid_identifier (gen_sym pref)

  let rec print emit p l e =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.indent in
    let unary_op op e1 =
      let n1 = print emit p l e1 in
      emit p l e (fun oc -> pp oc "%s %s" op n1) in
    let binary_infix_op e1 op e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2 in
      emit p l e (fun oc -> pp oc "%s %s %s" n1 op n2) in
    let method_call e1 m args =
      let n1 = print emit p l e1
      and ns = List.map (print emit p l) args in
      emit p l e (fun oc ->
        pp oc "%s.%s%a" n1 m
          (List.print ~first:"(" ~last:")" ~sep:", " String.print) ns) in
    let member e1 m =
      let n1 = print emit p l e1 in
      emit p l e (fun oc -> pp oc "%s.%s" n1 m) in
    match e with
    | Expression.Comment (c, e1) ->
        pp p.def "%s/* %s */\n" p.indent c ;
        print emit p l e1
    | Seq es ->
        List.fold_left (fun _ e -> print emit p l e) "must_not_be_used1" es
    | Dump e1 ->
        let n = print emit p l e1 in
        pp p.def "%sstd::cout << %s;\n" p.indent n ;
        "must_not_be_used2"
    | IsNull e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "%s.has_value ()" n)
    | Coalesce (e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit p l e (fun oc -> pp oc "%s.has_value () |? %s : %s" n1 n1 n2)
    | Nullable e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> String.print oc n1)
    | NotNullable e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> Printf.fprintf oc "%s.value()" n1)
    | Null _ ->
        emit p l e (fun oc -> pp oc "std::nullopt")
    | Float f ->
        emit p l e (print_float_literal f)
    | String s ->
        emit p l e (fun oc -> String.print_quoted oc s)
    | Bit b | Bool b ->
        emit p l e (fun oc -> Bool.print oc b)
    | Char c ->
        emit p l e (fun oc -> pp oc "%C" c)
    | Byte i | U8 i ->
        emit p l e (fun oc -> pp oc "%d" i)
    | Word i | U16 i ->
        emit p l e (fun oc -> pp oc "%d" i)
    | DWord u | U32 u ->
        emit p l e (fun oc -> pp oc "%sU" (Uint32.to_string u))
    | QWord u | U64 u ->
        emit p l e (fun oc -> pp oc "%sUL" (Uint64.to_string u))
    | OWord u | U128 u ->
        emit p l e (fun oc ->
          let lo = Uint128.to_uint64 u
          and hi = Uint128.(to_uint64 (shift_right_logical u 64)) in
          pp oc "((((uint128_t)%sULL) << 64U) | %sULL)"
            (Uint64.to_string hi)
            (Uint64.to_string lo))
    | I8 i ->
        emit p l e (fun oc -> pp oc "%d" i)
    | I16 i ->
        emit p l e (fun oc -> pp oc "%d" i)
    | I32 i ->
        emit p l e (fun oc -> pp oc "%sL" (Int32.to_string i))
    | I64 i ->
        emit p l e (fun oc -> pp oc "%sLL" (Int64.to_string i))
    | I128 i ->
        emit p l e (fun oc ->
          let lo = Int128.to_int64 i
          and hi = Int128.(to_int64 (shift_right_logical i 64)) in
          pp oc "((((int128_t)%sLL) << 64) | %sLL)"
            (Int64.to_string hi)
            (Int64.to_string lo))
    | Size s ->
        emit p l e (fun oc -> pp oc "%dUL" s)
    | Gt (e1, e2) ->
        binary_infix_op e1 ">" e2
    | Ge (e1, e2) ->
        binary_infix_op e1 ">=" e2
    | Eq (e1, e2) ->
        binary_infix_op e1 "==" e2
    | Ne (e1, e2) ->
        binary_infix_op e1 "!=" e2
    | Add (e1, e2) ->
        binary_infix_op e1 "+" e2
    | Sub (e1, e2) ->
        binary_infix_op e1 "-" e2
    | Mul (e1, e2) ->
        binary_infix_op e1 "*" e2
    | Div (e1, e2) ->
        binary_infix_op e1 "/" e2
    | Rem (e1, e2) ->
        binary_infix_op e1 "%%" e2
    | LogAnd (e1, e2) ->
        binary_infix_op e1 "&" e2
    | LogOr (e1, e2) ->
        binary_infix_op e1 "|" e2
    | LogXor (e1, e2) ->
        binary_infix_op e1 "^" e2
    | LogNot e1 ->
        unary_op "~" e1
    | LeftShift (e1, e2) ->
        binary_infix_op e1 "<<" e2
    | RightShift (e1, e2) ->
        binary_infix_op e1 ">>" e2
    | StringOfInt e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> pp oc "std::to_string(%s)" n1)
    | U8OfString e1
    | I8OfString e1
    | U16OfString e1
    | I16OfString e1
    | U32OfString e1
    | I32OfString e1
    | U64OfString e1
    | I64OfString e1
    | U128OfString e1
    | I128OfString e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "std::stoll(%s)" n)
    | FloatOfQWord e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "floatOfQword(%s)" n)
    | QWordOfFloat e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "qwordOfFloat(%s)" n)
    | StringOfFloat e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "std::to_string(%s)" n)
    | StringOfChar e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "std::string(%s)" n)
    | CharOfString e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "%s[0]" n)
    | FloatOfString e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "std::stod(%s)" n)
    | ByteOfU8 e1 | U8OfByte e1
    | WordOfU16 e1 | U16OfWord e1
    | U32OfDWord e1 | DWordOfU32 e1
    | U64OfQWord e1 | QWordOfU64 e1
    | U128OfOWord e1 | OWordOfU128 e1
    | BitOfBool e1 | BoolOfBit e1
    | U8OfChar e1 | CharOfU8 e1
    | SizeOfU32 e1 | U32OfSize e1
    | ToU8 e1 | ToI8 e1
    | ToU16 e1 | ToI16 e1
    | ToU32 e1 | ToI32 e1
    | ToU64 e1 | ToI64 e1
    | ToU128 e1 | ToI128 e1
    | U8OfBool e1 | BoolOfU8 e1 ->
        print emit p l e1
    | AppendBytes (e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | AppendString (e1, e2) ->
        binary_infix_op e1 "+" e2
    | StringLength e1
    | ListLength e1 ->
        method_call e1 "length" []
    | StringOfBytes e1 ->
        method_call e1 "toString" []
    | BytesOfString e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> pp oc "%s" n1)
    | DataPtrOfString s ->
        emit p l e (fun oc -> pp oc "%S" s)
    | TestBit (e1, e2) ->
        method_call e1 "getBit" [ e2 ]
    | SetBit (e1, e2, e3) ->
        method_call e1 "setBit" [ e2 ; e3 ]
    | ReadByte e1 ->
        method_call e1 "readByte" []
    | ReadWord (LittleEndian, e1) ->
        method_call e1 "readWordLe" []
    | ReadWord (BigEndian, e1) ->
        method_call e1 "readWordBe" []
    | ReadDWord (LittleEndian, e1) ->
        method_call e1 "readDWordLe" []
    | ReadDWord (BigEndian, e1) ->
        method_call e1 "readDWordBe" []
    | ReadQWord (LittleEndian, e1) ->
        method_call e1 "readQWordLe" []
    | ReadQWord (BigEndian, e1) ->
        method_call e1 "readQWordBe" []
    | ReadOWord (LittleEndian, e1) ->
        method_call e1 "readOWordLe" []
    | ReadOWord (BigEndian, e1) ->
        method_call e1 "readOWordBe" []
    | ReadBytes (e1, e2) ->
        method_call e1 "readBytes" [ e2 ]
    | PeekByte (e1, e2) ->
        method_call e1 "peekByte" [ e2 ]
    | PeekWord (LittleEndian, e1, e2) ->
        method_call e1 "peekWorkLe" [ e2 ]
    | PeekWord (BigEndian, e1, e2) ->
        method_call e1 "peekWorkBe" [ e2 ]
    | PeekDWord (LittleEndian, e1, e2) ->
        method_call e1 "peekDWorkLe" [ e2 ]
    | PeekDWord (BigEndian, e1, e2) ->
        method_call e1 "peekDWorkBe" [ e2 ]
    | PeekQWord (LittleEndian, e1, e2) ->
        method_call e1 "peekQWorkLe" [ e2 ]
    | PeekQWord (BigEndian, e1, e2) ->
        method_call e1 "peekQWorkBe" [ e2 ]
    | PeekOWord (LittleEndian, e1, e2) ->
        method_call e1 "peekOWorkLe" [ e2 ]
    | PeekOWord (BigEndian, e1, e2) ->
        method_call e1 "peekOWorkBe" [ e2 ]
    | WriteByte (e1, e2) ->
        method_call e1 "writeByte" [ e2 ]
    | WriteWord (LittleEndian, e1, e2) ->
        method_call e1 "writeWordLe" [ e2 ]
    | WriteWord (BigEndian, e1, e2) ->
        method_call e1 "writeWordBe" [ e2 ]
    | WriteDWord (LittleEndian, e1, e2) ->
        method_call e1 "writeDWordLe" [ e2 ]
    | WriteDWord (BigEndian, e1, e2) ->
        method_call e1 "writeDWordBe" [ e2 ]
    | WriteQWord (LittleEndian, e1, e2) ->
        method_call e1 "writeQWordLe" [ e2 ]
    | WriteQWord (BigEndian, e1, e2) ->
        method_call e1 "writeQWordBe" [ e2 ]
    | WriteOWord (LittleEndian, e1, e2) ->
        method_call e1 "writeOWordLe" [ e2 ]
    | WriteOWord (BigEndian, e1, e2) ->
        method_call e1 "writeOWordBe" [ e2 ]
    | WriteBytes (e1, e2) ->
        method_call e1 "writeBytes" [ e2 ]
    | PokeByte (e1, e2) ->
        method_call e1 "pokeByte" [ e2 ]
    | BlitBytes (e1, e2, e3) ->
        method_call e1 "blitBytes" [ e2 ; e3 ]
    | DataPtrAdd (e1, e2) ->
        method_call e1 "skip" [ e2 ]
    | DataPtrSub (e1, e2) ->
        method_call e1 "sub" [ e2 ]
    | RemSize e1 ->
        method_call e1 "remSize" []
    | And (e1, e2) ->
        binary_infix_op e1 "&&" e2
    | Or (e1, e2) ->
        binary_infix_op e1 "||" e2
    | Not e1 ->
        unary_op "!" e1
    | AllocValue vtyp ->
        let tn = type_identifier p (Type.Value vtyp) in
        emit p l e (fun oc -> Printf.fprintf oc "new %s" tn)
    | DerefValuePtr e1 ->
        print emit p l e1
    | Pair (e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | Fst e1 ->
        member e1 "first"
    | Snd e1 ->
        member e1 "second"
    | MapPair (e1, e2) ->
        let pair = print emit p l e1
        and func = print emit p l e2 in
        emit p l e (fun oc -> pp oc "%s(%s.first, %s.second)" func pair pair)
    | Identifier s ->
        valid_identifier s
    | Let (n, e1, e2) ->
        let n1 = print emit p l e1 in
        let t = Expression.type_of l e1 in
        let tn = type_identifier p t in
        let res = gen_sym "let_res_" in
        let l = (Expression.Identifier n, t) :: l in
        let t2 = Expression.type_of l e2 in
        ppi p.def "%s %s;" (type_identifier p t2) res ;
        ppi p.def "{" ;
        indent_more p (fun () ->
          ppi p.def "%s %s(%s);" tn (valid_identifier n) n1 ;
          let tmp = print emit p l e2 in
          ppi p.def "%s = %s;" res tmp) ;
        ppi p.def "}" ;
        res
    | Function0 (_fid, e1) ->
        emit p l e (fun oc ->
          pp oc "[&]() {\n" ;
          indent_more p (fun () ->
            let n = print emit p l e1 in
            ppi oc "return %s; }" n) ;
          pp oc "%s" p.indent)
    | Function1 (fid, t1, e1) ->
        emit p l e (fun oc ->
          pp oc "[&](%s %s) {\n" (type_identifier p t1) (param fid 0) ;
          let l = (Expression.Param (fid, 0), t1) :: l in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            ppi oc "return %s; }" n) ;
          pp oc "%s" p.indent)
    | Function2 (fid, t1, t2, e1) ->
        emit p l e (fun oc ->
          pp oc "[&](%s %s, %s %s) {\n"
            (type_identifier p t1) (param fid 0)
            (type_identifier p t2) (param fid 1) ;
          let l = (Expression.Param (fid, 0), t1) ::
                  (Expression.Param (fid, 1), t2) :: l in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            ppi oc "return %s; }" n) ;
          pp oc "%s" p.indent)
    | Param (fid, n) ->
        param fid n
    | Choose (e1, e2, e3) ->
        let cond = print emit p l e1 in
        let res = gen_sym "choose_res_" in
        let t2 = Expression.type_of l e2 in
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
    | ReadWhile (e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr0 = print emit p l e4 in
        let res = gen_sym "read_while_res_" in
        let t3 = Expression.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        let ptr = gen_sym "read_while_ptr_" in
        let t4 = Expression.type_of l e4 in
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
        emit p l e (fun oc -> pp oc "%s, %s" res ptr)
    | LoopWhile (e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym "while_res_" in
        let t3 = Expression.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "while (%s(%s)) {" cond res ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(%s);" res body res) ;
        ppi p.def "}" ;
        res
    | LoopUntil (e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym "until_res_" in
        let t3 = Expression.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "do {" ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(%s);" res body res) ;
        ppi p.def "} while (%s(%s));" cond res ;
        res
    | Repeat (e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        let res = gen_sym "repeat_res_" in
        let t3 = Expression.type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "for (int32_t idx_ = %s; idx != %s; idx++) {" from to_ ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(idx_, %s);" res body res) ;
        ppi p.def "}" ;
        res
    | SetField (path, e1, e2) ->
        let ptr = print emit p l e1
        and v = print emit p l e2 in
        (match Expression.type_of l e1 with
        | Type.ValuePtr vt ->
            let a = accessor_of_path vt path in
            if a = "" then
              ppi p.def "*%s = %s;" ptr v
            else
              ppi p.def "%s%s = %s;" ptr a v ;
            ptr
        | _ -> assert false)
    | FieldIsNull (path, e1) ->
        let ptr = print emit p l e1 in
        (match Expression.type_of l e1 with
        | Type.ValuePtr vt ->
            let a = accessor_of_path vt path in
            emit p l e (fun oc -> pp oc "!%s%s.has_value()" ptr a)
        | _ -> assert false)
    | GetField (path, e1) ->
        let ptr = print emit p l e1 in
        (match Expression.type_of l e1 with
        | Type.ValuePtr vt ->
            let a = accessor_of_path vt path in
            emit p l e (fun oc -> pp oc "%s%s" ptr a)
        | _ -> assert false)

  let print_binding_toplevel emit n p l e =
    (* In C++ toplevel expressions cannot be initialized with arbitrary code so we
     * must rely on a static function to produce the value: *)
    let t = Expression.type_of l e in
    let tn = type_identifier p t in
    pp p.def "%s%s static %s_init()\n" p.indent tn n ;
    pp p.def "%s{\n" p.indent ;
    indent_more p (fun () ->
      (* TODO: add other predefined globals in the environment: *)
      let l = [] in
      let n = print emit p l e in
      pp p.def "%sreturn %s;\n" p.indent n) ;
    pp p.def "%s}\n" p.indent ;
    pp p.def "%s%s %s(%s_init());\n" p.indent tn n n

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
