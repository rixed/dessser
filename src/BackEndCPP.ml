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
    Printf.sprintf "g++ -std=c++17 -g -O%d -W -Wall -I src %s %S -o %S"
      optim (if link then "" else "-c") src dst

  let tuple_field_name i = "field_"^ string_of_int i

  let rec print_struct p oc id mts =
    let id = valid_identifier id in
    pp oc "%sstruct %s {\n" p.indent id ;
    indent_more p (fun () ->
      Array.iter (fun (field_name, mt) ->
        let typ_id = type_identifier p (TValue mt) in
        pp oc "%s%s %s;\n" p.indent typ_id field_name
      ) mts
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
    | TValue (NotNullable (TTup mts)) as t ->
        let mts = Array.mapi (fun i mt -> tuple_field_name i, mt) mts in
        declared_type p t (fun oc type_id -> print_struct p oc type_id mts) |>
        valid_identifier
    | TValue (NotNullable (TRec mts)) as t ->
        declared_type p t (fun oc type_id -> print_struct p oc type_id mts) |>
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
    | TValuePtr mt -> type_identifier p (TValue mt) ^"*"
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

  let accessor_of_path mt path =
    let rec loop a field_accessor mt = function
      | [] -> a
      | i :: path ->
          let rec accessor_of_not_nullable = function
            | NotNullable (Mac _ | TMap _) ->
                assert false
            | NotNullable (Usr t) ->
                accessor_of_not_nullable (NotNullable t.def)
            | NotNullable (TVec (_, mt))
            | NotNullable (TList mt) ->
                loop (a ^"["^ string_of_int i ^"]") "." mt path
            | NotNullable (TTup mts) ->
                loop (a ^ field_accessor ^ tuple_field_name i) "." mts.(i) path
            | NotNullable (TRec mts) ->
                let name = valid_identifier (fst mts.(i)) in
                loop (a ^ field_accessor ^ name) "." (snd mts.(i)) path
            | Nullable x -> accessor_of_not_nullable (NotNullable x) in
          accessor_of_not_nullable mt in
    loop "" "->" mt path

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
    | Comment (c, e1) ->
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
    | ToNullable e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> String.print oc n1)
    | ToNotNullable e1 ->
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
    | U24 u ->
        emit p l e (fun oc -> pp oc "%dU" u)
    | DWord u | U32 u ->
        emit p l e (fun oc -> pp oc "%sU" (Uint32.to_string u))
    | U40 u ->
        emit p l e (fun oc -> pp oc "%sUL" (Uint40.to_string u))
    | U48 u ->
        emit p l e (fun oc -> pp oc "%sUL" (Uint48.to_string u))
    | U56 u ->
        emit p l e (fun oc -> pp oc "%sUL" (Uint56.to_string u))
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
    | I24 i ->
        emit p l e (fun oc -> pp oc "%dL" i)
    | I32 i ->
        emit p l e (fun oc -> pp oc "%sL" (Int32.to_string i))
    | I40 i ->
        emit p l e (fun oc -> pp oc "%LdLL" i)
    | I48 i ->
        emit p l e (fun oc -> pp oc "%LdLL" i)
    | I56 i ->
        emit p l e (fun oc -> pp oc "%LdLL" i)
    | I64 i ->
        emit p l e (fun oc -> pp oc "%LdLL" i)
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
    | U24OfString e1
    | I24OfString e1
    | U32OfString e1
    | I32OfString e1
    | U40OfString e1
    | I40OfString e1
    | U48OfString e1
    | I48OfString e1
    | U56OfString e1
    | I56OfString e1
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
    | ToU24 e1 | ToI24 e1
    | ToU32 e1 | ToI32 e1
    | ToU40 e1 | ToI40 e1
    | ToU48 e1 | ToI48 e1
    | ToU56 e1 | ToI56 e1
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
    | BlitByte (e1, e2, e3) ->
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
    | AllocValue mtyp ->
        let tn = type_identifier p (TValue mtyp) in
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
        let t = type_of l e1 in
        let tn = type_identifier p t in
        let res = gen_sym "let_res_" in
        let l = (Identifier n, t) :: l in
        let t2 = type_of l e2 in
        ppi p.def "%s %s;" (type_identifier p t2) res ;
        ppi p.def "{" ;
        indent_more p (fun () ->
          ppi p.def "%s %s(%s);" tn (valid_identifier n) n1 ;
          let tmp = print emit p l e2 in
          ppi p.def "%s = %s;" res tmp) ;
        ppi p.def "}" ;
        res
    | Function (fid, ts, e1) ->
        emit p l e (fun oc ->
          array_print_i ~first:"[&](" ~last:") {\n" ~sep:", "
            (fun i oc t -> Printf.fprintf oc "%s %s"
              (type_identifier p t) (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (Param (fid, i), t) :: l
            ) l ts in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            ppi oc "return %s; }" n) ;
          pp oc "%s" p.indent)
    | Param (fid, n) ->
        param fid n
    | Choose (e1, e2, e3) ->
        let cond = print emit p l e1 in
        let res = gen_sym "choose_res_" in
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
    | ReadWhile (e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr0 = print emit p l e4 in
        let res = gen_sym "read_while_res_" in
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
        emit p l e (fun oc -> pp oc "%s, %s" res ptr)
    | LoopWhile (e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym "while_res_" in
        let t3 = type_of l e3 in
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
        let t3 = type_of l e3 in
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
        let t3 = type_of l e3 in
        ppi p.def "%s %s(%s);" (type_identifier p t3) res accum ;
        ppi p.def "for (int32_t idx_ = %s; idx != %s; idx++) {" from to_ ;
        indent_more p (fun () ->
          ppi p.def "%s = %s(idx_, %s);" res body res) ;
        ppi p.def "}" ;
        res
    | SetField (path, e1, e2) ->
        let ptr = print emit p l e1
        and v = print emit p l e2 in
        (match type_of l e1 with
        | TValuePtr mt ->
            let a = accessor_of_path mt path in
            if a = "" then
              ppi p.def "*%s = %s;" ptr v
            else
              ppi p.def "%s%s = %s;" ptr a v ;
            ptr
        | _ -> assert false)
    | FieldIsNull (path, e1) ->
        let ptr = print emit p l e1 in
        (match type_of l e1 with
        | TValuePtr mt ->
            let a = accessor_of_path mt path in
            emit p l e (fun oc -> pp oc "!%s%s.has_value()" ptr a)
        | _ -> assert false)
    | GetField (path, e1) ->
        let ptr = print emit p l e1 in
        (match type_of l e1 with
        | TValuePtr mt ->
            let a = accessor_of_path mt path in
            emit p l e (fun oc -> pp oc "%s%s" ptr a)
        | _ -> assert false)

  let print_binding_toplevel emit n p l e =
    (* In C++ toplevel expressions cannot be initialized with arbitrary code so we
     * must rely on a static function to produce the value: *)
    let t = type_of l e in
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
