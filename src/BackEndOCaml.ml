open Batteries
open Stdint
open Dessser
open BackEndCLike
open DessserTools

module Config =
struct
  let preferred_file_extension = "ml"

  let tuple_field_name i = "field_"^ string_of_int i

  let rec print_record p oc id vts =
    let id = valid_identifier id in
    pp oc "%stype %s = {\n" p.indent id ;
    indent_more p (fun () ->
      Array.iter (fun (field_name, vt) ->
        let typ_id = type_identifier p (Type.Value vt) in
        pp oc "%smutable %s : %s;\n" p.indent field_name typ_id
      ) vts
    ) ;
    pp oc "%s}\n\n" p.indent

  and value_type_identifier p = function
    | ValueType.NotNullable Char -> "char"
    | NotNullable String -> "string"
    | NotNullable Bool -> "bool"
    | NotNullable Float -> "float"
    | NotNullable U8 -> "Uint8.t"
    | NotNullable I8 -> "Int8.t"
    | NotNullable U16 -> "Uint16.t"
    | NotNullable I16 -> "Int16.t"
    | NotNullable U32 -> "Uint32.t"
    | NotNullable I32 -> "Int32.t"
    | NotNullable U64 -> "Uint64.t"
    | NotNullable I64 -> "Int64.t"
    | NotNullable U128 -> "Uint128.t"
    | NotNullable I128 -> "Int128.t"
    | NotNullable (Vec (_, t))
    | NotNullable (List t) ->
        value_type_identifier p t ^" array"
    | NotNullable (Tup vts) as t ->
        let vts = Array.mapi (fun i vt -> tuple_field_name i, vt) vts in
        declared_type p t (fun oc type_id -> print_record p oc type_id vts) |>
        valid_identifier
    | NotNullable (Rec vts) as t ->
        declared_type p t (fun oc type_id -> print_record p oc type_id vts) |>
        valid_identifier
    | NotNullable (Map _) ->
        assert false (* no value of map type *)
    | Nullable t ->
        value_type_identifier p (NotNullable t) ^" option"

  and type_identifier p = function
    | Type.Value vt -> value_type_identifier p vt
    | Void -> "unit"
    | DataPtr -> "Pointer.t"
    | ValuePtr vt -> value_type_identifier p vt ^ " ref"
    | Size -> "Size.t"
    | Bit -> "bool"
    | Byte -> "Uint8.t"
    | Word -> "Uint16.t"
    | DWord -> "Uint32.t"
    | QWord -> "Uint64.t"
    | OWord -> "Uint128.t"
    | Bytes -> "Slice.t"
    | Pair (t1, t2) ->
        "("^ type_identifier p t1 ^" * "^ type_identifier p t2 ^")"
    | Function ([||], t) ->
        "(() -> "^ type_identifier p t ^")"
    | Function (args, ret) ->
        "("^ IO.to_string (
          Array.print ~first:"" ~last:"" ~sep:" -> " (fun oc t ->
            String.print oc (type_identifier p t))
        ) args ^" -> "^ type_identifier p ret ^")"

  let mod_name = function
    | Type.Value ValueType.(NotNullable Char) -> "Char"
    | Value (NotNullable String) -> "String"
    | Value (NotNullable Bool) -> "Bool"
    | Value (NotNullable Float) -> "Float"
    | Value (NotNullable U8) -> "Uint8"
    | Value (NotNullable I8) -> "Int8"
    | Value (NotNullable U16) -> "Uint16"
    | Value (NotNullable I16) -> "Int16"
    | Value (NotNullable U32) -> "Uint32"
    | Value (NotNullable I32) -> "Int32"
    | Value (NotNullable U64) -> "Uint64"
    | Value (NotNullable I64) -> "Int64"
    | Value (NotNullable U128) -> "Uint128"
    | Value (NotNullable I128) -> "Int128"
    | DataPtr -> "Pointer"
    | Size -> "Size"
    | Bit -> "Bool"
    | Byte -> "Uint8"
    | Word -> "Uint16"
    | DWord -> "Uint32"
    | QWord -> "Uint64"
    | OWord -> "Uint128"
    | Bytes -> "Slice"
    | _ -> assert false

  (* Identifiers used for function parameters: *)
  let param fid n = "_"^ string_of_int fid ^"_"^ string_of_int n

  let rec print_default_value indent oc vtyp =
    let open ValueType in
    match vtyp with
    | NotNullable Float ->
        String.print oc "0."
    | NotNullable String ->
        String.print oc "\"\""
    | NotNullable Bool ->
        String.print oc "false"
    | NotNullable Char ->
        String.print oc "'\\000'"
    | NotNullable I8 ->
        String.print oc "Int8.zero"
    | NotNullable I16 ->
        String.print oc "Int16.zero"
    | NotNullable I32 ->
        String.print oc "Int32.zero"
    | NotNullable I64 ->
        String.print oc "Int64.zero"
    | NotNullable I128 ->
        String.print oc "Int128.zero"
    | NotNullable U8 ->
        String.print oc "Uint8.zero"
    | NotNullable U16 ->
        String.print oc "Uint16.zero"
    | NotNullable U32 ->
        String.print oc "Uint32.zero"
    | NotNullable U64 ->
        String.print oc "Uint64.zero"
    | NotNullable U128 ->
        String.print oc "Uint128.zero"
    | NotNullable (Tup vts) ->
        Array.print ~first:("{\n"^indent^"  ") ~last:("\n"^indent^"}") ~sep:(";\n"^indent^"  ")
          (fun oc (i, t) ->
            let fname = tuple_field_name i in
            Printf.fprintf oc "%s = %a"
              fname (print_default_value (indent^"  ")) t)
          oc (Array.mapi (fun i t -> (i, t)) vts)
    | NotNullable (Rec vts) ->
        Array.print ~first:("{\n"^indent^"  ") ~last:("\n"^indent^"}") ~sep:(";\n"^indent^"  ")
          (fun oc (fname, t) ->
            Printf.fprintf oc "%s = %a"
              fname (print_default_value (indent^"  ")) t)
          oc vts
    | NotNullable (Vec (dim, t)) ->
        Printf.fprintf oc "[| " ;
        for i = 0 to dim - 1 do
          Printf.fprintf oc "%a; "
            (print_default_value (indent^"  ")) t
        done ;
        Printf.fprintf oc "%s|]" indent
    | NotNullable (List _) ->
        String.print oc "[]"
    | NotNullable (Map _) ->
        assert false (* no value of map type *)
    | Nullable t ->
        (* Unfortunately we cannot start with None as we want the whole tree
         * of values to be populated. *)
        Printf.fprintf oc "Some (%a)"
          (print_default_value indent) (NotNullable t)

  let print_binding n tn f oc =
    pp oc "let %s : %s = %t in" n tn f

  let print_comment oc s =
    pp oc "(* %s *)" s

  let print_float_literal v oc =
    (* printf "%F" would not work for infinity:
     * https://caml.inria.fr/mantis/view.php?id=7685
     * and "%h" not for neg_infinity. *)
    if v = infinity then String.print oc "infinity"
    else if v = neg_infinity then String.print oc "neg_infinity"
    else Legacy.Printf.sprintf "%h" v |> String.print oc

  let lift_u32 v oc =
    pp oc "Uint32.of_int32 (%ldl)" (Uint32.to_int32 v)

  let lift_u64 v oc =
    pp oc "Uint64.of_int64 (%LdL)" (Uint64.to_int64 v)

  let lift_u128 v oc =
    if Uint128.compare v (Uint128.of_int max_int) < 0 then
      pp oc "Uint128.of_int %d" (Uint128.to_int v)
    else (
      let bytes = Bytes.create 16 in
      Uint128.to_bytes_little_endian v bytes 0 ;
      pp oc "Uint128.of_bytes_little_endian (Bytes.of_string %S) 0"
        (Bytes.to_string bytes))

  let lift_i128 v oc =
    if Int128.compare v (Int128.of_int min_int) > 0 &&
       Int128.compare v (Int128.of_int max_int) < 0 then
      pp oc "Int128.of_int %d" (Int128.to_int v)
    else (
      let bytes = Bytes.create 16 in
      Int128.to_bytes_little_endian v bytes 0 ;
      pp oc "Int128.of_bytes_little_endian (Bytes.of_string %S) 0"
        (Bytes.to_string bytes))

  let accessor_of_path vt path =
    let open ValueType in
    let rec loop a vt = function
      | [] -> a
      | i :: path ->
          let rec accessor_of_not_nullable = function
            | NotNullable (Float | String | Bool | Char | Map _ |
                           U8 | U16 | U32 | U64 | U128 |
                           I8 | I16 | I32 | I64 | I128) ->
                assert false
            | NotNullable (Vec (_, vt))
            | NotNullable (List vt) ->
                loop (".("^ string_of_int i ^")") vt path
            | NotNullable (Tup vts) ->
                loop ("."^ tuple_field_name i) vts.(i) path
            | NotNullable (Rec vts) ->
                let name = valid_identifier (fst vts.(i)) in
                loop ("."^ name) (snd vts.(i)) path
            | Nullable x -> accessor_of_not_nullable (NotNullable x) in
          accessor_of_not_nullable vt in
    loop "" vt path

  let rec print emit p l e =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.indent in
    let unary_op op e1 =
      let n1 = print emit p l e1 in
      emit p l e (fun oc -> pp oc "%s %s" op n1) in
    let unary_mod_op op e1 =
      let op = mod_name (Expression.type_of l e) ^"."^ op in
      unary_op op e1 in
    let any_op op es =
      let ns = List.map (print emit p l) es in
      let ns = String.concat " " ns in
      emit p l e (fun oc -> pp oc "%s %s" op ns) in
    let binary_op op e1 e2 =
      any_op op [ e1 ; e2 ] in
    let binary_infix_op e1 op e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2 in
      emit p l e (fun oc -> pp oc "%s %s %s" n1 op n2) in
    let binary_mod_op op e1 e2 =
      let op = mod_name (Expression.type_of l e) ^"."^ op in
      binary_op op e1 e2 in
    let binary_mod_op_2nd_u8 op e1 e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2
      and m = mod_name (Expression.type_of l e) in
      emit p l e (fun oc ->
        pp oc "%s.%s %s (Uint8.to_int %s)" m op n1 n2)
    in
    match e with
    | Expression.Comment (c, e1) ->
        pp p.def "%s(* %s *)\n" p.indent c ;
        print emit p l e1
    | Seq es ->
        List.fold_left (fun _ e -> print emit p l e) "must_not_be_used1" es
    | Dump e1 ->
        let n = print emit p l e1 in
        pp p.def ("%s"^^
          (match Expression.type_of l e1 with
          | Type.Value ValueType.(NotNullable String) ->
              "print_string %s;"
          | _ ->
              "print_string (Batteries.dump %s);") ^^"\n")
          p.indent n ;
        "must_not_be_used2"
    | IsNull e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "%s <> Null" n)
    | Coalesce (e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit p l e (fun oc -> pp oc "%s |? %s" n1 n2)
    | Nullable e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> pp oc "Some %s" n1)
    | NotNullable e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> Printf.fprintf oc "Option.get %s" n1)
    | Null _ ->
        emit p l e (fun oc -> pp oc "None")
    | Float f ->
        emit p l e (print_float_literal f)
    | String s ->
        emit p l e (fun oc -> String.print_quoted oc s)
    | Bit b | Bool b ->
        emit p l e (fun oc -> Bool.print oc b)
    | Char c ->
        emit p l e (fun oc -> pp oc "%C" c)
    | Byte i | U8 i ->
        emit p l e (fun oc -> pp oc "Uint8.of_int %d" i)
    | Word i | U16 i ->
        emit p l e (fun oc -> pp oc "Uint16.of_int %d" i)
    | DWord u | U32 u ->
        emit p l e (lift_u32 u)
    | QWord u | U64 u ->
        emit p l e (lift_u64 u)
    | OWord u | U128 u ->
        emit p l e (lift_u128 u)
    | I8 i ->
        emit p l e (fun oc -> pp oc "Int8.of_int %d" i)
    | I16 i ->
        emit p l e (fun oc -> pp oc "Int16.of_int %d" i)
    | I32 i ->
        emit p l e (fun oc -> pp oc "Int32.of_int32 %ld" i)
    | I64 i ->
        emit p l e (fun oc -> pp oc "Int64.of_int64 %Ld" i)
    | I128 i ->
        emit p l e (lift_i128 i)
    | Size s ->
        emit p l e (fun oc -> pp oc "Size.of_int %d" s)
    | Gt (e1, e2) ->
        binary_infix_op e1 ">" e2
    | Ge (e1, e2) ->
        binary_infix_op e1 ">=" e2
    | Eq (e1, e2) ->
        binary_infix_op e1 "=" e2
    | Ne (e1, e2) ->
        binary_infix_op e1 "<>" e2
    | Add (e1, e2) ->
        binary_mod_op "add" e1 e2
    | Sub (e1, e2) ->
        binary_mod_op "sub" e1 e2
    | Mul (e1, e2) ->
        binary_mod_op "mul" e1 e2
    | Div (e1, e2) ->
        binary_mod_op "div" e1 e2
    | Rem (e1, e2) ->
        binary_mod_op "rem" e1 e2
    | LogAnd (e1, e2) ->
        binary_mod_op "logand" e1 e2
    | LogOr (e1, e2) ->
        binary_mod_op "logor" e1 e2
    | LogXor (e1, e2) ->
        binary_mod_op "logxor" e1 e2
    | LogNot e1 ->
        unary_mod_op "lognot" e1
    | LeftShift (e1, e2) ->
        binary_mod_op_2nd_u8 "shift_left" e1 e2
    | RightShift (e1, e2) ->
        binary_mod_op_2nd_u8 "shift_right_logical" e1 e2
    | StringOfInt e1 ->
        let op = mod_name (Expression.type_of l e1) ^".to_string" in
        unary_op op e1
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
        unary_mod_op "of_string" e1
    | FloatOfQWord e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc ->
          pp oc "BatInt64.float_of_bits (Uint64.to_int64 %s)" n)
    | QWordOfFloat e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc ->
          pp oc "Uint64.of_int64 (BatInt64.bits_of_float %s)" n)
    | StringOfFloat e1 ->
        unary_op "string_of_float" e1
    | StringOfChar e1 ->
        unary_mod_op "of_char" e1
    | CharOfString e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "%s.[0]" n)
    | FloatOfString e1 ->
        unary_op "float_of_string" e1
    | ByteOfU8 e1 | U8OfByte e1
    | WordOfU16 e1 | U16OfWord e1
    | U32OfDWord e1 | DWordOfU32 e1
    | U64OfQWord e1 | QWordOfU64 e1
    | U128OfOWord e1 | OWordOfU128 e1
    | BitOfBool e1 | BoolOfBit e1 ->
        print emit p l e1
    | U8OfChar e1 ->
        unary_op "Uint8.of_int @@ Char.code" e1
    | CharOfU8 e1 ->
        unary_op "Char.chr @@ Uint8.to_int" e1
    | SizeOfU32 e1 ->
        unary_op "Uint32.to_int" e1
    | U32OfSize e1 ->
        unary_op "Uint32.of_int" e1
    | ToU8 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_uint8") e1
    | ToI8 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_int8") e1
    | ToU16 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_uint16") e1
    | ToI16 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_int16") e1
    | ToU32 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_uint32") e1
    | ToI32 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_int32") e1
    | ToU64 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_uint64") e1
    | ToI64 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_int64") e1
    | ToU128 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_uint128") e1
    | ToI128 e1 ->
        let m = mod_name (Expression.type_of l e1) in
        unary_op (m ^".to_int128") e1
    | U8OfBool e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "if %s then Uint8.one else Uint8.zero" n)
    | BoolOfU8 e1 ->
        let n = print emit p l e1 in
        emit p l e (fun oc -> pp oc "Uint8.compare Uint8.one %s = 0" n)
    | AppendBytes (e1, e2) ->
        binary_op "Slice.append" e1 e2
    | AppendString (e1, e2) ->
        binary_infix_op e1 "^" e2
    | StringLength e1 ->
        unary_op "Uint32.of_int @@ String.length" e1
    | StringOfBytes e1 ->
        unary_op "Slice.to_string" e1
    | BytesOfString e1 ->
        unary_op "Slice.of_string" e1
    | ListLength e1 ->
        unary_op "Array.length" e1
    | DataPtrOfString s ->
        emit p l e (fun oc -> pp oc "Pointer.of_string %S" s)
    | TestBit (e1, e2) ->
        binary_op "Pointer.getBit" e1 e2
    | SetBit (e1, e2, e3) ->
        let ptr = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        ppi p.def "Pointer.setBit %s (Uint32.to_int %s) %s;" ptr n2 n3 ;
        ptr
    | ReadByte e1 ->
        unary_op "Pointer.readByte" e1
    | ReadWord (LittleEndian, e1) ->
        unary_op "Pointer.readWordLe" e1
    | ReadWord (BigEndian, e1) ->
        unary_op "Pointer.readWordBe" e1
    | ReadDWord (LittleEndian, e1) ->
        unary_op "Pointer.readDWordLe" e1
    | ReadDWord (BigEndian, e1) ->
        unary_op "Pointer.readDWordBe" e1
    | ReadQWord (LittleEndian, e1) ->
        unary_op "Pointer.readQWordLe" e1
    | ReadQWord (BigEndian, e1) ->
        unary_op "Pointer.readQWordBe" e1
    | ReadOWord (LittleEndian, e1) ->
        unary_op "Pointer.readOWordLe" e1
    | ReadOWord (BigEndian, e1) ->
        unary_op "Pointer.readOWordBe" e1
    | ReadBytes (e1, e2) ->
        binary_op "Pointer.readBytes" e1 e2
    | PeekByte (e1, e2) ->
        binary_op "Pointer.peekByte" e1 e2
    | PeekWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.peekWorkLe" e1 e2
    | PeekWord (BigEndian, e1, e2) ->
        binary_op "Pointer.peekWorkBe" e1 e2
    | PeekDWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.peekDWorkLe" e1 e2
    | PeekDWord (BigEndian, e1, e2) ->
        binary_op "Pointer.peekDWorkBe" e1 e2
    | PeekQWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.peekQWorkLe" e1 e2
    | PeekQWord (BigEndian, e1, e2) ->
        binary_op "Pointer.peekQWorkBe" e1 e2
    | PeekOWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.peekOWorkLe" e1 e2
    | PeekOWord (BigEndian, e1, e2) ->
        binary_op "Pointer.peekOWorkBe" e1 e2
    | WriteByte (e1, e2) ->
        binary_op "Pointer.writeByte" e1 e2
    | WriteWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.writeWordLe" e1 e2
    | WriteWord (BigEndian, e1, e2) ->
        binary_op "Pointer.writeWordBe" e1 e2
    | WriteDWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.writeDWordLe" e1 e2
    | WriteDWord (BigEndian, e1, e2) ->
        binary_op "Pointer.writeDWordBe" e1 e2
    | WriteQWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.writeQWordLe" e1 e2
    | WriteQWord (BigEndian, e1, e2) ->
        binary_op "Pointer.writeQWordBe" e1 e2
    | WriteOWord (LittleEndian, e1, e2) ->
        binary_op "Pointer.writeOWordLe" e1 e2
    | WriteOWord (BigEndian, e1, e2) ->
        binary_op "Pointer.writeOWordBe" e1 e2
    | WriteBytes (e1, e2) ->
        binary_op "Pointer.writeBytes" e1 e2
    | PokeByte (e1, e2) ->
        let ptr = print emit p l e1
        and v = print emit p l e2 in
        ppi p.def "Pointer.pokeByte %s %s;" ptr v ;
        ptr
    | BlitBytes (e1, e2, e3) ->
        any_op "Pointer.blitBytes" [ e1 ; e2 ; e3 ]
    | DataPtrAdd (e1, e2) ->
        binary_op "Pointer.skip" e1 e2
    | DataPtrSub (e1, e2) ->
        binary_op "Pointer.sub" e1 e2
    | RemSize e1 ->
        unary_op "Pointer.remSize" e1
    | And (e1, e2) ->
        binary_infix_op e1 "&&" e2
    | Or (e1, e2) ->
        binary_infix_op e1 "||" e2
    | Not e1 ->
        unary_op "not" e1
    | AllocValue vtyp ->
        emit p l e (fun oc -> pp oc "ref %a" (print_default_value p.indent) vtyp)
    | DerefValuePtr e1 ->
        let n1 = print emit p l e1 in
        emit p l e (fun oc -> pp oc "!%s" n1)
    | Pair (e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit p l e (fun oc -> pp oc "(%s, %s)" n1 n2)
    | Fst e1 ->
        unary_op "fst" e1
    | Snd e1 ->
        unary_op "snd" e1
    | MapPair (e1, e2) ->
        let n1 = print emit p l e1 (* the pair *)
        and n2 = print emit p l e2 (* the function2 *) in
        let n1_0 = "(fst "^ n1 ^")"
        and n1_1 = "(snd "^ n1 ^")" in
        emit p l e (fun oc -> pp oc "%s %s %s" n2 n1_0 n1_1)
    | Identifier s ->
        valid_identifier s
    | Let (n, e1, e2) ->
        let n1 = print emit p l e1 in
        let t = Expression.type_of l e1 in
        let tn = type_identifier p t in
        ppi p.def "let %s : %s = %s in" (valid_identifier n) tn n1 ;
        let l = (Expression.Identifier n, t) :: l in
        print emit p l e2
    | Function (_fid, [||], e1) ->
        emit p l e (fun oc ->
          pp oc "(fun () ->\n" ;
          indent_more p (fun () ->
            let n = print emit p l e1 in
            pp oc "%s%s)" p.indent n))
    | Function (fid, ts, e1) ->
        emit p l e (fun oc ->
          array_print_i ~first:"(fun " ~last:" ->\n" ~sep:" "
            (fun i oc _t -> String.print oc (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (Expression.Param (fid, i), t) :: l
            ) l ts in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            pp oc "%s%s)" p.indent n))
    | Param (fid, n) ->
        param fid n
    | Choose (e1, e2, e3) ->
        let cond = print emit p l e1 in
        emit p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "if %s then (" cond ;
            indent_more p (fun () ->
              let n = print emit p l e2 in
              ppi oc "%s" n) ;
            ppi oc ") else (" ;
            indent_more p (fun () ->
              let n = print emit p l e3 in
              ppi oc "%s" n) ;
            pp oc "%s)" p.indent))
    | ReadWhile (e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr = print emit p l e4 in
        emit p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec read_while_loop accum ptr =" ;
            indent_more p (fun () ->
              ppi oc "let next_byte = Pointer.peekByte ptr 0 in" ;
              ppi oc "if not (%s next_byte) then (accum, ptr) else" cond ;
              ppi oc "let accum = %s accum next_byte in" reduce ;
              ppi oc "let ptr = Pointer.skip ptr 1 in" ;
              ppi oc "read_while_loop %s %s in" accum ptr) ;
            ppi oc "read_while_loop %s %s" accum ptr))
    | LoopWhile (e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        emit p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec loop_while accum =" ;
            indent_more p (fun () ->
              ppi oc "if not (%s accum) then accum else" cond ;
              ppi oc "loop_while (%s accum) in" body) ;
            ppi oc "loop_while %s" accum))
    | LoopUntil (e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        emit p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec loop_until accum =" ;
            indent_more p (fun () ->
              ppi oc "let accum = %s accum in" body ;
              ppi oc "if %s accum then loop_until accum else accum in" cond) ;
            ppi oc "loop_until %s" accum))
    | Repeat (e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        emit p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec loop_repeat n accum =" ;
            indent_more p (fun () ->
              ppi oc "if n >= %s then accum else" to_ ;
              ppi oc "loop_repeat (Int32.succ n) (%s accum) in" body) ;
            ppi oc "loop_repeat %s %s" from accum))
    | SetField (path, e1, e2) ->
        let ptr = print emit p l e1
        and v = print emit p l e2 in
        (match Expression.type_of l e1 with
        | Type.ValuePtr vt ->
            let a = accessor_of_path vt path in
            if a = "" then
              ppi p.def "%s := %s;" ptr v
            else
              ppi p.def "!%s%s <- %s;" ptr a v ;
            ptr
        | _ -> assert false)
    | FieldIsNull (path, e1) ->
        let ptr = print emit p l e1 in
        (match Expression.type_of l e1 with
        | Type.ValuePtr vt ->
            let a = accessor_of_path vt path in
            emit p l e (fun oc -> pp oc "!%s%s = None" ptr a)
        | _ -> assert false)
    | GetField (path, e1) ->
        let ptr = print emit p l e1 in
        (match Expression.type_of l e1 with
        | Type.ValuePtr vt ->
            let a = accessor_of_path vt path in
            emit p l e (fun oc -> pp oc "!%s%s" ptr a)
        | _ -> assert false)

  let print_binding_toplevel emit n p l e =
    let t = Expression.type_of l e in
    let tn = type_identifier p t in
    pp p.def "%slet %s : %s =\n" p.indent n tn ;
    indent_more p (fun () ->
      let n = print emit p l e in
      pp p.def "%s%s\n" p.indent n)

  let source_intro =
    "open Batteries\n\
     open Stdint\n\
     open DessserOCamlBackendHelpers\n"
end

include BackEndCLike.Make (Config)
