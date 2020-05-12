open Batteries
open Stdint
open Dessser
open BackEndCLike
open DessserTools
module T = DessserTypes
module E = DessserExpressions

module Config =
struct
  let valid_identifier s =
    if s = "" then "v" else
    if s.[0] = '!' then s
    else BackEndCLike.valid_identifier s

  let preferred_def_extension = "ml"
  let preferred_decl_extension = "mli"
  let compile_cmd ~optim ~link src dst =
    let optim = cap 2 3 optim in
    Printf.sprintf
      "ocamlfind ocamlopt -g -annot -O%d -w -26 -I src -package stdint,batteries \
       -linkpkg src/DessserFloatTools.cmx src/DessserOCamlBackendHelpers.cmx \
       %s %S -o %S"
      optim (if link then "" else "-c") src dst

  let tuple_field_name i = "field_"^ string_of_int i

  let rec print_record p oc id mns =
    let id = valid_identifier id in
    pp oc "%stype %s = {\n" p.indent id ;
    indent_more p (fun () ->
      Array.iter (fun (field_name, mn) ->
        let typ_id = type_identifier p (T.TValue mn) in
        pp oc "%smutable %s : %s;\n" p.indent (valid_identifier field_name) typ_id
      ) mns
    ) ;
    pp oc "%s}\n\n" p.indent

  and value_type_identifier p = function
    | T.NotNullable (Mac TChar) -> "char"
    | T.NotNullable (Mac TString) -> "string"
    | T.NotNullable (Mac TBool) -> "bool"
    | T.NotNullable (Mac TFloat) -> "float"
    | T.NotNullable (Mac TU8) -> "Uint8.t"
    | T.NotNullable (Mac TI8) -> "Int8.t"
    | T.NotNullable (Mac TU16) -> "Uint16.t"
    | T.NotNullable (Mac TI16) -> "Int16.t"
    | T.NotNullable (Mac TU24) -> "Uint24.t"
    | T.NotNullable (Mac TI24) -> "Int24.t"
    | T.NotNullable (Mac TU32) -> "Uint32.t"
    | T.NotNullable (Mac TI32) -> "Int32.t"
    | T.NotNullable (Mac TU40) -> "Uint40.t"
    | T.NotNullable (Mac TI40) -> "Int40.t"
    | T.NotNullable (Mac TU48) -> "Uint48.t"
    | T.NotNullable (Mac TI48) -> "Int48.t"
    | T.NotNullable (Mac TU56) -> "Uint56.t"
    | T.NotNullable (Mac TI56) -> "Int56.t"
    | T.NotNullable (Mac TU64) -> "Uint64.t"
    | T.NotNullable (Mac TI64) -> "Int64.t"
    | T.NotNullable (Mac TU128) -> "Uint128.t"
    | T.NotNullable (Mac TI128) -> "Int128.t"
    | T.NotNullable (Usr t) ->
        value_type_identifier p (NotNullable t.def)
    | T.NotNullable (TVec (_, t))
    | T.NotNullable (TList t) ->
        value_type_identifier p t ^" array"
    | T.NotNullable (TTup mns) as mn ->
        let t = T.TValue mn in
        let mns = Array.mapi (fun i mn -> tuple_field_name i, mn) mns in
        declared_type p t (fun oc type_id -> print_record p oc type_id mns) |>
        valid_identifier
    | T.NotNullable (TRec mns) as mn ->
        let t = T.TValue mn in
        declared_type p t (fun oc type_id -> print_record p oc type_id mns) |>
        valid_identifier
    | T.NotNullable (TMap _) ->
        assert false (* no value of map type *)
    | T.Nullable t ->
        value_type_identifier p (NotNullable t) ^" option"

  and type_identifier p = function
    | T.TValue mn -> value_type_identifier p mn
    | T.TVoid -> "unit"
    | T.TDataPtr -> "Pointer.t"
    | T.TSize -> "Size.t"
    | T.TBit -> "bool"
    | T.TByte -> "Uint8.t"
    | T.TWord -> "Uint16.t"
    | T.TDWord -> "Uint32.t"
    | T.TQWord -> "Uint64.t"
    | T.TOWord -> "Uint128.t"
    | T.TBytes -> "Slice.t"
    | T.TPair (t1, t2) ->
        "("^ type_identifier p t1 ^" * "^ type_identifier p t2 ^")"
    | T.TSList t1 ->
        type_identifier p t1 ^" list"
    | T.TFunction ([||], t) ->
        "(() -> "^ type_identifier p t ^")"
    | T.TFunction (args, ret) ->
        "("^ IO.to_string (
          Array.print ~first:"" ~last:"" ~sep:" -> " (fun oc t ->
            String.print oc (type_identifier p t))
        ) args ^" -> "^ type_identifier p ret ^")"

  let rec mod_name = function
    | T.TValue (NotNullable (Mac TChar)) -> "Char"
    | T.TValue (NotNullable (Mac TString)) -> "String"
    | T.TValue (NotNullable (Mac TBool)) -> "Bool"
    | T.TValue (NotNullable (Mac TFloat)) -> "Float"
    | T.TValue (NotNullable (Mac TU8)) -> "Uint8"
    | T.TValue (NotNullable (Mac TI8)) -> "Int8"
    | T.TValue (NotNullable (Mac TU16)) -> "Uint16"
    | T.TValue (NotNullable (Mac TI16)) -> "Int16"
    | T.TValue (NotNullable (Mac TU24)) -> "Uint24"
    | T.TValue (NotNullable (Mac TI24)) -> "Int24"
    | T.TValue (NotNullable (Mac TU32)) -> "Uint32"
    | T.TValue (NotNullable (Mac TI32)) -> "Int32"
    | T.TValue (NotNullable (Mac TU40)) -> "Uint40"
    | T.TValue (NotNullable (Mac TI40)) -> "Int40"
    | T.TValue (NotNullable (Mac TU48)) -> "Uint48"
    | T.TValue (NotNullable (Mac TI48)) -> "Int48"
    | T.TValue (NotNullable (Mac TU56)) -> "Uint56"
    | T.TValue (NotNullable (Mac TI56)) -> "Int56"
    | T.TValue (NotNullable (Mac TU64)) -> "Uint64"
    | T.TValue (NotNullable (Mac TI64)) -> "Int64"
    | T.TValue (NotNullable (Mac TU128)) -> "Uint128"
    | T.TValue (NotNullable (Mac TI128)) -> "Int128"
    | T.TValue (NotNullable (Usr t)) -> mod_name (TValue (NotNullable t.def))
    | T.TDataPtr -> "Pointer"
    | T.TSize -> "Size"
    | T.TBit -> "Bool"
    | T.TByte -> "Uint8"
    | T.TWord -> "Uint16"
    | T.TDWord -> "Uint32"
    | T.TQWord -> "Uint64"
    | T.TOWord -> "Uint128"
    | T.TBytes -> "Slice"
    | t ->
        Printf.sprintf2 "No module implementing %a"
          T.print t |>
        failwith

  (* Identifiers used for function parameters: *)
  let param fid n = "p_"^ string_of_int fid ^"_"^ string_of_int n

  let rec print_default_value indent oc = function
    | T.NotNullable (Mac TFloat) ->
        String.print oc "0."
    | T.NotNullable (Mac TString) ->
        String.print oc "\"\""
    | T.NotNullable (Mac TBool) ->
        String.print oc "false"
    | T.NotNullable (Mac TChar) ->
        String.print oc "'\\000'"
    | T.NotNullable (Mac TI8) ->
        String.print oc "Int8.zero"
    | T.NotNullable (Mac TI16) ->
        String.print oc "Int16.zero"
    | T.NotNullable (Mac TI24) ->
        String.print oc "Int24.zero"
    | T.NotNullable (Mac TI32) ->
        String.print oc "Int32.zero"
    | T.NotNullable (Mac TI40) ->
        String.print oc "Int40.zero"
    | T.NotNullable (Mac TI48) ->
        String.print oc "Int48.zero"
    | T.NotNullable (Mac TI56) ->
        String.print oc "Int56.zero"
    | T.NotNullable (Mac TI64) ->
        String.print oc "Int64.zero"
    | T.NotNullable (Mac TI128) ->
        String.print oc "Int128.zero"
    | T.NotNullable (Mac TU8) ->
        String.print oc "Uint8.zero"
    | T.NotNullable (Mac TU16) ->
        String.print oc "Uint16.zero"
    | T.NotNullable (Mac TU24) ->
        String.print oc "Uint24.zero"
    | T.NotNullable (Mac TU32) ->
        String.print oc "Uint32.zero"
    | T.NotNullable (Mac TU40) ->
        String.print oc "Uint40.zero"
    | T.NotNullable (Mac TU48) ->
        String.print oc "Uint48.zero"
    | T.NotNullable (Mac TU56) ->
        String.print oc "Uint56.zero"
    | T.NotNullable (Mac TU64) ->
        String.print oc "Uint64.zero"
    | T.NotNullable (Mac TU128) ->
        String.print oc "Uint128.zero"
    | T.NotNullable (Usr nn) ->
        print_default_value indent oc (NotNullable nn.def)
    | T.NotNullable (TTup mns) ->
        Array.print ~first:("{\n"^indent^"  ") ~last:("\n"^indent^"}") ~sep:(";\n"^indent^"  ")
          (fun oc (i, mn) ->
            let fname = tuple_field_name i in
            Printf.fprintf oc "%s = %a"
              fname (print_default_value (indent^"  ")) mn)
          oc (Array.mapi (fun i mn -> (i, mn)) mns)
    | T.NotNullable (TRec mns) ->
        Array.print ~first:("{\n"^indent^"  ") ~last:("\n"^indent^"}") ~sep:(";\n"^indent^"  ")
          (fun oc (fname, mn) ->
            Printf.fprintf oc "%s = %a"
              (valid_identifier fname)
              (print_default_value (indent^"  ")) mn)
          oc mns
    | T.NotNullable (TVec (dim, mn)) ->
        Printf.fprintf oc "[| " ;
        for i = 0 to dim - 1 do
          Printf.fprintf oc "%a; "
            (print_default_value (indent^"  ")) mn
        done ;
        Printf.fprintf oc "%s|]" indent
    | T.NotNullable (TList _) ->
        String.print oc "[||]"
    | T.NotNullable (TMap _) ->
        assert false (* no value of map type *)
    | T.Nullable nn ->
        (* Unfortunately we cannot start with None as we want the whole tree
         * of values to be populated. *)
        Printf.fprintf oc "Some (%a)"
          (print_default_value indent) (NotNullable nn)

  let print_binding n tn f oc =
    pp oc "let %s : %s = %t in" n tn f

  let print_comment oc fmt =
    pp oc ("(* " ^^ fmt ^^ " *)\n")

  let print_float_literal v oc =
    (* printf "%F" would not work for infinity:
     * https://caml.inria.fr/mantis/view.php?id=7685
     * and "%h" not for neg_infinity. *)
    if v = infinity then String.print oc "infinity"
    else if v = neg_infinity then String.print oc "neg_infinity"
    else Legacy.Printf.sprintf "%h" v |> String.print oc

  let lift_u32 v oc =
    pp oc "Uint32.of_int32 (%ldl)" (Uint32.to_int32 v)

  let lift_u40 v oc =
    pp oc "Uint40.of_int64 (%LdL)" (Uint40.to_int64 v)

  let lift_u48 v oc =
    pp oc "Uint48.of_int64 (%LdL)" (Uint48.to_int64 v)

  let lift_u56 v oc =
    pp oc "Uint56.of_int64 (%LdL)" (Uint56.to_int64 v)

  let lift_u64 v oc =
    pp oc "Uint64.of_int64 (%LdL)" (Uint64.to_int64 v)

  let lift_u128 v oc =
    if Uint128.compare v (Uint128.of_int max_int) < 0 then
      pp oc "Uint128.of_int (%d)" (Uint128.to_int v)
    else (
      let bytes = Bytes.create 16 in
      Uint128.to_bytes_little_endian v bytes 0 ;
      pp oc "Uint128.of_bytes_little_endian (Bytes.of_string %S) 0"
        (Bytes.to_string bytes))

  let lift_i128 v oc =
    if Int128.compare v (Int128.of_int min_int) > 0 &&
       Int128.compare v (Int128.of_int max_int) < 0 then
      pp oc "Int128.of_int (%d)" (Int128.to_int v)
    else (
      let bytes = Bytes.create 16 in
      Int128.to_bytes_little_endian v bytes 0 ;
      pp oc "Int128.of_bytes_little_endian (Bytes.of_string %S) 0"
        (Bytes.to_string bytes))

  let rec deref_path v mn = function
    | [] -> v
    | i :: path ->
        let rec deref_not_nullable v = function
          | T.NotNullable (Mac _ | TMap _) ->
              assert false
          | T.NotNullable (Usr t) ->
              deref_not_nullable v (NotNullable t.def)
          | T.NotNullable (TVec (_, mn))
          | T.NotNullable (TList mn) ->
              deref_path (v ^".("^ string_of_int i ^")") mn path
          | T.NotNullable (TTup mns) ->
              deref_path (v ^"."^ tuple_field_name i) mns.(i) path
          | T.NotNullable (TRec mns) ->
              let name = valid_identifier (fst mns.(i)) in
              deref_path (v ^"."^ name) (snd mns.(i)) path
          | T.Nullable x ->
              deref_not_nullable ("(option_get "^ v ^")") (NotNullable x) in
        deref_not_nullable v mn

  let rec print ?name emit p l e =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.indent in
    let unary_op op e1 =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s %s" op n1) in
    let unary_mod_op op e1 =
      let op = mod_name (E.type_of l e) ^"."^ op in
      unary_op op e1 in
    let any_op op es =
      let ns = List.map (print emit p l) es in
      let ns = String.concat " " ns in
      emit ?name p l e (fun oc -> pp oc "%s %s" op ns) in
    let binary_op op e1 e2 =
      any_op op [ e1 ; e2 ] in
    let binary_infix_op e1 op e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2 in
      emit ?name p l e (fun oc -> pp oc "%s %s %s" n1 op n2) in
    let shortcutting_binary_infix_op e1 op e2 =
      emit ?name p l e (fun oc ->
        pp oc "(\n" ;
        indent_more p (fun () ->
          let n1 = print emit p l e1 in
          ppi oc "%s" n1
        ) ;
        ppi oc ") %s (" op ;
        indent_more p (fun () ->
          let n2 = print emit p l e2 in
          ppi oc "%s" n2
        ) ;
        pp oc "%s)" p.indent) in
    let binary_mod_op op e1 e2 =
      let op = mod_name (E.type_of l e) ^"."^ op in
      binary_op op e1 e2 in
    let binary_mod_op_2nd_u8 op e1 e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2
      and m = mod_name (E.type_of l e) in
      emit ?name p l e (fun oc ->
        pp oc "%s.%s %s (Uint8.to_int %s)" m op n1 n2)
    in
    match e with
    | E.E1 (Comment c, e1) ->
        ppi p.def "(* %s *)" c ;
        print ?name emit p l e1
    | E.E0S (Seq, es) ->
        List.fold_left (fun _ e -> print emit p l e) "()" es
    | E.E0S ((MakeVec | MakeList), es) ->
        let inits = List.map (print emit p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:"[| " ~last:" |]" ~sep:"; " String.print oc inits)
    | E.E0S (MakeTup, es) ->
        let inits = List.map (print emit p l) es in
        (* TODO: There is no good reason any longer to avoid using actual
         * OCaml tuples to represent tuples *)
        let i = ref 0 in
        emit ?name p l e (fun oc ->
          List.print ~first:"{ " ~last:" }" ~sep:"; " (fun oc n ->
            Printf.fprintf oc "%s = %s" (tuple_field_name !i) n ;
            incr i) oc inits)
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
          List.print ~first:"{ " ~last:" }" ~sep:"; "
            (fun oc (name, n) ->
              Printf.fprintf oc "%s = %s" name n) oc inits)
    | E.E1 (Ignore, e1) ->
        let n = print emit p l e1 in
        pp p.def "%signore %s;\n" p.indent n ;
        "()"
    | E.E1 (Dump, e1) ->
        let n = print emit p l e1 in
        pp p.def ("%s"^^
          (match E.type_of l e1 with
          | TValue (NotNullable (Mac TString)) ->
              "print_string %s;"
          | TValue (NotNullable (Mac TChar)) ->
              "print_char %s;"
          | _ ->
              "print_string (Batteries.dump %s);") ^^"\n")
          p.indent n ;
        "()"
    | E.E1 (Debug, e1) ->
        print ?name emit p l (E1 ((if !E.dump_debug then Dump else Ignore), e1))
    | E.E1 (IsNull, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s = None" n)
    | E.E2 (Coalesce, e1, e2) ->
        binary_infix_op e1 "|?" e2
    | E.E2 (Nth, e1, e2) ->
        let n1 = print emit p l e1 in
        let n2 = print emit p l e2 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.(%s.to_int %s)" n2 m n1)
    | E.E1 (ToNullable, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "Some %s" n1)
    | E.E1 (ToNotNullable, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> Printf.fprintf oc "option_get %s" n1)
    | E.E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "None")
    | E.E0 (Float f) ->
        emit ?name p l e (print_float_literal f)
    | E.E0 (String s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc s)
    | E.E0 (Bit b) | E.E0 (Bool b) ->
        emit ?name p l e (fun oc -> Bool.print oc b)
    | E.E0 (Char c) ->
        emit ?name p l e (fun oc -> pp oc "%C" c)
    | E.E0 (Byte i) | E.E0 (U8 i) ->
        emit ?name p l e (fun oc -> pp oc "Uint8.of_int (%d)" i)
    | E.E0 (Word i) | E.E0 (U16 i) ->
        emit ?name p l e (fun oc -> pp oc "Uint16.of_int (%d)" i)
    | E.E0 (U24 i) ->
        emit ?name p l e (fun oc -> pp oc "Uint24.of_int (%d)" i)
    | E.E0 (DWord u) | E.E0 (U32 u) ->
        emit ?name p l e (lift_u32 u)
    | E.E0 (U40 u) ->
        emit ?name p l e (lift_u40 u)
    | E.E0 (U48 u) ->
        emit ?name p l e (lift_u48 u)
    | E.E0 (U56 u) ->
        emit ?name p l e (lift_u56 u)
    | E.E0 (QWord u) | E.E0 (U64 u) ->
        emit ?name p l e (lift_u64 u)
    | E.E0 (OWord u) | E.E0 (U128 u) ->
        emit ?name p l e (lift_u128 u)
    | E.E0 (I8 i) ->
        emit ?name p l e (fun oc -> pp oc "Int8.of_int (%d)" i)
    | E.E0 (I16 i) ->
        emit ?name p l e (fun oc -> pp oc "Int16.of_int (%d)" i)
    | E.E0 (I24 i) ->
        emit ?name p l e (fun oc -> pp oc "Int24.of_int (%d)" i)
    | E.E0 (I32 i) ->
        emit ?name p l e (fun oc -> pp oc "Int32.of_int32 (%ldl)" i)
    | E.E0 (I40 i) ->
        emit ?name p l e (fun oc -> pp oc "Int40.of_int64 (%LdL)" i)
    | E.E0 (I48 i) ->
        emit ?name p l e (fun oc -> pp oc "Int48.of_int64 (%LdL)" i)
    | E.E0 (I56 i) ->
        emit ?name p l e (fun oc -> pp oc "Int56.of_int64 (%LdL)" i)
    | E.E0 (I64 i) ->
        emit ?name p l e (fun oc -> pp oc "Int64.of_int64 (%LdL)" i)
    | E.E0 (I128 i) ->
        emit ?name p l e (lift_i128 i)
    | E.E0 (Size s) ->
        emit ?name p l e (fun oc -> pp oc "Size.of_int (%d)" s)
    | E.E2 (Gt, e1, e2) ->
        binary_infix_op e1 ">" e2
    | E.E2 (Ge, e1, e2) ->
        binary_infix_op e1 ">=" e2
    | E.E2 (Eq, e1, e2) ->
        binary_infix_op e1 "=" e2
    | E.E2 (Ne, e1, e2) ->
        binary_infix_op e1 "<>" e2
    | E.E2 (Add, e1, e2) ->
        binary_mod_op "add" e1 e2
    | E.E2 (Sub, e1, e2) ->
        binary_mod_op "sub" e1 e2
    | E.E2 (Mul, e1, e2) ->
        binary_mod_op "mul" e1 e2
    | E.E2 (Div, e1, e2) ->
        binary_mod_op "div" e1 e2
    | E.E2 (Rem, e1, e2) ->
        binary_mod_op "rem" e1 e2
    | E.E2 (LogAnd, e1, e2) ->
        binary_mod_op "logand" e1 e2
    | E.E2 (LogOr, e1, e2) ->
        binary_mod_op "logor" e1 e2
    | E.E2 (LogXor, e1, e2) ->
        binary_mod_op "logxor" e1 e2
    | E.E1 (LogNot, e1) ->
        unary_mod_op "lognot" e1
    | E.E2 (LeftShift, e1, e2) ->
        binary_mod_op_2nd_u8 "shift_left" e1 e2
    | E.E2 (RightShift, e1, e2) ->
        binary_mod_op_2nd_u8 "shift_right_logical" e1 e2
    | E.E1 (StringOfInt, e1) ->
        let op = mod_name (E.type_of l e1) ^".to_string" in
        unary_op op e1
    | E.E1 (U8OfString, e1)
    | E.E1 (I8OfString, e1)
    | E.E1 (U16OfString, e1)
    | E.E1 (I16OfString, e1)
    | E.E1 (U24OfString, e1)
    | E.E1 (I24OfString, e1)
    | E.E1 (U32OfString, e1)
    | E.E1 (I32OfString, e1)
    | E.E1 (U40OfString, e1)
    | E.E1 (I40OfString, e1)
    | E.E1 (U48OfString, e1)
    | E.E1 (I48OfString, e1)
    | E.E1 (U56OfString, e1)
    | E.E1 (I56OfString, e1)
    | E.E1 (U64OfString, e1)
    | E.E1 (I64OfString, e1)
    | E.E1 (U128OfString, e1)
    | E.E1 (I128OfString, e1) ->
        unary_mod_op "of_string" e1
    | E.E1 (FloatOfQWord, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "BatInt64.float_of_bits (Uint64.to_int64 %s)" n)
    | E.E1 (QWordOfFloat, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "Uint64.of_int64 (BatInt64.bits_of_float %s)" n)
    | E.E1 (StringOfFloat, e1) ->
        unary_op "hexstring_of_float" e1
    | E.E1 (StringOfChar, e1) ->
        unary_op "string_of_char" e1
    | E.E1 (CharOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s.[0]" n)
    | E.E1 (FloatOfString, e1) ->
        unary_op "float_of_string" e1
    | E.E1 (ByteOfU8, e1) | E.E1 (U8OfByte, e1)
    | E.E1 (WordOfU16, e1) | E.E1 (U16OfWord, e1)
    | E.E1 (U32OfDWord, e1) | E.E1 (DWordOfU32, e1)
    | E.E1 (U64OfQWord, e1) | E.E1 (QWordOfU64, e1)
    | E.E1 (U128OfOWord, e1) | E.E1 (OWordOfU128, e1)
    | E.E1 (BitOfBool, e1) | E.E1 (BoolOfBit, e1) ->
        print ?name emit p l e1
    | E.E1 (U8OfChar, e1) ->
        unary_op "Uint8.of_int @@ Char.code" e1
    | E.E1 (CharOfU8, e1) ->
        unary_op "Char.chr @@ Uint8.to_int" e1
    | E.E1 (SizeOfU32, e1) ->
        unary_op "Uint32.to_int" e1
    | E.E1 (U32OfSize, e1) ->
        unary_op "Uint32.of_int" e1
    | E.E1 (ListOfSList, e1) ->
        unary_op "Array.of_list" e1
    | E.E1 (ListOfSListRev, e1) ->
        unary_op "array_of_list_rev" e1
    | E.E1 (ToU8, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint8") e1
    | E.E1 (ToI8, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int8") e1
    | E.E1 (ToU16, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint16") e1
    | E.E1 (ToI16, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int16") e1
    | E.E1 (ToU24, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint24") e1
    | E.E1 (ToI24, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int24") e1
    | E.E1 (ToU32, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint32") e1
    | E.E1 (ToI32, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int32") e1
    | E.E1 (ToU40, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint40") e1
    | E.E1 (ToI40, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int40") e1
    | E.E1 (ToU48, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint48") e1
    | E.E1 (ToI48, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int48") e1
    | E.E1 (ToU56, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint56") e1
    | E.E1 (ToI56, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int56") e1
    | E.E1 (ToU64, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint64") e1
    | E.E1 (ToI64, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int64") e1
    | E.E1 (ToU128, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_uint128") e1
    | E.E1 (ToI128, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".to_int128") e1
    | E.E1 (U8OfBool, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "if %s then Uint8.one else Uint8.zero" n)
    | E.E1 (BoolOfU8, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "Uint8.compare Uint8.one %s = 0" n)
    | E.E2 (AppendByte, e1, e2) ->
        binary_op "Slice.add" e1 e2
    | E.E2 (AppendBytes, e1, e2) ->
        binary_op "Slice.append" e1 e2
    | E.E2 (AppendString, e1, e2) ->
        binary_infix_op e1 "^" e2
    | E.E1 (StringLength, e1) ->
        unary_op "Uint32.of_int @@ String.length" e1
    | E.E1 (StringOfBytes, e1) ->
        unary_op "Slice.to_string" e1
    | E.E1 (BytesOfString, e1) ->
        unary_op "Slice.of_string" e1
    | E.E1 (ListLength, e1) ->
        unary_op "Uint32.of_int @@ Array.length" e1
    | E.E0 (DataPtrOfString s) ->
        emit ?name p l e (fun oc -> pp oc "Pointer.of_string %S" s)
    | E.E0 (DataPtrOfBuffer n) ->
        emit ?name p l e (fun oc -> pp oc "Pointer.of_buffer %d" n)
    | E.E2 (GetBit, e1, e2) ->
        binary_op "Pointer.getBit" e1 e2
    | E.E3 (SetBit, e1, e2, e3) ->
        let ptr = print ?name emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        ppi p.def "Pointer.setBit %s %s %s;" ptr n2 n3 ;
        "()"
    | E.E1 (ReadByte, e1) ->
        unary_op "Pointer.readByte" e1
    | E.E1 (ReadWord LittleEndian, e1) ->
        unary_op "Pointer.readWord ~big_endian:false" e1
    | E.E1 (ReadWord BigEndian, e1) ->
        unary_op "Pointer.readWord ~big_endian:true" e1
    | E.E1 (ReadDWord LittleEndian, e1) ->
        unary_op "Pointer.readDWord ~big_endian:false" e1
    | E.E1 (ReadDWord BigEndian, e1) ->
        unary_op "Pointer.readDWord ~big_endian:true" e1
    | E.E1 (ReadQWord LittleEndian, e1) ->
        unary_op "Pointer.readQWord ~big_endian:false" e1
    | E.E1 (ReadQWord BigEndian, e1) ->
        unary_op "Pointer.readQWord ~big_endian:true" e1
    | E.E1 (ReadOWord LittleEndian, e1) ->
        unary_op "Pointer.readOWord ~big_endian:false" e1
    | E.E1 (ReadOWord BigEndian, e1) ->
        unary_op "Pointer.readOWord ~big_endian:true" e1
    | E.E2 (ReadBytes, e1, e2) ->
        binary_op "Pointer.readBytes" e1 e2
    | E.E2 (PeekByte, e1, e2) ->
        binary_op "Pointer.peekByte" e1 e2
    | E.E2 (PeekWord LittleEndian, e1, e2) ->
        binary_op "Pointer.peekWork ~big_endian:false" e1 e2
    | E.E2 (PeekWord BigEndian, e1, e2) ->
        binary_op "Pointer.peekWork ~big_endian:true" e1 e2
    | E.E2 (PeekDWord LittleEndian, e1, e2) ->
        binary_op "Pointer.peekDWork ~big_endian:false" e1 e2
    | E.E2 (PeekDWord BigEndian, e1, e2) ->
        binary_op "Pointer.peekDWork ~big_endian:true" e1 e2
    | E.E2 (PeekQWord LittleEndian, e1, e2) ->
        binary_op "Pointer.peekQWork ~big_endian:false" e1 e2
    | E.E2 (PeekQWord BigEndian, e1, e2) ->
        binary_op "Pointer.peekQWork ~big_endian:true" e1 e2
    | E.E2 (PeekOWord LittleEndian, e1, e2) ->
        binary_op "Pointer.peekOWork ~big_endian:false" e1 e2
    | E.E2 (PeekOWord BigEndian, e1, e2) ->
        binary_op "Pointer.peekOWork ~big_endian:true" e1 e2
    | E.E2 (WriteByte, e1, e2) ->
        binary_op "Pointer.writeByte" e1 e2
    | E.E2 (WriteWord LittleEndian, e1, e2) ->
        binary_op "Pointer.writeWord ~big_endian:false" e1 e2
    | E.E2 (WriteWord BigEndian, e1, e2) ->
        binary_op "Pointer.writeWord ~big_endian:true" e1 e2
    | E.E2 (WriteDWord LittleEndian, e1, e2) ->
        binary_op "Pointer.writeDWord ~big_endian:false" e1 e2
    | E.E2 (WriteDWord BigEndian, e1, e2) ->
        binary_op "Pointer.writeDWord ~big_endian:true" e1 e2
    | E.E2 (WriteQWord LittleEndian, e1, e2) ->
        binary_op "Pointer.writeQWord ~big_endian:false" e1 e2
    | E.E2 (WriteQWord BigEndian, e1, e2) ->
        binary_op "Pointer.writeQWord ~big_endian:true" e1 e2
    | E.E2 (WriteOWord LittleEndian, e1, e2) ->
        binary_op "Pointer.writeOWord ~big_endian:false" e1 e2
    | E.E2 (WriteOWord BigEndian, e1, e2) ->
        binary_op "Pointer.writeOWord ~big_endian:true" e1 e2
    | E.E2 (WriteBytes, e1, e2) ->
        binary_op "Pointer.writeBytes" e1 e2
    | E.E2 (PokeByte, e1, e2) ->
        let ptr = print ?name emit p l e1
        and v = print emit p l e2 in
        ppi p.def "Pointer.pokeByte %s %s;" ptr v ;
        ptr
    | E.E3 (BlitByte, e1, e2, e3) ->
        any_op "Pointer.blitBytes" [ e1 ; e2 ; e3 ]
    | E.E2 (DataPtrAdd, e1, e2) ->
        binary_op "Pointer.skip" e1 e2
    | E.E2 (DataPtrSub, e1, e2) ->
        binary_op "Pointer.sub" e1 e2
    | E.E1 (DataPtrPush, e1) ->
        unary_op "Pointer.push" e1
    | E.E1 (DataPtrPop, e1) ->
        unary_op "Pointer.pop" e1
    | E.E1 (RemSize, e1) ->
        unary_op "Pointer.remSize" e1
    | E.E1 (DataPtrOffset, e1) ->
        unary_op "Pointer.offset" e1
    | E.E2 (And, e1, e2) ->
        shortcutting_binary_infix_op e1 "&&" e2
    | E.E2 (Or, e1, e2) ->
        shortcutting_binary_infix_op e1 "||" e2
    | E.E1 (Not, e1) ->
        unary_op "not" e1
    | E.E2 (Cons, e1, e2) ->
        binary_infix_op e1 "::" e2
    | E.E0 (EndOfList _) ->
        emit ?name p l e (fun oc -> pp oc "[]")
    | E.E1 (Head, e1) ->
        unary_op "List.hd" e1
    | E.E1 (Tail, e1) ->
        unary_op "List.tl" e1
    | E.E2 (Pair, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E1 (Fst, e1) ->
        unary_op "fst" e1
    | E.E1 (Snd, e1) ->
        unary_op "snd" e1
    | E.E2 (MapPair, e1, e2) ->
        let n1 = print emit p l e1 (* the pair *)
        and n2 = print emit p l e2 (* the function2 *) in
        let n1_0 = "(fst "^ n1 ^")"
        and n1_1 = "(snd "^ n1 ^")" in
        emit ?name p l e (fun oc -> pp oc "%s %s %s" n2 n1_0 n1_1)
    | E.E0 (Identifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            valid_identifier s)
    | E.E2 (Let n, e1, e2) ->
        (* Most of definitions we can actually choose the name (with ?name),
         * so we save a let. But for a few [e1] we will have no such choice,
         * so then another let is required: *)
        let n1 = print ~name:n emit p l e1 in
        if n1 <> n then
          ignore (emit ?name:(Some n) p l e1 (fun oc -> String.print oc n1)) ;
        let t = E.type_of l e1 in
        let l = (E.E0 (Identifier n), t) :: l in
        print ?name emit p l e2
    | E.E1 (Function (_fid, [||]), e1) ->
        emit ?name p l e (fun oc ->
          pp oc "(fun () ->\n" ;
          indent_more p (fun () ->
            let n = print emit p l e1 in
            pp oc "%s%s)" p.indent n))
    | E.E1 (Function (fid, ts), e1) ->
        emit ?name p l e (fun oc ->
          array_print_i ~first:"(fun " ~last:" ->\n" ~sep:" "
            (fun i oc _t -> String.print oc (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (E.E0 (Param (fid, i)), t) :: l
            ) l ts in
          indent_more p (fun () ->
            let n = print emit p l e1 in
            pp oc "%s%s)" p.indent n))
    | E.E0 (Param (fid, n)) ->
        param fid n
    | E.E3 (Choose, e1, e2, e3) ->
        let cond = print emit p l e1 in
        emit ?name p l e (fun oc ->
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
    | E.E4 (ReadWhile, e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr = print emit p l e4 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec read_while_loop accum ptr =" ;
            indent_more p (fun () ->
              ppi oc "if Pointer.remSize ptr <= 0 then (accum, ptr) else" ;
              ppi oc "let next_byte = Pointer.peekByte ptr 0 in" ;
              ppi oc "if not (%s next_byte) then (accum, ptr) else" cond ;
              ppi oc "let accum = %s accum next_byte in" reduce ;
              ppi oc "let ptr = Pointer.skip ptr 1 in" ;
              ppi oc "read_while_loop accum ptr in") ;
            pp oc "%sread_while_loop %s %s" p.indent accum ptr))
    | E.E3 (LoopWhile, e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec loop_while accum =" ;
            indent_more p (fun () ->
              ppi oc "if not (%s accum) then accum else" cond ;
              ppi oc "loop_while (%s accum) in" body) ;
            pp oc "%sloop_while %s" p.indent accum))
    | E.E3 (LoopUntil, e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec loop_until accum =" ;
            indent_more p (fun () ->
              ppi oc "let accum = %s accum in" body ;
              ppi oc "if %s accum then loop_until accum else accum in" cond) ;
            pp oc "%sloop_until %s" p.indent accum))
    | E.E4 (Repeat, e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          indent_more p (fun () ->
            ppi oc "let rec loop_repeat n accum =" ;
            indent_more p (fun () ->
              ppi oc "if n >= %s then accum else" to_ ;
              ppi oc "loop_repeat (Int32.succ n) (%s n accum) in" body) ;
            pp oc "%sloop_repeat %s %s" p.indent from accum))
    | E.E1 (GetItem n, e1) ->
        let n1 = print emit p l e1 in
(*      TODO: For when tuples are actual tuples:
        let res = gen_sym ?name "get_item_" in
        let max_n =
          match E.type_of l e1 with
          | TValue (NotNullable (TTup mns)) -> Array.length mns
          | _ -> assert false in
        ppi p.def "let %t = %s\n"
          (fun oc ->
            for i = 0 to max_n - 1 do
              if i > 0 then String.print oc ", " ;
              String.print oc (if i = n then res else "_")
            done) *)
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 (tuple_field_name n))
    | E.E1 (GetField s, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 s)
    | E.E1 (Assert, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "assert %s" n)

  let print_binding_toplevel emit n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.def "%slet %s : %s =\n" p.indent n tn ;
    indent_more p (fun () ->
      let n = print emit p l e in
      pp p.def "%s%s\n" p.indent n)

  let print_identifier_declaration n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.def "%sval %s : %s\n" p.indent n tn

  let source_intro =
    "open Stdint\n\
     open DessserOCamlBackendHelpers\n"
end

include BackEndCLike.Make (Config)
