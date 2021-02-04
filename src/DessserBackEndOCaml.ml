open Batteries
open Stdint
open Dessser
open DessserBackEndCLike
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module P = DessserPrinter

let debug = false

type T.backend_id += OCaml

module Config =
struct
  let id = OCaml

  let valid_identifier s =
    if s = "" then "v" else
    if s.[0] = '!' then s else
    DessserBackEndCLike.valid_identifier s

  let valid_module_name s =
    assert (s <> "" && s.[0] <> '!') ;
    String.capitalize (valid_identifier s)

  let valid_source_name n =
    if n = "" then "f" else
    String.mapi (fun i c ->
      if c >= 'a' && c <= 'z' ||
         c >= 'A' && c <= 'Z' ||
         i > 0 && (
           c >= '0' && c <= '9' ||
           c = '_'
        )
      then c else '_'
    ) n

  let preferred_def_extension = "ml"
  let preferred_decl_extension = "mli"
  let compile_cmd ~optim ~link src dst =
    let optim = cap 2 3 optim in
    Printf.sprintf
      "ocamlfind ocamlopt -g -annot -O%d -w -8-26 -I src \
         -package stdint,batteries,lmdb \
         -linkpkg src/DessserFloatTools.cmx src/DessserOCamlBackEndHelpers.cmx \
         %s %S -o %S"
      optim (if link then "" else "-c") src dst

  let module_of_type t =
    valid_module_name (T.uniq_id t)

  let tuple_field_name i = "field_"^ string_of_int i

  let cstr_name n =
    String.capitalize (valid_identifier n)

  let rec print_record p oc id mns =
    let m = valid_module_name id in
    let id = valid_identifier id in
    pp oc "%smodule %s = struct\n" p.P.indent m ;
    P.indent_more p (fun () ->
      pp oc "%stype t = {\n" p.P.indent ;
      P.indent_more p (fun () ->
        Array.iter (fun (field_name, mn) ->
          let typ_id = type_identifier p (T.Value mn) in
          pp oc "%smutable %s : %s;\n"
            p.P.indent (valid_identifier field_name) typ_id
        ) mns
      ) ;
      pp oc "%s}\n" p.P.indent
    ) ;
    pp oc "%send\n" p .indent ;
    (* Also define the type alias: *)
    pp oc "%stype %s = %s.t\n\n" p.P.indent id m

  and print_sum p oc id mns =
    let m = valid_module_name id in
    let id = valid_identifier id in
    pp oc "%smodule %s = struct\n" p.P.indent m ;
    P.indent_more p (fun () ->
      pp oc "%stype t = \n" p.P.indent ;
      P.indent_more p (fun () ->
        Array.iter (fun (n, mn) ->
          let typ_id = type_identifier p (T.Value mn) in
          pp oc "%s| %s of %s\n" p.P.indent (cstr_name n) typ_id
        ) mns
      ) ;
      pp oc "\n" ;
      (* Associated "label_of_cstr": *)
      pp oc "%slet label_of_cstr = function\n" p.P.indent ;
      P.indent_more p (fun () ->
        Array.iteri (fun i (n, _) ->
          pp oc "%s| %s _ -> Uint16.of_int %d" p.P.indent (cstr_name n) i
        ) mns
      ) ;
    ) ;
    pp oc "\n%send\n" p.P.indent ;
    (* Also define the type alias: *)
    pp oc "%stype %s = %s.t\n\n" p.P.indent id m

  and value_type_identifier p = function
    | T.{ vtyp ; nullable = true } ->
        value_type_identifier p { vtyp ; nullable = false } ^" nullable"
    | { vtyp = Unknown ; _ } -> invalid_arg "value_type_identifier"
    | { vtyp = Unit ; _ } -> "unit"
    | { vtyp = Mac Char ; _ } -> "char"
    | { vtyp = Mac String ; _ } -> "string"
    | { vtyp = Mac Bool ; _ } -> "bool"
    | { vtyp = Mac Float ; _ } -> "float"
    | { vtyp = Mac U8 ; _ } -> "Uint8.t"
    | { vtyp = Mac I8 ; _ } -> "Int8.t"
    | { vtyp = Mac U16 ; _ } -> "Uint16.t"
    | { vtyp = Mac I16 ; _ } -> "Int16.t"
    | { vtyp = Mac U24 ; _ } -> "Uint24.t"
    | { vtyp = Mac I24 ; _ } -> "Int24.t"
    | { vtyp = Mac U32 ; _ } -> "Uint32.t"
    | { vtyp = Mac I32 ; _ } -> "Int32.t"
    | { vtyp = Mac U40 ; _ } -> "Uint40.t"
    | { vtyp = Mac I40 ; _ } -> "Int40.t"
    | { vtyp = Mac U48 ; _ } -> "Uint48.t"
    | { vtyp = Mac I48 ; _ } -> "Int48.t"
    | { vtyp = Mac U56 ; _ } -> "Uint56.t"
    | { vtyp = Mac I56 ; _ } -> "Int56.t"
    | { vtyp = Mac U64 ; _ } -> "Uint64.t"
    | { vtyp = Mac I64 ; _ } -> "Int64.t"
    | { vtyp = Mac U128 ; _ } -> "Uint128.t"
    | { vtyp = Mac I128 ; _ } -> "Int128.t"
    | { vtyp = Usr t ; _ } ->
        value_type_identifier p { vtyp = t.def ; nullable = false }
    | { vtyp = Ext n ; _ } ->
        T.get_external_type n OCaml
    | { vtyp = (Vec (_, t) | Lst t) ; _ } ->
        value_type_identifier p t ^" array"
    | { vtyp = Set t ; _ } ->
        value_type_identifier p t ^" set"
    | { vtyp = Tup mns ; _ } as mn ->
        let t = T.Value mn in
        let mns = Array.mapi (fun i mn -> tuple_field_name i, mn) mns in
        P.declared_type p t (fun oc type_id -> print_record p oc type_id mns) |>
        valid_identifier
    | { vtyp = Rec mns ; _ } as mn ->
        let t = T.Value mn in
        P.declared_type p t (fun oc type_id -> print_record p oc type_id mns) |>
        valid_identifier
    | { vtyp = Sum mns ; _ } as mn ->
        let t = T.Value mn in
        P.declared_type p t (fun oc type_id -> print_sum p oc type_id mns) |>
        valid_identifier
    | { vtyp = Map _ ; _ } ->
        assert false (* no value of map type *)

  and type_identifier p = function
    | T.Value mn -> value_type_identifier p mn
    | T.Void -> "unit"
    | T.DataPtr -> "Pointer.t"
    | T.Size -> "Size.t"
    | T.Bit -> "bool"
    | T.Byte -> "Uint8.t"
    | T.Word -> "Uint16.t"
    | T.DWord -> "Uint32.t"
    | T.QWord -> "Uint64.t"
    | T.OWord -> "Uint128.t"
    | T.Bytes -> "Slice.t"
    | T.Pair (t1, t2) ->
        "("^ type_identifier p t1 ^" * "^ type_identifier p t2 ^")"
    | T.SList t1 ->
        type_identifier p t1 ^" list"
    | T.Function ([||], t) ->
        "(unit -> "^ type_identifier p t ^")"
    | T.Function (args, ret) ->
        "("^ IO.to_string (
          Array.print ~first:"" ~last:"" ~sep:" -> " (fun oc t ->
            String.print oc (type_identifier p t))
        ) args ^" -> "^ type_identifier p ret ^")"
    | T.Mask -> "DessserMasks.t"

  let rec mod_name = function
    | T.Value { vtyp = Mac Char ; nullable = false } -> "Char"
    | T.Value { vtyp = Mac String ; nullable = false } -> "String"
    | T.Value { vtyp = Mac Bool ; nullable = false } -> "Bool"
    | T.Value { vtyp = Mac Float ; nullable = false } -> "Float"
    | T.Value { vtyp = Mac U8 ; nullable = false } -> "Uint8"
    | T.Value { vtyp = Mac I8 ; nullable = false } -> "Int8"
    | T.Value { vtyp = Mac U16 ; nullable = false } -> "Uint16"
    | T.Value { vtyp = Mac I16 ; nullable = false } -> "Int16"
    | T.Value { vtyp = Mac U24 ; nullable = false } -> "Uint24"
    | T.Value { vtyp = Mac I24 ; nullable = false } -> "Int24"
    | T.Value { vtyp = Mac U32 ; nullable = false } -> "Uint32"
    | T.Value { vtyp = Mac I32 ; nullable = false } -> "Int32"
    | T.Value { vtyp = Mac U40 ; nullable = false } -> "Uint40"
    | T.Value { vtyp = Mac I40 ; nullable = false } -> "Int40"
    | T.Value { vtyp = Mac U48 ; nullable = false } -> "Uint48"
    | T.Value { vtyp = Mac I48 ; nullable = false } -> "Int48"
    | T.Value { vtyp = Mac U56 ; nullable = false } -> "Uint56"
    | T.Value { vtyp = Mac I56 ; nullable = false } -> "Int56"
    | T.Value { vtyp = Mac U64 ; nullable = false } -> "Uint64"
    | T.Value { vtyp = Mac I64 ; nullable = false } -> "Int64"
    | T.Value { vtyp = Mac U128 ; nullable = false } -> "Uint128"
    | T.Value { vtyp = Mac I128 ; nullable = false } -> "Int128"
    | T.Value { vtyp = Usr t ; nullable = false } ->
        mod_name (Value { vtyp = t.def ; nullable = false })
    | T.DataPtr -> "Pointer"
    | T.Size -> "Size"
    | T.Bit -> "Bool"
    | T.Byte -> "Uint8"
    | T.Word -> "Uint16"
    | T.DWord -> "Uint32"
    | T.QWord -> "Uint64"
    | T.OWord -> "Uint128"
    | T.Bytes -> "Slice"
    | t ->
        Printf.sprintf2 "No module implementing %a"
          T.print t |>
        invalid_arg

  let rec num_name = function
    | T.Value { vtyp = Mac (U8 | I8 | U16 | I16 | U24 | I24 | U32 | I32 |
                            U40 | I40 | U48 | I48 | U56 | I56 | U64 | I64 |
                            U128 | I128 | Float) ; nullable = false }
    | T.(Byte | Word | DWord | QWord | OWord) as t ->
        String.lowercase (mod_name t)
    | T.Value { vtyp = Usr t ; nullable = false } ->
        num_name (Value { vtyp = t.def ; nullable = false })
    | t ->
        Printf.sprintf2 "num_name: Not an integer (%a)"
          T.print t |>
        invalid_arg

  (* Identifiers used for function parameters: *)
  let param fid n = "p_"^ string_of_int fid ^"_"^ string_of_int n

  let print_binding n tn f oc =
    pp oc "let %s : %s = %t in" n tn f

  let print_inline p t f oc =
    let tn = type_identifier p t in
    pp oc "(%t : %s)" f tn

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

  let rec print ?name emit p l e =
    let gen_sym ?name pref =
      match name with
      | Some n -> n
      | None -> U.gen_sym pref |> valid_identifier in
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
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
        P.indent_more p (fun () ->
          let n1 = print emit p l e1 in
          ppi oc "%s" n1
        ) ;
        ppi oc ") %s (" op ;
        P.indent_more p (fun () ->
          let n2 = print emit p l e2 in
          ppi oc "%s" n2
        ) ;
        pp oc "%s)" p.P.indent) in
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
    | E.E1S (Apply, f, es) ->
        let nf = print emit p l f in
        let ns =
          List.fold_left (fun ns e -> print emit p l e :: ns) [] es |>
          List.rev in
        emit ?name p l e (fun oc ->
          pp oc "%s%a"
            nf
            (List.print ~first:" " ~last:"" ~sep:" " String.print) ns)
    | E.E1 (Comment c, e1) ->
        ppi p.P.def "(* %s *)" c ;
        print ?name emit p l e1
    | E.E0S (Seq, es) ->
        List.fold_left (fun _ e -> print emit p l e) "()" es
    | E.E0S ((MakeVec | MakeLst _), es) ->
        let inits = List.map (print emit p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:"[| " ~last:" |]" ~sep:"; " String.print oc inits)
    | E.E0S (MakeTup, es) ->
        let inits = List.map (print emit p l) es in
        (* TODO: There is no good reason any longer to avoid using actual
         * OCaml tuples to represent tuples *)
        let m = module_of_type (E.type_of l e) in
        let i = ref 0 in
        emit ?name p l e (fun oc ->
          List.print ~first:(m ^".{ ") ~last:" }" ~sep:"; " (fun oc n ->
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
        let m = module_of_type (E.type_of l e) in
        emit ?name p l e (fun oc ->
          List.print ~first:(m ^".{ ") ~last:" }" ~sep:"; "
            (fun oc (name, n) ->
              Printf.fprintf oc "%s = %s" name n) oc inits)
    | E.E1 (Identity, e1) ->
        print ?name emit p l e1
    | E.E1 (Ignore, e1) ->
        let n = print emit p l e1 in
        pp p.P.def "%signore %s;\n" p.P.indent n ;
        "()"
    | E.E1 (Dump, e1) ->
        let n = print emit p l e1 in
        pp p.P.def ("%s"^^
          (match E.type_of l e1 |> T.develop_user_types with
          | Value { vtyp = Mac String ; nullable = false } ->
              "print_string %s;"
          | Value { vtyp = Mac Char ; nullable = false } ->
              "print_char %s;"
          | _ ->
              "print_string (Batteries.dump %s);") ^^"\n")
          p.P.indent n ;
        pp p.P.def "%sflush stdout ;" p.P.indent ;
        "()"
    | E.E1 (Debug, e1) ->
        print ?name emit p l (E1 ((if !E.dump_debug then Dump else Ignore), e1))
    | E.E1 (IsNull, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s = Null" n)
    | E.E2 (Nth, e1, e2) ->
        let n1 = print emit p l e1 in
        let n2 = print emit p l e2 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.(%s.to_int %s)" n2 m n1)
    | E.E1 (NotNull, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "NotNull %s" n1)
    | E.E1 (Force, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> Printf.fprintf oc "Nullable.get %s" n1)
    | E.E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "Null")
    | E.E0 (Float f) ->
        emit ?name p l e (print_float_literal f)
    | E.E0 Unit ->
        emit ?name p l e (fun oc -> pp oc "()")
    | E.E0 (String s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc s)
    | E.E0 (Bit b) | E.E0 (Bool b) ->
        emit ?name p l e (fun oc -> Bool.print oc b)
    | E.E0 (Char c) ->
        emit ?name p l e (fun oc -> pp oc "%C" c)
    | E.E0 (Byte i) | E.E0 (U8 i) ->
        emit ?name p l e (fun oc -> pp oc "Uint8.of_int (%s)" (Uint8.to_string i))
    | E.E0 (Word i) | E.E0 (U16 i) ->
        emit ?name p l e (fun oc -> pp oc "Uint16.of_int (%s)" (Uint16.to_string i))
    | E.E0 (U24 i) ->
        emit ?name p l e (fun oc -> pp oc "Uint24.of_int (%s)" (Uint24.to_string i))
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
    | E.E0 (Bytes s) ->
        emit ?name p l e (fun oc -> pp oc "Slice.of_string %S" (Bytes.to_string s))
    | E.E0 (I8 i) ->
        emit ?name p l e (fun oc -> pp oc "Int8.of_int (%s)" (Int8.to_string i))
    | E.E0 (I16 i) ->
        emit ?name p l e (fun oc -> pp oc "Int16.of_int (%s)" (Int16.to_string i))
    | E.E0 (I24 i) ->
        emit ?name p l e (fun oc -> pp oc "Int24.of_int (%s)" (Int24.to_string i))
    | E.E0 (I32 i) ->
        emit ?name p l e (fun oc -> pp oc "Int32.of_int32 (%ldl)" i)
    | E.E0 (I40 i) ->
        emit ?name p l e (fun oc -> pp oc "Int40.of_int64 (%sL)" (Int40.to_string i))
    | E.E0 (I48 i) ->
        emit ?name p l e (fun oc -> pp oc "Int48.of_int64 (%sL)" (Int48.to_string i))
    | E.E0 (I56 i) ->
        emit ?name p l e (fun oc -> pp oc "Int56.of_int64 (%sL)" (Int56.to_string i))
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
    | E.E2 ((Div | Rem as op), e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        (match E.type_of l e1 |> T.develop_user_types with
        | Value { vtyp = Mac (U8|U16|U24|U32|U40|U48|U56|U64|U128
                             |I8|I16|I24|I32|I40|I48|I56|I64|I128) ;
                   _ } as t ->
            let op_name = match op with Div -> "div" | _ -> "rem" in
            emit ?name p l e (fun oc ->
              pp oc "try NotNull (%s.%s %s %s) with Division_by_zero -> Null"
                (mod_name t) op_name n1 n2)
        | Value { vtyp = Mac Float ; _ } ->
            let op_name = match op with Div -> "(/.)" | _ -> "(mod)" in
            binary_op ("(Nullable.of_nan % "^ op_name ^")") e1 e2
        | _ ->
            assert false)
    | E.E2 (Pow, e1, e2) ->
        (match E.type_of l e1 |> T.develop_user_types with
        | Value { vtyp = Mac Float ; _ } ->
            (* TODO: if e2 is constant and > 0 then do away with the
             * Nullable.of_nan: *)
            binary_op "(Nullable.of_nan % ( ** ))" e1 e2
        | Value { vtyp = Mac (I32 | I64) ; _ } as t ->
            let n1 = print emit p l e1
            and n2 = print emit p l e2 in
            emit ?name p l e (fun oc ->
              pp oc "try NotNullable %s.pow %s %s with Invalid_arg _ -> Null"
                (mod_name t) n1 n2)
        | Value {
            vtyp = Mac (U8|U16|U24|U32|U40|U48|U56|U64|U128
                       |I8|I16|I24|I40|I48|I56|I128) ; _ } as t ->
            (* For through floats *)
            let m = mod_name t in
            let n1 = print emit p l e1
            and n2 = print emit p l e2 in
            emit ?name p l e (fun oc ->
              pp oc "Nullable.map %s.of_float \
                       (Nullable.of_nan (%s.to_float %s ** %s.to_float %s))"
                m m n1 m n2)
        | _ ->
            assert false (* because of type-checking *))
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
    | E.E1 (StringOfIp, e1) ->
        let n1 = print emit p l e1 in
        let case_u mn n =
          match T.develop_maybe_nullable mn with
          | T.{ vtyp = Mac U32 ; _ } ->
              ppi p.P.def "DessserIpTools.V4.to_string %s" n
          | T.{ vtyp = Mac U128 ; _ } ->
              ppi p.P.def "DessserIpTools.V6.to_string %s" n
          | _ ->
              assert false (* because of type checking *)
        in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_user_types with
          | Value { vtyp = Sum mns ; _ } ->
              (* Since the type checking accept any sum type made of u32 and
               * u128, let's be as general as possible: *)
              ppi oc "match %s with\n" n1 ;
              P.indent_more p (fun () ->
                Array.iter (fun (cstr, mn) ->
                  ppi oc "| %s ip_ ->\n" (cstr_name cstr) ;
                  P.indent_more p (fun () -> case_u mn "ip_")
                ) mns)
          | Value mn ->
              case_u mn n1
          | _ ->
              assert false (* because of type checking *))
    | E.E1 (CharOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s.[0]" n)
    | E.E1 (FloatOfString, e1) ->
        if debug then
          let n1 = print emit p l e1 in
          emit ?name p l e (fun oc ->
            pp oc
              "(try float_of_string %s \
                with Failure _ as e -> Printf.eprintf \"float_of_string %%S\\n\" %s ; raise e)" n1 n1)
        else
          unary_op "float_of_string" e1
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
    | E.E1 (CharOfPtr, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "Char.chr (Pointer.peekByte %s 0), Pointer.skip %s 1" n n)
    | E.E1 (FloatOfPtr, e1) ->
        let n1 = print emit p l e1 in
        (* Note: Scanf uses two distinct format specifiers for "normal"
         * and hex notations so detect it and pick the proper one: *)
        pp p.P.def "%slet len_ = %s.Pointer.stop - %s.start in\n" p.P.indent n1 n1 ;
        pp p.P.def "%slet is_hex_ = len_ > 2 && (\n" p.P.indent ;
        P.indent_more p (fun () ->
          pp p.P.def "%slet o_ =\n" p.P.indent ;
          P.indent_more p (fun () ->
            pp p.P.def "%slet c_ = Bytes.get %s.bytes %s.start in\n"
              p.P.indent n1 n1 ;
            pp p.P.def "%sif c_ = '-' || c_ = '+' then 1 else 0 in\n" p.P.indent) ;
          pp p.P.def "%slen_ > 2 + o_ && \
                      Bytes.get %s.bytes (%s.start + o_) = '0' && \
                      (let c2_ = Bytes.get %s.bytes (%s.start + o_ + 1) in \
                       c2_ = 'x' || c2_ = 'X')) in\n"
            p.P.indent n1 n1 n1 n1) ;
        pp p.P.def "%slet s_ =\n" p.P.indent ;
        P.indent_more p (fun () ->
          pp p.P.def "%slet off_ = ref %s.Pointer.start in\n" p.P.indent n1 ;
          pp p.P.def "%sScanf.Scanning.from_function (fun () ->\n" p.P.indent ;
          P.indent_more p (fun () ->
            pp p.P.def "%sif !off_ >= %s.Pointer.stop then raise End_of_file ;\n"
              p.P.indent n1 ;
            pp p.P.def "%slet c_ = Bytes.get %s.Pointer.bytes !off_ in\n"
              p.P.indent n1 ;
            pp p.P.def "%sincr off_ ;\n" p.P.indent ;
            pp p.P.def "%sc_) in\n" p.P.indent)) ;
        emit ?name p l e (fun oc ->
          pp oc "Scanf.bscanf s_ (if is_hex_ then \" %%h%%n\" else \
                                                  \" %%f%%n\") \
                   (fun f_ o_ -> f_, Pointer.skip %s o_)" n1)
    | E.E1 (U8OfPtr, e1)
    | E.E1 (I8OfPtr, e1)
    | E.E1 (U16OfPtr, e1)
    | E.E1 (I16OfPtr, e1)
    | E.E1 (U24OfPtr, e1)
    | E.E1 (I24OfPtr, e1)
    | E.E1 (U32OfPtr, e1)
    | E.E1 (I32OfPtr, e1)
    | E.E1 (U40OfPtr, e1)
    | E.E1 (I40OfPtr, e1)
    | E.E1 (U48OfPtr, e1)
    | E.E1 (I48OfPtr, e1)
    | E.E1 (U56OfPtr, e1)
    | E.E1 (I56OfPtr, e1)
    | E.E1 (U64OfPtr, e1)
    | E.E1 (I64OfPtr, e1)
    | E.E1 (U128OfPtr, e1)
    | E.E1 (I128OfPtr, e1) ->
        let n1 = print emit p l e1 in
        let m = mod_name (E.type_of l e |> T.pair_of_tpair |> fst) in
        emit ?name p l e (fun oc ->
          P.indent_more p (fun () ->
            pp oc "\n%slet s_ = Bytes.unsafe_to_string %s.Pointer.bytes in\n"
              p.P.indent n1 ;
            pp oc "%slet n_, o_ = %s.of_substring ~pos:(%s.Pointer.start) s_ in\n"
              p.P.indent m n1 ;
            pp oc "%sn_, Pointer.skip %s (o_ - %s.start)" p.P.indent n1 n1))
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
    | E.E1 (SetOfSList, e1) ->
        unary_op "make_simple_set_of_slist" e1
    | E.E1 (ToU8, e1) ->
        let op = "Uint8.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI8, e1) ->
        let op = "Int8.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU16, e1) ->
        let op = "Uint16.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI16, e1) ->
        let op = "Int16.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU24, e1) ->
        let op = "Uint24.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI24, e1) ->
        let op = "Int24.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU32, e1) ->
        let op = "Uint32.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI32, e1) ->
        let op = "Int32.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU40, e1) ->
        let op = "Uint40.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI40, e1) ->
        let op = "Int40.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU48, e1) ->
        let op = "Uint48.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI48, e1) ->
        let op = "Int48.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU56, e1) ->
        let op = "Uint56.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI56, e1) ->
        let op = "Int56.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU64, e1) ->
        let op = "Uint64.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI64, e1) ->
        let op = "Int64.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToU128, e1) ->
        let op = "Uint128.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToI128, e1) ->
        let op = "Int128.of_"^ num_name (E.type_of l e1) in
        unary_op op e1
    | E.E1 (ToFloat, e1) ->
        let m = mod_name (E.type_of l e1) in
        if m = "Float" then print ?name emit p l e1
        else unary_op (m ^".to_float") e1
    | E.E1 (U8OfBool, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "if %s then Uint8.one else Uint8.zero" n)
    | E.E1 (BoolOfU8, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "Uint8.compare Uint8.zero %s <> 0" n)
    | E.E2 (AppendByte, e1, e2) ->
        binary_op "Slice.add" e1 e2
    | E.E2 (AppendBytes, e1, e2) ->
        binary_op "Slice.append" e1 e2
    | E.E2 (AppendString, e1, e2) ->
        binary_infix_op e1 "^" e2
    | E.E2 (StartsWith, e1, e2) ->
        binary_op "BatString.starts_with" e1 e2
    | E.E2 (EndsWith, e1, e2) ->
        binary_op "BatString.ends_with" e1 e2
    | E.E1 (StringLength, e1) ->
        unary_op "Uint32.of_int @@ String.length" e1
    | E.E1 (StringOfBytes, e1) ->
        unary_op "Slice.to_string" e1
    | E.E1 (BytesOfString, e1) ->
        unary_op "Slice.of_string" e1
    | E.E1 (Cardinality, e1) ->
        (match E.type_of l e1 |> T.develop_user_types with
        | Value { vtyp = Vec (d, _) ; _ } ->
            string_of_int d
        | Value { vtyp = Lst _ ; _ } ->
            unary_op "Uint32.of_int @@ Array.length" e1
        | Value { vtyp = Set _ ; _ } ->
            let n1 = print emit p l e1 in
            emit ?name p l e (fun oc -> pp oc "%s.cardinality ()" n1)
        | _ ->
            assert false (* Because type checking *))
    | E.E0 (DataPtrOfString s) ->
        emit ?name p l e (fun oc -> pp oc "Pointer.of_string %S" s)
    | E.E1 (DataPtrOfBuffer, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "Pointer.of_buffer %s" n1)
    | E.E1 (GetEnv, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "try NotNull (Sys.getenv_opt %s) with Not_found -> Null" n1)
    | E.E3 (DataPtrOfPtr, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc -> pp oc "Pointer.of_pointer %s %s %s" n1 n2 n3)
    | E.E2 (GetBit, e1, e2) ->
        binary_op "Pointer.getBit" e1 e2
    | E.E2 (GetVec, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and m = mod_name (E.type_of l e2) in
        emit ?name p l e (fun oc -> pp oc "%s.(%s.to_int %s)" n1 m n2)
    | E.E3 (SetBit, e1, e2, e3) ->
        let ptr = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc -> pp oc "Pointer.setBit %s %s %s" ptr n2 n3)
    | E.E3 (SetVec, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3
        and m = mod_name (E.type_of l e2) in
        emit ?name p l e (fun oc -> pp oc "%s.(%s.to_int %s) <- %s" n1 m n2 n3)
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
        ppi p.P.def "Pointer.pokeByte %s %s;" ptr v ;
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
    | E.E1 (Abs, e1) ->
        unary_mod_op "abs" e1
    | E.E1 (Neg, e1) ->
        unary_mod_op "neg" e1
    | E.E1 (Exp, e1) ->
        unary_op "exp" e1
    | E.E1 (Log, e1) ->
        unary_op "(Nullable.of_nan % log)" e1
    | E.E1 (Log10, e1) ->
        unary_op "(Nullable.of_nan % log10)" e1
    | E.E1 (Sqrt, e1) ->
        unary_op "(Nullable.of_nan % sqrt)" e1
    | E.E1 (Ceil, e1) ->
        unary_op "ceil" e1
    | E.E1 (Floor, e1) ->
        unary_op "floor" e1
    | E.E1 (Round, e1) ->
        unary_op "Float.round" e1
    | E.E1 (Cos, e1) ->
        unary_op "cos" e1
    | E.E1 (Sin, e1) ->
        unary_op "sin" e1
    | E.E1 (Tan, e1) ->
        unary_op "(Nullable.of_nan tan)" e1
    | E.E1 (ACos, e1) ->
        unary_op "(Nullable.of_nan acos)" e1
    | E.E1 (ASin, e1) ->
        unary_op "(Nullable.of_nan asin)" e1
    | E.E1 (ATan, e1) ->
        unary_op "atan" e1
    | E.E1 (CosH, e1) ->
        unary_op "cosh" e1
    | E.E1 (SinH, e1) ->
        unary_op "sinh" e1
    | E.E1 (TanH, e1) ->
        unary_op "tanh" e1
    | E.E1 (Lower, e1) ->
        (* FIXME: UTF-8 + ICU like library *)
        unary_op "String.lowercase_ascii" e1
    | E.E1 (Upper, e1) ->
        unary_op "String.uppercase_ascii" e1
    | E.E1 (Hash, e1) ->
        unary_op "(Uint64.of_int % Hashtbl.hash)" e1
    | E.E2 (Cons, e1, e2) ->
        binary_infix_op e1 "::" e2
    | E.E0 (EndOfList _) ->
        emit ?name p l e (fun oc -> pp oc "[]")
    | E.E0 (EmptySet _) ->
        emit ?name p l e (fun oc -> pp oc "make_simple_set ()")
    | E.E0 Now ->
        emit ?name p l e (fun oc -> pp oc "Unix.gettimeofday ()")
    | E.E0 Random ->
        emit ?name p l e (fun oc -> pp oc "Random.float 1.")
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
        and n2 = print emit p l e2 (* the function of 2 args *) in
        let n1_0 = "(fst "^ n1 ^")"
        and n1_1 = "(snd "^ n1 ^")" in
        emit ?name p l e (fun oc -> pp oc "%s %s %s" n2 n1_0 n1_1)
    | E.E2 (Map, e1, e2) ->
        let n1 = print emit p l e1 (* the iterable *)
        and n2 = print emit p l e2 (* the function of 1 arg *) in
        let t1 = E.type_of l e1 in
        emit ?name p l e (fun oc ->
          let mod_name =
            match t1 with
            | T.Value { vtyp = (Vec _ | Lst _) ; _ } -> "Array"
            | T.Value { vtyp = Set _ ; _ } -> todo "map on sets"
            | T.SList _ -> "List"
            | _ -> assert false (* because of E.type_check *) in
          pp oc "%s.map %s %s" mod_name n2 n1)
    | E.E2 (Min, e1, e2) ->
        binary_op "min" e1 e2
    | E.E2 (Max, e1, e2) ->
        binary_op "max" e1 e2
    | E.E2 (Member, e1, e2) ->
        let n1 = print emit p l e1 in
        (* If there are plenty of constants it is worth it to build a constant
         * hashtable with all of them: *)
        (* TODO: actually, should count as constant any expression that can be
         * computed from constants or other expressions in that set *)
        let rec split_csts csts non_csts = function
          | [] -> csts, non_csts
          | e :: es ->
              let csts, non_csts =
                if E.can_precompute [] [] e then
                  (e :: csts), non_csts
                else
                  csts, (e :: non_csts) in
              split_csts csts non_csts es in
        (* A word about nullability of the elements of the vector:
         * We are going to emit code such as "A=x1||A=x2" in lieu of
         * "A IN [x1; x2]". Notice that nulls do not propagate from the xs in
         * case A is found in the set, but do if it is not; Indeed, "1 IN
         * [1;NULL]" is true but "2 IN [1;NULL]" is NULL. If A is NULL though,
         * then the result is NULL unless the set is empty: "NULL in [1; 2]" is
         * NULL, but "NULL in []" is false. *)
        (match e2 with
        | E0S ((MakeVec | MakeLst _), []) ->
            "false"
        | E0S ((MakeVec | MakeLst _), es) ->
            let csts, non_csts = split_csts [] [] es in
            (* Given a list of constant expressions, build a function that check
             * membership in this set: *)
            let check_csts_n =
              (* Temporarily, enter new definitions at the top-level: *)
              P.new_top_level p (fun p ->
                let check_csts_n = gen_sym "check_csts_" in
                pp p.P.def "let %s =" check_csts_n ;
                P.indent_more p (fun () ->
                  let ns =
                    List.map (fun e ->
                      print emit p [] e
                    ) csts in
                  ppi p.P.def "fun x_ ->" ;
                  P.indent_more p (fun () ->
                    if csts = [] then (
                      String.print p.P.def "false"
                    ) else if List.length csts < 6 (* guessed *) then (
                      List.print ~first:"" ~last:"" ~sep:" || "
                        (fun oc n -> Printf.fprintf oc "x_ = %s" n) p.P.def ns
                    ) else (
                      (* TODO: if the csts are nullable filter out the Null and
                       * make this a list of non nullable values: *)
                      ppi p.P.def "let h_ = Hashtbl.of_list %a in"
                        (List.print String.print) ns ;
                      ppi p.P.def "Hashtbl.mem h_ x_"
                    )) ;
                  check_csts_n)) in
            emit ?name p l e (fun oc ->
              if non_csts = [] then (
                pp oc "%s x_" check_csts_n
              ) else (
                (* FIXME: nullability of non_csts *)
                let ns = List.map (print emit p l) non_csts in
                pp oc "let x_ = %s in" n1 ;
                pp oc "%s x_ || %a"
                  check_csts_n
                  (List.print ~first:"" ~last:"" ~sep:" || "
                     (fun oc n -> Printf.fprintf oc "x_ = %s" n)) ns
              ))
        | _ -> assert false (* because of type-checking *))
    | E.E0 (Identifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            valid_identifier s)
    | E.E0 (ExtIdentifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            s)
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
          P.indent_more p (fun () ->
            let n = print emit p l e1 in
            pp oc "%s%s)" p.P.indent n))
    | E.E1 (Function (fid, ts), e1) ->
        emit ?name p l e (fun oc ->
          array_print_i ~first:"(fun " ~last:" ->\n" ~sep:" "
            (fun i oc _t -> String.print oc (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (E.E0 (Param (fid, i)), t) :: l
            ) l ts in
          P.indent_more p (fun () ->
            let n = print emit p l e1 in
            pp oc "%s%s)" p.P.indent n))
    | E.E0 (Param (fid, n)) ->
        param fid n
    | E.E3 (If, e1, e2, e3) ->
        let cond = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "if %s then (" cond ;
            P.indent_more p (fun () ->
              let n = print emit p l e2 in
              ppi oc "%s" n) ;
            ppi oc ") else (" ;
            P.indent_more p (fun () ->
              let n = print emit p l e3 in
              ppi oc "%s" n) ;
            pp oc "%s)" p.P.indent))
    | E.E4 (ReadWhile, e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr = print emit p l e4 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "let rec read_while_loop accum ptr =" ;
            P.indent_more p (fun () ->
              ppi oc "if Pointer.remSize ptr <= 0 then (accum, ptr) else" ;
              ppi oc "let next_byte = Pointer.peekByte ptr 0 in" ;
              ppi oc "if not (%s next_byte) then (accum, ptr) else" cond ;
              ppi oc "let accum = %s accum next_byte in" reduce ;
              ppi oc "let ptr = Pointer.skip ptr 1 in" ;
              ppi oc "read_while_loop accum ptr in") ;
            pp oc "%sread_while_loop %s %s" p.P.indent accum ptr))
    | E.E3 (LoopWhile, e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "let rec loop_while accum =" ;
            P.indent_more p (fun () ->
              ppi oc "if not (%s accum) then accum else" cond ;
              ppi oc "loop_while (%s accum) in" body) ;
            ppi oc "loop_while %s" accum))
    | E.E3 (LoopUntil, e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "let rec loop_until accum =" ;
            P.indent_more p (fun () ->
              ppi oc "let accum = %s accum in" body ;
              ppi oc "if %s accum then loop_until accum else accum in" cond) ;
            pp oc "%sloop_until %s" p.P.indent accum))
    | E.E3 (Fold, e1, e2, e3) ->
        let init = print emit p l e1
        and body = print emit p l e2
        and lst = print emit p l e3 in
        (match E.type_of l e3 |> T.develop_user_types with
        | Value { vtyp = (Vec _ | Lst _) ; _ } ->
            (* Both lists and vectors are represented by arrays so
             * Array.fold_left will do in both cases: *)
            emit ?name p l e (fun oc ->
              pp oc "Array.fold_left %s %s %s" body init lst)
        | Value { vtyp = Set _ ; _ } ->
            emit ?name p l e (fun oc ->
              pp oc "%s.fold %s %s" lst init body)
        | _ ->
            assert false (* Because type checking *))
    | E.E4 (Repeat, e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "let rec loop_repeat n accum =" ;
            P.indent_more p (fun () ->
              ppi oc "if n >= %s then accum else" to_ ;
              ppi oc "loop_repeat (Int32.succ n) (%s n accum) in" body) ;
            pp oc "%sloop_repeat %s %s" p.P.indent from accum))
    | E.E1 (GetItem n, e1) ->
        let n1 = print emit p l e1 in
(*      TODO: For when tuples are actual tuples:
        let res = gen_sym ?name "get_item_" in
        let max_n =
          match E.type_of l e1 |> T.develop_user_types with
          | Value { vtyp = Tup mns ; nullable = false } -> Array.length mns
          | _ -> assert false in
        ppi p.P.def "let %t = %s\n"
          (fun oc ->
            for i = 0 to max_n - 1 do
              if i > 0 then String.print oc ", " ;
              String.print oc (if i = n then res else "_")
            done) *)
        let m = module_of_type (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s.%s" n1 m (tuple_field_name n))
    | E.E1 (GetField s, e1) ->
        let n1 = print emit p l e1 in
        let m = module_of_type (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s.%s" n1 m s)
    | E.E1 (GetAlt s, e1) ->
        let n1 = print emit p l e1 in
        let m = module_of_type (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          let cstr = cstr_name s in
          (* FIXME: figure out where to add this whether the expression is a
           * binding or inlined: [@@ocaml.warning "-8"] *)
          Printf.fprintf oc "(match %s with %s.%s x -> x)"
            n1 m cstr)
    | E.E1 (Construct (mns, i), e1) ->
        let n1 = print emit p l e1 in
        assert (i < Array.length mns) ;
        let cstr = cstr_name (fst mns.(i)) in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "(%s %s)" cstr n1)
    | E.E1 (Assert, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "assert %s" n)
    | E.E1 (MaskGet i, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "mask_get %s %d" n1 i)
    | E.E1 (LabelOf, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          let t1 = E.type_of l e1 in
          let m = T.uniq_id t1 |> valid_module_name in
          pp oc "%s.label_of_cstr %s" m n1)
    | E.E0 CopyField ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.Copy")
    | E.E0 SkipField ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.Skip")
    | E.E0 SetFieldNull ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.SetNull")
    | E.E1 (SlidingWindow t, e1) ->
        let n1 = print emit p l e1
        and def = print emit p l (E.default_value t)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "make_sliding_window %s (%s.to_int %s)" def m n1)
    | E.E1 (TumblingWindow t, e1) ->
        let n1 = print emit p l e1
        and def = print emit p l (E.default_value t)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "make_tumbling_window %s (%s.to_int %s)" def m n1)
    | E.E1 (Sampling t, e1) ->
        let n1 = print emit p l e1
        and def = print emit p l (E.default_value t)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "make_sampling %s (%s.to_int %s)" def m n1)
    | E.E2 (Insert, e1, e2) ->
        let set = print emit p l e1
        and item = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s.insert %s" set item)
    | E.E2 (SplitBy, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "String.split_on_string %s %s" n1 n2)
    | E.E2 (SplitAt, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "" ;
          pp oc "{ %s = String.sub %s 0 %s ;"
            (tuple_field_name 0) n2 n1 ;
          pp oc "  %s = String.sub %s %s (String.length %s - %s) }"
            (tuple_field_name 1) n2 n1 n2 n1)
    | E.E2 (Join, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          (* TODO: faster impl with a single string alloc: *)
          pp oc "String.join %s (Array.to_list %s)" n1 n2)
    | E.E3 (FindSubstring, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "try NotNull ((if %s then String.find else String.rfind) %s %s)"
            n1 n3 n2 ;
          pp oc "with Not_found -> Null")

  let print_binding_toplevel emit n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.P.def "%slet %s : %s =\n" p.P.indent n tn ;
    P.indent_more p (fun () ->
      let n = print emit p l e in
      pp p.P.def "%s%s\n" p.P.indent n)

  let print_identifier_declaration n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.P.def "%sval %s : %s\n" p.P.indent n tn

  let source_intro =
    "open Stdint\n\
     open DessserOCamlBackEndHelpers\n\
     \n\
     module DessserGen = struct\n"

  let source_outro =
    "end\n"
end

include DessserBackEndCLike.Make (Config)
