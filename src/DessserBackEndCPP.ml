open Batteries
open Stdint
open Dessser
open DessserBackEndCLike
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module P = DessserPrinter

type T.backend_id += Cpp

let cpp_std_version = 17

module Config =
struct
  let id = Cpp
  let valid_identifier = DessserBackEndCLike.valid_identifier
  let valid_source_name n = n

  let preferred_def_extension = "cc"

  let preferred_decl_extension = "h"

  let preferred_comp_extension = function
    | Object -> "o"
    | SharedObject -> "so"
    | Executable -> ""

  let compile_cmd ?(dev_mode=false) ?(extra_search_paths=[]) ~optim ~link src dst =
    let optim = cap 0 3 optim in
    Printf.sprintf2
      "g++ -std=c++%d -g -O%d -W -Wall \
           -Wno-unused-parameter -Wno-unused-variable \
           -Wno-shift-negative-value %a %s %s %S -o %S"
      cpp_std_version
      optim
      (List.print ~first:"" ~last:"" ~sep:" " (fun oc path ->
        Printf.fprintf oc "-I %s" path)) extra_search_paths
      (if dev_mode then "-I src" else "")
      (match link with
      | Object -> "-c"
      | SharedObject -> "-fPIC -c"
      | Executable -> "")
      src dst

  let tuple_field_name i = "field_"^ string_of_int i

  let is_mutable t =
    match T.develop_user_types t with
    | T.Bytes
    | T.Value { vtyp = Vec _ ; _ } ->
        true
    | _ ->
        false

  (* Some types are hidden behind a pointer (for instance because we want
   * to use inheritance) *)
  let is_pointy t =
    match T.develop_user_types t with
    | T.Value { vtyp = Set _ ; _ } ->
        true
    | _ ->
        false

  let rec print_struct p oc id mns =
    let id = valid_identifier id in
    pp oc "%sstruct %s {\n" p.P.indent id ;
    P.indent_more p (fun () ->
      Array.iter (fun (field_name, vt) ->
        let typ_id = type_identifier p (T.Value vt) in
        pp oc "%s%s %s;\n" p.P.indent typ_id (valid_identifier field_name)
      ) mns ;
      if cpp_std_version >= 20 then
        pp oc "%sbool operator==(%s const &) const & = default;\n" p.P.indent id
      else (
        pp oc "%sbool operator==(%s const &other) const {\n" p.P.indent id ;
        P.indent_more p (fun () ->
          pp oc "%sreturn %a;\n"
            p.P.indent
            (Array.print ~first:"" ~last:"" ~sep:" && "
              (fun oc (field_name, _) ->
                let id = valid_identifier field_name in
                Printf.fprintf oc "%s == other.%s" id id)) mns) ;
        pp oc "%s}\n" p.P.indent
      )) ;
    pp oc "%s};\n\n" p.P.indent

  and print_variant p oc id mns =
    let id = valid_identifier id in
    pp oc "%stypedef std::variant<\n" p.P.indent ;
    P.indent_more p (fun () ->
      Array.iteri (fun i (_, mn) ->
        let typ_id = type_identifier p (T.Value mn) in
        pp oc "%s%s%s\n"
          p.P.indent typ_id (if i < Array.length mns - 1 then "," else "")
      ) mns
    ) ;
    pp oc "%s> %s;\n\n" p.P.indent id

  and type_identifier p = function
    | T.Value { vtyp ; nullable = true } ->
        "std::optional<"^
          type_identifier p (Value { vtyp ; nullable = false })
        ^">"
    | T.Value { vtyp = Unknown ; _ } -> invalid_arg "type_identifier"
    | T.Value { vtyp = Unit ; _ } -> "Unit"
    | T.Value { vtyp = Mac Float ; _ } -> "double"
    | T.Value { vtyp = Mac String ; _ } -> "std::string"
    | T.Value { vtyp = Mac Bool ; _ } -> "bool"
    | T.Value { vtyp = Mac Char ; _ } -> "char"
    | T.Value { vtyp = Mac I8 ; _ } -> "int8_t"
    | T.Value { vtyp = Mac U8 ; _ } -> "uint8_t"
    | T.Value { vtyp = Mac I16 ; _ } -> "int16_t"
    | T.Value { vtyp = Mac U16 ; _ } -> "uint16_t"
    | T.Value { vtyp = Mac I24 ; _ } -> "int32_t"
    | T.Value { vtyp = Mac U24 ; _ } -> "uint32_t"
    | T.Value { vtyp = Mac I32 ; _ } -> "int32_t"
    | T.Value { vtyp = Mac U32 ; _ } -> "uint32_t"
    | T.Value { vtyp = Mac I40 ; _ } -> "int64_t"
    | T.Value { vtyp = Mac U40 ; _ } -> "uint64_t"
    | T.Value { vtyp = Mac I48 ; _ } -> "int64_t"
    | T.Value { vtyp = Mac U48 ; _ } -> "uint64_t"
    | T.Value { vtyp = Mac I56 ; _ } -> "int64_t"
    | T.Value { vtyp = Mac U56 ; _ } -> "uint64_t"
    | T.Value { vtyp = Mac I64 ; _ } -> "int64_t"
    | T.Value { vtyp = Mac U64 ; _ } -> "uint64_t"
    | T.Value { vtyp = Mac I128 ; _ } -> "int128_t"
    | T.Value { vtyp = Mac U128 ; _ } -> "uint128_t"
    | T.Value { vtyp = Usr t ; _ } ->
        type_identifier p (Value { vtyp = t.def ; nullable = false })
    | T.Value { vtyp = Ext n ; _ } ->
        P.get_external_type p n Cpp
    | T.Value { vtyp = Tup mns ; _ } as t ->
        let mns = Array.mapi (fun i vt -> tuple_field_name i, vt) mns in
        P.declared_type p t (fun oc type_id -> print_struct p oc type_id mns) |>
        valid_identifier
    | T.Value { vtyp = Rec mns ; _ } as t ->
        P.declared_type p t (fun oc type_id -> print_struct p oc type_id mns) |>
        valid_identifier
    | T.Value { vtyp = Sum mns ; _ } as t ->
        P.declared_type p t (fun oc type_id -> print_variant p oc type_id mns) |>
        valid_identifier
    | T.Value { vtyp = Vec (dim, typ) ; _ } ->
        Printf.sprintf "Vec<%d, %s>" dim (type_identifier p (Value typ))
    | T.Value { vtyp = Lst typ ; _ } ->
        Printf.sprintf "Lst<%s>" (type_identifier p (Value typ))
    | T.Value { vtyp = Set typ ; _ } ->
        Printf.sprintf "Set<%s> *" (type_identifier p (Value typ))
    | T.Value { vtyp = Map _ ; _ } ->
        assert false (* No value of map type *)
    | T.Pair (t1, t2) ->
        "Pair<"^ type_identifier p t1 ^", "^ type_identifier p t2 ^">"
    | T.SList t1 ->
        "SList<"^ type_identifier p t1 ^">"
    | T.Function (args, ret) ->
        (* We want all modifiable types (ir bytes, vectors, ...?) passed by
         * reference: *)
        "std::function<"^ type_identifier p ret ^
          IO.to_string (
            Array.print ~first:"(" ~last:")" ~sep:"," (fun oc t ->
              Printf.fprintf oc "%s%s"
                (type_identifier p t)
                (if is_mutable t then "&" else ""))
          ) args ^">"
    | T.Void -> "void"
    | T.DataPtr -> "Pointer"
    | T.Size -> "Size"
    | T.Bit -> "bool"
    | T.Byte -> "uint8_t"
    | T.Word -> "uint16_t"
    | T.DWord -> "uint32_t"
    | T.QWord -> "uint64_t"
    | T.OWord -> "uint128_t"
    | T.Bytes -> "Bytes"
    | T.Mask -> "Mask"

  (* Identifiers used for function parameters: *)
  let param fid n = "p_"^ string_of_int fid ^"_"^ string_of_int n

  let print_binding n tn f oc =
    if tn = "void" then (
      (* Variables of type void are invalid: *)
      pp oc "%t;" f
    ) else (
      (* Beware that this must not be parsed as a function declaration. Thus
       * the use of the "uniform initialization" syntax. But then a new issue
       * arises since when the function [f] will emit an immediate structure
       * the presence of two curly braces will cause a syntax error.
       * Work around: check that f's output does not start with a curly brace.
       * Oh boy! *)
      pp oc "%s %s { %t };" tn n f
    )

  let print_cast p t f oc =
    let tn = type_identifier p t in
    if is_pointy t then
      (* Outer parenth required since a following "->" would apply first *)
      pp oc "((%s)(%t))" tn f
    else
      pp oc "%s(%t)" tn f

  let print_inline = print_cast

  let print_comment oc fmt =
    pp oc ("/* "^^ fmt ^^" */\n")

  let print_float_literal v oc =
    if v = infinity then
      String.print oc "std::numeric_limits<double>::infinity"
    else if v = neg_infinity then
      String.print oc "-std::numeric_limits<double>::infinity"
    else
      Legacy.Printf.sprintf "%h" v |> String.print oc

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

  (* Print the code for returning the value [n] of expression [e].
   * This is merely `return n;` unless e has type void: *)
  let print_return n p l e =
    match E.type_of l e with
    | T.Void ->
        (* Then we could not have defined n, let's just return: *)
        pp p.P.def "%sreturn;\n" p.P.indent
    | _ ->
        pp p.P.def "%sreturn %s;\n" p.P.indent n

  let rec print ?name emit p l e =
    let gen_sym ?name pref =
      match name with
      | Some n -> n
      | None -> U.gen_sym pref |> valid_identifier in
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    let unary_op op e1 =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s %s" op n1) in
    let unary_func f e1 =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s(%s)" f n1) in
    let binary_infix_op e1 op e2 =
      let n1 = print emit p l e1
      and n2 = print emit p l e2 in
      let t = E.type_of l e in
      emit ?name p l e (fun oc ->
        (* Prevent integer promotion by casting to type_of e: *)
        print_cast p t (fun oc -> pp oc "%s %s %s" n1 op n2) oc) in
    let shortcutting_binary_infix_op e1 e2 short_cond_on_e1 =
      let n1 = print emit p l e1 in
      let res = gen_sym ?name "shortcut_res_" in
      let t1 = E.type_of l e1 in
      ppi p.P.def "%s %s;" (type_identifier p t1) res ;
      ppi p.P.def "if (%s == %b) {" n1 short_cond_on_e1 ;
      P.indent_more p (fun () ->
        ppi p.P.def "%s = %s;" res n1) ;
      ppi p.P.def "} else {" ;
      P.indent_more p (fun () ->
        let n2 = print emit p l e2 in
        ppi p.P.def "%s = %s;" res n2) ;
      ppi p.P.def "}" ;
      res in
    let method_call e1 m args =
      let deref_with =
        if is_pointy (E.type_of l e1 |> T.develop_user_types) then "->"
                                                              else "." in
      let n1 = print emit p l e1
      and ns = List.map (print emit p l) args in
      emit ?name p l e (fun oc ->
        pp oc "%s%s%s%a" n1 deref_with m
          (List.print ~first:"(" ~last:")" ~sep:", " String.print) ns) in
    let member e1 m =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s.%s" n1 m) in
    let null_of_nan res =
      ppi p.P.def "if (std::isnan(*%s)) %s.reset();" res res ;
      res in
    (* Convert from string to nullable number: *)
    let of_string e1 prefix cpp_op =
      let n1 = print emit p l e1 in
      let res = gen_sym ?name (prefix ^"_res_")
      and pos = gen_sym (prefix ^"_pos_") in
      let t = E.type_of l e in
      assert (T.is_nullable t) ; (* tn must be an optional<...> *)
      let tn = type_identifier p (T.force t) in
      ppi p.P.def "std::optional<%s> %s;" tn res ;
      ppi p.P.def "std::size_t %s;" pos ;
      ppi p.P.def "try {" ;
      ppi p.P.def "  %s const v_ { std::%s(%s, &%s) };" tn cpp_op n1 pos ;
      ppi p.P.def "  if (%s == %s.length()) %s = v_;" pos n1 res ;
      ppi p.P.def "} catch (const std::exception&) {}" ;
      res
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
            (List.print ~first:"(" ~last:")" ~sep:", " String.print) ns)
    | E.E1 (Comment c, e1) ->
        ppi p.P.def "/* %s */" c ;
        print ?name emit p l e1
    | E.E0S (Seq, es) ->
        List.fold_left (fun _ e -> print emit p l e) "" es
    | E.E0S ((MakeVec | MakeLst _ | MakeTup), es) ->
        let inits = List.map (print emit p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:" " ~last:" " ~sep:", " String.print oc inits)
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
          List.print ~first:" " ~last:" " ~sep:", "
            (fun oc (name, n) ->
              Printf.fprintf oc ".%s = %s" name n) oc inits)
    | E.E1 (Identity, e1) ->
        print ?name emit p l e1
    | E.E1 (Ignore, e1) ->
        let n = print emit p l e1 in
        ppi p.P.def "(void)%s;" n ;
        ""
    | E.E1 (Dump, e1) ->
        let n = print emit p l e1 in
        ppi p.P.def "std::cout << %s;" n ;
        ""
    | E.E1 (IsNull, e1) ->
        if E.is_const_null e1 then
          (* Cannot call has_value on nullopt: *)
          emit ?name p l e (fun oc -> pp oc "true")
        else
          let n = print emit p l e1 in
          emit ?name p l e (fun oc -> pp oc "!(%s.has_value ())" n)
    | E.E2 (Nth, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s[%s]" n2 n1)
    | E.E1 (NotNull, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E.E1 (Force what, e1) ->
        let n1 = print emit p l e1 in
        if what <> "" then ppi p.P.def "/* Force: %s */" what ;
        emit ?name p l e (fun oc -> Printf.fprintf oc "%s.value()" n1)
    | E.E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "std::nullopt")
    | E.E0 Unit ->
        emit ?name p l e ignore
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
    | E.E2 ((Div | Rem as op), e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        let op_name = match op with Div -> "/" | _ -> "%" in
        (match E.type_of l e1 |> T.develop_user_types with
        | Value { vtyp = Mac (U8|U16|U24|U32|U40|U48|U56|U64|U128
                             |I8|I16|I24|I32|I40|I48|I56|I64|I128) ;
                   _ } ->
            emit ?name p l e (fun oc ->
              pp oc "%s == 0 ? std::nullopt : std::make_optional(%s %s %s)"
                n2 n1 op_name n2)
        | Value { vtyp = Mac Float ; _ } ->
            binary_infix_op e1 op_name e2 |> null_of_nan
        | _ ->
            assert false)
    | E.E2 (Pow, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        let t = E.type_of l e in
        emit ?name p l e (fun oc ->
          print_cast p t (fun oc ->
            pp oc "std::pow(%s, %s)" n1 n2) oc) |> null_of_nan
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
        (match E.type_of l e1 |> T.develop_user_types with
        | Value { vtyp = Mac U128 ; _ } ->
            emit ?name p l e (fun oc -> pp oc "string_of_u128(%s)" n1)
        | Value { vtyp = Mac I128 ; _ } ->
            emit ?name p l e (fun oc -> pp oc "string_of_i128(%s)" n1)
        | _ ->
            emit ?name p l e (fun oc -> pp oc "std::to_string(%s)" n1))
    | E.E1 (StringOfIp, e1) ->
        let n1 = print emit p l e1 in
        let str = gen_sym ?name "str_" in
        let ip = gen_sym ?name "ip_" in
        ppi p.P.def "char %s[INET6_ADDRSTRLEN];\n" str ;
        let case_u mn n =
          match T.develop_maybe_nullable mn with
          | T.{ vtyp = Mac U32 ; _ } ->
              (* Make sure we can take the address of that thing: *)
              ppi p.P.def "const uint32_t %s { %s };\n" ip n ;
              ppi p.P.def
                "inet_ntop(AF_INET, &%s, %s, sizeof(%s));\n" ip str str ;
          | T.{ vtyp = Mac U128 ; _ } ->
              ppi p.P.def "const uint128_t %s{ %s };\n" ip n ;
              ppi p.P.def
                "inet_ntop(AF_INET6, &%s, %s, sizeof(%s));\n" ip str str ;
          | _ ->
              assert false (* because of type checking *)
        in
        (match E.type_of l e1 |> T.develop_user_types with
        | Value { vtyp = Sum mns ; _ } ->
            (* Since the type checking accept any sum type made of u32 and
             * u128, let's be as general as possible: *)
            ppi p.P.def "switch (%s.index()) {\n" n1 ;
            P.indent_more p (fun () ->
              Array.iteri (fun i (cstr, mn) ->
                ppi p.P.def "case %d: { /* %s */\n" i cstr ;
                P.indent_more p (fun () ->
                  let n = Printf.sprintf "std::get<%d>(%s)" i n1 in
                  case_u mn n ;
                  ppi p.P.def "break; }\n")
              ) mns) ;
            ppi p.P.def "}"
        | Value mn ->
            case_u mn n1
        | _ ->
            assert false (* because of type checking *)) ;
        emit ?name p l e (fun oc -> pp oc "%s" str)
    | E.E1 (CharOfString, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s[0]" n)
    | E.E1 (FloatOfString, e1) ->
        of_string e1 "float_of_string" "stod"
    | E.E1 (U8OfString, e1) ->
        of_string e1 "u8_of_string" "stoul"
    | E.E1 (U16OfString, e1) ->
        of_string e1 "u16_of_string" "stoul"
    | E.E1 (U24OfString, e1) ->
        of_string e1 "u24_of_string" "stoul"
    | E.E1 (U32OfString, e1) ->
        of_string e1 "u32_of_string" "stoul"
    | E.E1 (U40OfString, e1) ->
        of_string e1 "u40_of_string" "stoull"
    | E.E1 (U48OfString, e1) ->
        of_string e1 "u48_of_string" "stoull"
    | E.E1 (U56OfString, e1) ->
        of_string e1 "u56_of_string" "stoull"
    | E.E1 (U64OfString, e1) ->
        of_string e1 "u64_of_string" "stoull"
    | E.E1 (I8OfString, e1) ->
        of_string e1 "i8_of_string" "stol"
    | E.E1 (I16OfString, e1) ->
        of_string e1 "i16_of_string" "stol"
    | E.E1 (I24OfString, e1) ->
        of_string e1 "i24_of_string" "stol"
    | E.E1 (I32OfString, e1) ->
        of_string e1 "i32_of_string" "stol"
    | E.E1 (I40OfString, e1) ->
        of_string e1 "i40_of_string" "stoll"
    | E.E1 (I48OfString, e1) ->
        of_string e1 "i48_of_string" "stoll"
    | E.E1 (I56OfString, e1) ->
        of_string e1 "i60_of_string" "stoll"
    | E.E1 (I64OfString, e1) ->
        of_string e1 "i64_of_string" "stoll"
    | E.E1 ((I128OfString | U128OfString), e1) ->
        unary_func "i128_of_string" e1
    | E.E1 (CharOfPtr, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "char(%s.peekByte(0))" n)
    | E.E1 (FloatOfPtr, e1) ->
        let n = print emit p l e1 in
        ppi p.P.def "char const *start_ { (char *)%s.buffer.get() + %s.offset };"
          n n ;
        (* Nice but not supported yet on ordinary g++/clang:
        ppi p.P.def "char const *stop_ { (char *)%s.buffer.get() + %s.size };"
          n n ;
        ppi p.P.def "double val_ { 0. /* don't warn */ };" ;
        ppi p.P.def "bool const is_hex_ { stop_ > start_ + 1 && *start_ == '0' && \
                    (*start_ == 'x' || *start_ == 'X') };"
          ;
        ppi p.P.def "if (is_hex_) start_ += 2;" ;
        ppi p.P.def "struct std::from_chars_result res_ = \
                    std::from_chars(\
                      start_ + (is_hex_ ? 2 : 0), stop_, val_, \
                      is_hex_ ? std::chars_format::hex : \
                                std::chars_format::general);" ;
        emit ?name p l e (fun oc ->
          pp oc "val_, %s.skip(res_.ptr - start_)" n)
        *)
        ppi p.P.def "char *end_;" ;
        (* This assumes there will always be a non-digit at the end to prevent
         * strtod to read past the end of the buffer: *)
        ppi p.P.def "double const val_ = strtod(start_, &end_);" ;
        emit ?name p l e (fun oc ->
          pp oc "val_, %s.skip(end_ - start_)" n)
    | E.E1 ((U8OfPtr | U16OfPtr | U24OfPtr | U32OfPtr | U40OfPtr |
             U48OfPtr | U56OfPtr | U64OfPtr |
             I8OfPtr | I16OfPtr | I24OfPtr | I32OfPtr | I40OfPtr |
             I48OfPtr | I56OfPtr | I64OfPtr), e1) ->
        let n = print emit p l e1 in
        let tn = E.type_of l e |> T.pair_of_tpair |> fst |> type_identifier p in
        ppi p.P.def "%s val_ { 0 /* don't warn */ };" tn ;
        ppi p.P.def "char const *start_ { (char *)%s.buffer.get() + %s.offset };"
          n n ;
        ppi p.P.def "char const *stop_ { (char *)%s.buffer.get() + %s.size };"
          n n ;
        ppi p.P.def "struct std::from_chars_result res_ = \
                    std::from_chars(start_, stop_, val_);" ;
        emit ?name p l e (fun oc ->
          pp oc "val_, %s.skip(res_.ptr - start_)" n)
    | E.E1 (U128OfPtr, e1) ->
        let n = print emit p l e1 in
        let tn = E.type_of l e |> T.pair_of_tpair |> fst |> type_identifier p in
        ppi p.P.def "%s val_ { 0 /* don't warn */ };" tn ;
        ppi p.P.def "char const *start_ { (char *)%s.buffer.get() + %s.offset };"
          n n ;
        ppi p.P.def "char const *stop_ { (char *)%s.buffer.get() + %s.size };" n n ;
        ppi p.P.def "size_t count_ = u128_from_chars(start_, stop_, &val_);" ;
        emit ?name p l e (fun oc -> pp oc "val_, %s.skip(count_)" n)
    | E.E1 (I128OfPtr, e1) ->
        let n = print emit p l e1 in
        let tn = E.type_of l e |> T.pair_of_tpair |> fst |> type_identifier p in
        ppi p.P.def "%s val_ { 0 /* don't warn */ };" tn ;
        ppi p.P.def "char const *start_ { (char *)%s.buffer.get() + %s.offset };"
          n n ;
        ppi p.P.def "char const *stop_ { (char *)%s.buffer.get() + %s.size };"
          n n ;
        ppi p.P.def "size_t count_ = i128_from_chars(start_, stop_, &val_);" ;
        emit ?name p l e (fun oc -> pp oc "val_, %s.skip(count_)" n)
    | E.E1 (FloatOfQWord, e1) ->
        unary_func "float_of_qword" e1
    | E.E1 (QWordOfFloat, e1) ->
        unary_func "qword_of_float" e1
    | E.E1 (StringOfFloat, e1) ->
        unary_func "hex_string_of_float" e1
    | E.E1 (StringOfChar, e1) ->
        let n = print emit p l e1 in
        (* This will use the list-initializer. Beware that "1, %s" would _also_ use
         * the list initializer, not the (count, char) constructor! *)
        emit ?name p l e (fun oc -> pp oc "%s" n)
    | E.E1 (ByteOfU8, e1) | E.E1 (U8OfByte, e1)
    | E.E1 (WordOfU16, e1) | E.E1 (U16OfWord, e1)
    | E.E1 (U32OfDWord, e1) | E.E1 (DWordOfU32, e1)
    | E.E1 (U64OfQWord, e1) | E.E1 (QWordOfU64, e1)
    | E.E1 (U128OfOWord, e1) | E.E1 (OWordOfU128, e1)
    | E.E1 (BitOfBool, e1) | E.E1 (BoolOfBit, e1) ->
        (* Those are NoOps: *)
        let n = print emit p l e1 in
        n
    | E.E1 (U8OfChar, e1) | E.E1 (CharOfU8, e1)
    | E.E1 (SizeOfU32, e1) | E.E1 (U32OfSize, e1)
    | E.E1 ((ToU8 | ToI8 | ToU16 | ToI16 | ToU24 | ToI24 | ToU32 | ToI32 |
             ToU40 | ToI40 | ToU48 | ToI48 | ToU56 | ToI56 | ToU64 | ToI64 |
             ToU128 | ToI128 | ToFloat), e1)
    | E.E1 (U8OfBool, e1) | E.E1 (BoolOfU8, e1) ->
        let n = print emit p l e1 in
        let t = E.type_of l e in
        emit ?name p l e (fun oc ->
          print_cast p t (fun oc -> pp oc "%s" n) oc)
    | E.E1 (ListOfSList, e1) ->
        method_call e1 "toList" []
    | E.E1 (ListOfSListRev, e1) ->
        method_call e1 "toListRev" []
    | E.E1 (SetOfSList, e1) ->
        method_call e1 "toSet" []
    | E.E1 (ListOfVec, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E.E1 (ListOfSet, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "&%s" n1)
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
    | E.E2 ((StartsWith | EndsWith as op), e1, e2) ->
        let op = match op with StartsWith -> "starts_with" | _ -> "ends_with" in
        method_call e1 op [ e2 ]
    | E.E1 (StringLength, e1) ->
        method_call e1 "size" []
    | E.E1 (Cardinality, e1) ->
        method_call e1 "size" []
    | E.E1 (StringOfBytes, e1) ->
        method_call e1 "toString" []
    | E.E1 (BytesOfString, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E.E0 (DataPtrOfString s) ->
        emit ?name p l e (fun oc -> pp oc "%S" s)
    | E.E1 (DataPtrOfBuffer, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E.E1 (GetEnv, e1) ->
        let n1 = print emit p l e1 in
        let res = gen_sym "getenv_res_" in
        ppi p.P.def "char *%s { std::getenv(%s.c_str()) };" res n1 ;
        emit ?name p l e (fun oc ->
          pp oc "%s == nullptr ? std::nullopt : %s" res res)
    | E.E3 (DataPtrOfPtr, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc -> pp oc "%s, %s, %s" n1 n2 n3)
    | E.E2 (GetBit, e1, e2) ->
        method_call e1 "getBit" [ e2 ]
    | E.E2 (GetVec, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s[%s]" n2 n1)
    | E.E3 (SetBit, e1, e2, e3) ->
        method_call e1 "setBit" [ e2 ; e3 ]
    | E.E3 (SetVec, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc -> pp oc "%s[%s] = %s" n2 n1 n3)
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
    | E.E1 (Abs, e1) ->
        unary_func "std::abs" e1
    | E.E1 (Neg, e1) ->
        unary_op "-" e1
    | E.E1 (Exp, e1) ->
        unary_func "std::exp" e1
    | E.E1 (Log, e1) ->
        unary_func "std::log" e1 |> null_of_nan
    | E.E1 (Log10, e1) ->
        unary_func "std::log10" e1 |> null_of_nan
    | E.E1 (Sqrt, e1) ->
        unary_func "std::sqrt" e1 |> null_of_nan
    | E.E1 (Ceil, e1) ->
        unary_func "std::ceil" e1
    | E.E1 (Floor, e1) ->
        unary_func "std::floor" e1
    | E.E1 (Round, e1) ->
        unary_func "std::round" e1
    | E.E1 (Cos, e1) ->
        unary_func "std::cos" e1
    | E.E1 (Sin, e1) ->
        unary_func "std::sin" e1
    | E.E1 (Tan, e1) ->
        unary_func "std::tan" e1 |> null_of_nan
    | E.E1 (ACos, e1) ->
        unary_func "std::acos" e1 |> null_of_nan
    | E.E1 (ASin, e1) ->
        unary_func "std::asin" e1 |> null_of_nan
    | E.E1 (ATan, e1) ->
        unary_func "std::atan" e1
    | E.E1 (CosH, e1) ->
        unary_func "std::cosh" e1
    | E.E1 (SinH, e1) ->
        unary_func "std::sinh" e1
    | E.E1 (TanH, e1) ->
        unary_func "std::tanh" e1
    | E.E1 ((Lower | Upper as op), e1) ->
        (* FIXME: proper UTF-8 + use a lib for proper lower/upper casing *)
        let n1 = print emit p l e1 in
        (* Beware that n1 is used twice below so we must "un-inlining" it: *)
        let tmp1 = gen_sym ?name "str_" in
        ppi p.P.def "std::string const &%s { %s };" tmp1 n1 ;
        let res = gen_sym ?name "case_str_" in
        ppi p.P.def "std::string %s(%s.length(), ' ');" res tmp1 ;
        let op = match op with Lower -> "tolower" | _ -> "toupper" in
        ppi p.P.def "std::transform(%s.cbegin(), %s.cend(), %s.begin(), ::%s);"
          tmp1 tmp1 res op ;
        res
    | E.E1 (Hash, e1) ->
        let n1 = print emit p l e1 in
        let t = E.type_of l e1 in
        let tn = type_identifier p t in
        emit ?name p l e (fun oc -> pp oc "uint64_t(std::hash<%s>{}(%s))" tn n1)
    | E.E0 (EndOfList _) ->
        (* Default constructor cannot be called with no-args as that would
         * not be C++ish enough: *)
        let res = gen_sym ?name "endoflist_" in
        let t = E.type_of l e in
        let tn = type_identifier p t in
        ppi p.P.def "%s %s;" tn res ;
        res
    | E.E0 (EmptySet mn) ->
        let tn = type_identifier p (Value mn) in
        emit ?name p l e (fun oc ->
          pp oc "new SimpleSet<%s>()" tn)
    | E.E0 Now ->
        emit ?name p l e (fun oc ->
          pp oc "std::chrono::duration<double>(std::chrono::high_resolution_clock::now().time_since_epoch()).count()")
    | E.E0 RandomFloat ->
        emit ?name p l e (fun oc -> pp oc "_random_float_(_random_engine_)")
    | E.E0 RandomU8 ->
        emit ?name p l e (fun oc -> pp oc "_random_u8_(_random_engine_)")
    | E.E0 RandomU32 ->
        emit ?name p l e (fun oc -> pp oc "_random_u32_(_random_engine_)")
    | E.E0 RandomU64 ->
        emit ?name p l e (fun oc -> pp oc "_random_u64_(_random_engine_)")
    | E.E0 RandomU128 ->
        emit ?name p l e (fun oc ->
          pp oc "_random_u64_(_random_engine_) |\
                 ((uint128_t)_random_u64_(_random_engine_) << 64)")
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
    | E.E2 (Map, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E2 ((Min | Max as op), e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and op = match op with Min -> "min" | _ -> "max" in
        let t = E.type_of l e in
        let tn = type_identifier p t in
        emit ?name p l e (fun oc -> pp oc "std::%s<%s>(%s, %s)" op tn n1 n2)
    | E.E2 (Member, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        (* un-inline n2, used several times below: *)
        let tmp2 = gen_sym ?name "set_" in
        ppi p.P.def "auto const &%s { %s };" tmp2 n2 ;
        emit ?name p l e (fun oc ->
          pp oc "std::end(%s) != std::find(std::begin(%s), std::end(%s), %s)"
            tmp2 tmp2 tmp2 n1)
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
        let n1 = print emit p l e1 in
        let t = E.type_of l e1 in
        let tn = type_identifier p t in
        let res = gen_sym ?name "let_res_" in
        let l = (E.E0 (Identifier n), t) :: l in
        let t2 = E.type_of l e2 in
        ppi p.P.def "%s %s;" (type_identifier p t2) res ;
        ppi p.P.def "{" ;
        P.indent_more p (fun () ->
          ppi p.P.def "%s %s(%s);" tn (valid_identifier n) n1 ;
          let tmp = print emit p l e2 in
          ppi p.P.def "%s = %s;" res tmp) ;
        ppi p.P.def "}" ;
        res
    | E.E1 (Function (fid, ts), e1) ->
        emit ?name p l e (fun oc ->
          array_print_i ~first:"[&](" ~last:") {\n" ~sep:", "
            (fun i oc t -> Printf.fprintf oc "%s%s %s"
              (type_identifier p t)
              (if is_mutable t then "&" else "")
              (param fid i))
            oc ts ;
          let l =
            Array.fold_lefti (fun l i t ->
              (E.E0 (Param (fid, i)), t) :: l
            ) l ts in
          P.indent_more p (fun () ->
            let n = print emit p l e1 in
            print_return n p l e1) ;
          pp oc "%s}\n" p.P.indent ;
          pp oc "%s" p.P.indent)
    | E.E0 (Param (fid, n)) ->
        param fid n
    | E.E3 (If, e1, e2, e3) ->
        let cond = print emit p l e1 in
        let res = gen_sym ?name "choose_res_" in
        let t2 = E.type_of l e2 in
        ppi p.P.def "%s %s;" (type_identifier p t2) res ;
        ppi p.P.def "if (%s) {" cond ;
        P.indent_more p (fun () ->
          let n = print emit p l e2 in
          ppi p.P.def "%s = %s;" res n) ;
        ppi p.P.def "} else {" ;
        P.indent_more p (fun () ->
          let n = print emit p l e3 in
          ppi p.P.def "%s = %s;" res n) ;
        ppi p.P.def "}" ;
        res
    | E.E4 (ReadWhile, e1, e2, e3, e4) ->
        let cond = print emit p l e1
        and reduce = print emit p l e2
        and accum = print emit p l e3
        and ptr0 = print emit p l e4 in
        let res = gen_sym ?name "read_while_res_" in
        let t3 = E.type_of l e3 in
        ppi p.P.def "%s %s(%s);" (type_identifier p t3) res accum ;
        let ptr = gen_sym "read_while_ptr_" in
        let t4 = E.type_of l e4 in
        ppi p.P.def "%s %s(%s);" (type_identifier p t4) ptr ptr0 ;
        ppi p.P.def "while (true) {" ;
        P.indent_more p (fun () ->
          ppi p.P.def "if (%s.rem() <= 0) { break; } else {" ptr ;
          P.indent_more p (fun () ->
            ppi p.P.def "uint8_t const next_byte_(%s.peekByte(0));" ptr ;
            ppi p.P.def "if (%s(next_byte_)) {" cond ;
            P.indent_more p (fun () ->
              ppi p.P.def "%s = %s(%s, next_byte_);" res reduce res ;
              ppi p.P.def "%s = %s.skip(1);" ptr ptr) ;
            ppi p.P.def "} else break;") ;
          ppi p.P.def "}") ;
        ppi p.P.def "}" ;
        emit ?name p l e (fun oc -> pp oc "%s, %s" res ptr)
    | E.E3 (LoopWhile, e1, e2, e3) ->
        let cond = print emit p l e1
        and body = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym ?name "while_res_" in
        let t3 = E.type_of l e3 in
        ppi p.P.def "%s %s { %s };" (type_identifier p t3) res accum ;
        ppi p.P.def "while (%s(%s)) {" cond res ;
        P.indent_more p (fun () ->
          ppi p.P.def "%s = %s(%s);" res body res) ;
        ppi p.P.def "}" ;
        res
    | E.E3 (LoopUntil, e1, e2, e3) ->
        let body = print emit p l e1
        and cond = print emit p l e2
        and accum = print emit p l e3 in
        let res = gen_sym ?name "until_res_" in
        let t3 = E.type_of l e3 in
        ppi p.P.def "%s %s { %s };" (type_identifier p t3) res accum ;
        ppi p.P.def "do {" ;
        P.indent_more p (fun () ->
          ppi p.P.def "%s = %s(%s);" res body res) ;
        ppi p.P.def "} while (%s(%s));" cond res ;
        res
    | E.E3 (Fold, e1, e2, e3) ->
        let init = print emit p l e1
        and body = print emit p l e2
        and lst = print emit p l e3
        and res = gen_sym ?name "fold_res_"
        and item_t = E.get_item_type ~lst:true ~vec:true ~set:true e l e3
        and t1 = E.type_of l e1 in
        let item_tn = type_identifier p (T.Value item_t) in
        ppi p.P.def "%s %s { %s };" (type_identifier p t1) res init ;
        let is_set =
          match E.type_of l e3 |> T.develop_user_types with
          | T.Value { vtyp = Set _ ; _ } -> true
          | _ -> false in
        if is_set then
          ppi p.P.def "%s->iter([&](%s &x_) {" lst item_tn
        else
          ppi p.P.def "for (%s x_ : %s) {" item_tn lst ;
        P.indent_more p (fun () ->
          ppi p.P.def "%s = %s(%s, x_);" res body res) ;
        if is_set then
          ppi p.P.def "});"
        else
          ppi p.P.def "}" ;
        res
    | E.E4 (Repeat, e1, e2, e3, e4) ->
        let from = print emit p l e1
        and to_ = print emit p l e2
        and body = print emit p l e3
        and accum = print emit p l e4 in
        let res = gen_sym ?name "repeat_res_" in
        let t4 = E.type_of l e4 in
        ppi p.P.def "%s %s { %s };" (type_identifier p t4) res accum ;
        ppi p.P.def "for (int32_t idx_ = %s; idx_ != %s; idx_++) {" from to_ ;
        P.indent_more p (fun () ->
          ppi p.P.def "%s = %s(idx_, %s);" res body res) ;
        ppi p.P.def "}" ;
        res
    | E.E1 (GetItem n, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 (tuple_field_name n))
    | E.E1 (GetField s, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 (valid_identifier s))
    | E.E1 (GetAlt s, e1) ->
        (match E.type_of l e1 |> T.develop_user_types with
        | T.Value { vtyp = Sum mns ; nullable = false } ->
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
    | E.E1 (LabelOf, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "uint16_t(%s.index())" n1)
    | E.E0 CopyField ->
        emit ?name p l e (fun oc -> pp oc "Mask::COPY")
    | E.E0 SkipField ->
        emit ?name p l e (fun oc -> pp oc "Mask::SKIP")
    | E.E0 SetFieldNull ->
        emit ?name p l e (fun oc -> pp oc "Mask::SET_NULL")
    | E.E1 (SlidingWindow mn, e1) ->
        let n1 = print emit p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier p (Value mn) in
        let res = gen_sym ?name "sliding_win_" in
        ppi p.P.def "SlidingWindow<%s> *%s = new SlidingWindow<%s>(%s);"
          tn res tn n1 ;
        res
    | E.E1 (TumblingWindow mn, e1) ->
        let n1 = print emit p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier p (Value mn) in
        let res = gen_sym ?name "tumbling_win_" in
        ppi p.P.def "TumblingWindow<%s> *%s = new TumblingWindow<%s>(%s);"
          tn res tn n1 ;
        res
    | E.E1 (Sampling mn, e1) ->
        let n1 = print emit p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier p (Value mn) in
        let res = gen_sym ?name "sampling_" in
        ppi p.P.def "Sampling<%s> *%s = new Sampling<%s>(%s);"
          tn res tn n1 ;
        res
    | E.E1 (HashTable mn, e1) ->
        let n1 = print emit p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier p (Value mn) in
        let res = gen_sym ?name "hash_table_" in
        ppi p.P.def "HashTable<%s> *%s = new HashTable<%s>(%s);"
          tn res tn n1 ;
        res
    | E.E2 (Insert, e1, e2) ->
        let set = print emit p l e1 in
        let item = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s->insert(%s)" set item)
    | E.E2 (SplitBy, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "string_split(%s, %s)" n1 n2)
    | E.E2 (SplitAt, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "%s.substr(0, %s), %s.substr(%s)" n2 n1 n2 n1)
    | E.E2 (Join, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "string_join(%s, %s)" n1 n2)
    | E.E2 (AllocLst, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        (* Use the constructor inherited from std::vector: *)
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E2 (PartialSort, e1, e2) ->
        let n1 = print ?name emit p l e1
        and n2 = print emit p l e2 in
        ppi p.P.def "%s.partial_sort(%s);" n1 n2 ;
        n1
    | E.E3 (FindSubstring, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        let pos = gen_sym ?name "pos_" in
        ppi p.P.def "std::size %s { %s ? %s.find(%s) : %s.rfind(%s) };"
          pos n1 n3 n2 n3 n2 ;
        emit ?name p l e (fun oc ->
          pp oc "%s != std::string::npos ? %s : std::nullopt" pos pos)

  let print_binding_toplevel emit n p l e =
    (* In C++ toplevel expressions cannot be initialized with arbitrary code so we
     * must rely on a static function to produce the value: *)
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.P.def "%sstatic %s %s_init()\n" p.P.indent tn n ;
    pp p.P.def "%s{\n" p.P.indent ;
    P.indent_more p (fun () ->
      let n = print emit p l e in
      print_return n p l e) ;
    pp p.P.def "%s}\n" p.P.indent ;
    pp p.P.def "%s%s %s(%s_init());\n\n" p.P.indent tn n n

  let print_identifier_declaration n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.P.def "%s%s %s;\n" p.P.indent tn n

  let source_intro =
    "#include <algorithm>\n\
     #include <arpa/inet.h>\n\
     #include <charconv>\n\
     #include <chrono>\n\
     #include <cmath>\n\
     #include <cstdlib>\n\
     #include <exception>\n\
     #include <fstream>\n\
     #include <functional>\n\
     #include <iostream>\n\
     #include <optional>\n\
     #include <random>\n\
     #include <utility>\n\
     #include <variant>\n\
     #include <vector>\n\
     #include \"dessser/runtime.h\"\n\
     \n\
     std::uniform_real_distribution<double> _random_float_(0, 1);\n\
     std::uniform_int_distribution<uint8_t> _random_u8_(0);\n\
     std::uniform_int_distribution<uint32_t> _random_u32_(0);\n\
     std::uniform_int_distribution<uint64_t> _random_u64_(0);\n\
     std::default_random_engine _random_engine_;\n\
     \n\
     namespace dessser_gen {\n\n"

  let source_outro =
    "\n}\n"
end

include DessserBackEndCLike.Make (Config)
