open Batteries
open Stdint

open Dessser
open DessserBackEndCLike
open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module P = DessserPrinter

let cpp_std_version = 17

let valid_identifier =
  (* Taken from https://en.cppreference.com/w/cpp/keyword: *)
  let keywords =
    [ "alignas" ; "alignof" ; "and" ; "and_eq" ; "asm" ; "atomic_cancel" ;
      "atomic_commit" ; "atomic_noexcept" ; "auto" ; "bitand" ; "bitor" ;
      "bool" ; "break" ; "case" ; "catch" ; "char" ; "char8_t" ; "char16_t" ;
      "char32_t" ; "class" ; "compl" ; "concept" ; "const" ; "consteval" ;
      "constexpr" ; "constinit" ; "const_cast" ; "continue" ; "co_await" ;
      "co_return" ; "co_yield" ; "decltype" ; "default" ; "delete" ; "do" ;
      "double" ; "dynamic_cast" ; "else" ; "enum" ; "explicit" ; "export" ;
      "extern" ; "false" ; "float" ; "for" ; "friend" ; "goto" ; "if" ;
      "inline" ; "int" ; "long" ; "mutable" ; "namespace" ; "new" ; "noexcept" ;
      "not" ; "not_eq" ; "nullptr" ; "operator" ; "or" ; "or_eq" ; "private" ;
      "protected" ; "public" ; "reflexpr" ; "register" ; "reinterpret_cast" ;
      "requires" ; "return" ; "short" ; "signed" ; "sizeof" ; "static" ;
      "static_assert" ; "static_cast" ; "struct" ; "switch" ; "synchronized" ;
      "template" ; "this" ; "thread_local" ; "throw" ; "true" ; "try" ;
      "typedef" ; "typeid" ; "typename" ; "union" ; "unsigned" ; "using" ;
      "virtual" ; "void" ; "volatile" ; "wchar_t" ; "while" ; "xor" ; "xor_eq"
    ] |>
    Set.String.of_list in
  fun s ->
    if Set.String.mem s keywords then s ^ "_" else
    DessserBackEndCLike.valid_identifier s

module Config =
struct
  let id = Cpp
  let valid_source_name n = n

  let preferred_def_extension = "cc"

  let preferred_decl_extension = "h"

  let preferred_comp_extension = function
    | Object -> "o"
    | SharedObject -> "so"
    | Executable -> ""

  let compile_cmd ?(dev_mode=false) ?(extra_search_paths=[]) ?(optim=0) ~link src dst =
    let optim = clamp 0 3 optim in
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
    match T.develop_mn t with
    | T.{ typ = (TBytes | TVec _ | TArr _) ; _ } ->
        true
    | _ ->
        false

  (* Some types are hidden behind a pointer (for instance because we want
   * to use inheritance) *)
  let is_pointy t =
    match T.develop_mn t with
    | T.{ typ = (TThis _ | TSet _) ; _ } ->
        true
    | _ ->
        false

  let rec print_tuple p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    let id = valid_identifier id in
    let is_pair = Array.length mns = 2 in
    if is_pair then
      ppi oc "typedef std::tuple<"
    else
      ppi oc "struct %s : public std::tuple<" id ;
    P.indent_more p (fun () ->
      Array.iteri (fun i mn ->
        let typ_id = type_identifier_mn p mn in
        ppi oc "%s%s"
          typ_id (if i < Array.length mns - 1 then "," else "")
      ) mns) ;
    if is_pair then
      ppi oc "> %s;\n" id
    else
      ppi oc "> { using tuple::tuple; };" ;
    if not is_pair then (
      ppi oc "std::ostream &operator<<(std::ostream &os, %s const &t) {" id ;
      P.indent_more p (fun () ->
        ppi oc "os << '<'" ;
        for i = 0 to Array.length mns - 1 do
          ppi oc "   << std::get<%d>(t)%s"
            i (if i < Array.length mns - 1 then " << \", \"" else "")
        done ;
        ppi oc "   << '>';" ;
        ppi oc "return os;") ;
      ppi oc "}\n"
    )

  and print_rec p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    let id = valid_identifier id in
    ppi oc "struct %s {" id ;
    P.indent_more p (fun () ->
      Array.iter (fun (field_name, mn) ->
        let typ_id = type_identifier_mn p mn in
        ppi oc "%s %s;" typ_id (valid_identifier field_name)
      ) mns ;
      if cpp_std_version >= 20 then
        ppi oc "bool operator==(%s const &) const & = default;" id
      else (
        ppi oc "bool operator==(%s const &other) const {" id ;
        P.indent_more p (fun () ->
          ppi oc "return %a;"
            (Array.print ~first:"" ~last:"" ~sep:" && "
              (fun oc (field_name, _) ->
                let id = valid_identifier field_name in
                Printf.fprintf oc "%s == other.%s" id id)) mns) ;
        ppi oc "}"
      )) ;
    ppi oc "};" ;
    ppi oc "std::ostream &operator<<(std::ostream &os, %s const &r) {" id ;
    P.indent_more p (fun () ->
      ppi oc "os << '{';" ;
      Array.iteri (fun i (field_name, _) ->
        ppi oc "os << %S << r.%s%s;"
          (field_name ^ ":")
          (valid_identifier field_name)
          (if i < Array.length mns - 1 then " << ','" else "")
      ) mns ;
      ppi oc "os << '}';" ;
      ppi oc "return os;") ;
    ppi oc "}\n"

  and print_variant p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    let id = valid_identifier id in
    ppi oc "struct %s : public std::variant<" id ;
    P.indent_more p (fun () ->
      Array.iteri (fun i (_, mn) ->
        let typ_id = type_identifier_mn p mn in
        ppi oc "%s%s"
          typ_id (if i < Array.length mns - 1 then "," else "")
      ) mns
    ) ;
    ppi oc "> { using variant::variant; };" ;
    ppi oc "std::ostream &operator<<(std::ostream &os, %s const &v) {" id ;
    P.indent_more p (fun () ->
      (* too smart for its own good:
       * ppi oc "std::visit([&os](auto arg){ os << arg; }, v);" ;*)
      ppi oc "switch (v.index()) {" ;
      P.indent_more p (fun () ->
        for i = 0 to Array.length mns - 1 do
          ppi oc "case %d: os << std::get<%d>(v); break;" i i
        done) ;
      ppi oc "}" ;
      ppi oc "return os;") ;
    ppi oc "}\n"

  and type_identifier_mn p mn =
    if mn.T.nullable then
      "std::optional<"^
        type_identifier p mn.typ
      ^">"
    else
      type_identifier p mn.typ

  and type_identifier p t =
    let type_identifier = type_identifier p
    and type_identifier_mn = type_identifier_mn p in
    let declare_if_named s =
      P.declare_if_named p t s (fun oc type_id ->
        pp oc "typedef %s %s;\n" s type_id) in
    match t with
    | TUnknown -> invalid_arg "type_identifier"
    | TNamed (_, t) ->
        type_identifier t
    | TThis n ->
        let t = T.find_this n in
        (* If t is a constructed type unknown to the compiler, then its name
         * has to be disclosed to the compiler (FIXME: once is enough!): *)
        (match t |> T.develop with
        | TRec _ | TSum _ | TTup _ ->
            (* All those are structs: *)
            P.prepend_declaration p (fun oc ->
              let id = if n = "" then "t" else valid_identifier n in
              pp oc "struct %s;\n" id) ;
        | _ ->
            Printf.sprintf2
              "type_identifier: C++ backend does not support recursive %a"
              T.print t |>
            failwith) ;
        type_identifier t ^"*"
    | TVoid -> declare_if_named "Void"
    | TFloat -> declare_if_named "double"
    | TString -> declare_if_named "std::string"
    | TBool -> declare_if_named "bool"
    | TChar -> declare_if_named "char"
    | TI8 -> declare_if_named "int8_t"
    | TU8 -> declare_if_named "uint8_t"
    | TI16 -> declare_if_named "int16_t"
    | TU16 -> declare_if_named "uint16_t"
    | TI24 -> declare_if_named "int32_t"
    | TU24 -> declare_if_named "uint32_t"
    | TI32 -> declare_if_named "int32_t"
    | TU32 -> declare_if_named "uint32_t"
    | TI40 -> declare_if_named "int64_t"
    | TU40 -> declare_if_named "uint64_t"
    | TI48 -> declare_if_named "int64_t"
    | TU48 -> declare_if_named "uint64_t"
    | TI56 -> declare_if_named "int64_t"
    | TU56 -> declare_if_named "uint64_t"
    | TI64 -> declare_if_named "int64_t"
    | TU64 -> declare_if_named "uint64_t"
    | TI128 -> declare_if_named "int128_t"
    | TU128 -> declare_if_named "uint128_t"
    | TUsr mn -> type_identifier mn.def |> declare_if_named
    | TExt n ->
        P.get_external_type p n Cpp |> declare_if_named
    | TTup mns ->
        P.declared_type p t (fun oc type_id ->
          print_tuple p oc type_id mns) |>
        valid_identifier
    | TRec mns ->
        P.declared_type p t (fun oc type_id ->
          print_rec p oc type_id mns) |>
        valid_identifier
    | TSum mns ->
        P.declared_type p t (fun oc type_id ->
          print_variant p oc type_id mns) |>
        valid_identifier
    | TVec (dim, mn) ->
        Printf.sprintf "Vec<%d, %s>" dim (type_identifier_mn mn) |>
        declare_if_named
    | TArr mn ->
        Printf.sprintf "Arr<%s>" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (Simple, mn) ->
        Printf.sprintf "Set<%s> *" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (Sliding, mn) ->
        Printf.sprintf "SlidingWindow<%s> *" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (Tumbling, mn) ->
        Printf.sprintf "TumblingWindow<%s> *" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (Sampling, mn) ->
        Printf.sprintf "Sampling<%s> *" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (HashTable, mn) ->
        Printf.sprintf "HashTable<%s> *" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (Heap, mn) ->
        Printf.sprintf "Heap<%s> *" (type_identifier_mn mn) |>
        declare_if_named
    | TSet (Top, _) ->
        todo "C++ back-end for TOPs"
    | TMap _ ->
        assert false (* No value of map type *)
    | TLst mn1 ->
        "Lst<"^ type_identifier_mn mn1 ^">" |>
        declare_if_named
    | TFunction (args, ret) ->
        (* We want all modifiable types (ir bytes, vectors, ...?) passed by
         * reference: *)
        "std::function<"^ type_identifier_mn ret ^
          IO.to_string (
            Array.print ~first:"(" ~last:")" ~sep:"," (fun oc t ->
              Printf.fprintf oc "%s%s"
                (type_identifier_mn t)
                (if is_mutable t then "&" else ""))
          ) args ^">" |>
        declare_if_named
    | TPtr -> declare_if_named "Pointer"
    | TSize -> declare_if_named "Size"
    | TAddress -> declare_if_named "Address"
    | TBytes -> declare_if_named "Bytes"
    | TMask -> declare_if_named "Mask"

  (* Identifiers used for function parameters: *)
  let param n = "p_"^ string_of_int n

  let print_binding p t n f oc =
    let tn = type_identifier_mn p t in
    if T.eq_mn t T.void then (
      pp oc "%s %s { ((void)(%t), VOID) };" tn n f
    ) else (
      (* Beware that this must not be parsed as a function declaration. Thus
       * the use of the "uniform initialization" syntax, which, this being
       * C++, cannot be used uniformly, as it favors the initialization-list
       * constructor. *)
      pp oc "%s %s { %t };" tn n f
    )

  let print_cast p t f oc =
    let tn = type_identifier_mn p t in
    if is_pointy t then
      (* Outer parenth required since a following "->" would apply first *)
      pp oc "((%s)(%t))" tn f
    else
      pp oc "%s(%t)" tn f

  let print_comment oc fmt =
    pp oc ("/* "^^ fmt ^^" */\n")

  let print_float_literal v oc =
    if v = infinity then
      String.print oc "std::numeric_limits<double>::infinity()"
    else if v = neg_infinity then
      String.print oc "-std::numeric_limits<double>::infinity()"
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

  (* Print the code for returning the value [n] of expression [e]. *)
  let print_return n p =
    pp p.P.def "%sreturn %s;\n" p.P.indent n

  let rec print emit ?name p l e =
    let print = print emit in
    let gen_sym ?name pref =
      match name with
      | Some n -> n
      | None -> U.gen_sym pref |> valid_identifier in
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    let unary_op op e1 =
      let n1 = print p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s %s" op n1) in
    let unary_func f e1 =
      let n1 = print p l e1 in
      emit ?name p l e (fun oc -> pp oc "%s(%s)" f n1) in
    let binary_infix_op e1 op e2 =
      let n1 = print p l e1
      and n2 = print p l e2 in
      let t = E.type_of l e in
      emit ?name p l e (fun oc ->
        (* Prevent integer promotion by casting to type_of e: *)
        print_cast p t (fun oc -> pp oc "%s %s %s" n1 op n2) oc) in
    let binary_op op e1 e2 =
      let n1 = print p l e1
      and n2 = print p l e2 in
      let t = E.type_of l e in
      emit ?name p l e (fun oc ->
        (* Prevent integer promotion by casting to type_of e: *)
        print_cast p t (fun oc -> pp oc "%s(%s, %s)" op n1 n2) oc) in
    let shortcutting_binary_infix_op e1 e2 short_cond_on_e1 =
      let n1 = print p l e1 in
      let res = gen_sym ?name "shortcut_res_" in
      let t1 = E.type_of l e1 in
      ppi p.P.def "%s %s;" (type_identifier_mn p t1) res ;
      ppi p.P.def "if (%s == %b) {" n1 short_cond_on_e1 ;
      P.indent_more p (fun () ->
        ppi p.P.def "%s = %s;" res n1) ;
      ppi p.P.def "} else {" ;
      P.indent_more p (fun () ->
        let n2 = print p l e2 in
        ppi p.P.def "%s = %s;" res n2) ;
      ppi p.P.def "}" ;
      res in
    let method_call e1 m args =
      let deref_with =
        if is_pointy (E.type_of l e1 |> T.develop_mn) then "->"
                                                      else "." in
      let n1 = print p l e1
      and ns = List.map (print p l) args in
      emit ?name p l e (fun oc ->
        pp oc "%s%s%s%a" n1 deref_with m
          (List.print ~first:"(" ~last:")" ~sep:", " String.print) ns) in
    let null_of_nan res =
      ppi p.P.def "if (std::isnan(*%s)) %s.reset();" res res ;
      res in
    (* Convert from string to nullable number: *)
    let of_string e1 prefix cpp_op =
      let n1 = print p l e1 in
      let res = gen_sym ?name (prefix ^"_res_")
      and pos = gen_sym (prefix ^"_pos_") in
      let t = E.type_of l e in
      assert t.T.nullable ; (* tn must be an optional<...> *)
      let tn = type_identifier_mn p (T.force t) in
      ppi p.P.def "std::optional<%s> %s;" tn res ;
      ppi p.P.def "std::size_t %s;" pos ;
      ppi p.P.def "try {" ;
      ppi p.P.def "  %s const v_ { (%s)std::%s(%s, &%s) };"
        tn tn cpp_op n1 pos ;
      ppi p.P.def "  if (%s == %s.length()) %s = v_;" pos n1 res ;
      ppi p.P.def "} catch (const std::exception&) {}" ;
      res
    in
    match e with
    | T.E1S (Apply, f, es) ->
        let nf = print p l f in
        let ns =
          List.fold_left (fun ns e -> print p l e :: ns) [] es |>
          List.rev in
        emit ?name p l e (fun oc ->
          pp oc "%s%a"
            nf
            (List.print ~first:"(" ~last:")" ~sep:", " String.print) ns)
    | E1 (Comment c, e1) ->
        ppi p.P.def "/* %s */" c ;
        print ?name p l e1
    | E0S (Seq, []) ->
        "VOID"
    | E0S (Seq, es) ->
        List.fold_left (fun _ e -> print p l e) "VOID" es
    | E0S (MakeTup, es) ->
        let inits = List.map (print p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:" " ~last:" " ~sep:", " String.print oc inits)
    | E0R ((MakeVec | MakeArr _), es) ->
        let inits = Array.map (print p l) es in
        emit ?name p l e (fun oc ->
          Array.print ~first:" " ~last:" " ~sep:", " String.print oc inits)
    | E0S (MakeRec, es) ->
        let _, inits =
          List.fold_left (fun (prev_name, inits) e ->
            match prev_name with
            | None ->
                Some (E.field_name_of_expr e), inits
            | Some name ->
                let n = print p l e in
                None, (valid_identifier name, n) :: inits
          ) (None, []) es in
        let inits = List.rev inits in
        emit ?name p l e (fun oc ->
          List.print ~first:" " ~last:" " ~sep:", "
            (fun oc (name, n) ->
              Printf.fprintf oc ".%s = %s" name n) oc inits)
    | E0S (MakeUsr n, ins) ->
        let e = E.apply_constructor e l n ins in
        print ?name p l e
    | E0S (Verbatim (temps, _), ins) ->
        let args = List.map (print p l) ins in
        emit ?name p l e (fun oc ->
          String.print oc (E.expand_verbatim id temps args))
    | E1 (Identity, e1) ->
        print ?name p l e1
    | E1 (Ignore, e1) ->
        let n = print p l e1 in
        ppi p.P.def "((void)%s, VOID);" n ;
        "VOID"
    | E1 (Dump, e1) ->
        let n = print p l e1 in
        ppi p.P.def "std::cout << %s;" n ;
        "VOID"
    | E2 (Nth, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        let mn2 = E.type_of l e2 |> T.develop_mn in
        let item_t = T.get_item_type ~vec:true ~arr:true ~lst:true ~str:true
                                     ~bytes:true mn2.T.typ in
        let need_optional = not item_t.T.nullable in
        emit ?name p l e (fun oc ->
          match (E.type_of l e2 |> T.develop_mn).T.typ with
          | T.(TVec _ | TArr _) ->
              pp oc "%s < %s.size() ? \
                     %s(%s[%s]) : std::nullopt"
                 n1 n2
                 (if need_optional then "std::make_optional" else "")
                 n2 n1
          | TLst _
          | TString
          | TBytes ->
              pp oc "%s < %s.length() ? \
                     %s(%s[%s]) : std::nullopt"
                 n1 n2
                 (if need_optional then "std::make_optional" else "")
                 n2 n1
          | _ ->
              assert false)
    | E2 (UnsafeNth, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          match (E.type_of l e2 |> T.develop_mn).T.typ with
          | T.TVec _
          | TArr _
          | TLst _
          | TString
          | TBytes ->
              pp oc "%s[%s]" n2 n1
          | _ ->
              assert false)
    | E1 (NotNull, e1) ->
        if (E.type_of l e1).T.nullable then
          print ?name p l e1
        else
          let n1 = print p l e1 in
          emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E1 (Force what, e1) ->
        if not (E.type_of l e1).T.nullable then
          print ?name p l e1
        else
          let n1 = print p l e1 in
          if what <> "" then ppi p.P.def "/* Force: %s */" what ;
          emit ?name p l e (fun oc -> Printf.fprintf oc "%s.value()" n1)
    | E1 (IsNull, e1) ->
        if (E.type_of l e1).T.nullable then
          if E.is_const_null e1 then
            (* Cannot call has_value on nullopt: *)
            emit ?name p l e (fun oc -> pp oc "true")
          else
            let n = print p l e1 in
            emit ?name p l e (fun oc -> pp oc "!(%s.has_value ())" n)
        else
          emit ?name p l e (fun oc -> pp oc "false")
    | E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "std::nullopt")
    | E0 (Float f) ->
        emit ?name p l e (print_float_literal f)
    | E0 (String s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc s)
    | E0 (Bool b) ->
        emit ?name p l e (fun oc -> Bool.print oc b)
    | E0 (Char c) ->
        emit ?name p l e (fun oc -> pp oc "'%s'" (c_char_of c))
    | E0 (U8 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint8.to_string i))
    | E0 (U16 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint16.to_string i))
    | E0 (U24 u) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint24.to_string u))
    | E0 (U32 u) ->
        emit ?name p l e (fun oc -> pp oc "%sU" (Uint32.to_string u))
    | E0 (U40 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint40.to_string u))
    | E0 (U48 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint48.to_string u))
    | E0 (U56 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint56.to_string u))
    | E0 (U64 u) ->
        emit ?name p l e (fun oc -> pp oc "%sUL" (Uint64.to_string u))
    | E0 (U128 u) ->
        emit ?name p l e (fun oc ->
          let lo = Uint128.to_uint64 u
          and hi = Uint128.(to_uint64 (shift_right_logical u 64)) in
          pp oc "((((uint128_t)%sULL) << 64U) | %sULL)"
            (Uint64.to_string hi)
            (Uint64.to_string lo))
    | E0 (Bytes s) ->
        emit ?name p l e (fun oc -> String.print_quoted oc (Bytes.to_string s))
    | E0 (I8 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Int8.to_string i))
    | E0 (I16 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Int16.to_string i))
    | E0 (I24 i) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Int24.to_string i))
    | E0 (I32 i) ->
        emit ?name p l e (fun oc -> pp oc "%sL" (Int32.to_string i))
    | E0 (I40 i) ->
        emit ?name p l e (fun oc -> pp oc "%sLL" (Int40.to_string i))
    | E0 (I48 i) ->
        emit ?name p l e (fun oc -> pp oc "%sLL" (Int48.to_string i))
    | E0 (I56 i) ->
        emit ?name p l e (fun oc -> pp oc "%sLL" (Int56.to_string i))
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
    | E0 (Address a) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint64.to_string a))
    | E2 (Gt, e1, e2) ->
        binary_infix_op e1 ">" e2
    | E2 (Ge, e1, e2) ->
        binary_infix_op e1 ">=" e2
    | E2 (Eq, e1, e2) ->
        binary_infix_op e1 "==" e2
    | E2 (Add, e1, e2) ->
        binary_infix_op e1 "+" e2
    | E2 (Sub, e1, e2) ->
        binary_infix_op e1 "-" e2
    | E2 (Mul, e1, e2) ->
        binary_infix_op e1 "*" e2
    | E2 ((Div | Rem as op), e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        (match E.type_of l e1 |> T.develop_mn with
        | { typ = (TU8|TU16|TU24|TU32|TU40|TU48|TU56|TU64|TU128
                  |TI8|TI16|TI24|TI32|TI40|TI48|TI56|TI64|TI128) ; _ } ->
            let op_name = if op = Div then "/" else "%" in
            emit ?name p l e (fun oc ->
              pp oc "%s == 0 ? std::nullopt : std::make_optional(%s %s %s)"
                n2 n1 op_name n2)
        | { typ = TFloat ; _ } ->
            if op = Div then
              binary_infix_op e1 "/" e2 |> null_of_nan
            else
              binary_op "fmod" e1 e2 |> null_of_nan
        | _ ->
            assert false)
    | E2 ((UnsafeDiv | UnsafeRem as op), e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        (match E.type_of l e1 |> T.develop_mn with
        | { typ = (TU8|TU16|TU24|TU32|TU40|TU48|TU56|TU64|TU128
                  |TI8|TI16|TI24|TI32|TI40|TI48|TI56|TI64|TI128) ; _ } ->
            let op_name = if op = UnsafeDiv then "/" else "%" in
            emit ?name p l e (fun oc -> pp oc "%s %s %s" n1 op_name n2)
        | { typ = TFloat ; _ } ->
            if op = UnsafeDiv then
              binary_infix_op e1 "/" e2
            else
              binary_op "fmod" e1 e2
        | _ ->
            assert false)
    | E2 ((Pow | UnsafePow) as op, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        let t = E.type_of l e in
        emit ?name p l e (fun oc ->
          print_cast p t (fun oc ->
            pp oc "std::pow(%s, %s)" n1 n2) oc) |>
        (if op = Pow then null_of_nan else identity)
    | E2 (BitAnd, e1, e2) ->
        binary_infix_op e1 "&" e2
    | E2 (BitOr, e1, e2) ->
        binary_infix_op e1 "|" e2
    | E2 (BitXor, e1, e2) ->
        binary_infix_op e1 "^" e2
    | E1 (BitNot, e1) ->
        unary_op "~" e1
    | E2 (LeftShift, e1, e2) ->
        binary_infix_op e1 "<<" e2
    | E2 (RightShift, e1, e2) ->
        binary_infix_op e1 ">>" e2
    | E1 (StringOfInt, e1) ->
        let n1 = print p l e1 in
        (match E.type_of l e1 |> T.develop_mn with
        | { typ = TU128 ; _ } ->
            emit ?name p l e (fun oc -> pp oc "string_of_u128(%s)" n1)
        | { typ = TI128 ; _ } ->
            emit ?name p l e (fun oc -> pp oc "string_of_i128(%s)" n1)
        | _ ->
            emit ?name p l e (fun oc -> pp oc "std::to_string(%s)" n1))
    | E1 (StringOfIp, e1) ->
        let n1 = print p l e1 in
        let str = gen_sym ?name "str_" in
        let ip = gen_sym ?name "ip_" in
        ppi p.P.def "char %s[INET6_ADDRSTRLEN];\n" str ;
        let case_u mn n =
          match T.develop_mn mn with
          | T.{ typ = TU32 ; _ } ->
              (* Make sure we can take the address of that thing: *)
              ppi p.P.def "const uint32_t %s { %s };\n" ip n ;
              ppi p.P.def
                "inet_ntop(AF_INET, &%s, %s, sizeof(%s));\n" ip str str ;
          | T.{ typ = TU128 ; _ } ->
              ppi p.P.def "const uint128_t %s{ %s };\n" ip n ;
              ppi p.P.def
                "inet_ntop(AF_INET6, &%s, %s, sizeof(%s));\n" ip str str ;
          | _ ->
              assert false (* because of type checking *)
        in
        (match E.type_of l e1 |> T.develop_mn with
        | { typ = TSum mns ; _ } ->
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
        | mn ->
            case_u mn n1) ;
        emit ?name p l e (fun oc -> pp oc "%s" str)
    | E1 (FloatOfString, e1) ->
        of_string e1 "float_of_string" "stod"
    | E1 (U8OfString, e1) ->
        of_string e1 "u8_of_string" "stoul"
    | E1 (U16OfString, e1) ->
        of_string e1 "u16_of_string" "stoul"
    | E1 (U24OfString, e1) ->
        of_string e1 "u24_of_string" "stoul"
    | E1 (U32OfString, e1) ->
        of_string e1 "u32_of_string" "stoul"
    | E1 (U40OfString, e1) ->
        of_string e1 "u40_of_string" "stoull"
    | E1 (U48OfString, e1) ->
        of_string e1 "u48_of_string" "stoull"
    | E1 (U56OfString, e1) ->
        of_string e1 "u56_of_string" "stoull"
    | E1 (U64OfString, e1) ->
        of_string e1 "u64_of_string" "stoull"
    | E1 (I8OfString, e1) ->
        of_string e1 "i8_of_string" "stol"
    | E1 (I16OfString, e1) ->
        of_string e1 "i16_of_string" "stol"
    | E1 (I24OfString, e1) ->
        of_string e1 "i24_of_string" "stol"
    | E1 (I32OfString, e1) ->
        of_string e1 "i32_of_string" "stol"
    | E1 (I40OfString, e1) ->
        of_string e1 "i40_of_string" "stoll"
    | E1 (I48OfString, e1) ->
        of_string e1 "i48_of_string" "stoll"
    | E1 (I56OfString, e1) ->
        of_string e1 "i60_of_string" "stoll"
    | E1 (I64OfString, e1) ->
        of_string e1 "i64_of_string" "stoll"
    | E1 ((I128OfString | U128OfString), e1) ->
        unary_func "i128_of_string" e1
    | E1 (CharOfPtr, e1) ->
        let n = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "char(%s.peekU8(0))" n)
    | E1 (FloatOfPtr, e1) ->
        let n = print p l e1 in
        let start = gen_sym "start_" in
        let stop = gen_sym "stop_" in
        let val_ = gen_sym "val_" in
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.offset };"
          start n n ;
        (* Nice but not supported yet on ordinary g++/clang:
        let res = gen_sym "res_" in
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.size };"
          stop n n ;
        ppi p.P.def "double %s { 0. /* don't warn */ };" val_ ;
        ppi p.P.def "bool const is_hex_ { %s > %s + 1 && *%s == '0' && \
                    (*%s== 'x' || *%s== 'X') };"
          stop start start start start ;
        ppi p.P.def "if (is_hex_) %s += 2;" start ;
        ppi p.P.def "struct std::from_chars_result %s = \
                    std::from_chars(\
                      %s + (is_hex_ ? 2 : 0), %s, %s, \
                      is_hex_ ? std::chars_format::hex : \
                                std::chars_format::general);"
          res start stop val_ ;
        emit ?name p l e (fun oc ->
          pp oc "%s, %s.skip(%s.ptr - %s)" val_ n res start)
        *)
        ppi p.P.def "char *%s;" stop ;
        (* This assumes there will always be a non-digit at the end to prevent
         * strtod to read past the end of the buffer: *)
        ppi p.P.def "double const %s = strtod(%s, &%s);" val_ start stop ;
        emit ?name p l e (fun oc ->
          pp oc "%s, %s.skip(%s - %s)" val_ n stop start)
    | E1 ((U8OfPtr | U16OfPtr | U24OfPtr | U32OfPtr | U40OfPtr |
             U48OfPtr | U56OfPtr | U64OfPtr |
             I8OfPtr | I16OfPtr | I24OfPtr | I32OfPtr | I40OfPtr |
             I48OfPtr | I56OfPtr | I64OfPtr), e1) ->
        let n = print p l e1 in
        let tn = E.type_of l e |> T.pair_of_tpair |> fst |> type_identifier_mn p in
        let start = gen_sym "start_" in
        let stop = gen_sym "stop_" in
        let val_ = gen_sym "val_" in
        let res = gen_sym "res_" in
        ppi p.P.def "%s %s { 0 /* don't warn */ };" tn val_ ;
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.offset };"
          start n n ;
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.size };"
          stop n n ;
        ppi p.P.def "struct std::from_chars_result %s = \
                    std::from_chars(%s, %s, %s);" res start stop val_ ;
        emit ?name p l e (fun oc ->
          pp oc "%s, %s.skip(%s.ptr - %s)" val_ n res start)
    | E1 (U128OfPtr, e1) ->
        let n = print p l e1 in
        let tn = E.type_of l e |> T.pair_of_tpair |> fst |> type_identifier_mn p in
        let start = gen_sym "start_" in
        let stop = gen_sym "stop_" in
        let val_ = gen_sym "val_" in
        ppi p.P.def "%s %s { 0 /* don't warn */ };" tn val_ ;
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.offset };"
          start n n ;
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.size };"
          stop n n ;
        ppi p.P.def "std::size_t count_ = u128_from_chars(%s, %s, &%s);"
          start stop val_ ;
        emit ?name p l e (fun oc -> pp oc "%s, %s.skip(count_)" val_ n)
    | E1 (I128OfPtr, e1) ->
        let n = print p l e1 in
        let tn = E.type_of l e |> T.pair_of_tpair |> fst |> type_identifier_mn p in
        let start = gen_sym "start_" in
        let stop = gen_sym "stop_" in
        let val_ = gen_sym "val_" in
        ppi p.P.def "%s %s { 0 /* don't warn */ };" tn val_ ;
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.offset };"
          start n n ;
        ppi p.P.def "char const *%s { (char *)%s.buffer.get() + %s.size };"
          stop n n ;
        ppi p.P.def "std::size_t count_ = i128_from_chars(%s, %s, &%s);"
          start stop val_ ;
        emit ?name p l e (fun oc -> pp oc "%s, %s.skip(count_)" val_ n)
    | E1 (FloatOfU64, e1) ->
        unary_func "float_of_qword" e1
    | E1 (U64OfFloat, e1) ->
        unary_func "qword_of_float" e1
    | E1 (StringOfFloat, e1) ->
        unary_func "hex_string_of_float" e1
    | E1 (DecimalStringOfFloat, e1) ->
        unary_func "dec_string_of_float" e1
    | E1 (StringOfChar, e1) ->
        let n = print p l e1 in
        (* This will use the list-initializer. Beware that "1, %s" would _also_ use
         * the list initializer, not the (count, char) constructor! *)
        emit ?name p l e (fun oc -> pp oc "%s" n)
    | E1 (U8OfChar, e1) | E1 (CharOfU8, e1)
    | E1 (SizeOfU32, e1) | E1 (U32OfSize, e1)
    | E1 (AddressOfU64, e1) | E1 (U64OfAddress, e1)
    | E1 ((ToU8 | ToI8 | ToU16 | ToI16 | ToU24 | ToI24 | ToU32 | ToI32 |
             ToU40 | ToI40 | ToU48 | ToI48 | ToU56 | ToI56 | ToU64 | ToI64 |
             ToU128 | ToI128 | ToFloat), e1)
    | E1 (U8OfBool, e1) | E1 (BoolOfU8, e1) ->
        let n = print p l e1 in
        let t = E.type_of l e in
        emit ?name p l e (fun oc ->
          print_cast p t (fun oc -> pp oc "%s" n) oc)
    | E1 (ArrOfLst, e1) ->
        method_call e1 "toList" []
    | E1 (ArrOfLstRev, e1) ->
        method_call e1 "toListRev" []
    | E1 (SetOfLst, e1) ->
        method_call e1 "toSet" []
    | E1 (ArrOfVec, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E1 (ArrOfSet, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "&%s" n1)
    | E2 (AppendByte, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E2 (AppendBytes, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E2 (AppendString, e1, e2) ->
        binary_infix_op e1 "+" e2
    | E2 ((StartsWith | EndsWith as op), e1, e2) ->
        let op = match op with StartsWith -> "starts_with" | _ -> "ends_with" in
        method_call e1 op [ e2 ]
    | E1 (StringLength, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "(uint32_t)%s.size()" n1)
    | E1 (BytesLength, e1) ->
        method_call e1 "length" []
    | E1 (Cardinality, e1) ->
        method_call e1 "size" []
    | E1 (StringOfBytes, e1) ->
        method_call e1 "toString" []
    | E1 (BytesOfString, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E1 (PtrOfString, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E1 (PtrOfBuffer, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s" n1)
    | E2 (PtrOfAddress, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E1 (GetEnv, e1) ->
        let n1 = print p l e1 in
        let res = gen_sym "getenv_res_" in
        ppi p.P.def "char *%s { std::getenv(%s.c_str()) };" res n1 ;
        emit ?name p l e (fun oc ->
          pp oc "%s == nullptr ? std::nullopt : std::make_optional(%s)" res res)
    | E3 (PtrOfPtr, e1, e2, e3) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3 in
        emit ?name p l e (fun oc -> pp oc "%s, %s, %s" n1 n2 n3)
    | E2 (GetBit, e1, e2) ->
        method_call e1 "getBit" [ e2 ]
    | E3 (SetBit, e1, e2, e3) ->
        method_call e1 "setBit" [ e2 ; e3 ]
    | E3 (SetVec, e1, e2, e3) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3 in
        emit ?name p l e (fun oc -> pp oc "%s[%s] = %s" n2 n1 n3)
    | E1 (ReadU8, e1) ->
        method_call e1 "readU8" []
    | E1 (ReadU16 LittleEndian, e1) ->
        method_call e1 "readU16Le" []
    | E1 (ReadU16 BigEndian, e1) ->
        method_call e1 "readU16Be" []
    | E1 (ReadU32 LittleEndian, e1) ->
        method_call e1 "readU32Le" []
    | E1 (ReadU32 BigEndian, e1) ->
        method_call e1 "readU32Be" []
    | E1 (ReadU64 LittleEndian, e1) ->
        method_call e1 "readU64Le" []
    | E1 (ReadU64 BigEndian, e1) ->
        method_call e1 "readU64Be" []
    | E1 (ReadU128 LittleEndian, e1) ->
        method_call e1 "readU128Le" []
    | E1 (ReadU128 BigEndian, e1) ->
        method_call e1 "readU128Be" []
    | E2 (ReadBytes, e1, e2) ->
        method_call e1 "readBytes" [ e2 ]
    | E2 (PeekU8, e1, e2) ->
        method_call e1 "peekU8" [ e2 ]
    | E2 (PeekU16 LittleEndian, e1, e2) ->
        method_call e1 "peekU16Le" [ e2 ]
    | E2 (PeekU16 BigEndian, e1, e2) ->
        method_call e1 "peekU16Be" [ e2 ]
    | E2 (PeekU32 LittleEndian, e1, e2) ->
        method_call e1 "peekU32Le" [ e2 ]
    | E2 (PeekU32 BigEndian, e1, e2) ->
        method_call e1 "peekU32Be" [ e2 ]
    | E2 (PeekU64 LittleEndian, e1, e2) ->
        method_call e1 "peekU64Le" [ e2 ]
    | E2 (PeekU64 BigEndian, e1, e2) ->
        method_call e1 "peekU64Be" [ e2 ]
    | E2 (PeekU128 LittleEndian, e1, e2) ->
        method_call e1 "peekU128Le" [ e2 ]
    | E2 (PeekU128 BigEndian, e1, e2) ->
        method_call e1 "peekU128Be" [ e2 ]
    | E2 (WriteU8, e1, e2) ->
        method_call e1 "writeU8" [ e2 ]
    | E2 (WriteU16 LittleEndian, e1, e2) ->
        method_call e1 "writeU16Le" [ e2 ]
    | E2 (WriteU16 BigEndian, e1, e2) ->
        method_call e1 "writeU16Be" [ e2 ]
    | E2 (WriteU32 LittleEndian, e1, e2) ->
        method_call e1 "writeU32Le" [ e2 ]
    | E2 (WriteU32 BigEndian, e1, e2) ->
        method_call e1 "writeU32Be" [ e2 ]
    | E2 (WriteU64 LittleEndian, e1, e2) ->
        method_call e1 "writeU64Le" [ e2 ]
    | E2 (WriteU64 BigEndian, e1, e2) ->
        method_call e1 "writeU64Be" [ e2 ]
    | E2 (WriteU128 LittleEndian, e1, e2) ->
        method_call e1 "writeU128Le" [ e2 ]
    | E2 (WriteU128 BigEndian, e1, e2) ->
        method_call e1 "writeU128Be" [ e2 ]
    | E2 (WriteBytes, e1, e2) ->
        method_call e1 "writeBytes" [ e2 ]
    | E2 (PokeU8, e1, e2) ->
        method_call e1 "pokeU8" [ e2 ]
    | E3 (BlitByte, e1, e2, e3) ->
        method_call e1 "blitBytes" [ e2 ; e3 ]
    | E2 (PtrAdd, e1, e2) ->
        method_call e1 "skip" [ e2 ]
    | E2 (PtrSub, e1, e2) ->
        binary_infix_op e1 "-" e2
    | E2 (Rewind, e1, e2) ->
        method_call e1 "rewind" [ e2 ]
    | E1 (RemSize, e1) ->
        method_call e1 "remSize" []
    | E1 (Offset, e1) ->
        method_call e1 "getOffset" []
    | E2 (And, e1, e2) ->
        shortcutting_binary_infix_op e1 e2 false
    | E2 (Or, e1, e2) ->
        shortcutting_binary_infix_op e1 e2 true
    | E1 (Not, e1) ->
        unary_op "!" e1
    | E1 (Abs, e1) ->
        unary_func "std::abs" e1
    | E1 (Neg, e1) ->
        unary_op "-" e1
    | E1 (Exp, e1) ->
        unary_func "std::exp" e1
    | E1 (Log, e1) ->
        unary_func "std::log" e1 |> null_of_nan
    | E1 (UnsafeLog, e1) ->
        unary_func "std::log" e1
    | E1 (Log10, e1) ->
        unary_func "std::log10" e1 |> null_of_nan
    | E1 (UnsafeLog10, e1) ->
        unary_func "std::log10" e1
    | E1 (Sqrt, e1) ->
        unary_func "std::sqrt" e1 |> null_of_nan
    | E1 (UnsafeSqrt, e1) ->
        unary_func "std::sqrt" e1
    | E1 (Ceil, e1) ->
        unary_func "std::ceil" e1
    | E1 (Floor, e1) ->
        unary_func "std::floor" e1
    | E1 (Round, e1) ->
        unary_func "std::round" e1
    | E1 (Cos, e1) ->
        unary_func "std::cos" e1
    | E1 (Sin, e1) ->
        unary_func "std::sin" e1
    | E1 (Tan, e1) ->
        unary_func "std::tan" e1 |> null_of_nan
    | E1 (ACos, e1) ->
        unary_func "std::acos" e1 |> null_of_nan
    | E1 (ASin, e1) ->
        unary_func "std::asin" e1 |> null_of_nan
    | E1 (ATan, e1) ->
        unary_func "std::atan" e1
    | E1 (CosH, e1) ->
        unary_func "std::cosh" e1
    | E1 (SinH, e1) ->
        unary_func "std::sinh" e1
    | E1 (TanH, e1) ->
        unary_func "std::tanh" e1
    | E1 ((Lower | Upper as op), e1) ->
        (* FIXME: proper UTF-8 + use a lib for proper lower/upper casing *)
        let n1 = print p l e1 in
        (* Beware that n1 is used twice below so we must "un-inlining" it: *)
        let tmp1 = gen_sym ?name "str_" in
        ppi p.P.def "std::string const &%s { %s };" tmp1 n1 ;
        let res = gen_sym ?name "case_str_" in
        ppi p.P.def "std::string %s(%s.length(), ' ');" res tmp1 ;
        let op = match op with Lower -> "tolower" | _ -> "toupper" in
        ppi p.P.def "std::transform(%s.cbegin(), %s.cend(), %s.begin(), ::%s);"
          tmp1 tmp1 res op ;
        res
    | E1 (Hash, e1) ->
        let n1 = print p l e1 in
        let t = E.type_of l e1 in
        let tn = type_identifier_mn p t in
        emit ?name p l e (fun oc -> pp oc "uint64_t(std::hash<%s>{}(%s))" tn n1)
    | E0 (EndOfList _) ->
        (* Default constructor cannot be called with no-args as that would
         * not be C++ish enough: *)
        let res = gen_sym ?name "endoflist_" in
        let t = E.type_of l e in
        let tn = type_identifier_mn p t in
        ppi p.P.def "%s %s;" tn res ;
        res
    | E0 (EmptySet mn) ->
        let tn = type_identifier_mn p mn in
        emit ?name p l e (fun oc ->
          pp oc "new SimpleSet<%s>()" tn)
    | E0 Now ->
        emit ?name p l e (fun oc ->
          pp oc "std::chrono::duration<double>(std::chrono::high_resolution_clock::now().time_since_epoch()).count()")
    | E0 RandomFloat ->
        emit ?name p l e (fun oc -> pp oc "_random_float_(_random_engine_)")
    | E0 RandomU8 ->
        emit ?name p l e (fun oc -> pp oc "_random_u8_(_random_engine_)")
    | E0 RandomU32 ->
        emit ?name p l e (fun oc -> pp oc "_random_u32_(_random_engine_)")
    | E0 RandomU64 ->
        emit ?name p l e (fun oc -> pp oc "_random_u64_(_random_engine_)")
    | E0 RandomU128 ->
        emit ?name p l e (fun oc ->
          pp oc "_random_u64_(_random_engine_) |\
                 ((uint128_t)_random_u64_(_random_engine_) << 64)")
    | E2 (Cons, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E1 (Head, e1) ->
        method_call e1 "head" []
    | E1 (Tail, e1) ->
        method_call e1 "tail" []
    | E2 ((Min | Max as op), e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and op = match op with Min -> "min" | _ -> "max" in
        let t = E.type_of l e in
        let tn = type_identifier_mn p t in
        emit ?name p l e (fun oc -> pp oc "std::%s<%s>(%s, %s)" op tn n1 n2)
    | E2 (Member, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        (* un-inline n2, used several times below: *)
        let tmp2 = gen_sym ?name "set_" in
        ppi p.P.def "auto const &%s { %s };" tmp2 n2 ;
        emit ?name p l e (fun oc ->
          pp oc "std::end(%s) != std::find(std::begin(%s), std::end(%s), %s)"
            tmp2 tmp2 tmp2 n1)
    | E0 (Identifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            valid_identifier s)
    | E0 (ExtIdentifier (Verbatim s)) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            s)
    | E0 (ExtIdentifier (Method { typ ; meth })) ->
        emit ?name p l e (fun oc ->
          pp oc "dessser::gen::%s::%s"
            (valid_identifier typ)
            (string_of_type_method meth |> valid_identifier))
    | E2 (Let (n, r), e1, e2) ->
        let n1 = print p l e1 in
        let t = E.get_memo_mn r l e1 in
        let tn = type_identifier_mn p t in
        let l = E.add_local n t l in
        let t2 = E.type_of l e2 in
        let has_res = not (T.eq_mn t2 T.void) in
        let res = if has_res then gen_sym ?name "let_res_" else "VOID" in
        if has_res then ppi p.P.def "%s %s;" (type_identifier_mn p t2) res ;
        ppi p.P.def "{" ;
        P.indent_more p (fun () ->
          ppi p.P.def "%s %s { %s };" tn (valid_identifier n) n1 ;
          let tmp = print p l e2 in
          if has_res then ppi p.P.def "%s = %s;" res tmp) ;
        ppi p.P.def "}" ;
        res
    | E2 (LetPair (name1, r1, name2, r2), e1, e2) ->
        let n1 = print p l e1 in
        let t1 = E.get_memo_mn r1 l (E.Ops.first e1)
        and t2 = E.get_memo_mn r2 l (E.Ops.secnd e1) in
        let l = E.add_local name1 t1 l |>
                E.add_local name2 t2 in
        let t2 = E.type_of l e2 in
        let has_res = not (T.eq_mn t2 T.void) in
        let res = if has_res then gen_sym ?name "letpair_res_" else "VOID" in
        if has_res then ppi p.P.def "%s %s;" (type_identifier_mn p t2) res ;
        ppi p.P.def "{" ;
        P.indent_more p (fun () ->
          ppi p.P.def "auto %s { std::get<0>(%s) };"
            (valid_identifier name1) n1 ;
          ppi p.P.def "auto %s { std::get<1>(%s) };"
            (valid_identifier name2) n1 ;
          let tmp = print p l e2 in
          if has_res then ppi p.P.def "%s = %s;" res tmp) ;
        ppi p.P.def "}" ;
        res
    | E1 (Function ts, e1) ->
        (* Pick the name here so we can add it to the environment, where it
         * can later be found by Myself: *)
        let name = gen_sym ?name "fun" in
        emit ?name:(Some name) p l e (fun oc ->
          (* Make sure this function can be called recursively by capturing
           * its name: *)
          let first = "[&"^ name ^"](" in
          array_print_i ~first ~last:") {\n" ~sep:", "
            (fun i oc t -> Printf.fprintf oc "%s%s %s"
              (type_identifier_mn p t)
              (if is_mutable t then "&" else "")
              (param i))
            oc ts ;
          let l = E.enter_function ~name ~ts l in
          P.indent_more p (fun () ->
            let n = print p l e1 in
            print_return n p) ;
          pp oc "%s}\n" p.P.indent ;
          pp oc "%s" p.P.indent)
    | E0 (Param n) ->
        param n
    | E0 (Myself _) ->
        (match l.E.name with
        | None -> invalid_arg "print Myself while function name is unknown"
        | Some n -> n)
    | E3 (If, e1, e2, e3) ->
        let cond = print p l e1 in
        let t2 = E.type_of l e2 in
        let has_res = not (T.eq_mn t2 T.void) in
        let res = if has_res then gen_sym ?name "choose_res_" else "VOID" in
        if has_res then ppi p.P.def "%s %s;" (type_identifier_mn p t2) res ;
        ppi p.P.def "if (%s) {" cond ;
        P.indent_more p (fun () ->
          let n = print p l e2 in
          if has_res then ppi p.P.def "%s = %s;" res n) ;
        ppi p.P.def "} else {" ;
        P.indent_more p (fun () ->
          let n = print p l e3 in
          if has_res then ppi p.P.def "%s = %s;" res n) ;
        ppi p.P.def "}" ;
        res
    | E2 (While, cond, body) ->
        let flag = gen_sym ?name "while_flag_" in
        ppi p.P.def "bool %s { true };" flag ;
        ppi p.P.def "do {" ;
        P.indent_more p (fun () ->
          let cond = print p l cond in
          ppi p.P.def "%s = %s;" flag cond ;
          ppi p.P.def "if (%s) {" flag ;
          P.indent_more p (fun () ->
            print p l body |> ignore) ;
          ppi p.P.def "}") ;
        ppi p.P.def "} while (%s);" flag ;
        "VOID"
    | E2 (ForEach (n, r), lst, body) ->
        let n1 = valid_identifier n in
        let t = E.get_memo_item_mn r l lst in
        let lst_t = E.type_of l lst |> T.develop_mn in
        let lst = print p l lst in
        let item_tn = type_identifier_mn p t in
        let is_set =
          match lst_t with
          | T.{ typ = TSet _ ; _ } -> true
          | _ -> false in
        if is_set then
          ppi p.P.def "%s->iter([&](%s &%s) {" lst item_tn n1
        else
          ppi p.P.def "for (%s %s : %s) {" item_tn n1 lst ;
        P.indent_more p (fun () ->
          let l = E.add_local n t l in
          print p l body |> ignore) ;
        if is_set then
          ppi p.P.def "});"
        else
          ppi p.P.def "}" ;
        "VOID"
    | E2 (Index, chr, str) ->
        let chr = print p l chr in
        let str = print p l str in
        let pos = gen_sym ?name "pos_" in
        ppi p.P.def "std::size_t %s { %s.find(%s) };"
          pos str chr ;
        emit ?name p l e (fun oc ->
          pp oc "%s != std::string::npos ? \
                  std::make_optional(%s) : std::nullopt" pos pos)
    | E3 (Map, init, f, lst) ->
        let init = print p l init
        and f = print p l f
        and lst = print p l lst in
        emit ?name p l e (fun oc -> pp oc "%s, %s, %s" init f lst)
    | E1 (GetItem n, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "std::get<%d>(%s)" n n1)
    | E1 (GetField s, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 (valid_identifier s))
    | E1 (GetAlt s, e1) ->
        (match E.type_of l e1 |> T.develop_mn with
        | T.{ typ = TSum mns ; nullable = false } ->
            let n1 = print p l e1 in
            let lbl = Array.findi (fun (n, _) -> n = s) mns in
            emit ?name p l e (fun oc ->
              Printf.fprintf oc "std::get<%d>(%s)" lbl n1)
        | _ ->
            assert false)
    | E1 (Construct (_, i), e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "std::in_place_index<%d>, %s" i n1)
    | E1 (Assert, e1) ->
        let n = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "assert(%s)" n)
    | E1 (MaskGet i, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s.get(%d)" n1 i)
    | E1 (LabelOf, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "uint16_t(%s.index())" n1)
    | E0 CopyField ->
        emit ?name p l e (fun oc -> pp oc "Mask::COPY")
    | E0 SkipField ->
        emit ?name p l e (fun oc -> pp oc "Mask::SKIP")
    | E0 SetFieldNull ->
        emit ?name p l e (fun oc -> pp oc "Mask::SET_NULL")
    | E1 (SlidingWindow mn, e1) ->
        let n1 = print p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier_mn p mn in
        let res = gen_sym ?name "sliding_win_" in
        ppi p.P.def "SlidingWindow<%s> *%s = new SlidingWindow<%s>(%s);"
          tn res tn n1 ;
        res
    | E1 (TumblingWindow mn, e1) ->
        let n1 = print p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier_mn p mn in
        let res = gen_sym ?name "tumbling_win_" in
        ppi p.P.def "TumblingWindow<%s> *%s = new TumblingWindow<%s>(%s);"
          tn res tn n1 ;
        res
    | E1 (Sampling mn, e1) ->
        let n1 = print p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier_mn p mn in
        let res = gen_sym ?name "sampling_" in
        ppi p.P.def "Sampling<%s> *%s = new Sampling<%s>(%s);"
          tn res tn n1 ;
        res
    | E1 (HashTable mn, e1) ->
        let n1 = print p l e1 in
        (* Cannot use emit since we want to select a specific type of set: *)
        let tn = type_identifier_mn p mn in
        let res = gen_sym ?name "hash_table_" in
        ppi p.P.def "HashTable<%s> *%s = new HashTable<%s>(%s);"
          tn res tn n1 ;
        res
    | E1 (Heap, cmp) ->
        let n1 = print p l cmp in
        (* Cannot use emit since we want to select a specific type of set: *)
        let item_t = E.get_compared_type l cmp in
        let tn = type_identifier_mn p item_t in
        let res = gen_sym ?name "heap_" in
        ppi p.P.def "Heap<%s> *%s = new Heap<%s>(%s);"
          tn res tn n1 ;
        res
    | E1 (GetMin, set) ->
        let set = print p l set in
        emit ?name p l e (fun oc -> pp oc "%s->getMin();" set)
    | E1 (AllocVec d, init) ->
        let init = print p l init in
        let t = E.type_of l e in
        (* We want the size+item constructor, not a list initialization, so
         * since Arr is a std::vector we must avoid brace-initializer.
         * Work around: *)
        let tmp = gen_sym "vec" in
        ppi p.P.def "%s %s(std::size_t(%d), %s); /* size+init constructor */"
          (type_identifier_mn p t) tmp d init ;
        emit ?name p l e (fun oc -> pp oc "%s" tmp)
    | E1 (Convert _, _)
    | E2 (NullMap _, _, _) ->
        assert false (* because of type checking *)
    | E2 (Insert, set, x) ->
        let set = print p l set in
        let x = print p l x in
        (* Do not use [emit] to avoid generating more identifiers: *)
        ppi p.P.def "%s->insert(%s);" set x ;
        "VOID"
    | E2 (DelMin, set, n) ->
        let set = print p l set in
        let n = print p l n in
        ppi p.P.def "%s->delMin(%s);" set n ;
        "VOID"
    | E2 (SplitBy, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "string_split(%s, %s)" n1 n2)
    | E2 (SplitAt, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "%s.substr(0, %s), %s.substr(%s)" n2 n1 n2 n1)
    | E2 (Join, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "string_join(%s, %s)" n1 n2)
    | E2 (AllocArr, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and t = E.type_of l e in
        (* We want the size+item constructor, not a list initialization, so
         * since Arr is a std::vector we must avoid brace-initializer.
         * Work around: *)
        let tmp = gen_sym "arr" in
        ppi p.P.def "%s %s(%s, %s); /* size+init constructor */"
          (type_identifier_mn p t) tmp n1 n2 ;
        emit ?name p l e (fun oc -> pp oc "%s" tmp)
    | E2 (PartialSort, e1, e2) ->
        let n1 = print ?name p l e1
        and n2 = print p l e2 in
        ppi p.P.def "%s.partial_sort(%s);" n1 n2 ;
        n1
    | E2 ((ChopBegin | ChopEnd as op), lst, len) ->
        let op = match op with ChopBegin -> "chopBegin" | _ -> "chopEnd" in
        method_call lst op [ len ]
    | E2 (ScaleWeights, set, d) ->
        let set = print p l set in
        let d = print p l d in
        ppi p.P.def "%s->scale(%s);" set d ;
        "VOID"
    | E2 (Strftime, fmt, time) ->
        let fmt = print p l fmt
        and time = print p l time
        and buf = gen_sym ?name "buf" in
        (* 256 bytes should be enough for every dates *)
        ppi p.P.def "char %s[256];" buf ;
        emit ?name p l e (fun oc ->
          pp oc "0 == std::strftime(%s, sizeof(%s), %s, %s) ? \
            \"date too long\" : %s"
          buf buf fmt time buf)
    | E3 (FindSubstring, e1, e2, e3) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3 in
        let pos = gen_sym ?name "pos_" in
        ppi p.P.def "std::size_t %s { %s ? %s.find(%s) : %s.rfind(%s) };"
          pos n1 n3 n2 n3 n2 ;
        emit ?name p l e (fun oc ->
          pp oc "%s != std::string::npos ? \
                  std::make_optional(%s) : std::nullopt" pos pos)
    | E3 (Top _, _, _, _) ->
        todo "C++ back-end for TOPs"
    | E3 (InsertWeighted, set, w, x) ->
        let set = print p l set
        and w = print p l w
        and x = print p l x in
        (* Do not use [emit] to avoid generating more identifiers: *)
        ppi p.P.def "%s->insertWeighted(%s, %s);" set w x ;
        "VOID"
    | E3 (SubString, str, start, stop) ->
        let str = print p l str
        and start = print p l start
        and stop = print p l stop in
        let len = gen_sym "len_" in
        ppi p.P.def "std::size_t const %s { %s.size() };" len str ;
        let clamp_start = gen_sym "start_" in
        ppi p.P.def "std::size_t const %s { clamp_to_length(%s, %s) };"
          clamp_start start len ;
        let clamp_stop = gen_sym "stop_" in
        ppi p.P.def "std::size_t const %s { clamp_to_length(%s, %s) };"
          clamp_stop stop len ;
        emit ?name p l e (fun oc ->
          pp oc "%s, %s, (%s - %s)" str start stop start)

  let print_binding_toplevel emit n p l e =
    (* In C++ toplevel expressions cannot be initialized with arbitrary code so we
     * must rely on a static function to produce the value: *)
    let t = E.type_of l e in
    let tn = type_identifier_mn p t in
    pp p.P.def "%sstatic %s %s_init()\n" p.P.indent tn n ;
    pp p.P.def "%s{\n" p.P.indent ;
    P.indent_more p (fun () ->
      let n = print emit p l e in
      print_return n p) ;
    pp p.P.def "%s}\n" p.P.indent ;
    pp p.P.def "%s%s %s(%s_init());\n\n" p.P.indent tn n n

  let print_identifier_declaration n p l e =
    let t = E.type_of l e in
    let tn = type_identifier_mn p t in
    pp p.P.def "%s%s %s;\n" p.P.indent tn n

  let source_intro module_name mode =
    let m = valid_identifier module_name in
    match mode with
    | P.Declaration ->
        "#ifndef DESSSER_GEN_"^ m ^"\n\
         #define DESSSER_GEN_"^ m ^"\n\
         #include <arpa/inet.h>\n\
         #include <functional>\n\
         #include <optional>\n\
         #include <tuple>\n\
         #include <variant>\n\
         #include <vector>\n\
         #include \"dessser/runtime.h\"\n\
         \n\
         namespace dessser::gen::"^ m ^" {\n\
         // don't ask me why:\n\
         using dessser::operator<<;\n\n"
    | P.Definition ->
        "#include <algorithm>\n\
         #include <arpa/inet.h>\n\
         #include <charconv>\n\
         #include <chrono>\n\
         #include <cmath>\n\
         #include <cstdlib>\n\
         #include <ctime>\n\
         #include <exception>\n\
         #include <fstream>\n\
         #include <functional>\n\
         #include <iostream>\n\
         #include <optional>\n\
         #include <random>\n\
         #include <tuple>\n\
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
         namespace dessser::gen::"^ m ^" {\n\
         // don't ask me why:\n\
         using dessser::operator<<;\n\n"

  let source_outro _ = function
    | P.Declaration ->
        "\n}\n\
         #endif\n"
    | P.Definition ->
        "\n}\n"

  let adapt_type t = t
end

include DessserBackEndCLike.Make (Config)
