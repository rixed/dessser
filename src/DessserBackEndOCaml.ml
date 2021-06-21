open Batteries
open Stdint
open Dessser
open DessserBackEndCLike
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module P = DessserPrinter

let debug = false

module Config =
struct
  let id = T.OCaml

  let valid_identifier s =
    let keywords =
      [ "and" ; "as" ; "assert" ; "asr" ; "begin" ; "class" ; "constraint" ;
        "do" ; "done" ; "downto" ; "else" ; "end" ; "exception" ; "external" ;
        "false" ; "for" ; "fun" ; "function" ; "functor" ; "if" ; "in" ;
        "include" ; "inherit" ; "initializer" ; "land" ; "lazy" ; "let" ;
        "lor" ; "lsl" ; "lsr" ; "lxor" ; "match" ; "method" ; "mod" ; "module" ;
        "mutable" ; "new" ; "nonrec" ; "object" ; "of" ; "open" ; "or" ;
        "private" ; "rec" ; "sig" ; "struct" ; "then" ; "to" ; "true" ; "try" ;
        "type" ; "val" ; "virtual" ; "when" ; "while" ; "with" ] in
    if s = "" then "v" else
    if s.[0] = '!' then s else
    if List.mem s keywords then s ^ "_" else
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

  let preferred_comp_extension = function
  | Object -> "cmx"
  | SharedObject -> "cmxs"
  | Executable -> ""

  let compile_cmd ?(dev_mode=false) ?(extra_search_paths=[]) ?(optim=0) ~link src dst =
    let optim = clamp 2 3 optim in
    (* FIXME: path to src files! *)
    Printf.sprintf2
      "ocamlfind ocamlopt -g -annot -O%d -w -8-26 %a %s \
         -package batteries,lmdb,stdint%s \
         %s %S -o %S"
      optim
      (List.print ~first:"" ~last:"" ~sep:" " (fun oc path ->
        Printf.fprintf oc "-I %s" path)) extra_search_paths
      (if dev_mode then "-I src" else "")
      (if dev_mode then
         " src/DessserFloatTools.cmx \
           src/DessserTools.cmx \
           src/DessserLeftistHeap.cmx \
           src/DessserOCamlBackEndHelpers.cmx \
           src/libdessser_ext.a"
       else
         ",dessser")
      (match link with
      | Object -> "-c"
      | SharedObject -> "-shared"
      | Executable -> "-linkpkg")
      src dst

  let module_of_type t =
    valid_module_name (T.uniq_id t)

  let tuple_field_name i = "field_"^ string_of_int i

  let cstr_name n =
    String.capitalize (valid_identifier n)

  let mod_of_set_type = function
    | T.Simple -> "SimpleSet"
    | Sliding -> "SlidingWindow"
    | Tumbling -> "TumblingWindow"
    | Sampling -> "Sampling"
    | HashTable -> "HashTable"
    | Heap -> "Heap"
    | Top -> "Top"

 let mod_of_set_type_of_expr l set =
   match E.type_of l set |> T.develop_user_types with
   | Data { vtyp = Set (st, _) ; nullable = false } ->
       mod_of_set_type st
   | _ ->
       invalid_arg "mod_of_set_type_of_expr"

  let open_module m p oc f =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    (match p.context with
    | P.Definition -> ppi oc "module %s = struct" m
    | P.Declaration -> ppi oc "module %s : sig" m) ;
    P.indent_more p f ;
    ppi oc "end"

  let sum_has_arg (_, mn) =
    mn.T.vtyp <> Base Unit

  let rec print_record p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    let m = valid_module_name id in
    let id = valid_identifier id in
    open_module m p oc (fun () ->
      pp oc "%stype t = {\n" p.P.indent ;
      P.indent_more p (fun () ->
        Array.iter (fun (field_name, mn) ->
          let typ_id = type_identifier p (T.Data mn) in
          pp oc "%smutable %s : %s;\n"
            p.P.indent (valid_identifier field_name) typ_id
        ) mns
      ) ;
      pp oc "%s}\n" p.P.indent
    ) ;
    (* Also define the type alias: *)
    ppi oc "type %s = %s.t\n" id m

  and print_sum p oc id mns =
    let m = valid_module_name id in
    let id = valid_identifier id in
    open_module m p oc (fun () ->
      pp oc "%stype t =\n" p.P.indent ;
      P.indent_more p (fun () ->
        Array.iter (fun (n, mn as n_mn) ->
          if sum_has_arg n_mn then
            let typ_id = type_identifier p (T.Data mn) in
            pp oc "%s| %s of %s\n" p.P.indent (cstr_name n) typ_id
          else
            pp oc "%s| %s\n" p.P.indent (cstr_name n)
        ) mns
      ) ;
      pp oc "\n" ;
      (* Associated "label_of_cstr": *)
      match p.context with
      | P.Declaration ->
          pp oc "%sval label_of_cstr : t -> int\n" p.P.indent ;
      | P.Definition ->
          pp oc "%slet label_of_cstr = function\n" p.P.indent ;
          P.indent_more p (fun () ->
            Array.iteri (fun i (n, _ as n_mn) ->
              pp oc "%s| %s %s-> %d\n" p.P.indent
                (cstr_name n) (if sum_has_arg n_mn then "_ " else "") i
            ) mns
          )
    ) ;
    (* Also define the type alias: *)
    pp oc "%stype %s = %s.t\n\n" p.P.indent id m

  and value_type_identifier p = function
    | T.{ vtyp ; nullable = true } ->
        value_type_identifier p { vtyp ; nullable = false } ^" nullable"
    | { vtyp = Unknown ; _ } -> invalid_arg "value_type_identifier"
    | { vtyp = Base Unit ; _ } -> "unit"
    | { vtyp = Base Char ; _ } -> "char"
    | { vtyp = Base String ; _ } -> "string"
    | { vtyp = Base Bool ; _ } -> "bool"
    | { vtyp = Base Float ; _ } -> "float"
    | { vtyp = Base U8 ; _ } -> "Uint8.t"
    | { vtyp = Base I8 ; _ } -> "Int8.t"
    | { vtyp = Base U16 ; _ } -> "Uint16.t"
    | { vtyp = Base I16 ; _ } -> "Int16.t"
    | { vtyp = Base U24 ; _ } -> "Uint24.t"
    | { vtyp = Base I24 ; _ } -> "Int24.t"
    | { vtyp = Base U32 ; _ } -> "Uint32.t"
    | { vtyp = Base I32 ; _ } -> "Int32.t"
    | { vtyp = Base U40 ; _ } -> "Uint40.t"
    | { vtyp = Base I40 ; _ } -> "Int40.t"
    | { vtyp = Base U48 ; _ } -> "Uint48.t"
    | { vtyp = Base I48 ; _ } -> "Int48.t"
    | { vtyp = Base U56 ; _ } -> "Uint56.t"
    | { vtyp = Base I56 ; _ } -> "Int56.t"
    | { vtyp = Base U64 ; _ } -> "Uint64.t"
    | { vtyp = Base I64 ; _ } -> "Int64.t"
    | { vtyp = Base U128 ; _ } -> "Uint128.t"
    | { vtyp = Base I128 ; _ } -> "Int128.t"
    | { vtyp = Usr t ; _ } ->
        value_type_identifier p { vtyp = t.def ; nullable = false }
    | { vtyp = Ext n ; _ } ->
        P.get_external_type p n OCaml
    | { vtyp = (Vec (_, t) | Lst t) ; _ } ->
        value_type_identifier p t ^" array"
    | { vtyp = Set (st, t) ; _ } ->
        let m = mod_of_set_type st in
        value_type_identifier p t ^" "^ m ^".t"
    | { vtyp = Tup mns ; _ } as mn ->
        let t = T.Data mn in
        let mns = Array.mapi (fun i mn -> tuple_field_name i, mn) mns in
        P.declared_type p t (fun oc type_id -> print_record p oc type_id mns) |>
        valid_identifier
    | { vtyp = Rec mns ; _ } as mn ->
        let t = T.Data mn in
        P.declared_type p t (fun oc type_id -> print_record p oc type_id mns) |>
        valid_identifier
    | { vtyp = Sum mns ; _ } as mn ->
        let t = T.Data mn in
        P.declared_type p t (fun oc type_id -> print_sum p oc type_id mns) |>
        valid_identifier
    | { vtyp = Map _ ; _ } ->
        assert false (* no value of map type *)

  and type_identifier p = function
    | T.Data mn -> value_type_identifier p mn
    | T.Void -> "unit"
    | T.DataPtr -> "Pointer.t"
    | T.Size -> "Size.t"
    | T.Address -> "Uint64.t"
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
    | T.Data { vtyp = Base Char ; nullable = false } -> "Char"
    | T.Data { vtyp = Base String ; nullable = false } -> "String"
    | T.Data { vtyp = Base Bool ; nullable = false } -> "Bool"
    | T.Data { vtyp = Base Float ; nullable = false } -> "Float"
    | T.Data { vtyp = Base U8 ; nullable = false } -> "Uint8"
    | T.Data { vtyp = Base I8 ; nullable = false } -> "Int8"
    | T.Data { vtyp = Base U16 ; nullable = false } -> "Uint16"
    | T.Data { vtyp = Base I16 ; nullable = false } -> "Int16"
    | T.Data { vtyp = Base U24 ; nullable = false } -> "Uint24"
    | T.Data { vtyp = Base I24 ; nullable = false } -> "Int24"
    | T.Data { vtyp = Base U32 ; nullable = false } -> "Uint32"
    | T.Data { vtyp = Base I32 ; nullable = false } -> "Int32"
    | T.Data { vtyp = Base U40 ; nullable = false } -> "Uint40"
    | T.Data { vtyp = Base I40 ; nullable = false } -> "Int40"
    | T.Data { vtyp = Base U48 ; nullable = false } -> "Uint48"
    | T.Data { vtyp = Base I48 ; nullable = false } -> "Int48"
    | T.Data { vtyp = Base U56 ; nullable = false } -> "Uint56"
    | T.Data { vtyp = Base I56 ; nullable = false } -> "Int56"
    | T.Data { vtyp = Base U64 ; nullable = false } -> "Uint64"
    | T.Data { vtyp = Base I64 ; nullable = false } -> "Int64"
    | T.Data { vtyp = Base U128 ; nullable = false } -> "Uint128"
    | T.Data { vtyp = Base I128 ; nullable = false } -> "Int128"
    | T.Data { vtyp = Usr t ; nullable = false } ->
        mod_name (Data { vtyp = t.def ; nullable = false })
    | T.DataPtr -> "Pointer"
    | T.Size -> "Size"
    | T.Address -> "Uint64"
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
    | T.Data { vtyp = Base (U8 | I8 | U16 | I16 | U24 | I24 | U32 | I32 |
                            U40 | I40 | U48 | I48 | U56 | I56 | U64 | I64 |
                            U128 | I128 | Float) ; nullable = false }
    | T.(Byte | Word | DWord | QWord | OWord) as t ->
        String.lowercase (mod_name t)
    | T.Data { vtyp = Usr t ; nullable = false } ->
        num_name (Data { vtyp = t.def ; nullable = false })
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

  let print_float_literal oc v =
    (* printf "%F" would not work for infinity:
     * https://caml.inria.fr/mantis/view.php?id=7685
     * and "%h" not for neg_infinity. *)
    if v = infinity then String.print oc "infinity"
    else if v = neg_infinity then String.print oc "neg_infinity"
    else Legacy.Printf.sprintf "%h" v |> String.print oc

  let lift_i32 oc v =
    if Int32.(compare v (of_int (to_int v))) = 0 then
      pp oc "Int32.of_int (%ld)" v
    else
      pp oc "Int32.of_int32 (%ldl)" v

  let lift_i40 oc v =
    if Int40.(compare v (of_int (to_int v))) = 0 then
      pp oc "Int40.of_int (%d)" (Int40.to_int v)
    else
      pp oc "Int40.of_int64 (%sL)" (Int40.to_string v)

  let lift_i48 oc v =
    if Int48.(compare v (of_int (to_int v))) = 0 then
      pp oc "Int48.of_int (%d)" (Int48.to_int v)
    else
      pp oc "Int48.of_int64 (%sL)" (Int48.to_string v)

  let lift_i56 oc v =
    if Int56.(compare v (of_int (to_int v))) = 0 then
      pp oc "Int56.of_int (%d)" (Int56.to_int v)
    else
      pp oc "Int56.of_int64 (%sL)" (Int56.to_string v)

  (* Even for unsigned the parentheses are required because the relation
   *   of_int (to_int v) = v
   * might hold using a negative integer. *)
  let lift_u32 oc v =
    if Uint32.(compare v (of_int (to_int v))) = 0 then
      pp oc "Uint32.of_int (%d)" (Uint32.to_int v)
    else
      pp oc "Uint32.of_int32 (%ldl)" (Uint32.to_int32 v)

  let lift_u40 oc v =
    if Uint40.(compare v (of_int (to_int v))) = 0 then
      pp oc "Uint40.of_int (%d)" (Uint40.to_int v)
    else
      pp oc "Uint40.of_int64 (%LdL)" (Uint40.to_int64 v)

  let lift_u48 oc v =
    if Uint48.(compare v (of_int (to_int v))) = 0 then
      pp oc "Uint48.of_int (%d)" (Uint48.to_int v)
    else
      pp oc "Uint48.of_int64 (%LdL)" (Uint48.to_int64 v)

  let lift_u56 oc v =
    if Uint56.(compare v (of_int (to_int v))) = 0 then
      pp oc "Uint56.of_int (%d)" (Uint56.to_int v)
    else
      pp oc "Uint56.of_int64 (%LdL)" (Uint56.to_int64 v)

  let lift_u64 oc v =
    if Uint64.(compare v (of_int (to_int v))) = 0 then
      pp oc "Uint64.of_int (%d)" (Uint64.to_int v)
    else
      pp oc "Uint64.of_int64 (%LdL)" (Uint64.to_int64 v)

  let lift_u128 oc v =
    if Uint128.compare v (Uint128.of_int max_int) < 0 then
      pp oc "Uint128.of_int (%d)" (Uint128.to_int v)
    else (
      let bytes = Bytes.create 16 in
      Uint128.to_bytes_little_endian v bytes 0 ;
      pp oc "Uint128.of_bytes_little_endian (Bytes.of_string %S) 0"
        (Bytes.to_string bytes))

  let lift_i128 oc v =
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
    let unary_op_or_null op e1 =
      let n1 = print emit p l e1 in
      emit ?name p l e (fun oc ->
        pp oc "(try NotNull (%s %s)" op n1 ;
        pp oc " with _ as e ->" ;
        if debug then
          pp oc "   Printf.eprintf \"%s %%S\\n\" %s ;" op n1 ;
        pp oc "   Null)") in
    let unary_mod_op_or_null op e1 =
      let t =
        match E.type_of l e with
        | T.Data { vtyp ; _ } -> T.Data { vtyp ; nullable = false }
        | t -> t in
      let op = mod_name t ^"."^ op in
      unary_op_or_null op e1 in
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
        pp oc "%s.%s %s (Uint8.to_int %s)" m op n1 n2) in
    let conv_to_num e1 =
      let t = E.type_of l e
      and t1 = E.type_of l e1 in
      if T.eq t t1 then
        print emit p l e1
      else
        let op = mod_name t ^".of_"^ num_name t1 in
        unary_op op e1 in
    let to_float ?name emit p l e =
      let m = mod_name (E.type_of l e) in
      if m = "Float" then
        print ?name emit p l e
      else
        let n = print emit p l e in
        "("^ m ^".to_float "^ n ^")" in
    let preallocate printer i oc =
      let value= IO.to_string printer i in
      let uniq_id = "const_"^ Digest.(string value |> to_hex) in
      let def = "let "^ uniq_id ^" = "^ value ^"\n" in
      if not (List.mem def p.P.defs) then p.P.defs <- def :: p.P.defs ;
      String.print oc uniq_id
    in
    match e with
    | E.E1S (Apply, f, es) ->
        let nf = print emit p l f in
        let ns =
          if es = [] then [ "()" ]
          else
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
          let last = "\n"^ p.P.indent ^"}" in
          P.indent_more p (fun () ->
            let first = m ^".{\n"^ p.P.indent
            and sep = ";\n"^ p.P.indent in
            List.print ~first ~last ~sep
              (fun oc (name, n) ->
                Printf.fprintf oc "%s = %s" name n) oc inits))
    | E.E0S (MakeUsr n, ins) ->
        let e = E.apply_constructor e l n ins in
        print ?name emit p l e
    | E.E0S (Verbatim (temps, _), ins) ->
        let args = List.map (print emit p l) ins in
        emit ?name p l e (fun oc ->
          String.print oc (E.expand_verbatim id temps args))
    | E.E1 (Identity, e1) ->
        print ?name emit p l e1
    | E.E1 (Ignore, e1) ->
        let n = print emit p l e1 in
        ppi p.P.def "ignore %s ;" n ;
        "()"
    | E.E1 (Dump, e1) ->
        let n = print emit p l e1 in
        pp p.P.def ("%s"^^
          (match E.type_of l e1 |> T.develop_user_types with
          | Data { vtyp = Base String ; nullable = false } ->
              "print_string %s;"
          | Data { vtyp = Base Char ; nullable = false } ->
              "print_char %s;"
          | _ ->
              "print_string (Batteries.dump %s);") ^^"\n")
          p.P.indent n ;
        ppi p.P.def "flush stdout ;" ;
        "()"
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
    | E.E1 (Force what, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "Nullable.get %s%s"
            (if what = "" then "" else "~what:"^ String.quote what ^" ")
            n1)
    | E.E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "Null")
    | E.E0 (Float f) ->
        emit ?name p l e (preallocate print_float_literal f)
    | E.E0 Unit ->
        emit ?name p l e (fun oc -> pp oc "()")
    | E.E0 (String s) ->
        emit ?name p l e (preallocate String.print_quoted s)
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
        emit ?name p l e (preallocate lift_u32 u)
    | E.E0 (U40 u) ->
        emit ?name p l e (preallocate lift_u40 u)
    | E.E0 (U48 u) ->
        emit ?name p l e (preallocate lift_u48 u)
    | E.E0 (U56 u) ->
        emit ?name p l e (preallocate lift_u56 u)
    | E.E0 (QWord u) | E.E0 (U64 u) ->
        emit ?name p l e (preallocate lift_u64 u)
    | E.E0 (OWord u) | E.E0 (U128 u) ->
        emit ?name p l e (preallocate lift_u128 u)
    | E.E0 (Bytes s) ->
        emit ?name p l e (fun oc -> pp oc "Slice.of_string %S" (Bytes.to_string s))
    | E.E0 (I8 i) ->
        emit ?name p l e (fun oc -> pp oc "Int8.of_int (%s)" (Int8.to_string i))
    | E.E0 (I16 i) ->
        emit ?name p l e (fun oc -> pp oc "Int16.of_int (%s)" (Int16.to_string i))
    | E.E0 (I24 i) ->
        emit ?name p l e (fun oc -> pp oc "Int24.of_int (%s)" (Int24.to_string i))
    | E.E0 (I32 i) ->
        emit ?name p l e (preallocate lift_i32 i)
    | E.E0 (I40 i) ->
        emit ?name p l e (preallocate lift_i40 i)
    | E.E0 (I48 i) ->
        emit ?name p l e (preallocate lift_i48 i)
    | E.E0 (I56 i) ->
        emit ?name p l e (preallocate lift_i56 i)
    | E.E0 (I64 i) ->
        emit ?name p l e (preallocate (fun oc i -> pp oc "(%LdL)" i) i)
    | E.E0 (I128 i) ->
        emit ?name p l e (preallocate lift_i128 i)
    | E.E0 (Size s) ->
        emit ?name p l e (fun oc -> pp oc "Size.of_int (%d)" s)
    | E.E0 (Address a) ->
        emit ?name p l e (fun oc -> pp oc "%s" (Uint64.to_string a))
    | E.E2 (Gt, e1, e2) ->
        binary_infix_op e1 ">" e2
    | E.E2 (Ge, e1, e2) ->
        binary_infix_op e1 ">=" e2
    | E.E2 (Eq, e1, e2) ->
        binary_infix_op e1 "=" e2
    | E.E2 (Add, e1, e2) ->
        binary_mod_op "add" e1 e2
    | E.E2 (Sub, e1, e2) ->
        binary_mod_op "sub" e1 e2
    | E.E2 (Mul, e1, e2) ->
        binary_mod_op "mul" e1 e2
    | E.E2 ((Div | Rem as op), e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_user_types with
          | Data { vtyp = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                               |I8|I16|I24|I32|I40|I48|I56|I64|I128) ;
                     _ } as t ->
              let op_name = match op with Div -> "div" | _ -> "rem" in
              pp oc "try NotNull (%s.%s %s %s) with Division_by_zero -> Null"
                (mod_name t) op_name n1 n2
          | Data { vtyp = Base Float ; _ } ->
              let op_name = match op with Div -> "(/.)" | _ -> "Float.rem" in
              pp oc "Nullable.of_nan (%s %s %s)" op_name n1 n2
          | _ ->
              assert false)
    | E.E2 ((UnsafeDiv | UnsafeRem as op), e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_user_types with
          | Data { vtyp = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                               |I8|I16|I24|I32|I40|I48|I56|I64|I128) ;
                   _ } as t ->
              let op_name = match op with UnsafeDiv -> "div" | _ -> "rem" in
              pp oc "%s.%s %s %s" (mod_name t) op_name n1 n2
          | Data { vtyp = Base Float ; _ } ->
              let op_name = match op with UnsafeDiv -> "(/.)" | _ -> "Float.rem" in
              pp oc "%s %s %s" op_name n1 n2
          | _ ->
              assert false)
    | E.E2 (Pow, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_user_types with
          | Data { vtyp = Base Float ; _ } ->
              pp oc "Nullable.of_nan (%s ** %s)" n1 n2
          | Data { vtyp = Base (I32 | I64) ; _ } as t ->
              pp oc "try NotNull (Bat%s.pow %s %s) \
                     with Invalid_argument _ -> Null"
                (mod_name t) n1 n2
          | Data {
              vtyp = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                          |I8|I16|I24|I40|I48|I56|I128) ; _ } as t ->
              (* For through floats *)
              let m = mod_name t in
              pp oc "Nullable.map %s.of_float \
                       (Nullable.of_nan (%s.to_float %s ** %s.to_float %s))"
                m m n1 m n2
          | _ ->
              assert false (* because of type-checking *))
    | E.E2 (UnsafePow, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_user_types with
          | Data { vtyp = Base Float ; _ } ->
              pp oc "%s ** %s" n1 n2
          | Data { vtyp = Base (I32 | I64) ; _ } as t ->
              pp oc "Bat%s.pow %s %s" (mod_name t) n1 n2
          | Data {
              vtyp = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                          |I8|I16|I24|I40|I48|I56|I128) ; _ } as t ->
              (* For through floats *)
              let m = mod_name t in
              pp oc "%s.of_float (%s.to_float %s ** %s.to_float %s)"
                m m n1 m n2
          | _ ->
              assert false (* because of type-checking *))
    | E.E2 (BitAnd, e1, e2) ->
        binary_mod_op "logand" e1 e2
    | E.E2 (BitOr, e1, e2) ->
        binary_mod_op "logor" e1 e2
    | E.E2 (BitXor, e1, e2) ->
        binary_mod_op "logxor" e1 e2
    | E.E1 (BitNot, e1) ->
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
          | T.{ vtyp = Base U32 ; _ } ->
              ppi p.P.def "DessserIpTools.V4.to_string %s" n
          | T.{ vtyp = Base U128 ; _ } ->
              ppi p.P.def "DessserIpTools.V6.to_string %s" n
          | _ ->
              assert false (* because of type checking *)
        in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_user_types with
          | Data { vtyp = Sum mns ; _ } ->
              (* Since the type checking accept any sum type made of u32 and
               * u128, let's be as general as possible: *)
              ppi oc "match %s with\n" n1 ;
              P.indent_more p (fun () ->
                Array.iter (fun (cstr, mn) ->
                  ppi oc "| %s ip_ ->\n" (cstr_name cstr) ;
                  P.indent_more p (fun () -> case_u mn "ip_")
                ) mns)
          | Data mn ->
              case_u mn n1
          | _ ->
              assert false (* because of type checking *))
    | E.E1 (FloatOfString, e1) ->
        unary_op_or_null "float_of_string" e1
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
        unary_mod_op_or_null "of_string" e1
    | E.E1 (CharOfPtr, e1) ->
        let n = print emit p l e1 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "Char.chr (Uint8.to_int (%s.peekByte %s)), %s.skip %s 1"
            m n m n)
    | E.E1 (FloatOfPtr, e1) ->
        let n1 = print emit p l e1 in
        (* Note: Scanf uses two distinct format specifiers for "normal"
         * and hex notations so detect it and pick the proper one: *)
        pp p.P.def "%slet len_ = (fst %s).Pointer.stop - snd %s in\n"
          p.P.indent n1 n1 ;
        pp p.P.def "%slet is_hex_ = len_ > 2 && (\n" p.P.indent ;
        P.indent_more p (fun () ->
          pp p.P.def "%slet o_ =\n" p.P.indent ;
          P.indent_more p (fun () ->
            pp p.P.def "%slet c_ = peek_char %s (snd %s) in\n"
              p.P.indent n1 n1 ;
            pp p.P.def "%sif c_ = '-' || c_ = '+' then 1 else 0 in\n" p.P.indent) ;
          pp p.P.def "%slen_ > 2 + o_ && \
                      peek_char %s (snd %s + o_) = '0' && \
                      (let c2_ = peek_char %s (snd %s + o_ + 1) in \
                       c2_ = 'x' || c2_ = 'X')) in\n"
            p.P.indent n1 n1 n1 n1) ;
        pp p.P.def "%slet s_ =\n" p.P.indent ;
        P.indent_more p (fun () ->
          pp p.P.def "%slet off_ = ref (snd %s) in\n" p.P.indent n1 ;
          pp p.P.def "%sScanf.Scanning.from_function (fun () ->\n" p.P.indent ;
          P.indent_more p (fun () ->
            pp p.P.def "%sif !off_ >= (fst %s).stop then raise End_of_file ;\n"
              p.P.indent n1 ;
            pp p.P.def "%slet c_ = peek_char %s !off_ in\n"
              p.P.indent n1 ;
            pp p.P.def "%sincr off_ ;\n" p.P.indent ;
            pp p.P.def "%sc_) in\n" p.P.indent)) ;
        emit ?name p l e (fun oc ->
          ppi oc "(try" ;
          P.indent_more p (fun () ->
            ppi oc "Scanf.bscanf s_ (if is_hex_ then \" %%h%%n\" else \
                                                     \" %%f%%n\") \
                      (fun f_ o_ -> f_, Pointer.skip %s o_)" n1) ;
          ppi oc "with Scanf.Scan_failure msg_ ->" ;
          ppi oc "  Printf.eprintf \"Scanf failure: %%s\\n\" msg_ ;" ;
          ppi oc "  nan, %s)" n1)
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
        let m1 = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          P.indent_more p (fun () ->
            pp oc "\n%slet s_ = (fst %s).%s.impl.to_string () in\n"
              p.P.indent n1 m1 ;
            pp oc "%slet n_, o_ = %s.of_substring ~pos:(snd %s) s_ in\n"
              p.P.indent m n1 ;
            pp oc "%sn_, %s.skip %s (o_ - snd %s)" p.P.indent m1 n1 n1))
    | E.E1 (FloatOfQWord, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "BatInt64.float_of_bits (Uint64.to_int64 %s)" n)
    | E.E1 (QWordOfFloat, e1) ->
        let n = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "Uint64.of_int64 (BatInt64.bits_of_float %s)" n)
    | E.E1 (StringOfFloat, e1) ->
        unary_op "DessserFloatTools.hexstring_of_float" e1
    | E.E1 (StringOfChar, e1) ->
        unary_op "string_of_char" e1
    | E.E1 (ByteOfU8, e1) | E.E1 (U8OfByte, e1)
    | E.E1 (WordOfU16, e1) | E.E1 (U16OfWord, e1)
    | E.E1 (U32OfDWord, e1) | E.E1 (DWordOfU32, e1)
    | E.E1 (U64OfQWord, e1) | E.E1 (QWordOfU64, e1)
    | E.E1 (U128OfOWord, e1) | E.E1 (OWordOfU128, e1)
    | E.E1 (BitOfBool, e1) | E.E1 (BoolOfBit, e1)
    | E.E1 (ListOfVec, e1) ->
        (* Those are NOPs *)
        print ?name emit p l e1
    | E.E1 (ListOfSet, e1) ->
        let n1 = print emit p l e1 in
        let m = mod_of_set_type_of_expr l e1 in
        emit ?name p l e (fun oc ->
          (* FIXME: this operation needs to be faster, ideally a nop! *)
          pp oc "%s.fold %s [] (fun l_ x_ -> x_ :: l_) |> \
                 %sArray.of_list"
            m n1
            (* Revert the direction of the list depending on the set type.
             * Top already gives us the list in most important first (ie. 1st
             * entry is the top 1, etc) while other fold functions iterate in
             * ascending order: *)
            (if m = "Top" then "" else "List.rev |> "))
    | E.E1 (U8OfChar, e1) ->
        unary_op "Uint8.of_int @@ Char.code" e1
    | E.E1 (CharOfU8, e1) ->
        unary_op "Char.chr @@ Uint8.to_int" e1
    | E.E1 (SizeOfU32, e1) ->
        unary_op "Uint32.to_int" e1
    | E.E1 (U32OfSize, e1) ->
        unary_op "Uint32.of_int" e1
    | E.E1 ((AddressOfU64 | U64OfAddress), e1) ->
        print ?name emit p l e1
    | E.E1 (ListOfSList, e1) ->
        unary_op "Array.of_list" e1
    | E.E1 (ListOfSListRev, e1) ->
        unary_op "array_of_list_rev" e1
    | E.E1 (SetOfSList, e1) ->
        unary_op "SimpleSet.of_list" e1
    | E.E1 ((ToU8 | ToI8 | ToU16 | ToI16 | ToU24 | ToI24 | ToU32 | ToI32 |
             ToU40 | ToI40 | ToU48 | ToI48 | ToU56 | ToI56 | ToU64 | ToI64 |
             ToU128 | ToI128), e1) ->
        conv_to_num e1
    | E.E1 (ToFloat, e1) ->
        to_float ?name emit p l e1
    | E.E1 (U8OfBool, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "if %s then Uint8.one else Uint8.zero" n1)
    | E.E1 (BoolOfU8, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "Uint8.compare Uint8.zero %s <> 0" n1)
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
        | Data { vtyp = Vec (d, _) ; _ } ->
            emit ?name p l e (fun oc -> pp oc "Uint32.of_int %d" d)
        | Data { vtyp = Lst _ ; _ } ->
            unary_op "Uint32.of_int @@ Array.length" e1
        | Data { vtyp = Set (st, _) ; _ } ->
            let n1 = print emit p l e1 in
            let m = mod_of_set_type st in
            emit ?name p l e (fun oc -> pp oc "%s.cardinality %s" m n1)
        | _ ->
            assert false (* Because type checking *))
    | E.E1 (DataPtrOfString, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "pointer_of_string %s" n1)
    | E.E1 (DataPtrOfBuffer, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc -> pp oc "pointer_of_buffer %s" n1)
    | E.E2 (DataPtrOfAddress, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "pointer_of_address %s %s" n1 n2)
    | E.E1 (GetEnv, e1) ->
        let n1 = print emit p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "try NotNull (Sys.getenv %s) with Not_found -> Null" n1)
    | E.E3 (DataPtrOfPtr, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.of_pointer %s %s %s" m n1 n2 n3)
    | E.E2 (GetBit, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".getBit") e1 e2
    | E.E2 (GetVec, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.(%s.to_int %s)" n2 m n1)
    | E.E3 (SetBit, e1, e2, e3) ->
        let ptr = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.setBit %s %s %s" m ptr n2 n3)
    | E.E3 (SetVec, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.(%s.to_int %s) <- %s" n2 m n1 n3)
    | E.E1 (ReadByte, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readByte") e1
    | E.E1 (ReadWord LittleEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readWordLittle") e1
    | E.E1 (ReadWord BigEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readWordBig") e1
    | E.E1 (ReadDWord LittleEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readDWordLittle") e1
    | E.E1 (ReadDWord BigEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readDWordBig") e1
    | E.E1 (ReadQWord LittleEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readQWordLittle") e1
    | E.E1 (ReadQWord BigEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readQWordBig") e1
    | E.E1 (ReadOWord LittleEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readOWordLittle") e1
    | E.E1 (ReadOWord BigEndian, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".readOWordBig") e1
    | E.E2 (ReadBytes, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".readBytes") e1 e2
    | E.E2 (PeekByte, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekByte") e1 e2
    | E.E2 (PeekWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekWordLittle") e1 e2
    | E.E2 (PeekWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekWordBig") e1 e2
    | E.E2 (PeekDWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekDWordLittle") e1 e2
    | E.E2 (PeekDWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekDWordBig") e1 e2
    | E.E2 (PeekQWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekQWordLittle") e1 e2
    | E.E2 (PeekQWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekQWordBig") e1 e2
    | E.E2 (PeekOWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekOWordLittle") e1 e2
    | E.E2 (PeekOWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".peekOWordBig") e1 e2
    | E.E2 (WriteByte, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeByte") e1 e2
    | E.E2 (WriteWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeWordLittle") e1 e2
    | E.E2 (WriteWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeWordBig") e1 e2
    | E.E2 (WriteDWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeDWordLittle") e1 e2
    | E.E2 (WriteDWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeDWordBig") e1 e2
    | E.E2 (WriteQWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeQWordLittle") e1 e2
    | E.E2 (WriteQWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeQWordBig") e1 e2
    | E.E2 (WriteOWord LittleEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeOWordLittle") e1 e2
    | E.E2 (WriteOWord BigEndian, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeOWordBig") e1 e2
    | E.E2 (WriteBytes, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".writeBytes") e1 e2
    | E.E2 (PokeByte, e1, e2) ->
        let ptr = print ?name emit p l e1
        and v = print emit p l e2 in
        let m = mod_name (E.type_of l e1) in
        ppi p.P.def "%s.pokeByte %s %s;" m ptr v ;
        ptr
    | E.E3 (BlitByte, e1, e2, e3) ->
        let m = mod_name (E.type_of l e1) in
        any_op (m ^".blitBytes") [ e1 ; e2 ; e3 ]
    | E.E2 (DataPtrAdd, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".skip") e1 e2
    | E.E2 (DataPtrSub, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".sub") e1 e2
    | E.E1 (RemSize, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".remSize") e1
    | E.E1 (Offset, e1) ->
        let m = mod_name (E.type_of l e1) in
        unary_op (m ^".offset") e1
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
    | E.E1 (UnsafeLog, e1) ->
        unary_op "log" e1
    | E.E1 (Log10, e1) ->
        unary_op "(Nullable.of_nan % log10)" e1
    | E.E1 (UnsafeLog10, e1) ->
        unary_op "log10" e1
    | E.E1 (Sqrt, e1) ->
        unary_op "(Nullable.of_nan % sqrt)" e1
    | E.E1 (UnsafeSqrt, e1) ->
        unary_op "sqrt" e1
    | E.E1 (Ceil, e1) ->
        unary_op "ceil" e1
    | E.E1 (Floor, e1) ->
        unary_op "floor" e1
    | E.E1 (Round, e1) ->
        unary_op "BatFloat.round" e1
    | E.E1 (Cos, e1) ->
        unary_op "cos" e1
    | E.E1 (Sin, e1) ->
        unary_op "sin" e1
    | E.E1 (Tan, e1) ->
        unary_op "(Nullable.of_nan % tan)" e1
    | E.E1 (ACos, e1) ->
        unary_op "(Nullable.of_nan % acos)" e1
    | E.E1 (ASin, e1) ->
        unary_op "(Nullable.of_nan % asin)" e1
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
        emit ?name p l e (fun oc -> pp oc "SimpleSet.make ()")
    | E.E0 Now ->
        emit ?name p l e (fun oc -> pp oc "Unix.gettimeofday ()")
    | E.E0 RandomFloat ->
        emit ?name p l e (fun oc -> pp oc "Random.float 1.")
    | E.E0 RandomU8 ->
        emit ?name p l e (fun oc -> pp oc "Uint8.(of_int (Random.bits ()))")
    | E.E0 RandomU32 ->
        emit ?name p l e (fun oc ->
          pp oc "Uint32.(logor" ;
          pp oc "  (of_int (Random.bits ()))" ;
          pp oc "  (shift_left (of_int (Random.bits ())) 30))")
    | E.E0 RandomU64 ->
        emit ?name p l e (fun oc ->
          pp oc "Uint64.(logor" ;
          pp oc "  (of_int (Random.bits ()))" ;
          pp oc "  (logor (shift_left (of_int (Random.bits ())) 30)" ;
          pp oc "         (shift_left (of_int (Random.bits ())) 60)))")
    | E.E0 RandomU128 ->
        emit ?name p l e (fun oc ->
          pp oc "Uint128.(logor" ;
          pp oc "  (of_int (Random.bits ()))" ;
          pp oc "  (logor (shift_left (of_int (Random.bits ())) 30)" ;
          pp oc "         (logor (shift_left (of_int (Random.bits ())) 60)" ;
          pp oc "                (logor (shift_left (of_int (Random.bits ())) 90)" ;
          pp oc "                       (shift_left (of_int (Random.bits ())) 120)))))")
    | E.E1 (Head, e1) ->
        unary_op "List.hd" e1
    | E.E1 (Tail, e1) ->
        unary_op "List.tl" e1
    | E.E2 (MakePair, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E1 (Fst, e1) ->
        unary_op "fst" e1
    | E.E1 (Snd, e1) ->
        unary_op "snd" e1
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
                (* TODO: a printer function directly in DessserPrinter.t? *)
                let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
                let check_csts_n = gen_sym "check_csts_" in
                ppi p.P.def "let %s x_ =" check_csts_n ;
                P.indent_more p (fun () ->
                  let ns =
                    List.map (fun e ->
                      print emit p E.no_env e
                    ) csts in
                  if csts = [] then (
                    ppi p.P.def "false"
                  ) else if List.length csts < 6 (* guessed *) then (
                    ppi p.P.def "%a"
                      (List.print ~first:"" ~last:"\n" ~sep:" || "
                        (fun oc n -> Printf.fprintf oc "x_ = %s" n)) ns
                  ) else (
                    (* TODO: if the csts are nullable filter out the Null and
                     * make this a list of non nullable values: *)
                    ppi p.P.def "let h_ = Hashtbl.of_list %a in"
                      (List.print String.print) ns ;
                    ppi p.P.def "Hashtbl.mem h_ x_"
                  )) ;
                check_csts_n) in
            emit ?name p l e (fun oc ->
              if non_csts = [] then (
                pp oc "%s %s" check_csts_n n1
              ) else (
                (* FIXME: nullability of non_csts *)
                let ns = List.map (print emit p l) non_csts in
                let x_ = gen_sym "member_item_" in
                pp oc "let %s = %s in" x_ n1 ;
                pp oc "%s %s || %a"
                  check_csts_n
                  x_
                  (List.print ~first:"" ~last:"" ~sep:" || "
                     (fun oc n -> Printf.fprintf oc "%s = %s" x_ n)) ns
              ))
        | set ->
            let n2 = print emit p l set in
            let m = mod_of_set_type_of_expr l set in
            emit ?name p l e (fun oc -> pp oc "%s.member %s %s" m n2 n1))
    | E.E0 (Identifier s) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            valid_identifier s)
    | E.E0 (ExtIdentifier (Verbatim s)) ->
        (match name with
        | Some _ ->
            (* If we want another name for that identifier, emit a binding: *)
            emit ?name p l e (fun oc -> String.print oc (valid_identifier s))
        | None ->
            s)
    | E.E0 (ExtIdentifier (TypeMethod { typ ; meth })) ->
        emit ?name p l e (fun oc ->
          pp oc "%s.DessserGen.%s"
            (valid_module_name typ)
            (E.string_of_type_method meth))
    | E.E2 (Let (n, t), e1, e2) ->
        (* Most of definitions we can actually choose the name (with ?name),
         * so we save a let. But for a few [e1] we will have no such choice,
         * so then another let is required: *)
        let n1 = print ~name:n emit p l e1 in
        if n1 <> n then
          ignore (emit ?name:(Some n) p l e1 (fun oc -> String.print oc n1)) ;
        let l = E.add_local n t l in
        print ?name emit p l e2
    | E.E2 (LetPair (n1, t1, n2, t2), e1, e2) ->
        let n = "("^ valid_identifier n1 ^", "^ valid_identifier n2 ^")" in
        let n1_n2 = print ~name:n emit p l e1 in
        if n1_n2 <> n then
          ignore (emit ?name:(Some n) p l e1 (fun oc -> String.print oc n1_n2)) ;
        let l = E.add_local n1 t1 l |>
                E.add_local n2 t2 in
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
          let l = E.enter_function fid ts l in
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
        let m = mod_name (E.type_of l e4) in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "let rec read_while_loop accum ptr =" ;
            P.indent_more p (fun () ->
              ppi oc "if %s.remSize ptr <= 0 then (accum, ptr) else" m ;
              ppi oc "let next_byte = %s.peekByte ptr 0 in" m ;
              ppi oc "if not (%s accum next_byte) then (accum, ptr) else" cond ;
              ppi oc "let accum = %s accum next_byte in" reduce ;
              ppi oc "let ptr = %s.skip ptr 1 in" m ;
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
        | Data { vtyp = (Vec _ | Lst _) ; _ } ->
            (* Both lists and vectors are represented by arrays so
             * Array.fold_left will do in both cases: *)
            emit ?name p l e (fun oc ->
              pp oc "Array.fold_left %s %s %s" body init lst)
        | Data { vtyp = Set (st, _) ; _ } ->
            let m = mod_of_set_type st in
            emit ?name p l e (fun oc ->
              pp oc "%s.fold %s %s %s" m lst init body)
        | _ ->
            assert false (* Because type checking *))
    | E.E3 (Map, init, f, lst) ->
        let lst_t = E.type_of l lst in
        let init = print emit p l init
        and f = print emit p l f
        and lst = print emit p l lst in
        emit ?name p l e (fun oc ->
          let mod_name =
            match lst_t with
            | T.Data { vtyp = (Vec _ | Lst _) ; _ } -> "Array"
            | T.Data { vtyp = Set _ ; _ } -> todo "map on sets"
            | T.SList _ -> "List"
            | _ -> assert false (* because of E.type_check *) in
          pp oc "%s.map (fun item_ -> %s %s item_) %s" mod_name f init lst)
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
          | Data { vtyp = Tup mns ; nullable = false } -> Array.length mns
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
          Printf.fprintf oc "%s.%s.%s" n1 m (valid_identifier s))
    | E.E1 (GetAlt s, e1) ->
        let t1 = E.type_of l e1 |> T.develop_user_types in
        let m = module_of_type t1 in
        let n1 = print emit p l e1 in
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
        if sum_has_arg mns.(i) then
          emit ?name p l e (fun oc ->
            Printf.fprintf oc "(%s %s)" cstr n1)
        else
          if name = None then cstr else
          emit ?name p l e (fun oc -> String.print oc cstr)
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
          pp oc "Uint16.of_int (%s.label_of_cstr %s)" m n1)
    | E.E0 CopyField ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.Copy")
    | E.E0 SkipField ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.Skip")
    | E.E0 SetFieldNull ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.SetNull")
    | E.E1 (SlidingWindow mn, e1) ->
        let n1 = print emit p l e1
        and def = print emit p l (E.default_value mn)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "SlidingWindow.make %s (%s.to_int %s)" def m n1)
    | E.E1 (TumblingWindow mn, e1) ->
        let n1 = print emit p l e1
        and def = print emit p l (E.default_value mn)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "TumblingWindow.make %s (%s.to_int %s)" def m n1)
    | E.E1 (Sampling mn, e1) ->
        let n1 = print emit p l e1
        and def = print emit p l (E.default_value mn)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "Sampling.make %s (%s.to_int %s)" def m n1)
    | E.E1 (HashTable _, e1) ->
        let n1 = print emit p l e1
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "HashTable.make (%s.to_int %s)" m n1)
    | E.E1 (Heap, cmp) ->
        let n1 = print emit p l cmp in
        (* comparison function need to be adapted to return an int: *)
        let cmp_res_t =
          match E.type_of l cmp with
          | T.Function (_, res_t) -> res_t
          | _ -> assert false (* Because of [type_check] *) in
        let m = mod_name cmp_res_t in
        emit ?name p l e (fun oc ->
          pp oc "Heap.make (fun a_ b_ -> %s.to_int (%s a_ b_))" m n1)
    | E.E2 (Insert, set, x) ->
        let set = print emit p l set
        and x = print emit p l x
        and m = mod_of_set_type_of_expr l set in
        (* Avoids using [emit] to not generate a binding for unit: *)
        ppi p.P.def "%s.insert %s %s ;" m set x ;
        "()"
    | E.E2 (DelMin, set, n) ->
        let set = print emit p l set
        and n = print emit p l n
        and m = mod_name (E.type_of l n)
        and ms = mod_of_set_type_of_expr l set in
        ppi p.P.def "%s.del_min %s (%s.to_int %s) ;" ms set m n ;
        "()"
    | E.E1 (GetMin, set) ->
        let set = print emit p l set
        and m = mod_of_set_type_of_expr l set in
        emit ?name p l e (fun oc ->
          pp oc "%s.get_min %s" m set)
    | E.E2 (SplitBy, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "String.split_on_string %s %s" n1 n2)
    | E.E2 (SplitAt, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "let pos_ = %s.to_int %s in" (mod_name (E.type_of l e1)) n1 ;
          pp oc "{ %s = String.sub %s 0 pos_ ;"
            (tuple_field_name 0) n2 ;
          pp oc "  %s = String.sub %s pos_ (String.length %s - pos_) }"
            (tuple_field_name 1) n2 n2)
    | E.E2 (Join, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          (* TODO: faster impl with a single string alloc: *)
          pp oc "String.concat %s (Array.to_list %s)" n1 n2)
    | E.E2 (AllocLst, e1, e2) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "Array.make (%s.to_int %s) %s"
            (mod_name (E.type_of l e1)) n1 n2)
    | E.E2 (PartialSort, e1, e2) ->
        let n1 = print ?name emit p l e1
        and n2 = print emit p l e2 in
        let item2_t =
          match E.type_of l e2 |> T.develop_user_types with
          | Data { vtyp = (Vec (_, t) | Lst t) ; _ } -> t
          | _ -> assert false (* because of type_check *) in
        let m = mod_name (T.Data item2_t) in
        ppi p.P.def "BatArray.enum %s |>" n2 ;
        ppi p.P.def "BatEnum.map %s.to_int |>" m ;
        ppi p.P.def "BatList.of_enum |>" ;
        ppi p.P.def "partial_sort %s ;" n1 ;
        "()"
    | E.E2 (ChopBegin, lst, len) ->
        let m = mod_name (E.type_of l len) in
        let lst = print ?name emit p l lst
        and len = print emit p l len in
        emit ?name p l e (fun oc ->
          pp oc "lst_lchop %s (%s.to_int %s)" lst m len)
    | E.E2 (ChopEnd, lst, len) ->
        let m = mod_name (E.type_of l len) in
        let lst = print ?name emit p l lst
        and len = print emit p l len in
        emit ?name p l e (fun oc ->
          pp oc "lst_rchop %s (%s.to_int %s)" lst m len)
    | E.E2 (ScaleWeights, set, d) ->
        let set = print emit p l set
        and d = print emit p l d
        and m = mod_of_set_type_of_expr l set in
        ppi p.P.def "%s.scale %s %s ;" m set d ;
        "()"
    | E.E2 (CharOfString, idx, str) ->
        let m_idx = mod_name (E.type_of l idx) in
        let idx = print emit p l idx (* guaranteed to be unsigned *)
        and str = print emit p l str in
        emit ?name p l e (fun oc ->
          pp oc "(let n_ = %s.to_int %s and s_ = %s in \
                  if n_ < String.length s_ then NotNull s_.[n_] \
                  else Null)" m_idx idx str)
    | E.E2 (Strftime, fmt, time) ->
        let fmt = print emit p l fmt
        and time = to_float emit p l time in
        emit ?name p l e (fun oc ->
          pp oc "strftime %s %s" fmt time)
    | E.E3 (FindSubstring, e1, e2, e3) ->
        let n1 = print emit p l e1
        and n2 = print emit p l e2
        and n3 = print emit p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "try NotNull (Uint24.of_int \
                   ((if %s then string_find else string_rfind) %s %s))"
            n1 n3 n2 ;
          pp oc "with Not_found -> Null")
    | E.E3 (Top _, size, max_size, sigmas) ->
        let m_size = mod_name (E.type_of l size)
        and m_max_size = mod_name (E.type_of l max_size) in
        let size = print emit p l size
        and max_size = print emit p l max_size
        and sigmas = to_float emit p l sigmas in
        emit ?name p l e (fun oc ->
          pp oc "Top.make (%s.to_int %s) (%s.to_int %s) %s"
            m_size size
            m_max_size max_size
            sigmas)
    | E.E3 (InsertWeighted, set, w, x) ->
        let set = print emit p l set
        and w = print emit p l w
        and x = print emit p l x
        and m = mod_of_set_type_of_expr l set in
        (* Avoids using [emit] to not generate a binding for unit: *)
        ppi p.P.def "%s.insert_weighted %s %s %s ;" m set w x ;
        "()"
    | E.E3 (Substring, str, start, stop) ->
        let m_start = mod_name (E.type_of l start)
        and m_stop = mod_name (E.type_of l stop) in
        let str = print emit p l str
        and start = print emit p l start
        and stop = print emit p l stop in
        emit ?name p l e (fun oc ->
          pp oc "substring %s (%s.to_int %s) (%s.to_int %s)"
            str m_start start m_stop stop)

  let print_binding_toplevel emit n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.P.def "%slet %s : %s =\n" p.P.indent n tn ;
    P.indent_more p (fun () ->
      (* TODO: find a way to force the first call to emit to inline
       * the expression, in order to avoid the useless "let id = x in id" *)
      let n = print emit p l e in
      pp p.P.def "%s%s\n\n" p.P.indent n)

  let print_identifier_declaration n p l e =
    let t = E.type_of l e in
    let tn = type_identifier p t in
    pp p.P.def "%sval %s : %s\n" p.P.indent n tn

  let source_intro =
    "open Stdint\n\
     open DessserOCamlBackEndHelpers\n\
     \n\
     module DessserGen = struct\n\n"

  let source_outro =
    "\nend (* DessserGen module *)\n"
end

include DessserBackEndCLike.Make (Config)
