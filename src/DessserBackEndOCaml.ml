open Batteries
open Stdint

open Dessser
open DessserBackEndCLike
open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module P = DessserPrinter

let debug = false

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

let valid_upper_identifier s =
  String.uncapitalize s |> valid_identifier |> String.capitalize

let valid_module_name s =
  assert (s <> "" && s.[0] <> '!') ;
  valid_upper_identifier s

(* Uniquify in a human friendly way all record field and sum constructor
 * names.
 * Beware that when the exact same type is encountered several times in [t]
 * it is supposed to stay the same type after adaptation, so all instances
 * of [t] must have the same prefix! *)
let make_get_prefix mn =
  (* Hash from name to the list of original type and path this name is used
   * within: *)
  let renamings = Hashtbl.create 10 in
  let shortest_path p1 p2 =
    if List.compare_lengths p1 p2 > 0 then p2 else p1 in
  let record_name n vt path =
    Hashtbl.modify_opt n (function
      | Some l ->
          let l, found =
            List.fold_left (fun (l, found) (vt', path') ->
              if T.eq vt vt' then (
                (vt, shortest_path path path') :: l, true
              ) else
                (vt', path') :: l, found
            ) ([], false) l in
          let l =
            if found then l else
            (vt, path) :: l in
          Some l
      | None ->
          Some [ vt, path ]
    ) renamings in
  let rec sensus_mn path mn =
    sensus_vt path mn.T.typ
  and sensus_vt path = function
    | Usr { name ; def } ->
        sensus_vt (name :: path) def
    | Vec (_, mn) ->
        sensus_mn path mn
    | Lst mn ->
        sensus_mn path mn
    | Set (_, mn) ->
        sensus_mn path mn
    | Tup mns ->
        Array.iter (sensus_mn path) mns
    | Rec mns as vt ->
        Array.iter (fun (n, mn) ->
          record_name n vt path ;
          sensus_mn (n :: path) mn
        ) mns
    | Sum mns as vt ->
        Array.iter (fun (n, mn) ->
          record_name n vt path ;
          sensus_mn (n :: path) mn
        ) mns
    | Map (kmn, vmn) ->
        sensus_mn path kmn ;
        sensus_mn path vmn
    | _ ->
        () in
  let uniq_prefix n vt =
    let l = Hashtbl.find renamings n in
    let num_vts =
      List.fold_left (fun num (vt', _) ->
        match vt, vt' with
        | T.Sum _, T.Sum _ | Rec _, Rec _ -> num + 1
        | _ -> num
      ) 0 l in
    if num_vts > 1 then
      let _vt, path = List.find (fun (vt', _) -> T.eq vt vt') l in
      String.join "_" (List.rev path) ^"_"
    else
      "" in
  (* Gather information on all field/constructor names: *)
  sensus_mn [] mn ;
  (* Now for each type that requires a prefix (ie. some of their names are
   * present in several types of the same kind), compute the prefix: *)
  let prefixes = Hashtbl.create 10 in
  Hashtbl.iter (fun n l ->
    List.iter (fun (vt, _) ->
      let pref = uniq_prefix n vt in
      if pref <> "" then
        Hashtbl.modify_opt vt (function
          | None -> Some pref
          | Some pref' ->
              Some (
                if String.length pref < String.length pref' then pref
                else pref')
        ) prefixes
    ) l
  ) renamings ;
  fun vt ->
    Hashtbl.find_default prefixes vt ""

(* FIXME: if not initialized, make the default output a guaranteed unique field name prefixed with a hash of the type *)
let get_prefix = ref (fun _ -> "")

let init mn =
  get_prefix := make_get_prefix mn

module Config =
struct
  let id = OCaml

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

  let uniq_field_name vt n =
    let prefix = !get_prefix vt in
    valid_identifier (prefix ^ n)

  let uniq_cstr_name vt n =
    let prefix = !get_prefix vt in
    valid_upper_identifier (prefix ^ n)

  let mod_of_set_type = function
    | Simple -> "SimpleSet"
    | Sliding -> "SlidingWindow"
    | Tumbling -> "TumblingWindow"
    | Sampling -> "Sampling"
    | HashTable -> "HashTable"
    | Heap -> "Heap"
    | Top -> "Top"

 let mod_of_set_type_of_expr l set =
   match E.type_of l set |> T.develop_mn with
   | { typ = Set (st, _) ; nullable = false } ->
       mod_of_set_type st
   | _ ->
       invalid_arg "mod_of_set_type_of_expr"

  let sum_has_arg (_, mn) =
    not T.(eq_mn mn void)

  let rec print_tuple p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    ppi oc "and %s = (" (valid_identifier id) ;
    P.indent_more p (fun () ->
      Array.iteri (fun i mn ->
        let typ_id = type_identifier_mn p mn in
        let is_last = i >= Array.length mns - 1 in
        ppi oc "%s%s" typ_id (if is_last then "" else " *")
      ) mns
    ) ;
    ppi oc ")\n"

  and print_record p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    ppi oc "and %s = {" (valid_identifier id) ;
    P.indent_more p (fun () ->
      Array.iter (fun (field_name, mn) ->
        let field_name = uniq_field_name (T.Rec mns) field_name in
        let typ_id = type_identifier_mn p mn in
        ppi oc "%s : %s ;" field_name typ_id
      ) mns
    ) ;
    ppi oc "}\n"

  and print_sum p oc id mns =
    let ppi oc fmt = pp oc ("%s" ^^ fmt ^^"\n") p.P.indent in
    ppi oc "and %s =" (valid_identifier id) ;
    P.indent_more p (fun () ->
      Array.iter (fun (n, mn as n_mn) ->
        let n = uniq_cstr_name (T.Sum mns) n in
        if sum_has_arg n_mn then
          let typ_id = type_identifier_mn p mn in
          ppi oc "| %s of %s" n typ_id
        else
          ppi oc "| %s" n
      ) mns
    ) ;
    pp oc "\n"

  and type_identifier_mn p mn =
    if mn.T.nullable then
      type_identifier p mn.typ ^" nullable"
    else
      type_identifier p mn.typ

  and type_identifier p mn =
    let type_identifier = type_identifier p in
    match mn with
    | Unknown -> invalid_arg "type_identifier"
    | This -> "t"
    | Base Char -> "char"
    | Base String -> "string"
    | Base Bool -> "bool"
    | Base Float -> "float"
    | Base U8 -> "Uint8.t"
    | Base I8 -> "Int8.t"
    | Base U16 -> "Uint16.t"
    | Base I16 -> "Int16.t"
    | Base U24 -> "Uint24.t"
    | Base I24 -> "Int24.t"
    | Base U32 -> "Uint32.t"
    | Base I32 -> "Int32.t"
    | Base U40 -> "Uint40.t"
    | Base I40 -> "Int40.t"
    | Base U48 -> "Uint48.t"
    | Base I48 -> "Int48.t"
    | Base U56 -> "Uint56.t"
    | Base I56 -> "Int56.t"
    | Base U64 -> "Uint64.t"
    | Base I64 -> "Int64.t"
    | Base U128 -> "Uint128.t"
    | Base I128 -> "Int128.t"
    | Usr t ->
        type_identifier t.def
    | Ext n ->
        P.get_external_type p n OCaml
    | (Vec (_, t) | Lst t) ->
        type_identifier_mn p t ^" array"
    | Set (st, t) ->
        let m = mod_of_set_type st in
        type_identifier_mn p t ^" "^ m ^".t"
    | Tup mns as mn ->
        P.declared_type p mn (fun oc type_id ->
          print_tuple p oc type_id mns) |>
        valid_identifier
    | Rec mns as mn ->
        P.declared_type p mn (fun oc type_id ->
          print_record p oc type_id mns) |>
        valid_identifier
    | Sum mns as mn ->
        P.declared_type p mn (fun oc type_id ->
          print_sum p oc type_id mns) |>
        valid_identifier
    | Map _ ->
        assert false (* no value of map type *)
    | Void -> "unit"
    | Ptr -> "Pointer.t"
    | Size -> "Size.t"
    | Address -> "Uint64.t"
    | Bit -> "bool"
    | Byte -> "Uint8.t"
    | Word -> "Uint16.t"
    | DWord -> "Uint32.t"
    | QWord -> "Uint64.t"
    | OWord -> "Uint128.t"
    | Bytes -> "Slice.t"
    | Pair (mn1, mn2) ->
        "("^ type_identifier_mn p mn1 ^" * "^ type_identifier_mn p mn2 ^")"
    | SList mn ->
        type_identifier_mn p mn ^" list"
    | Function ([||], mn) ->
        "(unit -> "^ type_identifier_mn p mn ^")"
    | Function (args, ret) ->
        "("^ IO.to_string (
          Array.print ~first:"" ~last:"" ~sep:" -> " (fun oc mn ->
            String.print oc (type_identifier_mn p mn))
        ) args ^" -> "^ type_identifier_mn p ret ^")"
    | Mask -> "DessserMasks.t"
    | Ref mn -> type_identifier_mn p mn ^" ref"

  let rec mod_name = function
    | T.{ typ = Base Char ; nullable = false } -> "Char"
    | { typ = Base String ; nullable = false } -> "String"
    | { typ = Base Bool ; nullable = false } -> "Bool"
    | { typ = Base Float ; nullable = false } -> "Float"
    | { typ = Base U8 ; nullable = false } -> "Uint8"
    | { typ = Base I8 ; nullable = false } -> "Int8"
    | { typ = Base U16 ; nullable = false } -> "Uint16"
    | { typ = Base I16 ; nullable = false } -> "Int16"
    | { typ = Base U24 ; nullable = false } -> "Uint24"
    | { typ = Base I24 ; nullable = false } -> "Int24"
    | { typ = Base U32 ; nullable = false } -> "Uint32"
    | { typ = Base I32 ; nullable = false } -> "Int32"
    | { typ = Base U40 ; nullable = false } -> "Uint40"
    | { typ = Base I40 ; nullable = false } -> "Int40"
    | { typ = Base U48 ; nullable = false } -> "Uint48"
    | { typ = Base I48 ; nullable = false } -> "Int48"
    | { typ = Base U56 ; nullable = false } -> "Uint56"
    | { typ = Base I56 ; nullable = false } -> "Int56"
    | { typ = Base U64 ; nullable = false } -> "Uint64"
    | { typ = Base I64 ; nullable = false } -> "Int64"
    | { typ = Base U128 ; nullable = false } -> "Uint128"
    | { typ = Base I128 ; nullable = false } -> "Int128"
    | { typ = Usr t ; nullable = false } ->
        mod_name { typ = t.def ; nullable = false }
    | { typ = Ptr ; nullable = false } -> "Pointer"
    | { typ = Size ; nullable = false } -> "Size"
    | { typ = Address ; nullable = false } -> "Uint64"
    | { typ = Bit ; nullable = false } -> "Bool"
    | { typ = Byte ; nullable = false } -> "Uint8"
    | { typ = Word ; nullable = false } -> "Uint16"
    | { typ = DWord ; nullable = false } -> "Uint32"
    | { typ = QWord ; nullable = false } -> "Uint64"
    | { typ = OWord ; nullable = false } -> "Uint128"
    | { typ = Bytes ; nullable = false } -> "Slice"
    | t ->
        Printf.sprintf2 "No module implementing %a"
          T.print_mn t |>
        invalid_arg

  let rec num_name = function
    | T.{ typ = (Byte | Word | DWord | QWord | OWord |
                 Base (U8 | I8 | U16 | I16 | U24 | I24 | U32 | I32 |
                       U40 | I40 | U48 | I48 | U56 | I56 | U64 | I64 |
                       U128 | I128 | Float)) ;
          nullable = false } as mn ->
        String.lowercase (mod_name mn)
    | T.{ typ = Usr t ; nullable = false } ->
        num_name { typ = t.def ; nullable = false }
    | mn ->
        Printf.sprintf2 "num_name: Not an integer (%a)"
          T.print_mn mn |>
        invalid_arg

  (* Identifiers used for function parameters: *)
  let param fid n = "p_"^ string_of_int fid ^"_"^ string_of_int n

  let print_binding p mn n f oc =
    let tn = type_identifier_mn p mn in
    let need_rec =
      match mn with
      | T.{ typ = Function _ ; nullable = false } -> true
      (* Some T.Ext might denote functions, but then they are not recursive *)
      | _ -> false in
    pp oc "let %s%s : %s = %t in"
      (if need_rec then "rec " else "") n tn f

  let print_inline p mn f oc =
    let tn = type_identifier_mn p mn in
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
    let unary_mod_op op e1 =
      let op = mod_name (E.type_of l e) ^"."^ op in
      unary_op op e1 in
    let unary_op_or_null op e1 =
      let n1 = print p l e1 in
      emit ?name p l e (fun oc ->
        pp oc "(try NotNull (%s %s)" op n1 ;
        pp oc " with _ as e ->" ;
        if debug then
          pp oc "   Printf.eprintf \"%s %%S\\n\" %s ;" op n1 ;
        pp oc "   Null)") in
    let unary_mod_op_or_null op e1 =
      let mn = E.type_of l e in
      let mn = { mn with nullable = false } in
      let op = mod_name mn ^"."^ op in
      unary_op_or_null op e1 in
    let any_op op es =
      let ns = List.map (print p l) es in
      let ns = String.concat " " ns in
      emit ?name p l e (fun oc -> pp oc "%s %s" op ns) in
    let binary_op op e1 e2 =
      any_op op [ e1 ; e2 ] in
    let binary_infix_op e1 op e2 =
      let n1 = print p l e1
      and n2 = print p l e2 in
      emit ?name p l e (fun oc -> pp oc "%s %s %s" n1 op n2) in
    let shortcutting_binary_infix_op e1 op e2 =
      emit ?name p l e (fun oc ->
        pp oc "(\n" ;
        P.indent_more p (fun () ->
          let n1 = print p l e1 in
          ppi oc "%s" n1
        ) ;
        ppi oc ") %s (" op ;
        P.indent_more p (fun () ->
          let n2 = print p l e2 in
          ppi oc "%s" n2
        ) ;
        pp oc "%s)" p.P.indent) in
    let binary_mod_op op e1 e2 =
      let op = mod_name (E.type_of l e) ^"."^ op in
      binary_op op e1 e2 in
    let binary_mod_op_2nd_u8 op e1 e2 =
      let n1 = print p l e1
      and n2 = print p l e2
      and m = mod_name (E.type_of l e) in
      emit ?name p l e (fun oc ->
        pp oc "%s.%s %s (Uint8.to_int %s)" m op n1 n2) in
    let conv_to_num e1 =
      let t = E.type_of l e
      and t1 = E.type_of l e1 in
      if T.eq_mn t t1 then
        print p l e1
      else
        let op = mod_name t ^".of_"^ num_name t1 in
        unary_op op e1 in
    let to_float ?name p l e =
      let m = mod_name (E.type_of l e) in
      if m = "Float" then
        print ?name p l e
      else
        let n = print p l e in
        "("^ m ^".to_float "^ n ^")" in
    let preallocate printer i oc =
      let value = IO.to_string printer i in
      let uniq_id = "const_"^ Digest.(string value |> to_hex) in
      let def = "let "^ uniq_id ^" = "^ value ^"\n" in
      if not (List.mem def p.P.defs) then p.P.defs <- def :: p.P.defs ;
      String.print oc uniq_id
    in
    match e with
    | E.E1S (Apply, f, es) ->
        let nf = print p l f in
        let ns =
          if es = [] then [ "()" ]
          else
            List.fold_left (fun ns e -> print p l e :: ns) [] es |>
            List.rev in
        emit ?name p l e (fun oc ->
          pp oc "%s%a"
            nf
            (List.print ~first:" " ~last:"" ~sep:" " String.print) ns)
    | E.E1 (Comment c, e1) ->
        ppi p.P.def "(* %s *)" c ;
        print ?name p l e1
    | E.E0S (Seq, es) ->
        List.fold_left (fun _ e -> print p l e) "()" es
    | E.E0S ((MakeVec | MakeLst _), es) ->
        let inits = List.map (print p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:"[| " ~last:" |]" ~sep:"; " String.print oc inits)
    | E.E0S (MakeTup, es) ->
        let inits = List.map (print p l) es in
        emit ?name p l e (fun oc ->
          List.print ~first:"(" ~last:")" ~sep:", " (fun oc n ->
            String.print oc n
          ) oc inits)
    | E.E0S (MakeRec, es) ->
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
        let mn = E.type_of l e |> T.develop_mn in
        emit ?name p l e (fun oc ->
          let last = "\n"^ p.P.indent ^"}" in
          P.indent_more p (fun () ->
            let first = "{\n"^ p.P.indent
            and sep = ";\n"^ p.P.indent in
            List.print ~first ~last ~sep
              (fun oc (name, n) ->
                Printf.fprintf oc "%s = %s"
                  (uniq_field_name mn.typ name) n) oc inits))
    | E.E0S (MakeUsr n, ins) ->
        let e = E.apply_constructor e l n ins in
        print ?name p l e
    | E.E0S (Verbatim (temps, _), ins) ->
        let args = List.map (print p l) ins in
        emit ?name p l e (fun oc ->
          String.print oc (E.expand_verbatim id temps args))
    | E.E1 (Identity, e1) ->
        print ?name p l e1
    | E.E1 (Ignore, e1) ->
        let n = print p l e1 in
        ppi p.P.def "ignore %s ;" n ;
        "()"
    | E.E1 (Dump, e1) ->
        let n = print p l e1 in
        pp p.P.def ("%s"^^
          (match E.type_of l e1 |> T.develop_mn with
          | { typ = Base String ; nullable = false } ->
              "print_string %s;"
          | { typ = Base Char ; nullable = false } ->
              "print_char %s;"
          | _ ->
              "print_string (Batteries.dump %s);") ^^"\n")
          p.P.indent n ;
        ppi p.P.def "flush stdout ;" ;
        "()"
    | E.E1 (IsNull, e1) ->
        let n = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "%s = Null" n)
    | E.E2 (Nth, e1, e2) ->
        let n1 = print p l e1 in
        let n2 = print p l e2 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.(%s.to_int %s)" n2 m n1)
    | E.E1 (NotNull, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "NotNull %s" n1)
    | E.E1 (Force what, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "Nullable.get %s%s"
            (if what = "" then "" else "~what:"^ String.quote what ^" ")
            n1)
    | E.E0 (Null _) ->
        emit ?name p l e (fun oc -> pp oc "Null")
    | E.E0 (Float f) ->
        emit ?name p l e (preallocate print_float_literal f)
    | E.E0 Void ->
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
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_mn with
          | { typ = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                         |I8|I16|I24|I32|I40|I48|I56|I64|I128) ;
              _ } as t ->
              let op_name = match op with Div -> "div" | _ -> "rem" in
              pp oc "try NotNull (%s.%s %s %s) with Division_by_zero -> Null"
                (mod_name t) op_name n1 n2
          | { typ = Base Float ; _ } ->
              let op_name = match op with Div -> "(/.)" | _ -> "Float.rem" in
              pp oc "Nullable.of_nan (%s %s %s)" op_name n1 n2
          | _ ->
              assert false)
    | E.E2 ((UnsafeDiv | UnsafeRem as op), e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_mn with
          | { typ = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                         |I8|I16|I24|I32|I40|I48|I56|I64|I128) ;
              _ } as t ->
              let op_name = match op with UnsafeDiv -> "div" | _ -> "rem" in
              pp oc "%s.%s %s %s" (mod_name t) op_name n1 n2
          | { typ = Base Float ; _ } ->
              let op_name = match op with UnsafeDiv -> "(/.)" | _ -> "Float.rem" in
              pp oc "%s %s %s" op_name n1 n2
          | _ ->
              assert false)
    | E.E2 (Pow, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_mn with
          | { typ = Base Float ; _ } ->
              pp oc "Nullable.of_nan (%s ** %s)" n1 n2
          | { typ = Base (I32 | I64) ; _ } as t ->
              pp oc "try NotNull (Bat%s.pow %s %s) \
                     with Invalid_argument _ -> Null"
                (mod_name t) n1 n2
          | {
              typ = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
                          |I8|I16|I24|I40|I48|I56|I128) ; _ } as t ->
              (* For through floats *)
              let m = mod_name t in
              pp oc "Nullable.map %s.of_float \
                       (Nullable.of_nan (%s.to_float %s ** %s.to_float %s))"
                m m n1 m n2
          | _ ->
              assert false (* because of type-checking *))
    | E.E2 (UnsafePow, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          match E.type_of l e1 |> T.develop_mn with
          | { typ = Base Float ; _ } ->
              pp oc "%s ** %s" n1 n2
          | { typ = Base (I32 | I64) ; _ } as t ->
              pp oc "Bat%s.pow %s %s" (mod_name t) n1 n2
          | {
              typ = Base (U8|U16|U24|U32|U40|U48|U56|U64|U128
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
        let n1 = print p l e1 in
        let case_u mn n =
          match T.develop_mn mn with
          | T.{ typ = Base U32 ; _ } ->
              ppi p.P.def "DessserIpTools.V4.to_string %s" n
          | T.{ typ = Base U128 ; _ } ->
              ppi p.P.def "DessserIpTools.V6.to_string %s" n
          | _ ->
              assert false (* because of type checking *)
        in
        emit ?name p l e (fun oc ->
          (* Keep the user type (which might have been renamed) for building
           * the constructor names: *)
          let t = E.type_of l e1 in
          match T.develop_mn t with
          | { typ = Sum mns as typ ; _ } ->
              (* Since the type checking accept any sum type made of u32 and
               * u128, let's be as general as possible: *)
              ppi oc "match %s with\n" n1 ;
              P.indent_more p (fun () ->
                Array.iter (fun (cstr, mn) ->
                  ppi oc "| %s ip_ ->\n" (uniq_cstr_name typ cstr) ;
                  P.indent_more p (fun () -> case_u mn "ip_")
                ) mns)
          | mn ->
              case_u mn n1)
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
        let n = print p l e1 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "Char.chr (Uint8.to_int (%s.peekByte %s)), %s.skip %s 1"
            m n m n)
    | E.E1 (FloatOfPtr, e1) ->
        let n1 = print p l e1 in
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
        let n1 = print p l e1 in
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
        let n = print p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "BatInt64.float_of_bits (Uint64.to_int64 %s)" n)
    | E.E1 (QWordOfFloat, e1) ->
        let n = print p l e1 in
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
        print ?name p l e1
    | E.E1 (ListOfSet, e1) ->
        let n1 = print p l e1 in
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
        print ?name p l e1
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
        to_float ?name p l e1
    | E.E1 (U8OfBool, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "if %s then Uint8.one else Uint8.zero" n1)
    | E.E1 (BoolOfU8, e1) ->
        let n1 = print p l e1 in
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
        (match E.type_of l e1 |> T.develop_mn with
        | { typ = Vec (d, _) ; _ } ->
            emit ?name p l e (fun oc -> pp oc "Uint32.of_int %d" d)
        | { typ = Lst _ ; _ } ->
            unary_op "Uint32.of_int @@ Array.length" e1
        | { typ = Set (st, _) ; _ } ->
            let n1 = print p l e1 in
            let m = mod_of_set_type st in
            emit ?name p l e (fun oc -> pp oc "%s.cardinality %s" m n1)
        | _ ->
            assert false (* Because type checking *))
    | E.E1 (PtrOfString, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "pointer_of_string %s" n1)
    | E.E1 (PtrOfBuffer, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "pointer_of_buffer %s" n1)
    | E.E2 (PtrOfAddress, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc -> pp oc "pointer_of_address %s %s" n1 n2)
    | E.E1 (GetEnv, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "try NotNull (Sys.getenv %s) with Not_found -> Null" n1)
    | E.E3 (PtrOfPtr, e1, e2, e3) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.of_pointer %s %s %s" m n1 n2 n3)
    | E.E2 (GetBit, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".getBit") e1 e2
    | E.E2 (GetVec, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.(%s.to_int %s)" n2 m n1)
    | E.E3 (SetBit, e1, e2, e3) ->
        let ptr = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3 in
        let m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc -> pp oc "%s.setBit %s %s %s" m ptr n2 n3)
    | E.E3 (SetVec, e1, e2, e3) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3
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
        let ptr = print ?name p l e1
        and v = print p l e2 in
        let m = mod_name (E.type_of l e1) in
        ppi p.P.def "%s.pokeByte %s %s;" m ptr v ;
        ptr
    | E.E3 (BlitByte, e1, e2, e3) ->
        let m = mod_name (E.type_of l e1) in
        any_op (m ^".blitBytes") [ e1 ; e2 ; e3 ]
    | E.E2 (PtrAdd, e1, e2) ->
        let m = mod_name (E.type_of l e1) in
        binary_op (m ^".skip") e1 e2
    | E.E2 (PtrSub, e1, e2) ->
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
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc -> pp oc "%s, %s" n1 n2)
    | E.E1 (MakeRef, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "ref %s" n1)
    | E.E1 (GetRef, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "!%s" n1)
    | E.E2 (SetRef, e1, e2) ->
        binary_infix_op e1 ":=" e2
    | E.E1 (Fst, e1) ->
        unary_op "fst" e1
    | E.E1 (Snd, e1) ->
        unary_op "snd" e1
    | E.E2 (Min, e1, e2) ->
        binary_op "min" e1 e2
    | E.E2 (Max, e1, e2) ->
        binary_op "max" e1 e2
    | E.E2 (Member, e1, e2) ->
        let n1 = print p l e1 in
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
                      print p E.no_env e
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
                let ns = List.map (print p l) non_csts in
                let x_ = gen_sym "member_item_" in
                pp oc "let %s = %s in" x_ n1 ;
                pp oc "%s %s || %a"
                  check_csts_n
                  x_
                  (List.print ~first:"" ~last:"" ~sep:" || "
                     (fun oc n -> Printf.fprintf oc "%s = %s" x_ n)) ns
              ))
        | set ->
            let n2 = print p l set in
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
    | E.E0 (ExtIdentifier (Method { typ ; meth })) ->
        emit ?name p l e (fun oc ->
          pp oc "%s.DessserGen.%s"
            (valid_module_name typ)
            (E.string_of_type_method meth |> valid_identifier))
    | E.E2 (Let (n, t), e1, e2) ->
        (* Most of definitions we can actually choose the name (with ?name),
         * so we save a let. But for a few [e1] we will have no such choice,
         * so then another let is required: *)
        let n1 = print ~name:n p l e1 in
        if n1 <> n then
          ignore (emit ?name:(Some n) p l e1 (fun oc -> String.print oc n1)) ;
        let l = E.add_local n t l in
        print ?name p l e2
    | E.E2 (LetPair (n1, t1, n2, t2), e1, e2) ->
        let n = "("^ valid_identifier n1 ^", "^ valid_identifier n2 ^")" in
        let n1_n2 = print ~name:n p l e1 in
        if n1_n2 <> n then
          ignore (emit ?name:(Some n) p l e1 (fun oc -> String.print oc n1_n2)) ;
        let l = E.add_local n1 t1 l |>
                E.add_local n2 t2 in
        print ?name p l e2
    | E.E1 (Function (_fid, [||]), e1) ->
        emit ?name p l e (fun oc ->
          pp oc "(fun () ->\n" ;
          P.indent_more p (fun () ->
            let n = print p l e1 in
            pp oc "%s%s)" p.P.indent n))
    | E.E1 (Function (fid, ts), e1) ->
        (* Pick the name here so we can add it to the environment, where it
         * can later be found by Myself: *)
        let name = gen_sym ?name ("fun_"^ string_of_int fid) in
        emit ?name:(Some name) p l e (fun oc ->
          array_print_i ~first:"(fun " ~last:" ->\n" ~sep:" "
            (fun i oc _t -> String.print oc (param fid i))
            oc ts ;
          let l = E.enter_function ~name fid ts l in
          P.indent_more p (fun () ->
            let n = print p l e1 in
            pp oc "%s%s)" p.P.indent n))
    | E.E0 (Param (fid, n)) ->
        param fid n
    | E.E0 (Myself _) ->
        (match l.E.name with
        | None -> invalid_arg "print Myself while function name is unknown"
        | Some n -> n)
    | E.E3 (If, e1, e2, e3) ->
        let cond = print p l e1 in
        emit ?name p l e (fun oc ->
          pp oc "\n" ;
          P.indent_more p (fun () ->
            ppi oc "if %s then (" cond ;
            P.indent_more p (fun () ->
              let n = print p l e2 in
              ppi oc "%s" n) ;
            ppi oc ") else (" ;
            P.indent_more p (fun () ->
              let n = print p l e3 in
              ppi oc "%s" n) ;
            pp oc "%s)" p.P.indent))
    | E.E2 (While, cond, body) ->
        ppi p.P.def "while (" ;
        P.indent_more p (fun () ->
          let cond = print p l cond in
          ppi p.P.def "%s" cond) ;
        ppi p.P.def ") do" ;
        P.indent_more p (fun () ->
          let body = print p l body in
          ppi p.P.def "%s" body) ;
        ppi p.P.def "done ;" ;
        "()"
    | E.E2 (ForEach (n, t), lst, body) ->
        let n1 = valid_identifier n in
        let lst_t = E.type_of l lst |> T.develop_mn in
        let lst = print p l lst in
        (match lst_t with
        | { typ = (Vec _ | Lst _) ; _ } ->
            (* Both lists and vectors are represented as arrays *)
            ppi p.P.def "for i_ = 0 to Array.length (%s) - 1 do" lst ;
            P.indent_more p (fun () ->
              ppi p.P.def "let %s = %s.(i_) in" n1 lst ;
              let l = E.add_local n t l in
              let body = print p l body in
              ppi p.P.def "%s" body) ;
            ppi p.P.def "done ;"
        | { typ = Set (st, _) ; _ } ->
            let m = mod_of_set_type st in
            ppi p.P.def "%s.fold %s () (fun () %s ->" m lst n1 ;
            P.indent_more p (fun () ->
              let l = E.add_local n t l in
              let body = print p l body in
              ppi p.P.def "%s" body) ;
            ppi p.P.def ") ;"
        | _ ->
            assert false (* Because type checking *)) ;
        "()"
    | E.E3 (Map, init, f, lst) ->
        let lst_t = E.type_of l lst in
        let init = print p l init
        and f = print p l f
        and lst = print p l lst in
        emit ?name p l e (fun oc ->
          let mod_name =
            match lst_t with
            | T.{ typ = (Vec _ | Lst _) ; _ } -> "Array"
            | T.{ typ = Set _ ; _ } -> todo "map on sets"
            | T.{ typ = SList _ ; _ } -> "List"
            | _ -> assert false (* because of E.type_check *) in
          pp oc "%s.map (fun item_ -> %s %s item_) %s" mod_name f init lst)
    | E.E1 (GetItem n, e1) ->
        let n1 = print p l e1 in
        let res = gen_sym ?name "get_item_" in
        let max_n =
          match E.type_of l e1 |> T.develop_mn with
          | { typ = Tup mns ; nullable = false } -> Array.length mns
          | _ -> assert false in
        ppi p.P.def "let %t = %s in\n"
          (fun oc ->
            for i = 0 to max_n - 1 do
              if i > 0 then String.print oc ", " ;
              String.print oc (if i = n then res else "_")
            done)
          n1 ;
        res
    | E.E1 (GetField s, e1) ->
        let n1 = print p l e1 in
        let mn = E.type_of l e1 |> T.develop_mn in
        emit ?name p l e (fun oc ->
          Printf.fprintf oc "%s.%s" n1 (uniq_field_name mn.typ s))
    | E.E1 (GetAlt s, e1) ->
        let n1 = print p l e1 in
        let mn = E.type_of l e1 |> T.develop_mn in
        emit ?name p l e (fun oc ->
          let cstr = uniq_cstr_name mn.typ s in
          (* FIXME: figure out where to add this whether the expression is a
           * binding or inlined: [@@ocaml.warning "-8"] *)
          Printf.fprintf oc "(match %s with %s x -> x)"
            n1 cstr)
    | E.E1 (Construct (mns, i), e1) ->
        let n1 = print p l e1 in
        assert (i < Array.length mns) ;
        let cstr = uniq_cstr_name (T.Sum mns) (fst mns.(i)) in
        if sum_has_arg mns.(i) then
          emit ?name p l e (fun oc ->
            Printf.fprintf oc "(%s %s)" cstr n1)
        else
          if name = None then cstr else
          emit ?name p l e (fun oc -> String.print oc cstr)
    | E.E1 (Assert, e1) ->
        let n = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "assert %s" n)
    | E.E1 (MaskGet i, e1) ->
        let n1 = print p l e1 in
        emit ?name p l e (fun oc -> pp oc "mask_get %s %d" n1 i)
    | E.E1 (LabelOf, e1) ->
        let n1 = print p l e1 in
        let t1 = E.type_of l e1 in
        emit ?name p l e (fun oc ->
          pp oc "match %s with\n" n1 ;
          match T.develop_mn t1 with
          | T.{ typ = Sum mns as typ ; _ } ->
              P.indent_more p (fun () ->
                Array.iteri (fun i (n, _ as n_mn) ->
                  let n = uniq_cstr_name typ n in
                  ppi oc "| %s %s-> Uint16.of_int %d"
                    n (if sum_has_arg n_mn then "_ " else "") i
                ) mns) ;
              pp oc "%s" p.P.indent
          | _ ->
              assert false (* Because of type checking *))
    | E.E0 CopyField ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.Copy")
    | E.E0 SkipField ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.Skip")
    | E.E0 SetFieldNull ->
        emit ?name p l e (fun oc -> pp oc "DessserMasks.SetNull")
    | E.E1 (SlidingWindow mn, e1) ->
        let n1 = print p l e1
        and def = print p l (E.default_mn mn)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "SlidingWindow.make %s (%s.to_int %s)" def m n1)
    | E.E1 (TumblingWindow mn, e1) ->
        let n1 = print p l e1
        and def = print p l (E.default_mn mn)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "TumblingWindow.make %s (%s.to_int %s)" def m n1)
    | E.E1 (Sampling mn, e1) ->
        let n1 = print p l e1
        and def = print p l (E.default_mn mn)
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "Sampling.make %s (%s.to_int %s)" def m n1)
    | E.E1 (HashTable _, e1) ->
        let n1 = print p l e1
        and m = mod_name (E.type_of l e1) in
        emit ?name p l e (fun oc ->
          pp oc "HashTable.make (%s.to_int %s)" m n1)
    | E.E1 (Heap, cmp) ->
        let n1 = print p l cmp in
        (* comparison function need to be adapted to return an int: *)
        let cmp_res_mn =
          match E.type_of l cmp with
          | T.{ typ = Function (_, res_mn) ; nullable = false } -> res_mn
          | _ -> assert false (* Because of [type_check] *) in
        let m = mod_name cmp_res_mn in
        emit ?name p l e (fun oc ->
          pp oc "Heap.make (fun a_ b_ -> %s.to_int (%s a_ b_))" m n1)
    | E.E2 (Insert, set, x) ->
        let set = print p l set
        and x = print p l x
        and m = mod_of_set_type_of_expr l set in
        (* Avoids using [emit] to not generate a binding for unit: *)
        ppi p.P.def "%s.insert %s %s ;" m set x ;
        "()"
    | E.E2 (DelMin, set, n) ->
        let set = print p l set
        and n = print p l n
        and m = mod_name (E.type_of l n)
        and ms = mod_of_set_type_of_expr l set in
        ppi p.P.def "%s.del_min %s (%s.to_int %s) ;" ms set m n ;
        "()"
    | E.E1 (GetMin, set) ->
        let set = print p l set
        and m = mod_of_set_type_of_expr l set in
        emit ?name p l e (fun oc ->
          pp oc "%s.get_min %s" m set)
    | E.E2 (SplitBy, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "String.split_on_string %s %s" n1 n2)
    | E.E2 (SplitAt, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        let pos = gen_sym "pos" in
        emit ?name p l e (fun oc ->
          pp oc "let %s = %s.to_int %s in" pos (mod_name (E.type_of l e1)) n1 ;
          pp oc "(String.sub %s 0 %s, \
                  String.sub %s %s (String.length %s - %s))"
            n2 pos
            n2 pos n2 pos)
    | E.E2 (Join, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          (* TODO: faster impl with a single string alloc: *)
          pp oc "String.concat %s (Array.to_list %s)" n1 n2)
    | E.E2 (AllocLst, e1, e2) ->
        let n1 = print p l e1
        and n2 = print p l e2 in
        emit ?name p l e (fun oc ->
          pp oc "Array.make (%s.to_int %s) %s"
            (mod_name (E.type_of l e1)) n1 n2)
    | E.E2 (PartialSort, e1, e2) ->
        let n1 = print ?name p l e1
        and n2 = print p l e2 in
        let item2_t =
          match E.type_of l e2 |> T.develop_mn with
          | { typ = (Vec (_, t) | Lst t) ; _ } -> t
          | _ -> assert false (* because of type_check *) in
        let m = mod_name item2_t in
        ppi p.P.def "BatArray.enum %s |>" n2 ;
        ppi p.P.def "BatEnum.map %s.to_int |>" m ;
        ppi p.P.def "BatList.of_enum |>" ;
        ppi p.P.def "partial_sort %s ;" n1 ;
        "()"
    | E.E2 (ChopBegin, lst, len) ->
        let m = mod_name (E.type_of l len) in
        let lst = print ?name p l lst
        and len = print p l len in
        emit ?name p l e (fun oc ->
          pp oc "lst_lchop %s (%s.to_int %s)" lst m len)
    | E.E2 (ChopEnd, lst, len) ->
        let m = mod_name (E.type_of l len) in
        let lst = print ?name p l lst
        and len = print p l len in
        emit ?name p l e (fun oc ->
          pp oc "lst_rchop %s (%s.to_int %s)" lst m len)
    | E.E2 (ScaleWeights, set, d) ->
        let set = print p l set
        and d = print p l d
        and m = mod_of_set_type_of_expr l set in
        ppi p.P.def "%s.scale %s %s ;" m set d ;
        "()"
    | E.E2 (CharOfString, idx, str) ->
        let m_idx = mod_name (E.type_of l idx) in
        let idx = print p l idx (* guaranteed to be unsigned *)
        and str = print p l str in
        emit ?name p l e (fun oc ->
          pp oc "(let n_ = %s.to_int %s and s_ = %s in \
                  if n_ < String.length s_ then NotNull s_.[n_] \
                  else Null)" m_idx idx str)
    | E.E2 (Strftime, fmt, time) ->
        let fmt = print p l fmt
        and time = to_float p l time in
        emit ?name p l e (fun oc ->
          pp oc "strftime %s %s" fmt time)
    | E.E3 (FindSubstring, e1, e2, e3) ->
        let n1 = print p l e1
        and n2 = print p l e2
        and n3 = print p l e3 in
        emit ?name p l e (fun oc ->
          pp oc "try NotNull (Uint24.of_int \
                   ((if %s then string_find else string_rfind) %s %s))"
            n1 n3 n2 ;
          pp oc "with Not_found -> Null")
    | E.E3 (Top _, size, max_size, sigmas) ->
        let m_size = mod_name (E.type_of l size)
        and m_max_size = mod_name (E.type_of l max_size) in
        let size = print p l size
        and max_size = print p l max_size
        and sigmas = to_float p l sigmas in
        emit ?name p l e (fun oc ->
          pp oc "Top.make (%s.to_int %s) (%s.to_int %s) %s"
            m_size size
            m_max_size max_size
            sigmas)
    | E.E3 (InsertWeighted, set, w, x) ->
        let set = print p l set
        and w = print p l w
        and x = print p l x
        and m = mod_of_set_type_of_expr l set in
        (* Avoids using [emit] to not generate a binding for unit: *)
        ppi p.P.def "%s.insert_weighted %s %s %s ;" m set w x ;
        "()"
    | E.E3 (Substring, str, start, stop) ->
        let m_start = mod_name (E.type_of l start)
        and m_stop = mod_name (E.type_of l stop) in
        let str = print p l str
        and start = print p l start
        and stop = print p l stop in
        emit ?name p l e (fun oc ->
          pp oc "substring %s (%s.to_int %s) (%s.to_int %s)"
            str m_start start m_stop stop)

  let print_binding_toplevel emit n p l e =
    let mn = E.type_of l e in
    let tn = type_identifier_mn p mn in
    pp p.P.def "%slet %s : %s =\n" p.P.indent n tn ;
    P.indent_more p (fun () ->
      (* TODO: find a way to force the first call to emit to inline
       * the expression, in order to avoid the useless "let id = x in id" *)
      let n = print emit p l e in
      pp p.P.def "%s%s\n\n" p.P.indent n)

  let print_identifier_declaration n p l e =
    let mn = E.type_of l e in
    let tn = type_identifier_mn p mn in
    pp p.P.def "%sval %s : %s\n" p.P.indent n tn

  let source_intro = function
    | P.Declaration ->
        "open Stdint\n\
         open DessserOCamlBackEndHelpers\n\
         \n\
         module DessserGen : sig\n\n\
         type _unused_for_syntax_only_ = unit\n"
    | P.Definition ->
        "open Stdint\n\
         open DessserOCamlBackEndHelpers\n\
         \n\
         module DessserGen = struct\n\n\
         type _unused_for_syntax_only_ = unit\n"

  let source_outro _ =
    "\nend [@@ocaml.warning \"-8-26-30\"] (* DessserGen module *)\n"
end

include DessserBackEndCLike.Make (Config)
