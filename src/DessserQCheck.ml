(* Random genrator for types and expressions *)
open Batteries
open Stdint
open QCheck

open DessserTools
open DessserFloatTools
open DessserMiscTypes
module T = DessserTypes

(*$inject
  open Batteries
  module E = DessserExpressions
  module P = DessserParser
  module T = DessserTypes
  module TC = DessserTypeCheck
  module U = DessserCompilationUnit
  let dbg = false
  let test_num = ref 0
  let test_name () =
    incr test_num ;
    "dessser_test_"^ string_of_int !test_num
*)

(*
 * Some misc generators
 *)

let ui128_gen =
  let open Gen in
  map2 (fun lo hi ->
    Uint128.((shift_left (of_int64 hi) 64) + of_int64 lo)
  ) ui64 ui64

let i128_gen =
  let open Gen in
  map Int128.of_uint128 ui128_gen

let tiny_int =
  Gen.int_range 1 10

let tiny_array gen =
  (* Used for tuple/record arguments so must be longer than 2 *)
  Gen.(array_size (int_range 2 4) gen)

let tiny_list gen =
  Gen.(list_size (int_range 1 4) gen)

let field_name_gen =
  let open Gen in
  let all_chars = "abcdefghijklmnopqrstuvwxyz" in
  let gen = map (fun n -> all_chars.[n mod String.length all_chars]) nat in
  string_size ~gen (int_range 4 6)

let let_name_gen = field_name_gen

(* Useful when generate random comments: avoids including another comment
 * or an unterminated string *)
let printable_for_comments =
  Gen.(map (fun c ->
    if c = '*' || c = '"' || c = '|' then 'X' else c) printable)

(* For s-expr and csv strings, as long as escaping is not supported: *)
let printable_no_escape =
  Gen.(map (fun c ->
    if c = '\\' || c = '"' || c = '\'' || c = '\n' || c = '\r' || c = '\t' ||
       c = '\b' || c = ','
    then 'X' else c) printable)

(*
 * Random types generator
 *)

let mac_type_gen =
  Gen.sized (fun n _st ->
    match n mod 22 with
    | 0 -> T.TFloat
    | 1 -> T.TString
    | 2 -> T.TBool
    | 3 -> T.TChar
    | 4 -> T.TU8
    | 5 -> T.TU16
    | 6 -> T.TU24
    | 7 -> T.TU32
    | 8 -> T.TU40
    | 9 -> T.TU48
    | 10 -> T.TU56
    | 11 -> T.TU64
    | 12 -> T.TU128
    | 13 -> T.TI8
    | 14 -> T.TI16
    | 15 -> T.TI24
    | 16 -> T.TI32
    | 17 -> T.TI40
    | 18 -> T.TI48
    | 19 -> T.TI56
    | 20 -> T.TI64
    | 21 -> T.TI128
    | _ -> assert false)

let user_type_gen =
  (* This module is linked after DessserTypes and therefore is initialized after
   * it, so it is OK to get default user types now: *)
  let user_type_keys = Hashtbl.keys T.user_types |> Array.of_enum in
  Gen.(sized (fun n _st ->
    let k = user_type_keys.(n mod Array.length user_type_keys) in
    Hashtbl.find T.user_types k))

let rec value_type_gen_of_depth depth =
  let open Gen in
  assert (depth >= 0) ;
  if depth = 0 then
    frequency
      (* User types are always considered opaque: *)
      [ 1, map (fun ut -> T.TUsr ut) user_type_gen ;
        9, mac_type_gen ]
  else
    let mn_gen = maybe_nullable_gen_of_depth (depth - 1) in
    oneof
      [ map2 (fun dim mn -> T.TVec (dim, mn)) (int_range 1 10) mn_gen ;
        map (fun mn -> T.TArr mn) mn_gen ;
        map (fun mn -> T.TSet (Simple, mn)) mn_gen ;
        map (fun mns -> T.TTup mns) (tiny_array mn_gen) ;
        map (fun fs -> T.TRec fs) (tiny_array (pair field_name_gen mn_gen)) ;
        map (fun fs -> T.TSum fs) (tiny_array (pair field_name_gen mn_gen)) ;
        (* Avoid maps for now, as there is no manipulable values of that type:
        map2 (fun k v -> T.Map (k, v)) mn_gen mn_gen *) ]

and maybe_nullable_gen_of_depth depth =
  Gen.map2 (fun nullable value ->
    T.maybe_nullable ~nullable value
  ) Gen.bool (value_type_gen_of_depth depth)

(*$Q maybe_nullable_gen_of_depth
  (Q.int_range 0 9) (fun d -> \
    let mn = Q.Gen.generate1 (maybe_nullable_gen_of_depth d) in \
    let dep = T.depth ~opaque_user_type:true mn.T.typ in \
    if d <> dep then \
      BatPrintf.printf "type = %a, depth = %d <> %d\n%!" \
        T.print_mn mn dep d ; \
    dep = d)
*)

let value_type_gen =
  Gen.(frequency
    [ 6, value_type_gen_of_depth 0 ;
      2, value_type_gen_of_depth 1 ;
      2, value_type_gen_of_depth 2 ;
      1, value_type_gen_of_depth 3 ])

let maybe_nullable_gen =
  Gen.(sized_size (int_range 0 3) maybe_nullable_gen_of_depth)

let rec size_of_value_type = function
  (* Or the question has little practical interest: *)
  | T.TThis _ -> 1
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 -> 1
  | TUsr _ -> 1
  | TVec (_, mn) | TArr mn | TSet (_, mn) -> 1 + size_of_mn mn
  | TTup mns ->
      Array.fold_left (fun s mn -> s + size_of_mn mn) 0 mns
  | TRec mns ->
      Array.fold_left (fun s (_, mn) -> s + size_of_mn mn) 0 mns
  | TSum mns ->
      Array.fold_left (fun s (_, mn) -> s + size_of_mn mn) 0 mns
  | TMap (k, v) ->
      size_of_mn k + size_of_mn v
  | _ -> invalid_arg "size_of_value_type"

and size_of_mn mn =
  size_of_value_type mn.typ

let shrink_mac_type mt =
  let to_simplest =
    T.[ TString ; TFloat ;
        TI128 ; TU128 ; TI64 ; TU64 ; TI56 ; TU56 ; TI48 ; TU48 ; TI40 ; TU40 ;
        TI32 ; TU32 ; TI24 ; TU24 ; TI16 ; TU16 ; TI8 ; TU8 ; TChar ; TBool ] in
  (* Keep only types that are simpler than [mt]: *)
  let rec loop = function
    | [] -> Iter.empty
    | mt'::rest when T.eq mt' mt ->
        if rest = [] then Iter.empty else Iter.of_list rest
    | _::rest ->
        loop rest in
  loop to_simplest

let rec shrink_value_type =
  let open Iter in
  let vt_of_mn mn = mn.T.typ in
  let shrink_fields f =
    (Shrink.pair Shrink.nil shrink_maybe_nullable) f in
  function
  | T.TUnknown | TExt _ | TUsr _  | TVoid ->
      empty
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 as mt ->
      shrink_mac_type mt
  | TVec (dim, mn) ->
      let smn = shrink_maybe_nullable mn in
      (smn >|= vt_of_mn) <+> (smn >|= T.vec dim)
  | TArr mn ->
      let smn = shrink_maybe_nullable mn in
      (smn >|= vt_of_mn) <+> (smn >|= T.arr)
  | TSet (st, mn) ->
      let smn = shrink_maybe_nullable mn in
      (smn >|= vt_of_mn) <+> (smn >|= T.set st)
  | TTup mns ->
      (of_array mns >|= vt_of_mn)
       <+>
      (Shrink.array ~shrink:shrink_maybe_nullable mns |>
        filter (fun mns -> Array.length mns > 1) >|= T.tup)
  | TRec mns ->
      (of_array mns >|= vt_of_mn % snd)
       <+>
      (Shrink.array ~shrink:shrink_fields mns |>
        filter (fun mns -> Array.length mns > 1) >|= T.record)
  | TSum mns ->
      (of_array mns >|= vt_of_mn % snd)
       <+>
      (Shrink.array ~shrink:shrink_fields mns |>
        filter (fun mns -> Array.length mns > 1) >|= T.sum)
  | TMap (k, v) ->
      let sk = shrink_maybe_nullable k in
      let sv = shrink_maybe_nullable v in
      (sk >|= vt_of_mn) <+> (sv >|= vt_of_mn)
        <+>
      (sk >|= fun k -> T.map k v)
        <+>
      (sv >|= fun v -> T.map k v)
  | _ ->
      empty

and shrink_maybe_nullable mn =
  let vt = mn.typ in
  if mn.nullable then
    (fun f ->
      shrink_value_type vt (fun typ ->
        f (T.required typ) ;
        f (T.optional typ)))
  else
    (fun f ->
      shrink_value_type vt (fun typ ->
        f (T.required typ)))

let value_type =
  let print = IO.to_string T.print
  and small = size_of_value_type
  and shrink = shrink_value_type in
  make ~print ~small ~shrink value_type_gen

let maybe_nullable =
  let print = IO.to_string T.print_mn
  and small = size_of_mn
  and shrink = shrink_maybe_nullable in
  make ~print ~small ~shrink maybe_nullable_gen

(*$Q maybe_nullable & ~count:20
  maybe_nullable (fun mn -> \
    let str = IO.to_string T.print_mn mn in \
    let mn' = P.mn_of_string str in \
    T.eq_mn mn' mn)
*)

(*
 * Random expressions generator
 *)

module E = DessserExpressions
open E.Ops

let map4 f w x y z st = f (w st) (x st) (y st) (z st)
let map5 f v w x y z st = f (v st) (w st) (x st) (y st) (z st)

let endianness_gen =
  Gen.(map (function
    | true -> LittleEndian
    | false -> BigEndian
  ) bool)

let path_gen =
  Gen.(tiny_list tiny_int)

(* Those constructors with no arguments only *)
let e1_of_int n =
  let e1s =
    T.[|
      Dump ;
      Ignore ;
      IsNull ;
      NotNull ;
      Force "" ;
      StringOfFloat ;
      DecimalStringOfFloat ;
      StringOfChar ;
      StringOfInt ;
      StringOfIp ;
      FloatOfString ;
      U8OfString ;
      U16OfString ;
      U24OfString ;
      U32OfString ;
      U40OfString ;
      U48OfString ;
      U56OfString ;
      U64OfString ;
      U128OfString ;
      I8OfString ;
      I16OfString ;
      I24OfString ;
      I32OfString ;
      I40OfString ;
      I48OfString ;
      I56OfString ;
      I64OfString ;
      I128OfString ;
      FloatOfPtr ;
      CharOfPtr ;
      U8OfPtr ;
      U16OfPtr ;
      U24OfPtr ;
      U32OfPtr ;
      U40OfPtr ;
      U48OfPtr ;
      U56OfPtr ;
      U64OfPtr ;
      U128OfPtr ;
      I8OfPtr ;
      I16OfPtr ;
      I24OfPtr ;
      I32OfPtr ;
      I40OfPtr ;
      I48OfPtr ;
      I56OfPtr ;
      I64OfPtr ;
      I128OfPtr ;
      ToU8 ;
      ToU16 ;
      ToU24 ;
      ToU32 ;
      ToU40 ;
      ToU48 ;
      ToU56 ;
      ToU64 ;
      ToU128 ;
      ToI8 ;
      ToI16 ;
      ToI24 ;
      ToI32 ;
      ToI40 ;
      ToI48 ;
      ToI56 ;
      ToI64 ;
      ToI128 ;
      ToFloat ;
      BitNot ;
      FloatOfU64 ;
      U64OfFloat ;
      U8OfChar ;
      CharOfU8 ;
      SizeOfU32 ;
      U32OfSize ;
      ArrOfLst ;
      ArrOfLstRev ;
      SetOfLst ;
      ArrOfVec ;
      ArrOfSet ;
      U8OfBool ;
      BoolOfU8 ;
      StringLength ;
      BytesLength ;
      StringOfBytes ;
      BytesOfString ;
      Cardinality ;
      ReadU8 ;
      RemSize ;
      Not ;
      Abs ;
      Neg ;
      Exp ;
      Log ;
      UnsafeLog ;
      Log10 ;
      UnsafeLog10 ;
      Sqrt ;
      UnsafeSqrt ;
      Ceil ;
      Floor ;
      Round ;
      Cos ;
      Sin ;
      Tan ;
      ACos ;
      ASin ;
      ATan ;
      CosH ;
      SinH ;
      TanH ;
      Lower ;
      Upper ;
      Hash ;
      Identity ;
      GetEnv ;
      GetMin |] in
  e1s.(n mod Array.length e1s)

let e2_of_int n =
  let e2s =
    T.[|
      Nth ;
      Gt ;
      Ge ;
      Eq ;
      Add ;
      Sub ;
      Mul ;
      Div ;
      UnsafeDiv ;
      Rem ;
      UnsafeRem ;
      Pow ;
      UnsafePow ;
      BitAnd ;
      BitOr ;
      BitXor ;
      And ;
      Or ;
      Min ;
      Max ;
      Member ;
      Insert ;
      LeftShift ;
      RightShift ;
      AppendBytes ;
      AppendString ;
      StartsWith ;
      EndsWith ;
      GetBit ;
      ReadBytes ;
      PeekU8 ;
      WriteU8 ;
      WriteBytes ;
      PokeU8 ;
      PtrAdd ;
      PtrSub ;
      And ;
      Or ;
      Min ;
      Max ;
      Member ;
      Insert ;
      DelMin ;
      SplitBy ;
      SplitAt ;
      Join ;
      AllocArr ;
      PartialSort ;
      ChopBegin ;
      ChopEnd ;
      Strftime ;
      While ;
      Index |] in
  e2s.(n mod Array.length e2s)

let e3_of_int n =
  let e3s =
    T.[|
      SetBit ;
      SetVec ;
      BlitByte ;
      If ;
      Map ;
      FindSubstring ;
      InsertWeighted ;
      SubString |] in
  e3s.(n mod Array.length e3s)

let rec e0_gen l depth =
  assert (depth >= 0) ;
  let open Gen in
  let lst = [
    (* NULL and empty set can be considered scalars (depth=0) whatever their
     * item type *)
    1, map null (value_type_gen_of_depth depth) ;
    1, map E.Ops.empty_set (maybe_nullable_gen_of_depth depth) ;
    1, return E.Ops.now ;
    1, return E.Ops.random_float ;
    1, return E.Ops.random_u8 ;
    1, return E.Ops.random_u32 ;
    1, return E.Ops.random_u64 ;
    1, return E.Ops.random_u128 ;
    1, map E.Ops.float float ;
    1, map E.Ops.string small_string ;
    1, map E.Ops.bool bool ;
    1, map E.Ops.char char ;
    1, map (E.Ops.u8 % Uint8.of_int) (int_bound 255) ;
    1, map (E.Ops.u16 % Uint16.of_int) (int_bound 65535) ;
    1, map (E.Ops.u24 % Uint24.of_int) (int_bound 16777215) ;
    1, map (E.Ops.u32 % Uint32.of_int) nat ;
    1, map (E.Ops.u40 % Uint40.of_int) nat ;
    1, map (E.Ops.u48 % Uint48.of_int) nat ;
    1, map (E.Ops.u56 % Uint56.of_int) nat ;
    1, map (E.Ops.u64 % Uint64.of_int) nat ;
    1, map (E.Ops.u128 % Uint128.of_int) nat ;
    1, map (E.Ops.i8 % Int8.of_int) (int_range (-128) 127) ;
    1, map (E.Ops.i16 % Int16.of_int) (int_range (-32768) 32767) ;
    1, map (E.Ops.i24 % Int24.of_int) (int_range (-8388608) 8388607) ;
    1, map (E.Ops.i32 % Int32.of_int) int ;
    1, map (E.Ops.i40 % Int40.of_int) int ;
    1, map (E.Ops.i48 % Int48.of_int) int ;
    1, map (E.Ops.i56 % Int56.of_int) int ;
    1, map (E.Ops.i64 % Int64.of_int) int ;
    1, map (E.Ops.i128 % Int128.of_int) int ;
    1, map E.Ops.size small_nat ;
  ] in
  let lst =
    if depth > 0 then
      (1,
        pick_from_env l depth (function
          | T.E0 (Identifier _) -> true
          | _ -> false)) ::
      (1, (
        pick_from_env l depth (function
          | T.E0 (Param _) -> true
          | _ -> false))) ::
      lst
    else lst in
  frequency lst

and e0s_gen l depth =
  assert (depth > 0) ;
  let open Gen in
  let expr = expression_gen (l, depth - 1) in
  let expr_pair = pair field_name_gen expr in
  let lst = [
    1, map E.Ops.seq (tiny_list expr) ;
    1, map E.Ops.make_vec (tiny_list expr) ;
    1, map2 E.Ops.make_arr (maybe_nullable_gen_of_depth (depth - 1))
                           (tiny_list expr) ;
    1, map E.Ops.make_tup (tiny_list expr) ;
    1, map E.Ops.make_rec (tiny_list expr_pair) ;
  ] in
  let lst =
    if depth > 0 then
      (1,
        pick_from_env l depth (function
          | E0 (Identifier _) -> true
          | _ -> false)) ::
      (1, (
        pick_from_env l depth (function
          | E0 (Param _) -> true
          | _ -> false))) ::
      lst
    else lst in
  frequency lst

(* Pick a param or identifier at random in the environment: *)
and pick_from_env l depth f =
  let open Gen in
  let es =
    List.filter_map (fun (e, _) ->
      if f e then Some e else None
    ) (List.rev_append l.E.local l.E.global) in
  if es <> [] then
    oneofl es
  else
    (* Reroll the dice: *)
    expression_gen (l, depth)

and e1_gen l depth =
  assert (depth >= 0) ;
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  frequency [
    1,
      join (
        map (fun ts ->
          let l = E.enter_function ~ts l in
          map (fun e ->
            T.E1 (Function ts, e)
          ) (expression_gen (l, depth - 1))
        ) (tiny_array maybe_nullable_gen)
      ) ;
    1, map2 comment (string ~gen:printable_for_comments) expr ;
    1, map2 get_item tiny_int expr ;
    1, map2 get_field field_name_gen expr ;
    1, map2 get_alt field_name_gen expr ;
    1, map2 read_u16 endianness_gen expr ;
    1, map2 read_u32 endianness_gen expr ;
    1, map2 read_u64 endianness_gen expr ;
    1, map2 read_u128 endianness_gen expr ;
    1, map ptr_of_string expr ;
    10, map2 (fun n e -> T.E1 (e1_of_int n, e)) nat expr ]

and e1s_gen l depth =
  assert (depth > 0) ;
  let open Gen in
  let expr = expression_gen (l, depth - 1) in
  map (function
    | [] -> assert false (* Because of tiny_list *)
    | f :: es -> E.Ops.apply f es
  ) (tiny_list expr)

and e2_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  frequency [
    1, map2 (fun name e ->
              try
                let l = E.add_local name (E.type_of l e) l in
                let body = generate1 (expression_gen (l, depth - 1)) in
                let_ ~name e (fun _e -> body)
              with _ ->
                generate1 (e2_gen l depth)
            ) let_name_gen expr ;
    1, map3 peek_u16 endianness_gen expr expr ;
    1, map3 peek_u32 endianness_gen expr expr ;
    1, map3 peek_u64 endianness_gen expr expr ;
    1, map3 peek_u128 endianness_gen expr expr ;
    1, map3 write_u16 endianness_gen expr expr ;
    1, map3 write_u32 endianness_gen expr expr ;
    1, map3 write_u64 endianness_gen expr expr ;
    1, map3 write_u128 endianness_gen expr expr ;
    10, map3 (fun n e1 e2 -> T.E2 (e2_of_int n, e1, e2)) nat expr expr ]

and e3_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  map4 (fun n e1 e2 e3 ->
    T.E3 (e3_of_int n, e1, e2, e3)
  ) nat expr expr expr

and expression_gen (l, depth) =
  let open Gen in
  fix (fun _self (l, depth) ->
    let expr = expression_gen (l, depth - 1) in
    if depth > 0 then
      frequency [
        1, map seq (list_size tiny_int expr) ;
        5, e0_gen l depth ;
        5, e0s_gen l depth ;
        5, e1_gen l depth ;
        1, e1s_gen l depth ;
        5, e2_gen l depth ;
        5, e3_gen l depth ]
    else
      e0_gen l depth
  ) (l, depth)

let expression_gen =
  Gen.(sized_size (int_bound 3) (fun n -> expression_gen (E.no_env, n)))

let size_of_expression e =
  E.fold 0 (fun n _ -> n + 1) e

(* TODO: try to shrink expressions by replacing terms with constants of the
 * same type *)
let expression =
  let print = IO.to_string E.print
  and small = size_of_expression in
  make ~print ~small expression_gen

(*$Q expression & ~count:20
  expression (fun e -> \
    let str = E.to_string e in \
    match P.expr str with \
    | [ e' ] -> E.eq e' e \
    | _ -> false)
*)

(*$inject
  open Dessser
  open DessserTools
  open DessserDSTools

  let ocaml_be = (module DessserBackEndOCaml : BACKEND)
  let cpp_be = (module DessserBackEndCPP : BACKEND)

  let can_be_compiled_with_backend be e =
    let module BE = (val be : BACKEND) in
    let test_name = test_name () in
    let compunit = U.make test_name in
    let compunit, _, _ = U.add_identifier_of_expression compunit e in
    try BE.compile ~dev_mode:true ~optim:0 ~link:Object compunit |> ignore ;
        true
    with e ->
        Printf.eprintf "FAILURE: %s\n" (Printexc.to_string e) ;
        false

  let can_be_compiled e =
    can_be_compiled_with_backend ocaml_be e &&
    can_be_compiled_with_backend cpp_be  e
*)

(*$Q expression & ~count:20
  expression (fun e -> \
    match TC.type_check E.no_env e with \
    | exception _ -> \
        true \
    | e -> \
        T.eq_mn (E.type_of E.no_env e) T.void || can_be_compiled e)
*)

(* Non regression tests: *)
(*$inject
  let compile_check s =
    let e = P.expr s |> List.hd in
    can_be_compiled e
*)
(*$T compile_check
  compile_check \
    "(make-vec (u8 1) (u8 2) (u8 3))"
  compile_check \
    "(make-tup (u16 61159) (u128 5) (null \"((String?; String?; I128?; U32)[8]?; ((I48; I40?))?; Float[9]?[])\") (u48 7) (u8 188))"
  compile_check \
    "(is-null (null \"Bool\"))"
  compile_check \
    "(null \"[opfa U48 | lhlqkp I48?[2] | lqdjnf (Char?; I40; U48; U48?)? | fcioax String?[1]?]\")"
  compile_check "(make-vec ())"
  compile_check "(make-arr \"U8\" (u8 63))"
  compile_check "(to-u8 (float 1))"
  compile_check "(to-float (u8 1))"
  compile_check "(to-u8 (u8 1))"
  compile_check "(to-float (float 1))"
  compile_check "(string-of-ip (random-u128))"
  compile_check \
    "(map nop \
          (fun (\"void\" \"u8\") (mul (param 1) (param 1))) \
          (make-vec (u8 1) (u8 2) (u8 3)))"
  compile_check \
    "(map nop \
          (fun (\"void\" \"u8\") (mul (param 1) (param 1))) \
          (make-arr \"u8\" (u8 1) (u8 2) (u8 3)))"
*)

(*
 * Random S-Expression generator
 *)

let int_string_gen mi ma =
  let open Gen in
  map (fun i ->
    let ui =
      Uint64.(
        add (of_int64 mi)
            (rem (of_int64 i)
                 (sub (of_int64 ma) (of_int64 mi)))) in
    (* if mi was < 0 then this is meant as a signed integer: *)
    if mi >= 0L then Uint64.to_string ui
    else Int64.(to_string (of_uint64 ui))
  ) ui64

let to_sexpr lst = "("^ String.join " " lst ^")"

let rec sexpr_of_typ_gen ?sexpr_config typ =
  let open Gen in
  match typ with
  | T.TNamed (_, t) ->
      sexpr_of_typ_gen ?sexpr_config t
  | TThis n ->
      let t = T.find_this n in
      sexpr_of_typ_gen ?sexpr_config t
  | TVoid ->
      return "()"
  | TFloat ->
      map hexstring_of_float float
  | TString ->
      (* FIXME: support escaping in quotes: *)
      map String.quote (string_size ~gen:printable_no_escape (int_range 3 15))
  | TChar ->
      map String.quote (string_size ~gen:printable_no_escape (int_range 1 1))
  | TBool ->
      map (function true -> "T" | false -> "F") bool
  | TU8 ->
      int_string_gen 0L 255L
  | TU16 ->
      int_string_gen 0L 65535L
  | TU24 ->
      int_string_gen 0L 16777215L
  | TU32 ->
      int_string_gen 0L 4294967295L
  | TU40 ->
      int_string_gen 0L 1099511627775L
  | TU48 ->
      int_string_gen 0L 281474976710655L
  | TU56 ->
      int_string_gen 0L 72057594037927935L
  | TU64 ->
      map Uint64.(to_string % of_int64) ui64
  | TU128 ->
      map Uint128.to_string ui128_gen
  | TI8 ->
      int_string_gen (-128L) 127L
  | TI16 ->
      int_string_gen (-32768L) 32767L
  | TI24 ->
      int_string_gen (-8388608L) 8388607L
  | TI32 ->
      int_string_gen (-2147483648L) 2147483647L
  | TI40 ->
      int_string_gen (-549755813888L) 549755813887L
  | TI48 ->
      int_string_gen (-140737488355328L) 140737488355327L
  | TI56 ->
      int_string_gen (-36028797018963968L) 36028797018963967L
  | TI64 ->
      map (fun i -> Int64.(to_string (sub i 4611686018427387904L))) ui64
  | TI128 ->
      map Int128.to_string i128_gen
  | TUsr ut ->
      sexpr_of_typ_gen ?sexpr_config ut.def
  | TVec (dim, mn) ->
      list_repeat dim (sexpr_of_mn_gen ?sexpr_config mn) |> map to_sexpr
  | TArr mn ->
      tiny_list (sexpr_of_mn_gen ?sexpr_config mn) |> map (fun lst ->
        (if DessserSExpr.((sexpr_config |? DessserConfigs.SExpr.default).list_prefix_length)
        then
          Stdlib.string_of_int (List.length lst) ^ " "
        else "") ^
        to_sexpr lst)
  | TSet (_, mn) ->
      sexpr_of_typ_gen ?sexpr_config (TArr mn)
  | TTup mns ->
      tup_gen ?sexpr_config mns
  | TRec mns ->
      tup_gen ?sexpr_config (Array.map snd mns)
  | TSum mns ->
      join (
        map (fun i ->
          let i = (Stdlib.abs i) mod (Array.length mns) in
          sexpr_of_mn_gen ?sexpr_config (snd mns.(i)) |>
          map (fun se -> "("^ Stdlib.string_of_int i ^" "^ se ^")")
        ) int
      )
  | TMap (k, v) ->
      sexpr_of_typ_gen ?sexpr_config
        (TArr { typ = TTup [| k ; v |] ; nullable = false ; default = None })
  | _ ->
      invalid_arg "sexpr_of_typ_gen"

and tup_gen ?sexpr_config mns st =
  "("^ (
    Array.fold_left (fun sexpr mn ->
      (if sexpr = "" then "" else (sexpr ^ " ")) ^
      sexpr_of_mn_gen ?sexpr_config mn st
    ) "" mns
  ) ^")"

and sexpr_of_mn_gen ?sexpr_config mn =
  let open Gen in
  if mn.nullable then
    join (
      (* Note: This "null" must obviously match the one used in DessserSExpr.ml *)
      map (function true -> return "null"
                 | false -> sexpr_of_typ_gen ?sexpr_config mn.typ) bool)
  else
    sexpr_of_typ_gen ?sexpr_config mn.typ

let sexpr ?sexpr_config mn =
  let print = BatPervasives.identity
  and small = String.length in
  make ~print ~small (sexpr_of_mn_gen ?sexpr_config mn)

(* A program that convert from s-expr to s-expr for the given schema [mn],
 * to check s-expr is reliable before using it in further tests: *)
(*$inject
  open QCheck
  open E.Ops

  let keep_temp_files = true
  let () = DessserExpressions.dump_debug := false

  let sexpr_to_sexpr be mn =
    let module S2S = DesSer (DessserSExpr.Des) (DessserSExpr.Ser) in
    let e =
      func2 (T.dptr_of_enc DessserSExpr.Des.id) (T.sptr_of_enc DessserSExpr.Ser.id)
        (fun src dst -> S2S.desser mn src dst) in
    let compunit = U.make (test_name ()) in
    make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e

  let test_desser ?sexpr_config alloc_dst be mn des ser =
    let module Des = (val des : DES) in
    let module Ser = (val ser : SER) in
    let module S2T = DesSer (DessserSExpr.Des) (Ser : SER) in
    let module T2S = DesSer (Des : DES) (DessserSExpr.Ser) in
    let e =
      func2 (T.dptr_of_enc DessserSExpr.Des.id) (T.sptr_of_enc DessserSExpr.Ser.id)
        (fun src dst ->
          E.Ops.let_ alloc_dst (fun tdst ->
            let s2t = S2T.desser ?des_config:sexpr_config mn src tdst in
            E.with_sploded_pair "s2t" s2t (fun src tdst_end ->
              let tdst = ptr_of_ptr tdst (size 0) (ptr_sub tdst_end tdst) in
              let t2s = T2S.desser ?ser_config:sexpr_config mn tdst dst in
              let dst = secnd t2s in
              make_pair src dst))) in
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    if dbg then (
      let e' = DessserEval.peval E.no_env e in
      Format.eprintf "@[<v>After peval:@,%a@." (E.pretty_print ?max_depth:None) e'
    ) ;
    let compunit = U.make (test_name ()) |> DessserJson.init in
    try
      make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e
    with exn ->
      let e' =
        try DessserEval.peval E.no_env e
        with _ -> E.Ops.string "Failure to peval" in
      Format.eprintf
        "@.ERROR %s while compiling expression@.%a@.optimized into:@a%a@."
        (Printexc.to_string exn)
        (E.pretty_print ?max_depth:None) e
        (E.pretty_print ?max_depth:None) e' ;
      raise exn

  let test_data_desser ?sexpr_config =
    test_desser ?sexpr_config (ptr_of_buffer (size 50_000))
*)

(* Given a type and a backend, build a converter from s-expr to s-expr for
 * that type, and test it using many generated random s-exprs of that
 * type: *)
(*$R
  let test_sexpr be mn =
    let exe = sexpr_to_sexpr be mn in
    Gen.generate ~n:100 (sexpr_of_mn_gen mn) |>
    List.iter (fun s ->
      let s' = String.trim (run_converter ~timeout:2 exe s) in
      if dbg then Printf.eprintf "Testing s-expr %S of type %a -> %S\n%!"
        s T.print_mn mn s' ;
      assert_equal ~printer:BatPervasives.identity s s') in
  try
    Gen.generate ~n:5 maybe_nullable_gen |>
    List.iter (fun mn ->
      test_sexpr ocaml_be mn ;
      test_sexpr cpp_be mn)
  with e ->
    Printf.eprintf "FAILURE: %s:\n%s\n"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())
*)

(* Now that we trust the s-expr ser/des, we can use it to create random
 * values or arbitrary type in the other formats: *)
(*$inject
  let test_exe ?sexpr_config format mn exe =
    Gen.generate ~n:100 (sexpr_of_mn_gen ?sexpr_config mn) |>
    List.iter (fun s ->
      let s' = String.trim (run_converter ~timeout:2 exe s) in
      if dbg then Printf.eprintf "Testing %s %S of type %a -> %S\n%!"
        format s T.print_mn mn s' ;
      assert_equal ~printer:BatPervasives.identity s s')

  let test_format ?sexpr_config be mn des ser format =
    let exe = test_data_desser ?sexpr_config be mn des ser in
    test_exe ?sexpr_config format mn exe

  module ToValue = DessserHeapValue.Materialize (DessserSExpr.Des)
  module OfValue = DessserHeapValue.Serialize (DessserSExpr.Ser)

  let heap_convert_expr compunit mn =
    let compunit, ser_func, _ = OfValue.serialize mn compunit in
    let compunit, des, _ = ToValue.make mn compunit in
    compunit,
    func2 (T.dptr_of_enc DessserSExpr.Des.id) (T.sptr_of_enc DessserSExpr.Ser.id)
      (fun src dst ->
        let v_src = apply des [ src ] in
        E.with_sploded_pair "v_src" v_src (fun v src ->
          let dst = apply ser_func [ copy_field ; v ; dst ] in
          make_pair src dst))

  let test_heap be mn =
    let compunit = U.make (test_name ()) in
    let compunit, e = heap_convert_expr compunit mn in
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    let exe = make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e in
    test_exe "heap-value" mn exe
*)
(*$R
  Gen.generate ~n:5 maybe_nullable_gen |>
  List.iter (fun mn ->
    (* RamenRingBuffer cannot encode nullable with explicit defaults *)
    let mn_ringbuf = DessserRamenRingBuffer.make_serializable mn in
    (* CSV cannot encode some nullable compound types: *)
    let mn_csv = DessserCsv.make_serializable mn in

    test_heap ocaml_be mn ;
    test_heap cpp_be mn ;
    let format = "RamenRingBuffer" in
    test_format ocaml_be mn_ringbuf
      (module DessserRamenRingBuffer.Des : DES)
      (module DessserRamenRingBuffer.Ser : SER) format ;
    test_format cpp_be mn_ringbuf
      (module DessserRamenRingBuffer.Des : DES)
      (module DessserRamenRingBuffer.Ser : SER) format ;
    let format = "RowBinary" in
    test_format ocaml_be mn
      (module DessserRowBinary.Des : DES)
      (module DessserRowBinary.Ser : SER) format ;
    test_format cpp_be mn
      (module DessserRowBinary.Des : DES)
      (module DessserRowBinary.Ser : SER) format ;
    let format = "CSV" in
    test_format ocaml_be mn_csv
      (module DessserCsv.Des : DES)
      (module DessserCsv.Ser : SER) format ;
    test_format cpp_be mn_csv
      (module DessserCsv.Des : DES)
      (module DessserCsv.Ser : SER) format ;
    let format = "JSON" in
    let sexpr_config =
      DessserConfigs.SExpr.{ default with list_prefix_length = false } in
    test_format ~sexpr_config ocaml_be mn
      (module DessserJson.Des : DES)
      (module DessserJson.Ser : SER) format ;
    test_format ~sexpr_config cpp_be mn
      (module DessserJson.Des : DES)
      (module DessserJson.Ser : SER) format)
*)

(* A function to test specifically a given value of a given type for a given
 * back-end: *)
(*$inject
  let check_sexpr be ts vs =
    let mn = P.mn_of_string ts in
    let exe = sexpr_to_sexpr be mn in
    let rs = run_converter ~timeout:2 exe vs in
    if dbg then Printf.eprintf "\ncheck_sexpr: %S vs %S\n%!" rs vs ;
    String.trim rs = vs
  let check_rowbinary be ts vs =
    let mn = P.mn_of_string ts in
    let des = (module DessserRowBinary.Des : DES)
    and ser = (module DessserRowBinary.Ser : SER) in
    let exe = test_data_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_ringbuffer be ts vs =
    let mn = P.mn_of_string ts in
    let des = (module DessserRamenRingBuffer.Des : DES)
    and ser = (module DessserRamenRingBuffer.Ser : SER) in
    let exe = test_data_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_csv be ts vs =
    let mn = P.mn_of_string ts in
    let des = (module DessserCsv.Des : DES)
    and ser = (module DessserCsv.Ser : SER) in
    let exe = test_data_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_heapvalue be ts vs =
    let mn = P.mn_of_string ts in
    let compunit = U.make (test_name ()) in
    let compunit, e = heap_convert_expr compunit mn in
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    let exe = make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e in
    String.trim (run_converter ~timeout:2 exe vs)
*)

(* Check that the AND is short-cutting, otherwise [is_null] is going to
 * read past the input end: *)
(*$T check_sexpr
  check_sexpr ocaml_be "u8?" "1"
  check_sexpr ocaml_be "u24" "15134052"
  check_sexpr ocaml_be "I8[]" "1 (2)"
  check_sexpr ocaml_be "(I8?; I40?)[]" "1 ((2 1))"
  check_sexpr ocaml_be "i40" "-161920788051"
  check_sexpr ocaml_be "{bajg: CHAR; bqgbef: U32?; eibho: U24?; gvrh: U16?}[]" \
    "1 ((\"+\" 3545637917 null 14235))"
  check_sexpr cpp_be "{bajg: CHAR; bqgbef: U32?; eibho: U24?; gvrh: U16?}[]" \
    "1 ((\"+\" 3545637917 null 14235))"
  check_sexpr ocaml_be "float" "0x1.79c428d047e73p-16"
  check_sexpr ocaml_be "float" "-0x1.79c428d047e73p-16"
  check_sexpr cpp_be "float" "-0x1.79c428d047e73p-16"
*)

(* Non regression tests: *)
(*$= check_rowbinary & ~printer:BatPervasives.identity
  "15134052" (check_rowbinary ocaml_be "u24" "15134052")

  "15134052" (check_rowbinary cpp_be "u24" "15134052")

  "(0 (0x1.79c428d047e73p-16 1234 \"\"))" \
     (check_rowbinary ocaml_be "[ Foo (float; u32; string) ]" \
        "(0 (0x1.79c428d047e73p-16 1234 \"\"))")

  "(0 (0x1.79c428d047e73p-16 1234 \"\"))" \
     (check_rowbinary ocaml_be \
        "[ Foo (float; u32; string) ]" \
        "(0 (0x1.79c428d047e73p-16 1234 \"\"))")

  "(0 ((0 (0x1.79c428d047e73p-16 1234 \"\")) \"u\" T))" \
     (check_rowbinary ocaml_be \
        "[ D { v:[Foo (float;u32;string)]; \
               u:string; w:bool }]" \
        "(0 ((0 (0x1.79c428d047e73p-16 1234 \"\")) \"u\" T))")

  "(0 ((9 (7 \"s\")) (0 (0x1.79c428d047e73p-16 1234 \"\")) \"u\" T))" \
     (check_rowbinary ocaml_be \
        "[ D { k:[E|F|G|H|I|J|K|L|M|N (u32;string)?]; \
               v:[Foo (float;u32;string)]; \
               u:string; w:bool }]" \
        "(0 ((9 (7 \"s\")) (0 (0x1.79c428d047e73p-16 1234 \"\")) \"u\" T))")

  "(3 ((9 (1234 \"glop\")) (0 (0x1.79c428d047e73p-16 1234 \"\")) \"u\" -0x1.79c428d047e73p-16 T F \"o\" 0x1.79c428d047e73p-16))" \
     (check_rowbinary ocaml_be \
        "[A|B|C|D { k:[G|H|I|J|K|L|M|N|O|P (u32;string)? |Q|R]; \
                    v:[Foo (float;u32;string)]; \
                    u:string; m:float; w:bool; d:bool; o:string; e:float }|E|F]" \
        "(3 ((9 (1234 \"glop\")) (0 (0x1.79c428d047e73p-16 1234 \"\")) \"u\" -0x1.79c428d047e73p-16 T F \"o\" 0x1.79c428d047e73p-16))")
*)

(*$= check_ringbuffer & ~printer:BatPervasives.identity
  "\"foo\"" (check_ringbuffer ocaml_be "String" "\"foo\"")
  "(\"foo\" 1)" (check_ringbuffer ocaml_be "(String?; I40?)" "(\"foo\" 1)")
  "1 ((\"foo\" 1))" (check_ringbuffer ocaml_be "(String?; I40?)[]" "1 ((\"foo\" 1))")
  "1 ((2 1))" (check_ringbuffer ocaml_be "(I8?; I40?)[]" "1 ((2 1))")
  "-5424105" (check_ringbuffer ocaml_be "I24" "-5424105")
  "((\"a\") 1)" (check_ringbuffer ocaml_be "(String[1]; u8)" "((\"a\") 1)")
  "2 (null 1)" (check_ringbuffer ocaml_be "u8?[]" "2 (null 1)")
  "4 (null 1 2 3)" (check_ringbuffer ocaml_be "u8?[]" "4 (null 1 2 3)")
  "0 ()" (check_ringbuffer cpp_be "Bool[]" "0 ()")
  "(T)" (check_ringbuffer cpp_be "Bool[1]" "(T)")
  "(1 null)" (check_ringbuffer ocaml_be "[a U32 | b String?]" "(1 null)")
  "(\"Z\" \"W\" \"3\" \"a\" \";\")" \
    (check_ringbuffer ocaml_be \
      "CHAR[5]" "(\"Z\" \"W\" \"3\" \"a\" \";\")")
  "1" (check_ringbuffer ocaml_be "u8?" "1")
  "((0 2) 0x1.b8ba44f7958e1p+0)" \
    (check_ringbuffer ocaml_be \
      "([v4 U32 | v6 U8]; FLOAT)" "((0 2) 0x1.b8ba44f7958e1p+0)")
  "(76 null)" \
    (check_ringbuffer ocaml_be \
      "{qbjt: U8?; kduw: U32?}" \
      "(76 null)")
  "((48599 43985) 2 (null 45861965684654) null)" \
    (check_ringbuffer ocaml_be "(U16[2]?; I48?{}; BOOL?)" \
      "((48599 43985) 2 (null 45861965684654) null)")
*)
(*$= check_heapvalue & ~printer:BatPervasives.identity
  "1 ((1))" (check_heapvalue ocaml_be "U16[1][]" "1 ((1))")
  "3 (1 2 3)" (check_heapvalue ocaml_be "U16[]" "3 (1 2 3)")
  "(1 5)" (check_heapvalue cpp_be "{ejgvx: U16; kngke: U64}" "(1 5)")
  "1 (6)" (check_heapvalue cpp_be "U8[]?" "1 (6)")
  "2 (214 null)" (check_heapvalue ocaml_be "U8?{}" "2 (214 null)")
  "2 (214 null)" (check_heapvalue cpp_be "U8?{}" "2 (214 null)")
  "((1 T) (0 null))" \
    (check_heapvalue ocaml_be "[a U8? | b BOOL][2]" "((1 T) (0 null))")
*)
(*$= check_ringbuffer & ~printer:BatPervasives.identity
  "-5424105" (check_ringbuffer ocaml_be "I24" "-5424105")
  "-5424105" (check_ringbuffer cpp_be "I24" "-5424105")
*)

(* Return serialized string of a given value of a given type, for that
 * serializer and backend: *)
(*$inject
  let ringbuf_ser = (module DessserRamenRingBuffer.Ser : SER)
  let rowbinary_ser = (module DessserRowBinary.Ser : SER)
  let csv_ser = (module DessserCsv.Ser : SER)
  let sexpr_des = (module DessserSExpr.Des : DES)

  let check_ser ser be ts vs =
    let mn = P.mn_of_string ts in
    let module Ser = (val ser : SER) in
    let module DS = DesSer (DessserSExpr.Des) (Ser) in
    let exe =
      let e =
        func2 T.ptr T.ptr (fun src dst ->
          DS.desser mn src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      let compunit = U.make (test_name ()) in
      make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e in
    String.trim (run_converter ~timeout:2 exe vs) |>
    hexify_string

  let check_des des be ts vs =
    let mn = P.mn_of_string ts in
    let module Des = (val des : DES) in
    let module DS = DesSer (Des) (DessserSExpr.Ser) in
    let exe =
      let e =
        func2 T.ptr T.ptr (fun src dst ->
          DS.desser mn src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      let compunit = U.make (test_name ()) in
      make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e in
    String.trim (run_converter ~timeout:2 exe vs) |>
    hexify_string
*)
(*$= check_ser & ~printer:BatPervasives.identity
  "2a" \
    (check_ser rowbinary_ser  ocaml_be "u8" "42")
  "2a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "u8" "42")
  "01 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "u8?" "42")
  "01 00 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8; i8)" "(42 58)")
  "01 00 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8?; i8)" "(42 58)")
  "01 00 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8?; i8?)" "(42 58)")
  "01 00 00 00 01 00 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8?; i8?)?" "(42 58)")
  "00 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "[small u8 | big u16]" "(0 42)")
  "01 00 00 00 00 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "[small u8 | big u16]?" "(0 42)")
  "00 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "[small u8 | big u16?]" "(0 42)")
  "00 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "[small u8? | big u16]" "(0 42)")
  "30 2c 34 32" \
    (check_ser csv_ser ocaml_be "[small u8? | big u16]" "(0 42)")
*)

(* Special version of check_des with a custom CSV configuration: *)
(*$inject
  let check_des_csv ?config be ts vs =
    let mn = P.mn_of_string ts in
    let module DS = DesSer (DessserCsv.Des) (DessserSExpr.Ser) in
    let ser_config =
      DessserConfigs.SExpr.{ default with list_prefix_length = false } in
    let exe =
      let e =
        func2 T.ptr T.ptr (fun src dst ->
          DS.desser ~ser_config ?des_config:config mn src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      let compunit = U.make (test_name ()) in
      make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e in
    String.trim (run_converter ~timeout:2 exe vs)

  let check_ser_csv ?config be ts vs =
    let mn = P.mn_of_string ts in
    let module DS = DesSer (DessserSExpr.Des) (DessserCsv.Ser) in
    let exe =
      let e =
        func2 T.ptr T.ptr (fun src dst ->
          DS.desser ?ser_config:config mn src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      let compunit = U.make (test_name ()) in
      make_converter ~dev_mode:true ~keep_temp_files ~mn compunit be e in
    String.trim (run_converter ~timeout:2 exe vs)

  let csv_config_0 =
    DessserConfigs.Csv.{ default with
      separator = '|' ;
      null = "" ;
      true_ = "true" ;
      false_ = "false" ;
      clickhouse_syntax = false }

  let csv_config_1 =
    DessserConfigs.Csv.{ default with
      null = "" ;
      true_ = "true" ;
      false_ = "false" ;
      clickhouse_syntax = false }

  let csv_config_2 =
    DessserConfigs.Csv.{ default with quote = None }

  let csv_config_CH =
    DessserConfigs.Csv.{ default with
      true_ = "1" ;
      false_ = "0" ;
      clickhouse_syntax = true }

  let csv_config_ramen =
    DessserConfigs.Csv.{ default with
      separator = '\t' ;
      quote = None ;
      clickhouse_syntax = true }
*)
(*$= check_des_csv & ~printer:BatPervasives.identity
  "(1 F null)" \
    (check_des_csv ~config:csv_config_0 ocaml_be \
                   "{u:U8; b:BOOL; name:STRING?}" "1|false|\n")
  "(1 T \"one\")" \
    (check_des_csv ~config:csv_config_1 ocaml_be \
                   "{u:U8; b:BOOL; name:STRING?}" "1,true,\"one\"\n")
  "(2 T \"two\")" \
    (check_des_csv ~config:csv_config_1 ocaml_be \
                   "{u:U8; b:BOOL; name:STRING?}" "2,true,\"two\"\n")
  "(3 F null)" \
    (check_des_csv ~config:csv_config_1 ocaml_be \
                   "{u:U8; b:BOOL; name:STRING?}" "3,false,\n")
  "-0x1.79c428d047e73p-16" \
    (check_des_csv ~config:csv_config_0 ocaml_be \
                   "FLOAT?" "-0x1.79c428d047e73p-16\n")
  "-0x1.79c428d047e73p-16" \
    (check_des_csv ~config:csv_config_0 cpp_be \
                   "FLOAT?" "-0x1.79c428d047e73p-16\n")
  "((1 2 3) F null)" \
    (check_des_csv ~config:csv_config_CH ocaml_be \
                   "{u:U8[3]; b:BOOL; name:STRING?}" "\"[1,2,3]\",0,\\N\n")
*)
(* Test FixedStrings *)
(*$= check_des_csv & ~printer:BatPervasives.identity
  "(\"a\" \"b\" \"c\")" \
    (check_des_csv ~config:csv_config_1 ocaml_be \
                   "char[3]" "\"abc\"\n")
  "(\"a\" \"b\" \"c\")" \
    (check_des_csv ~config:csv_config_2 ocaml_be \
                   "char[3]" "abc\n")
*)
(* Test ClickHouse syntax for lists: *)
(*$= check_des_csv & ~printer:BatPervasives.identity
  "(() null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[]; u64?)" \
      "[]	\\N")
  "(() null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[]?; u64?)" \
      "[]	\\N")
  "(null null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[]?; u64?)" \
      "\\N	\\N")
  "(() null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16?[]; u64?)" \
      "[]	\\N")
  "((42) null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[]; u64?)" \
      "[42]	\\N")
  "((42 43 44) null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[]; u64?)" \
      "[42,43,44]	\\N")
  "((42 null 44) null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16?[]; u64?)" \
      "[42,\\N,44]	\\N")
  "(null null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[3]?; u64?)" \
      "\\N	\\N")
  "((42 43 44) null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16[3]; u64?)" \
      "[42,43,44]	\\N")
  "((42 null 44) null)" \
    (check_des_csv ~config:csv_config_ramen ocaml_be "(u16?[3]; u64?)" \
      "[42,\\N,44]	\\N")
*)
(*$= check_ser_csv & ~printer:BatPervasives.identity
  "\"abc\"" \
    (check_ser_csv ~config:csv_config_1 ocaml_be \
                   "char[3]" "(\"a\" \"b\" \"c\")")
  "abc" \
    (check_ser_csv ~config:csv_config_2 ocaml_be \
                   "char[3]" "(\"a\" \"b\" \"c\")")
*)

(* Test DessserCsv.make_serializable: *)
(*$Q maybe_nullable & ~count:20
  maybe_nullable (fun mn -> \
    let mn = DessserCsv.make_serializable mn in \
    DessserCsv.is_serializable mn)
*)
