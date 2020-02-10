(* Random genrator for types and expressions *)
open Batteries
open Stdint
open QCheck
open DessserTypes
open DessserTools
open DessserFloatTools

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
  Gen.(array_size (int_range 1 5) gen)

let tiny_list gen =
  Gen.(list_size (int_range 1 5) gen)

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

(* For s-expr strings, as long as escaping is not supported: *)
let printable_no_escape =
  Gen.(map (fun c ->
    if c = '\\' || c = '"' || c = '\'' || c = '\n' || c = '\r' || c = '\t' ||
       c = '\b'
    then 'X' else c) printable)

(*
 * Random types generator
 *)

let mac_type_gen =
  Gen.sized (fun n _st ->
    match n mod 22 with
    | 0 -> TFloat
    | 1 -> TString
    | 2 -> TBool
    | 3 -> TChar
    | 4 -> TU8
    | 5 -> TU16
    | 6 -> TU24
    | 7 -> TU32
    | 8 -> TU40
    | 9 -> TU48
    | 10 -> TU56
    | 11 -> TU64
    | 12 -> TU128
    | 13 -> TI8
    | 14 -> TI16
    | 15 -> TI24
    | 16 -> TI32
    | 17 -> TI40
    | 18 -> TI48
    | 19 -> TI56
    | 20 -> TI64
    | 21 -> TI128
    | _ -> assert false)

let user_type_gen =
  (* This module is linked after DessserTypes and therefore is initialized after
   * it, so it is OK to get default user types now: *)
  let user_type_keys = Hashtbl.keys user_types |> Array.of_enum in
  Gen.(sized (fun n _st ->
    let k = user_type_keys.(n mod Array.length user_type_keys) in
    Hashtbl.find user_types k))

let rec value_type_gen depth =
  let open Gen in
  if depth > 0 then
    let mn_gen = maybe_nullable_gen (depth - 1) in
    let lst =
      [ 4, map (fun mt -> Mac mt) mac_type_gen ;
        1, map (fun ut -> Usr ut) user_type_gen ;
        2, map2 (fun dim mn -> TVec (dim, mn)) (int_range 1 10) mn_gen ;
        2, map (fun mn -> TList mn) mn_gen ;
        2, map (fun mns -> TTup mns) (tiny_array mn_gen) ;
        2, map (fun fs -> TRec fs) (tiny_array (pair field_name_gen mn_gen)) ;
        (* Avoid maps for now, as there is no manipulable values of that type: *)
        0, map2 (fun k v -> TMap (k, v)) mn_gen mn_gen ] in
    frequency lst
  else
    map (fun mt -> Mac mt) mac_type_gen

and maybe_nullable_gen depth =
  Gen.(fix (fun _self depth ->
    map2 (fun b vt ->
      if b then Nullable vt else NotNullable vt
    ) bool (value_type_gen depth)
  ) depth)

let value_type_gen =
  Gen.(sized_size (int_bound 4) value_type_gen)

let maybe_nullable_gen =
  Gen.(sized_size (int_bound 4) maybe_nullable_gen)

let rec size_of_value_type = function
  | Mac _ | Usr _ -> 1
  | TVec (_, mn) | TList mn -> size_of_maybe_nullable mn
  | TTup typs ->
      Array.fold_left (fun s mn -> s + size_of_maybe_nullable mn) 0 typs
  | TRec typs ->
      Array.fold_left (fun s (_, mn) -> s + size_of_maybe_nullable mn) 0 typs
  | TMap (k, v) ->
      size_of_maybe_nullable k + size_of_maybe_nullable v

and size_of_maybe_nullable = function
  | Nullable vt | NotNullable vt -> size_of_value_type vt

let shrink_mac_type mt =
  let to_simplest =
    [ TString ; TFloat ;
      TI128 ; TU128 ; TI64 ; TU64 ; TI56 ; TU56 ; TI48 ; TU48 ; TI40 ; TU40 ;
      TI32 ; TU32 ; TI24 ; TU24 ; TI16 ; TU16 ; TI8 ; TU8 ; TChar ; TBool ] in
  let rec loop = function
    | [] -> Iter.empty
    | mt'::rest when mt' = mt ->
        if rest = [] then Iter.empty else Iter.of_list rest
    | _::rest ->
        loop rest in
  loop to_simplest

let rec shrink_value_type =
  let vt_of_mn = function NotNullable vt | Nullable vt -> vt
  in
  function
  | Mac mt ->
      (fun f ->
        shrink_mac_type mt (fun mt -> f (Mac mt)))
  | Usr _ ->
      Iter.empty
  | TVec (dim, mn) ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f (vt_of_mn mn) ;
          f (TVec (dim, mn))))
  | TList mn ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f (TList mn) ;
          f (vt_of_mn mn)))
  | TTup mns ->
      (fun f ->
        Array.iter (fun mn -> shrink_maybe_nullable mn (f % vt_of_mn)) mns ;
        let shrink_mns =
          Shrink.filter (fun mns -> Array.length mns > 1)
            (Shrink.array ~shrink:shrink_maybe_nullable) mns |>
          Iter.map (fun mns -> TTup mns) in
        shrink_mns f)
  | TRec mns ->
      (fun f ->
        Array.iter (fun (_, mn) -> shrink_maybe_nullable mn (f % vt_of_mn)) mns ;
        let shrink_mns =
          let shrink (fn, mn) =
            Iter.map (fun mn -> fn, mn) (shrink_maybe_nullable mn) in
          Shrink.filter (fun mns -> Array.length mns > 1)
            (Shrink.array ~shrink) mns |>
          Iter.map (fun mns -> TRec mns) in
        shrink_mns f)
  | TMap (k, v) ->
      (fun f ->
        shrink_maybe_nullable k (f % vt_of_mn) ;
        shrink_maybe_nullable v (f % vt_of_mn) ;
        let shrink_kv =
          (Shrink.pair shrink_maybe_nullable shrink_maybe_nullable) (k, v) |>
          Iter.map (fun (k, v) -> TMap (k, v)) in
        shrink_kv f)

and shrink_maybe_nullable = function
  | Nullable vt ->
      (fun f ->
        shrink_value_type vt (fun vt ->
          f (NotNullable vt) ;
          f (Nullable vt)))
  | NotNullable vt ->
      (fun f ->
        shrink_value_type vt (fun vt -> f (NotNullable vt)))

let value_type =
  let print = IO.to_string print_value_type
  and small = size_of_value_type
  and shrink = shrink_value_type in
  make ~print ~small ~shrink value_type_gen

let maybe_nullable =
  let print = IO.to_string print_maybe_nullable
  and small = size_of_maybe_nullable
  and shrink = shrink_maybe_nullable in
  make ~print ~small ~shrink maybe_nullable_gen

(*$inject
   open Batteries
   module T = DessserTypes
   module E = DessserExpressions *)

(*$Q maybe_nullable & ~count:100
  maybe_nullable (fun mn -> \
    let str = IO.to_string T.print_maybe_nullable mn in \
    let mn' = T.Parser.maybe_nullable_of_string str in \
    T.maybe_nullable_eq mn' mn)
*)

(*
 * Random expressions generator
 *)

open DessserExpressions
open Ops

let map4 f w x y z st = f (w st) (x st) (y st) (z st)
let map5 f v w x y z st = f (v st) (w st) (x st) (y st) (z st)

let endianness_gen =
  Gen.(map (function
    | true -> LittleEndian
    | false -> BigEndian
  ) bool)

let path_gen =
  Gen.(tiny_list tiny_int)

let get_next_fid =
  let next_fid = ref 0 in
  fun () ->
    incr next_fid ;
    !next_fid

(* Those with no arguments only *)
let e1_of_int n =
  let e1s =
    [| Dump ; Ignore ; IsNull ; ToNullable ; ToNotNullable ; StringOfFloat ;
       StringOfChar ; StringOfInt ; FloatOfString ; CharOfString ; U8OfString ;
       U16OfString ; U24OfString ; U32OfString ; U40OfString ; U48OfString ;
       U56OfString ; U64OfString ; U128OfString ; I8OfString ; I16OfString ;
       I24OfString ; I32OfString ; I40OfString ; I48OfString ; I56OfString ;
       I64OfString ; I128OfString ; ToU8 ; ToU16 ; ToU24 ; ToU32 ; ToU40 ;
       ToU48 ; ToU56 ; ToU64 ; ToU128 ; ToI8 ; ToI16 ; ToI24 ; ToI32 ; ToI40 ;
       ToI48 ; ToI56 ; ToI64 ; ToI128 ; LogNot ; FloatOfQWord ; QWordOfFloat ;
       U8OfByte ; ByteOfU8 ; U16OfWord ; WordOfU16 ; U32OfDWord ; DWordOfU32 ;
       U64OfQWord ; QWordOfU64 ; U128OfOWord ; OWordOfU128 ; U8OfChar ;
       CharOfU8 ; SizeOfU32 ; U32OfSize ; BitOfBool ; BoolOfBit ; U8OfBool ;
       BoolOfU8 ; StringLength ; StringOfBytes ; BytesOfString ; ListLength ;
       ReadByte ; DataPtrPush ; DataPtrPop ; RemSize ; Not ; DerefValuePtr ;
       Fst ; Snd |] in
  e1s.(n mod Array.length e1s)

let e2_of_int n =
  let e2s =
    [| Coalesce ; Nth ; Gt ; Ge ; Eq ; Ne ; Add ; Sub ; Mul ; Div ; Rem ;
       LogAnd ; LogOr ; LogXor ; LeftShift ; RightShift ; AppendBytes ;
       AppendString ; GetBit ; ReadBytes ; PeekByte ; WriteByte ; WriteBytes ;
       PokeByte ; DataPtrAdd ; DataPtrSub ; And ; Or ; Pair ; MapPair |] in
  e2s.(n mod Array.length e2s)

let e3_of_int n =
  let e3s = [| SetBit ; BlitByte ; Choose ; LoopWhile ; LoopUntil |] in
  e3s.(n mod Array.length e3s)

let e4_of_int n =
  let e4s = [| ReadWhile ; Repeat |] in
  e4s.(n mod Array.length e4s)

let rec e0_gen l depth =
  let open Gen in
  let lst = [
    1, map null value_type_gen ;
    1, map Ops.float float ;
    1, map Ops.string small_string ;
    1, map Ops.bool bool ;
    1, map Ops.char char ;
    1, map Ops.u8 (int_bound 255) ;
    1, map Ops.u16 (int_bound 65535) ;
    1, map Ops.u24 (int_bound 16777215) ;
    1, map (Ops.u32 % Uint32.of_int) nat ;
    1, map (Ops.u40 % Uint40.of_int) nat ;
    1, map (Ops.u48 % Uint48.of_int) nat ;
    1, map (Ops.u56 % Uint56.of_int) nat ;
    1, map (Ops.u64 % Uint64.of_int) nat ;
    1, map (Ops.u128 % Uint128.of_int) nat ;
    1, map Ops.i8 (int_range (-128) 127) ;
    1, map Ops.i16 (int_range (-32768) 32767) ;
    1, map Ops.i24 (int_range (-8388608) 8388607) ;
    1, map (Ops.i32 % Int32.of_int) int ;
    1, map (Ops.i40 % Int64.of_int) int ;
    1, map (Ops.i48 % Int64.of_int) int ;
    1, map (Ops.i56 % Int64.of_int) int ;
    1, map (Ops.i64 % Int64.of_int) int ;
    1, map (Ops.i128 % Int128.of_int) int ;
    1, map Ops.bit bool ;
    1, map Ops.size small_nat ;
    1, map Ops.byte (int_bound 255) ;
    1, map Ops.word (int_bound 65535) ;
    1, map (Ops.dword % Uint32.of_int32) ui32 ;
    1, map (Ops.qword % Uint64.of_int64) ui64 ;
    1, map oword ui128_gen ;
    1, map data_ptr_of_string small_string ;
    1, map alloc_value maybe_nullable_gen ;
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

and e0s_gen l depth =
  let open Gen in
  let expr = expression_gen (l, depth - 1) in
  let lst = [
    1, map Ops.seq (tiny_list expr) ;
    1, map Ops.make_vec (tiny_list expr) ;
    1, map Ops.make_list (tiny_list expr) ;
    1, map Ops.make_tup (tiny_list expr) ;
    1, map Ops.make_rec (tiny_list expr) ;
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
    List.filter_map (fun (e, _t) -> if f e then Some e else None) l in
  if es <> [] then
    oneofl es
  else
    (* Reroll the dice: *)
    expression_gen (l, depth)

and e1_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  frequency [
    1,
      join (
        map (fun ts ->
          let ts = Array.map (fun mn -> TValue mn) ts in
          let fid = get_next_fid () in
          let l =
            Array.fold_lefti (fun l i t ->
              (param fid i, t) :: l
            ) l ts in
          map (fun e ->
            E1 (Function (fid, ts), e)
          ) (expression_gen (l, depth - 1))
        ) (tiny_array maybe_nullable_gen)
      ) ;
    1, map2 comment (string ~gen:printable_for_comments) expr ;
    1, map2 field_is_null path_gen expr ;
    1, map2 get_field path_gen expr ;
    1, map2 get_item tiny_int expr ;
    1, map2 get_field_ field_name_gen expr ;
    1, map2 read_word endianness_gen expr ;
    1, map2 read_dword endianness_gen expr ;
    1, map2 read_qword endianness_gen expr ;
    1, map2 read_oword endianness_gen expr ;
    10, map2 (fun n e -> E1 (e1_of_int n, e)) nat expr ]

and e2_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  frequency [
    1, map3 let_ let_name_gen expr expr ;
    1, map3 set_field path_gen expr expr ;
    1, map3 peek_word endianness_gen expr expr ;
    1, map3 peek_dword endianness_gen expr expr ;
    1, map3 peek_qword endianness_gen expr expr ;
    1, map3 peek_oword endianness_gen expr expr ;
    1, map3 write_word endianness_gen expr expr ;
    1, map3 write_dword endianness_gen expr expr ;
    1, map3 write_qword endianness_gen expr expr ;
    1, map3 write_oword endianness_gen expr expr ;
    10, map3 (fun n e1 e2 -> E2 (e2_of_int n, e1, e2)) nat expr expr ]

and e3_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  map4 (fun n e1 e2 e3 -> E3 (e3_of_int n, e1, e2, e3)) nat expr expr expr

and e4_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  map5 (fun n e1 e2 e3 e4 -> E4 (e4_of_int n, e1, e2, e3, e4)) nat expr expr expr expr

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
        5, e2_gen l depth ;
        5, e3_gen l depth ;
        5, e4_gen l depth ]
    else
      e0_gen l depth
  ) (l, depth)

let expression_gen =
  Gen.(sized_size (int_bound 4) (fun n -> expression_gen ([], n)))

let size_of_expression e =
  fold_expr 0 [] (fun n _ _ -> succ n) e

let expression =
  let print = IO.to_string print_expr
  and small = size_of_expression in
  make ~print ~small expression_gen

(*$Q expression & ~count:100
  expression (fun e -> \
    let str = IO.to_string E.print_expr e in \
    match E.Parser.expr str with \
    | [ e' ] -> expr_eq e' e \
    | _ -> false)
*)

(*$inject
  open Dessser
  open DessserTools
  open DessserDSTools

  let can_be_compiled_with_backend be e =
    let module BE = (val be : BACKEND) in
    let state = BE.make_state () in
    let state, _, _ = BE.identifier_of_expression state e in
    let src_fname =
      let ext = "."^ BE.preferred_def_extension in
      Filename.temp_file "dessserQCheck_" ext in
    let obj_fname = Filename.remove_extension src_fname in
    write_source ~src_fname (BE.print_definitions state) ;
    try compile ~optim:0 ~link:false be src_fname obj_fname ;
        ignore_exceptions Unix.unlink src_fname ;
        ignore_exceptions Unix.unlink obj_fname ;
        true
    with _ -> false

  let can_be_compiled e =
    can_be_compiled_with_backend (module BackEndOCaml : BACKEND) e &&
    can_be_compiled_with_backend (module BackEndCPP : BACKEND) e
*)

(*$Q expression & ~count:100
  expression (fun e -> \
    match type_check [] e with \
    | exception _ -> true \
    | () -> \
        if type_of [] e = TVoid then true \
        else can_be_compiled e)
*)

(* Non regression tests: *)
(*$R
  let compile_check s =
    let e = Parser.expr s |> List.hd in
    let msg = "Cannot compile "^ s in
    assert_bool msg (can_be_compiled e) in

  compile_check
    "(alloc-value \"(I48?;\
        {ksryai: U40;qthlta: (U48?)?;\
         gbjahd: {ehhd: I24;gdrnue: U16;kcpcg: I32?};\
         zkcjdi: Ipv4?;qcrck: String}[9]?)?\")" ;

  compile_check
    "(make-vec (u8 1) (u8 2) (u8 3))" ;

  compile_check
    "(make-tup (u16 61159) (u128 5) (null \"((String?; String?; I128?; U32)[8]?; ((I48; I40?))?; Float[9]?[])\") (u48 7) (u8 188))"
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

let rec sexpr_of_vtyp_gen vtyp =
  let open Gen in
  match vtyp with
  | Mac TFloat ->
      map hexstring_of_float float
  | Mac TString ->
      (* FIXME: support escaping in quotes: *)
      map String.quote (string_size ~gen:printable_no_escape (int_range 3 15))
  | Mac TChar ->
      map String.quote (string_size ~gen:printable_no_escape (int_range 1 1))
  | Mac TBool ->
      map (function true -> "T" | false -> "F") bool
  | Mac TU8 -> int_string_gen 0L 255L
  | Mac TU16 -> int_string_gen 0L 65535L
  | Mac TU24 -> int_string_gen 0L 16777215L
  | Mac TU32 -> int_string_gen 0L 4294967295L
  | Mac TU40 -> int_string_gen 0L 1099511627775L
  | Mac TU48 -> int_string_gen 0L 281474976710655L
  | Mac TU56 -> int_string_gen 0L 72057594037927935L
  | Mac TU64 -> map Uint64.(to_string % of_int64) ui64
  | Mac TU128 -> map Uint128.to_string ui128_gen
  | Mac TI8 -> int_string_gen (-128L) 127L
  | Mac TI16 -> int_string_gen (-32768L) 32767L
  | Mac TI24 -> int_string_gen (-8388608L) 8388607L
  | Mac TI32 -> int_string_gen (-2147483648L) 2147483647L
  | Mac TI40 -> int_string_gen (-549755813888L) 549755813887L
  | Mac TI48 -> int_string_gen (-140737488355328L) 140737488355327L
  | Mac TI56 -> int_string_gen (-36028797018963968L) 36028797018963967L
  | Mac TI64 -> map (fun i -> Int64.(to_string (sub i 4611686018427387904L))) ui64
  | Mac TI128 -> map Int128.to_string i128_gen
  | Usr ut -> sexpr_of_vtyp_gen ut.def
  | TVec (dim, mn) ->
      list_repeat dim (sexpr_of_mn_gen mn) |> map to_sexpr
  | TList mn ->
      tiny_list (sexpr_of_mn_gen mn) |> map (fun lst ->
        (if SExpr.list_prefix_length then
          Pervasives.string_of_int (List.length lst) ^ " "
        else "") ^
        to_sexpr lst)
  | TTup mns ->
      tup_gen mns
  | TRec mns ->
      tup_gen (Array.map Pervasives.snd mns)
  | TMap (k, v) ->
      sexpr_of_vtyp_gen (TList (NotNullable (TTup [| k ; v |])))

and tup_gen mns st =
  "("^ (
    Array.fold_left (fun sexpr mn ->
      (if sexpr = "" then "" else (sexpr ^ " ")) ^ sexpr_of_mn_gen mn st
    ) "" mns
  ) ^")"

and sexpr_of_mn_gen mn =
  let open Gen in
  match mn with
  | Nullable vt ->
      join (
        (* Note: This "null" must obviously match the one used in SExpr.ml *)
        map (function true -> return "null"
                   | false -> sexpr_of_vtyp_gen vt) bool)
  | NotNullable vt ->
      sexpr_of_vtyp_gen vt

let sexpr mn =
  let print = identity
  and small = String.length in
  make ~print ~small (sexpr_of_mn_gen mn)

(* A program that convert from s-expr to s-expr for the given schema [mn],
 * to check s-expr is reliable before using it in further tests: *)
(*$inject
  open QCheck
  let sexpr_to_sexpr be mn =
    let module S2S = DesSer (SExpr.Des) (SExpr.Ser) in
    let e =
      func2 TDataPtr TDataPtr (fun src dst ->
        S2S.desser mn src dst) in
    make_converter be ~mn e

  let test_desser be mn des ser =
    let module Des = (val des : DES) in
    let module Ser = (val ser : SER) in
    let module S2T = DesSer (SExpr.Des) (Ser : SER) in
    let module T2S = DesSer (Des : DES) (SExpr.Ser) in
    let e =
      func2 TDataPtr TDataPtr (fun src dst ->
        let open Ops in
        let1 (data_ptr_of_buffer 50_000) (fun tdst ->
          let src = fst (S2T.desser mn src tdst) in
          let dst = snd (T2S.desser mn tdst dst) in
          pair src dst)) in
    Printf.eprintf "Expression:\n  %a\n" (print_expr ?max_depth:None) e ;
    make_converter be ~mn e

  let ocaml_be = (module BackEndOCaml : BACKEND)
  let cpp_be = (module BackEndCPP : BACKEND)
*)

(* Given a type and a backend, build a converter from s-expr to s-expr for
 * that type, and test it using many generated random s-exprs of that
 * type: *)
(*$R
  let test_sexpr be mn =
    let exe = sexpr_to_sexpr be mn in
    Gen.generate ~n:100 (sexpr_of_mn_gen mn) |>
    List.iter (fun s ->
      Printf.eprintf "Will test s-expr %S of type %a\n%!"
        s T.print_maybe_nullable mn ;
      let s' = String.trim (run_converter ~timeout:2 exe s) in
      assert_equal ~printer:identity s s') in
  Gen.generate ~n:5 maybe_nullable_gen |>
  List.iter (fun mn ->
    test_sexpr ocaml_be mn ;
    test_sexpr cpp_be mn)
*)

(* Now that we trust the s-expr ser/des, we can use it to create random
 * values or arbitrary type in the other formats: *)
(*$R
  let test_format be mn des ser format =
    let exe = test_desser be mn des ser in
    Gen.generate ~n:100 (sexpr_of_mn_gen mn) |>
    List.iter (fun s ->
      Printf.eprintf "Will test %s %S of type %a\n%!"
        format s T.print_maybe_nullable mn ;
      let s' = String.trim (run_converter ~timeout:2 exe s) in
      assert_equal ~printer:identity s s') in
  Gen.generate ~n:5 maybe_nullable_gen |>
  List.iter (fun mn ->
    (* RamenRingBuffer cannot encore nullable outermost values (FIXME) *)
    let nn = T.to_not_nullable mn in
    let format = "RamenRingBuf" in
    test_format ocaml_be nn
      (module RamenRingBuffer.Des : DES) (module RamenRingBuffer.Ser : SER) format ;
    test_format cpp_be nn
      (module RamenRingBuffer.Des : DES) (module RamenRingBuffer.Ser : SER) format ;
    let format = "RowBinary" in
    test_format ocaml_be mn
      (module RowBinary.Des : DES) (module RowBinary.Ser : SER) format ;
    test_format cpp_be mn
      (module RowBinary.Des : DES) (module RowBinary.Ser : SER) format)
*)

(* Non regression tests: *)

(* A function to test specifically a given value of a given type for a given
 * back-end: *)
(*$inject
  let check_sexpr be ts vs =
    let mn = T.Parser.maybe_nullable_of_string ts in
    let exe = sexpr_to_sexpr be mn in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_rowbinary be ts vs =
    let mn = T.Parser.maybe_nullable_of_string ts in
    let des = (module RowBinary.Des : DES)
    and ser = (module RowBinary.Ser : SER) in
    let exe = test_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_ringbuffer be ts vs =
    let mn = T.Parser.maybe_nullable_of_string ts in
    let des = (module RamenRingBuffer.Des : DES)
    and ser = (module RamenRingBuffer.Ser : SER) in
    let exe = test_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
*)
(* Check that the AND is short-cutting, otherwise [is_null] is going to
 * read past the input end: *)
(*$= check_sexpr & ~printer:identity
  "1" (check_sexpr ocaml_be "u8?" "1")
  "15134052" (check_sexpr ocaml_be "u24" "15134052")
  "1 (2)" (check_sexpr ocaml_be "I8[]" "1 (2)")
  "1 ((2 1))" (check_sexpr ocaml_be "(I8?; I40?)[]" "1 ((2 1))")
*)
(*$= check_rowbinary & ~printer:identity
  "15134052" (check_rowbinary ocaml_be "u24" "15134052")
  "15134052" (check_rowbinary cpp_be "u24" "15134052")
*)
(*$= check_ringbuffer & ~printer:identity
  "\"foo\"" (check_ringbuffer ocaml_be "String" "\"foo\"")
  "(\"foo\" 1)" (check_ringbuffer ocaml_be "(String?; I40?)" "(\"foo\" 1)")
  "1 ((\"foo\" 1))" (check_ringbuffer ocaml_be "(String?; I40?)[]" "1 ((\"foo\" 1))")
  "1 ((2 1))" (check_ringbuffer ocaml_be "(I8?; I40?)[]" "1 ((2 1))")
  "-5424105" (check_ringbuffer ocaml_be "I24" "-5424105")
  "((\"a\") 1)" (check_ringbuffer ocaml_be "(String[1]; u8)" "((\"a\") 1)")
  "2 (null 1)" (check_ringbuffer ocaml_be "u8?[]" "2 (null 1)")
  "0 ()" (check_ringbuffer cpp_be "Bool[]" "0 ()")
  "(T)" (check_ringbuffer cpp_be "Bool[1]" "(T)")
*)
