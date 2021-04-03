(* Random genrator for types and expressions *)
open Batteries
open Stdint
open QCheck
open DessserTools
open DessserFloatTools
module T = DessserTypes

(*$inject
   open Batteries
   module E = DessserExpressions
   module T = DessserTypes
   module U = DessserCompilationUnit
   let dbg = false *)

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
    | 0 -> T.Float
    | 1 -> T.String
    | 2 -> T.Bool
    | 3 -> T.Char
    | 4 -> T.U8
    | 5 -> T.U16
    | 6 -> T.U24
    | 7 -> T.U32
    | 8 -> T.U40
    | 9 -> T.U48
    | 10 -> T.U56
    | 11 -> T.U64
    | 12 -> T.U128
    | 13 -> T.I8
    | 14 -> T.I16
    | 15 -> T.I24
    | 16 -> T.I32
    | 17 -> T.I40
    | 18 -> T.I48
    | 19 -> T.I56
    | 20 -> T.I64
    | 21 -> T.I128
    | _ -> assert false)

let user_type_gen =
  (* This module is linked after DessserTypes and therefore is initialized after
   * it, so it is OK to get default user types now: *)
  let user_type_keys = Hashtbl.keys T.user_types |> Array.of_enum in
  Gen.(sized (fun n _st ->
    let k = user_type_keys.(n mod Array.length user_type_keys) in
    (Hashtbl.find T.user_types k).typ))

let rec value_type_gen_of_depth depth =
  let open Gen in
  assert (depth >= 0) ;
  if depth = 0 then
    frequency
      (* User types are always considered opaque: *)
      [ 1, map (fun ut -> T.Usr ut) user_type_gen ;
        9, map (fun mt -> T.Mac mt) mac_type_gen ]
  else
    let mn_gen = maybe_nullable_gen_of_depth (depth - 1) in
    oneof
      [ map2 (fun dim mn -> T.Vec (dim, mn)) (int_range 1 10) mn_gen ;
        map (fun mn -> T.Lst mn) mn_gen ;
        map (fun mn -> T.Set mn) mn_gen ;
        map (fun mns -> T.Tup mns) (tiny_array mn_gen) ;
        map (fun fs -> T.Rec fs) (tiny_array (pair field_name_gen mn_gen)) ;
        map (fun fs -> T.Sum fs) (tiny_array (pair field_name_gen mn_gen)) ;
        (* Avoid maps for now, as there is no manipulable values of that type:
        map2 (fun k v -> T.Map (k, v)) mn_gen mn_gen *) ]

and maybe_nullable_gen_of_depth depth =
  Gen.map2 (fun nullable vtyp ->
    T.make ~nullable vtyp
  ) Gen.bool (value_type_gen_of_depth depth)

(*$Q maybe_nullable_gen_of_depth
  (Q.int_range 0 9) (fun d -> \
    let mn = Q.Gen.generate1 (maybe_nullable_gen_of_depth d) in \
    let dep = T.depth ~opaque_user_type:true mn.T.vtyp in \
    if d <> dep then \
      BatPrintf.printf "type = %a, depth = %d <> %d\n%!" \
        T.print_maybe_nullable mn dep d ; \
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
  | T.Unknown | T.Ext _ -> invalid_arg "size_of_value_type"
  | T.Unit | T.Mac _ | T.Usr _ -> 1
  | T.Vec (_, mn) | T.Lst mn | T.Set mn -> 1 + size_of_maybe_nullable mn
  | T.Tup mns ->
      Array.fold_left (fun s mn -> s + size_of_maybe_nullable mn) 0 mns
  | T.Rec mns ->
      Array.fold_left (fun s (_, mn) -> s + size_of_maybe_nullable mn) 0 mns
  | T.Sum mns ->
      Array.fold_left (fun s (_, mn) -> s + size_of_maybe_nullable mn) 0 mns
  | T.Map (k, v) ->
      size_of_maybe_nullable k + size_of_maybe_nullable v

and size_of_maybe_nullable mn =
  size_of_value_type mn.vtyp

let shrink_mac_type mt =
  let to_simplest =
    T.[ String ; Float ;
        I128 ; U128 ; I64 ; U64 ; I56 ; U56 ; I48 ; U48 ; I40 ; U40 ;
        I32 ; U32 ; I24 ; U24 ; I16 ; U16 ; I8 ; U8 ; Char ; Bool ] in
  let rec loop = function
    | [] -> Iter.empty
    | mt'::rest when T.mac_type_eq mt' mt ->
        if rest = [] then Iter.empty else Iter.of_list rest
    | _::rest ->
        loop rest in
  loop to_simplest

let rec shrink_value_type =
  let vt_of_mn mn = mn.T.vtyp in
  let shrink_fields mns make_typ f =
    Array.iter (fun (_, mn) -> shrink_maybe_nullable mn (f % vt_of_mn)) mns ;
    let shrink_mns =
      let shrink (fn, mn) =
        Iter.map (fun mn -> fn, mn) (shrink_maybe_nullable mn) in
      Shrink.filter (fun mns -> Array.length mns > 1)
        (Shrink.array ~shrink) mns |>
      Iter.map make_typ in
    shrink_mns f in
  function
  | T.Unknown | T.Ext _ ->
      Iter.empty
  | T.Unit ->
      Iter.empty
  | T.Mac mt ->
      (fun f ->
        shrink_mac_type mt (fun mt -> f (T.Mac mt)))
  | T.Usr _ ->
      Iter.empty
  | T.Vec (dim, mn) ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f mn.vtyp ;
          f (T.Vec (dim, mn))))
  | T.Lst mn ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f (T.Lst mn) ;
          f mn.vtyp))
  | T.Set mn ->
      (fun f ->
        shrink_maybe_nullable mn (fun mn ->
          f (T.Set mn) ;
          f mn.vtyp))
  | T.Tup mns ->
      (fun f ->
        Array.iter (fun mn -> shrink_maybe_nullable mn (f % vt_of_mn)) mns ;
        let shrink_mns =
          Shrink.filter (fun mns -> Array.length mns > 1)
            (Shrink.array ~shrink:shrink_maybe_nullable) mns |>
          Iter.map (fun mns -> T.Tup mns) in
        shrink_mns f)
  | T.Rec mns ->
      shrink_fields mns (fun mns -> T.Rec mns)
  | T.Sum mns ->
      shrink_fields mns (fun mns -> T.Sum mns)
  | T.Map (k, v) ->
      (fun f ->
        shrink_maybe_nullable k (f % vt_of_mn) ;
        shrink_maybe_nullable v (f % vt_of_mn) ;
        let shrink_kv =
          (Shrink.pair shrink_maybe_nullable shrink_maybe_nullable) (k, v) |>
          Iter.map (fun (k, v) -> T.Map (k, v)) in
        shrink_kv f)

and shrink_maybe_nullable mn =
  let vt = mn.vtyp in
  if mn.nullable then
    (fun f ->
      shrink_value_type vt (fun vtyp ->
        f { vtyp ; nullable = false } ;
        f { vtyp ; nullable = true }))
  else
    (fun f ->
      shrink_value_type vt (fun vtyp -> f { vtyp ; nullable = false }))

let value_type =
  let print = IO.to_string T.print_value_type
  and small = size_of_value_type
  and shrink = shrink_value_type in
  make ~print ~small ~shrink value_type_gen

let maybe_nullable =
  let print = IO.to_string T.print_maybe_nullable
  and small = size_of_maybe_nullable
  and shrink = shrink_maybe_nullable in
  make ~print ~small ~shrink maybe_nullable_gen

(*$Q maybe_nullable & ~count:20
  maybe_nullable (fun mn -> \
    let str = IO.to_string T.print_maybe_nullable mn in \
    let mn' = T.maybe_nullable_of_string str in \
    T.maybe_nullable_eq mn' mn)
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
    | true -> E.LittleEndian
    | false -> E.BigEndian
  ) bool)

let path_gen =
  Gen.(tiny_list tiny_int)

let get_next_fid =
  let next_fid = ref 0 in
  fun () ->
    incr next_fid ;
    !next_fid

(* Those constructors with no arguments only *)
let e1_of_int n =
  let e1s =
    E.[| Dump ; Ignore ; IsNull ; NotNull ; Force ; StringOfFloat ;
         StringOfChar ; StringOfInt ; StringOfIp ; FloatOfString ; CharOfString
         ; U8OfString ; U16OfString ; U24OfString ; U32OfString ; U40OfString ;
         U48OfString ; U56OfString ; U64OfString ; U128OfString ; I8OfString ;
         I16OfString ; I24OfString ; I32OfString ; I40OfString ; I48OfString ;
         I56OfString ; I64OfString ; I128OfString ; FloatOfPtr ; CharOfPtr ;
         U8OfPtr ; U16OfPtr ; U24OfPtr ; U32OfPtr ; U40OfPtr ; U48OfPtr ;
         U56OfPtr ; U64OfPtr ; U128OfPtr ; I8OfPtr ; I16OfPtr ; I24OfPtr ;
         I32OfPtr ; I40OfPtr ; I48OfPtr ; I56OfPtr ; I64OfPtr ; I128OfPtr ;
         ToU8 ; ToU16 ; ToU24 ; ToU32 ; ToU40 ; ToU48 ; ToU56 ; ToU64 ; ToU128
         ; ToI8 ; ToI16 ; ToI24 ; ToI32 ; ToI40 ; ToI48 ; ToI56 ; ToI64 ;
         ToI128 ; ToFloat ; LogNot ; FloatOfQWord ; QWordOfFloat ; U8OfByte ;
         ByteOfU8 ; U16OfWord ; WordOfU16 ; U32OfDWord ; DWordOfU32 ;
         U64OfQWord ; QWordOfU64 ; U128OfOWord ; OWordOfU128 ; U8OfChar ;
         CharOfU8 ; SizeOfU32 ; U32OfSize ; BitOfBool ; BoolOfBit ; ListOfSList
         ; ListOfSListRev ; SetOfSList ; ListOfVec ; ListOfSet ; U8OfBool ;
         BoolOfU8 ; StringLength ; StringOfBytes ; BytesOfString ; Cardinality
         ; ReadByte ; DataPtrPush ; DataPtrPop ; RemSize ; Not ; Abs ; Neg ;
         Exp ; Log ; Log10 ; Sqrt ; Ceil ; Floor ; Round ; Cos ; Sin ; Tan ;
         ACos ; ASin ; ATan ; CosH ; SinH ; TanH ; Lower ; Upper ; Hash ; Fst ;
         Snd ; Identity ; GetEnv |]
  in
  e1s.(n mod Array.length e1s)

let e2_of_int n =
  let e2s =
    E.[| Nth ; Gt ; Ge ; Eq ; Ne ; Add ; Sub ; Mul ; Div ; Rem ;
         Pow ; LogAnd ; LogOr ; LogXor ; LeftShift ; RightShift ; AppendBytes ;
         AppendString ; StartsWith ; EndsWith ; GetBit ; GetVec ;
         ReadBytes ; PeekByte ; WriteByte ; WriteBytes ; PokeByte ;
         DataPtrAdd ; DataPtrSub ;
         And ; Or ; Pair ; MapPair ; Map ; Min ; Max ; Member ; Insert ;
         SplitBy ; SplitAt ; Join ; AllocLst |] in
  e2s.(n mod Array.length e2s)

let e3_of_int n =
  let e3s =
    E.[| SetBit ; SetVec ; BlitByte ; If ; LoopWhile ; LoopUntil ; Fold ;
         FindSubstring |] in
  e3s.(n mod Array.length e3s)

let e4_of_int n =
  let e4s = E.[| ReadWhile ; Repeat |] in
  e4s.(n mod Array.length e4s)

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
    1, map E.Ops.bit bool ;
    1, map E.Ops.size small_nat ;
    1, map (E.Ops.byte % Uint8.of_int) (int_bound 255) ;
    1, map (E.Ops.word % Uint16.of_int) (int_bound 65535) ;
    1, map (E.Ops.dword % Uint32.of_int32) ui32 ;
    1, map (E.Ops.qword % Uint64.of_int64) ui64 ;
    1, map oword ui128_gen ;
    1, map data_ptr_of_string small_string ;
  ] in
  let lst =
    if depth > 0 then
      (1,
        pick_from_env l depth (function
          | E.E0 (Identifier _) -> true
          | _ -> false)) ::
      (1, (
        pick_from_env l depth (function
          | E.E0 (Param _) -> true
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
    1, map2 E.Ops.make_lst (maybe_nullable_gen_of_depth (depth - 1))
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
    List.filter_map (fun (e, _t) -> if f e then Some e else None) l in
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
          let ts = Array.map (fun mn -> T.Value mn) ts in
          let fid = get_next_fid () in
          let l =
            Array.fold_lefti (fun l i t ->
              (param fid i, t) :: l
            ) l ts in
          map (fun e ->
            E.E1 (Function (fid, ts), e)
          ) (expression_gen (l, depth - 1))
        ) (tiny_array maybe_nullable_gen)
      ) ;
    1, map2 comment (string ~gen:printable_for_comments) expr ;
    1, map2 get_item tiny_int expr ;
    1, map2 get_field field_name_gen expr ;
    1, map2 get_alt field_name_gen expr ;
    1, map2 read_word endianness_gen expr ;
    1, map2 read_dword endianness_gen expr ;
    1, map2 read_qword endianness_gen expr ;
    1, map2 read_oword endianness_gen expr ;
    10, map2 (fun n e -> E.E1 (e1_of_int n, e)) nat expr ]

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
    1, map3 (fun name e in_ ->
               let_ ~name e (fun _ _ -> in_)) let_name_gen expr expr ;
    1, map3 peek_word endianness_gen expr expr ;
    1, map3 peek_dword endianness_gen expr expr ;
    1, map3 peek_qword endianness_gen expr expr ;
    1, map3 peek_oword endianness_gen expr expr ;
    1, map3 write_word endianness_gen expr expr ;
    1, map3 write_dword endianness_gen expr expr ;
    1, map3 write_qword endianness_gen expr expr ;
    1, map3 write_oword endianness_gen expr expr ;
    10, map3 (fun n e1 e2 -> E.E2 (e2_of_int n, e1, e2)) nat expr expr ]

and e3_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  map4 (fun n e1 e2 e3 ->
    E.E3 (e3_of_int n, e1, e2, e3)
  ) nat expr expr expr

and e4_gen l depth =
  let expr = expression_gen (l, depth - 1) in
  let open Gen in
  map5 (fun n e1 e2 e3 e4 ->
    E.E4 (e4_of_int n, e1, e2, e3, e4)
  ) nat expr expr expr expr

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
        5, e3_gen l depth ;
        5, e4_gen l depth ]
    else
      e0_gen l depth
  ) (l, depth)

let expression_gen =
  Gen.(sized_size (int_bound 3) (fun n -> expression_gen ([], n)))

let size_of_expression e =
  E.fold 0 [] (fun n _ _ -> succ n) e

(* TODO: try to shrink expressions by replacing terms with constants of the
 * same type *)
let expression =
  let print = IO.to_string E.print
  and small = size_of_expression in
  make ~print ~small expression_gen

(*$Q expression & ~count:20
  expression (fun e -> \
    let str = IO.to_string E.print e in \
    match E.Parser.expr str with \
    | [ e' ] -> E.eq e' e \
    | _ -> false)
*)

(*$inject
  open Dessser
  open DessserTools
  open DessserDSTools

  let can_be_compiled_with_backend be e =
    let module BE = (val be : BACKEND) in
    let compunit = U.make () in
    let compunit, _, _ = U.add_identifier_of_expression compunit e in
    let src_fname =
      let ext = "."^ BE.preferred_def_extension in
      Filename.temp_file "dessserQCheck_" ext in
    let obj_fname = Filename.remove_extension src_fname in
    write_source ~src_fname (fun oc -> BE.print_definitions oc compunit) ;
    try compile ~optim:0 ~link:Object be src_fname obj_fname ;
        ignore_exceptions Unix.unlink src_fname ;
        ignore_exceptions Unix.unlink obj_fname ;
        true
    with e ->
        Printf.eprintf "FAILURE: %s\n" (Printexc.to_string e) ;
        false

  let can_be_compiled e =
    can_be_compiled_with_backend (module DessserBackEndOCaml : BACKEND) e &&
    can_be_compiled_with_backend (module DessserBackEndCPP : BACKEND) e
*)

(*$Q expression & ~count:20
  expression (fun e -> \
    match type_check [] e with \
    | exception _ -> true \
    | () -> \
        type_of [] e = Void || can_be_compiled e)
*)

(* Non regression tests: *)
(*$inject
  let compile_check s =
    let e = Parser.expr s |> List.hd in
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
    "(null \"(opfa U48 | lhlqkp I48?[2] | lqdjnf (Char?; I40; U48; U48?)? | fcioax String?[1]?)\")"
  compile_check "(make-vec ())"
  compile_check "(make-lst \"U8\" (u8 63))"
  compile_check "(to-u8 (float 1))"
  compile_check "(to-float (u8 1))"
  compile_check "(to-u8 (u8 1))"
  compile_check "(to-float (float 1))"
  compile_check \
    "(map (make-vec (u8 1) (u8 2) (u8 3)) \
          (fun 0 \"u8\" (mul (param 0 0) (param 0 0))))"
  compile_check \
    "(map (make-lst \"u8\" (u8 1) (u8 2) (u8 3)) \
          (fun 0 \"u8\" (mul (param 0 0) (param 0 0))))"
  compile_check "(string-of-ip (random-u128))"
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
  | T.Unknown | T.Ext _ ->
      invalid_arg "sexpr_of_vtyp_gen"
  | T.Unit ->
      return "()"
  | T.Mac Float ->
      map hexstring_of_float float
  | T.Mac String ->
      (* FIXME: support escaping in quotes: *)
      map String.quote (string_size ~gen:printable_no_escape (int_range 3 15))
  | T.Mac Char ->
      map String.quote (string_size ~gen:printable_no_escape (int_range 1 1))
  | T.Mac Bool ->
      map (function true -> "T" | false -> "F") bool
  | T.Mac U8 -> int_string_gen 0L 255L
  | T.Mac U16 -> int_string_gen 0L 65535L
  | T.Mac U24 -> int_string_gen 0L 16777215L
  | T.Mac U32 -> int_string_gen 0L 4294967295L
  | T.Mac U40 -> int_string_gen 0L 1099511627775L
  | T.Mac U48 -> int_string_gen 0L 281474976710655L
  | T.Mac U56 -> int_string_gen 0L 72057594037927935L
  | T.Mac U64 -> map Uint64.(to_string % of_int64) ui64
  | T.Mac U128 -> map Uint128.to_string ui128_gen
  | T.Mac I8 -> int_string_gen (-128L) 127L
  | T.Mac I16 -> int_string_gen (-32768L) 32767L
  | T.Mac I24 -> int_string_gen (-8388608L) 8388607L
  | T.Mac I32 -> int_string_gen (-2147483648L) 2147483647L
  | T.Mac I40 -> int_string_gen (-549755813888L) 549755813887L
  | T.Mac I48 -> int_string_gen (-140737488355328L) 140737488355327L
  | T.Mac I56 -> int_string_gen (-36028797018963968L) 36028797018963967L
  | T.Mac I64 -> map (fun i -> Int64.(to_string (sub i 4611686018427387904L))) ui64
  | T.Mac I128 -> map Int128.to_string i128_gen
  | T.Usr ut -> sexpr_of_vtyp_gen ut.def
  | T.Vec (dim, mn) ->
      list_repeat dim (sexpr_of_mn_gen mn) |> map to_sexpr
  | T.Lst mn ->
      tiny_list (sexpr_of_mn_gen mn) |> map (fun lst ->
        (* FIXME: make list_prefix_length a parameter of this function *)
        (if DessserSExpr.default_config.list_prefix_length then
          Stdlib.string_of_int (List.length lst) ^ " "
        else "") ^
        to_sexpr lst)
  | T.Set mn ->
      sexpr_of_vtyp_gen (Lst mn)
  | T.Tup mns ->
      tup_gen mns
  | T.Rec mns ->
      tup_gen (Array.map snd mns)
  | T.Sum mns ->
      join (
        map (fun i ->
          let i = (Stdlib.abs i) mod (Array.length mns) in
          sexpr_of_mn_gen (snd mns.(i)) |>
          map (fun se -> "("^ Stdlib.string_of_int i ^" "^ se ^")")
        ) int
      )
  | T.Map (k, v) ->
      sexpr_of_vtyp_gen (Lst { vtyp = Tup [| k ; v |] ; nullable = false })

and tup_gen mns st =
  "("^ (
    Array.fold_left (fun sexpr mn ->
      (if sexpr = "" then "" else (sexpr ^ " ")) ^ sexpr_of_mn_gen mn st
    ) "" mns
  ) ^")"

and sexpr_of_mn_gen mn =
  let open Gen in
  if mn.nullable then
    join (
      (* Note: This "null" must obviously match the one used in DessserSExpr.ml *)
      map (function true -> return "null"
                 | false -> sexpr_of_vtyp_gen mn.vtyp) bool)
  else
    sexpr_of_vtyp_gen mn.vtyp

let sexpr mn =
  let print = BatPervasives.identity
  and small = String.length in
  make ~print ~small (sexpr_of_mn_gen mn)

(* A program that convert from s-expr to s-expr for the given schema [mn],
 * to check s-expr is reliable before using it in further tests: *)
(*$inject
  open QCheck
  open Ops
  let sexpr_to_sexpr be mn =
    let module S2S = DesSer (DessserSExpr.Des) (DessserSExpr.Ser) in
    let e =
      func2 (DessserSExpr.Des.ptr mn) (DessserSExpr.Ser.ptr mn) (fun l src dst ->
        S2S.desser mn l src dst) in
    make_converter be ~mn e

  let test_desser alloc_dst be mn des ser =
    let module Des = (val des : DES) in
    let module Ser = (val ser : SER) in
    let module S2T = DesSer (DessserSExpr.Des) (Ser : SER) in
    let module T2S = DesSer (Des : DES) (DessserSExpr.Ser) in
    let e =
      func2 (DessserSExpr.Des.ptr mn) (DessserSExpr.Ser.ptr mn) (fun l src dst ->
        Ops.let_ ~l alloc_dst (fun l tdst ->
          with_sploded_pair ~l "s2t" (S2T.desser mn l src tdst) (fun l src tdst_end ->
            let tdst = data_ptr_of_ptr tdst (size 0) (data_ptr_sub tdst_end tdst) in
            let dst = secnd (T2S.desser mn l tdst dst) in
            pair src dst))) in
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    make_converter be ~mn e

  let test_data_desser = test_desser (data_ptr_of_buffer (size 50_000))

  let ocaml_be = (module DessserBackEndOCaml : BACKEND)
  let cpp_be = (module DessserBackEndCPP : BACKEND)
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
        s T.print_maybe_nullable mn s' ;
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
  let test_exe format mn exe =
    Gen.generate ~n:100 (sexpr_of_mn_gen mn) |>
    List.iter (fun s ->
      let s' = String.trim (run_converter ~timeout:2 exe s) in
      if dbg then Printf.eprintf "Testing %s %S of type %a -> %S\n%!"
        format s T.print_maybe_nullable mn s' ;
      assert_equal ~printer:BatPervasives.identity s s')

  let test_format be mn des ser format =
    let exe = test_data_desser be mn des ser in
    test_exe format mn exe

  module ToValue = DessserHeapValue.Materialize (DessserSExpr.Des)
  module OfValue = DessserHeapValue.Serialize (DessserSExpr.Ser)

  let heap_convert_expr mn =
    func2 (DessserSExpr.Des.ptr mn) (DessserSExpr.Ser.ptr mn) (fun l src dst ->
      let v_src = ToValue.make mn l src in
      with_sploded_pair ~l "v_src" v_src (fun l v src ->
        let dst = OfValue.serialize mn l copy_field v dst in
        pair src dst))
*)
(*$R
  let test_heap be mn =
    let e = heap_convert_expr mn in
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    let exe = make_converter be ~mn e in
    test_exe "heap-value" mn exe in

  Gen.generate ~n:5 maybe_nullable_gen |>
  List.iter (fun mn ->
    (* RamenRingBuffer cannot encode nullable outermost values (FIXME) *)
    let mn_ringbuf = T.force_maybe_nullable mn in
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
      (module DessserCsv.Ser : SER) format)
*)

(* Non regression tests: *)

(* A function to test specifically a given value of a given type for a given
 * back-end: *)
(*$inject
  let check_sexpr be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let exe = sexpr_to_sexpr be mn in
    let rs = run_converter ~timeout:2 exe vs in
    if dbg then Printf.eprintf "\ncheck_sexpr: %S vs %S\n%!" rs vs ;
    String.trim rs = vs
  let check_rowbinary be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let des = (module DessserRowBinary.Des : DES)
    and ser = (module DessserRowBinary.Ser : SER) in
    let exe = test_data_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_ringbuffer be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let des = (module DessserRamenRingBuffer.Des : DES)
    and ser = (module DessserRamenRingBuffer.Ser : SER) in
    let exe = test_data_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_csv be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let des = (module DessserCsv.Des : DES)
    and ser = (module DessserCsv.Ser : SER) in
    let exe = test_data_desser be mn des ser in
    String.trim (run_converter ~timeout:2 exe vs)
  let check_heapvalue be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let e = heap_convert_expr mn in
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    let exe = make_converter be ~mn e in
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
(*$= check_rowbinary & ~printer:BatPervasives.identity
  "15134052" (check_rowbinary ocaml_be "u24" "15134052")
  "15134052" (check_rowbinary cpp_be "u24" "15134052")
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
  "(1 null)" (check_ringbuffer ocaml_be "(a U32 | b String?)" "(1 null)")
*)
(*$= check_heapvalue & ~printer:BatPervasives.identity
  "1 ((1))" (check_heapvalue ocaml_be "U16[1][]" "1 ((1))")
  "3 (1 2 3)" (check_heapvalue ocaml_be "U16[]" "3 (1 2 3)")
  "(1 5)" (check_heapvalue cpp_be "{ejgvx: U16; kngke: U64}" "(1 5)")
  "1 (6)" (check_heapvalue cpp_be "U8[]?" "1 (6)")
  "2 (214 null)" (check_heapvalue ocaml_be "U8?{}" "2 (214 null)")
  "2 (214 null)" (check_heapvalue cpp_be "U8?{}" "2 (214 null)")
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
    let mn = T.maybe_nullable_of_string ts in
    let module Ser = (val ser : SER) in
    let module DS = DesSer (DessserSExpr.Des) (Ser) in
    let exe =
      let e =
        func2 T.dataptr T.dataptr (fun l src dst ->
          DS.desser mn l src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      make_converter be ~mn e in
    String.trim (run_converter ~timeout:2 exe vs) |>
    hexify_string

  let check_des des be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let module Des = (val des : DES) in
    let module DS = DesSer (Des) (DessserSExpr.Ser) in
    let exe =
      let e =
        func2 T.dataptr T.dataptr (fun l src dst ->
          DS.desser mn l src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      make_converter be ~mn e in
    String.trim (run_converter ~timeout:2 exe vs) |>
    hexify_string
*)
(*$= check_ser & ~printer:BatPervasives.identity
  "2a" \
    (check_ser rowbinary_ser  ocaml_be "u8" "42")
  "2a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "u8" "42")
  "01 00 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8; i8)" "(42 58)")
  "01 01 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8?; i8)" "(42 58)")
  "01 03 00 00 2a 00 00 00 3a 00 00 00" \
    (check_ser ringbuf_ser  ocaml_be "(u8?; i8?)" "(42 58)")
  "00 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "(small u8 | big u16)" "(0 42)")
  "00 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "(small u8 | big u16?)" "(0 42)")
  "01 00 00 00 2a 00 00 00" \
    (check_ser ringbuf_ser ocaml_be "(small u8? | big u16)" "(0 42)")
  "30 2c 34 32" \
    (check_ser csv_ser ocaml_be "(small u8? | big u16)" "(0 42)")
*)

(* Special version of check_des with a custom CSV configuration: *)
(*$inject
  let check_des_csv ?config be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let module DS = DesSer (DessserCsv.Des) (DessserSExpr.Ser) in
    let exe =
      let e =
        func2 T.dataptr T.dataptr (fun l src dst ->
          DS.desser ?des_config:config mn l src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      make_converter be ~mn e in
    String.trim (run_converter ~timeout:2 exe vs)

  let check_ser_csv ?config be ts vs =
    let mn = T.maybe_nullable_of_string ts in
    let module DS = DesSer (DessserSExpr.Des) (DessserCsv.Ser) in
    let exe =
      let e =
        func2 T.dataptr T.dataptr (fun l src dst ->
          DS.desser ?ser_config:config mn l src dst) in
      if dbg then
        Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
      make_converter be ~mn e in
    String.trim (run_converter ~timeout:2 exe vs)

  let csv_config_0 =
    DessserCsv.{ default_config with
      separator = '|' ;
      null = "" ;
      true_ = "true" ;
      false_ = "false" ;
      clickhouse_syntax = false }

  let csv_config_1 =
    DessserCsv.{ default_config with
      null = "" ;
      true_ = "true" ;
      false_ = "false" ;
      clickhouse_syntax = false }

  let csv_config_2 =
    DessserCsv.{ default_config with quote = None }

  let csv_config_CH =
    DessserCsv.{ default_config with
      true_ = "1" ;
      false_ = "0" ;
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
                   "{u:U8[3]; b:BOOL; name:STRING?}" "\"[1\t2\t3]\",0,\\N\n")
*)
(* Test FixedStrings: *)
(*$= check_des_csv & ~printer:BatPervasives.identity
  "(\"a\" \"b\" \"c\")" \
    (check_des_csv ~config:csv_config_1 ocaml_be \
                   "char[3]" "\"abc\"\n")
  "(\"a\" \"b\" \"c\")" \
    (check_des_csv ~config:csv_config_2 ocaml_be \
                   "char[3]" "abc\n")
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
