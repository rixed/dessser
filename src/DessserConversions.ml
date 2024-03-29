(* Converting expressions from one type into another *)
open Batteries
open Stdint

module E = DessserExpressions
module T = DessserTypes
open DessserTools
open E.Ops

let debug = false

(* Convert a non-nullable value to the given value-type.
 * Also return the nullability of the resulting expression (result will be
 * nullable for instance when converting a string to a number). *)
(* TODO: move in dessser.StdLib as a "cast" function *)
let rec conv ?(depth=0) ~from ~to_ e =
  let fields_of_rec mns =
    let f = Array.map fst mns in
    Array.fast_sort String.compare f ;
    f in
  let conv_mn = conv_mn ~depth:(depth+1) in
  let map_items e from to_ =
    map_ nop (
      func2 T.void from (fun _void item ->
        conv_mn ~from ~to_ item)
      ) e in
  if T.eq from to_ then e, false else
  (* A null can be cast to whatever. Actually, type-checking will type nulls
   * arbitrarily. *)
  if match e with T.E0 (Null _) -> true | _ -> false then null to_, false else
  (* An empty set of any kind can also be cast from any item type to any other
   * item type: *)
  match e, from, to_ with
  | (T.E0 (EndOfList _) | E0 (EmptySet _) | E0R (MakeArr _, [||])),
    _, T.TLst mn ->
      T.E0 (EndOfList mn), false
  | (T.E0 (EndOfList _) | E0 (EmptySet _) | E0R (MakeArr _, [||])),
    _, TArr mn ->
      E0R (MakeArr mn, [||]), false
  | (T.E0 (EndOfList _) | E0 (EmptySet _) | E0R (MakeArr _, [||])),
    _, TSet (Simple, mn) ->
      E0 (EmptySet mn), false
  (* Anything beyond that requires actual conversion depending on the
   * types: *)
  (* Any cast from a user type to its implementation is a NOP, and the other
   * way around too: *)
  | _, TUsr { def ; _ }, to_ when T.eq def to_ ->
      e, false
  | _, from, TUsr { def ; _ } when T.eq def from ->
      e, false
  | _, T.(TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
         TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128),
    T.TString -> string_of_int_ e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
       TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128),
    T.TFloat -> to_float e, false
  | _, TU8, TBool -> bool_of_u8 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TBool ->
        let e, nullable = conv ~depth:(depth+1) ~from ~to_:T.TU8 e in
        assert (not nullable) ;
        bool_of_u8 e, false
  | _, TString, TFloat -> float_of_string_ e, true
  | _, TString, TChar -> nth (u8_of_int 0) e, true
  | _, TString, TI8 -> i8_of_string e, true
  | _, TString, TI16 -> i16_of_string e, true
  | _, TString, TI24 -> i24_of_string e, true
  | _, TString, TI32 -> i32_of_string e, true
  | _, TString, TI40 -> i40_of_string e, true
  | _, TString, TI48 -> i48_of_string e, true
  | _, TString, TI56 -> i56_of_string e, true
  | _, TString, TI64 -> i64_of_string e, true
  | _, TString, TI128 -> i128_of_string e, true
  | _, TString, TU8 -> u8_of_string e, true
  | _, TString, TU16 -> u16_of_string e, true
  | _, TString, TU24 -> u24_of_string e, true
  | _, TString, TU32 -> u32_of_string e, true
  | _, TString, TU40 -> u40_of_string e, true
  | _, TString, TU48 -> u48_of_string e, true
  | _, TString, TU56 -> u56_of_string e, true
  | _, TString, TU64 -> u64_of_string e, true
  | _, TString, TU128 -> u128_of_string e, true
  | _, TString, TBool -> gt (string_length e) (u32_of_int 0), false
  | _, TFloat, TString -> string_of_float_ e, false
  | _, TChar, TU8 -> u8_of_char e, false
  | _, TU8, TChar -> char_of_u8 e, false
  | _, TChar, TString -> string_of_char e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI8 -> to_i8 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI16 -> to_i16 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI24 -> to_i24 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI32 -> to_i32 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI40 -> to_i40 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI48 -> to_i48 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI56 -> to_i56 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI64 -> to_i64 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TI128 -> to_i128 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU8 -> to_u8 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU16 -> to_u16 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU24 -> to_u24 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU32 -> to_u32 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU40 -> to_u40 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU48 -> to_u48 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TUsr { name = "Eth" ; _ } ->
      make_usr "Eth" [ to_u48 e ], false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU56 -> to_u56 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU64 -> to_u64 e, false
  | _, (TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 |
        TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128 | TFloat),
    TU128 -> to_u128 e, false
  (* Bools can be (explicitly) converted into numbers: *)
  | _, TBool, TU8 -> u8_of_bool e, false
  | _, TBool, TU16 -> to_u16 (u8_of_bool e), false
  | _, TBool, TU24 -> to_u24 (u8_of_bool e), false
  | _, TBool, TU32 -> to_u32 (u8_of_bool e), false
  | _, TBool, TU40 -> to_u40 (u8_of_bool e), false
  | _, TBool, TU48 -> to_u48 (u8_of_bool e), false
  | _, TBool, TU56 -> to_u56 (u8_of_bool e), false
  | _, TBool, TU64 -> to_u64 (u8_of_bool e), false
  | _, TBool, TU128 -> to_u128 (u8_of_bool e), false
  | _, TBool, TI8 -> to_i8 (u8_of_bool e), false
  | _, TBool, TI16 -> to_i16 (u8_of_bool e), false
  | _, TBool, TI24 -> to_i24 (u8_of_bool e), false
  | _, TBool, TI32 -> to_i32 (u8_of_bool e), false
  | _, TBool, TI40 -> to_i40 (u8_of_bool e), false
  | _, TBool, TI48 -> to_i48 (u8_of_bool e), false
  | _, TBool, TI56 -> to_i56 (u8_of_bool e), false
  | _, TBool, TI64 -> to_i64 (u8_of_bool e), false
  | _, TBool, TI128 -> to_i128 (u8_of_bool e), false
  | _, TBool, TFloat -> to_float (u8_of_bool e), false
  (* A vector of 1 t into t and the other way around: *)
  | _, TVec (1, { nullable = false ; typ = vt1 }), vt2
    when T.eq vt1 vt2 ->
      unsafe_nth (u32_of_int 0) e, false
  | _, vt1, TVec (1, { typ = vt2 ; nullable })
    when T.eq vt1 vt2 ->
      make_vec [ if nullable then not_null e else e ], false
  | _, vt1, TArr ({ typ = vt2 ; nullable } as mn2)
    when T.eq vt1 vt2 ->
      make_arr mn2 [ if nullable then not_null e else e ], false
  (* Specialized version for arr/vec of chars that return the
   * string composed of those chars rather than an enumeration: *)
  | _, TVec (_, ({ typ = TChar ; _ } as mn)), TString
  | _, TArr ({ typ = TChar ; _ } as mn), TString ->
      conv_charseq_to_string ~depth:(depth+1) ~from:mn (cardinality e) e, false
  | _, TVec (_, mn), TString
  | _, TArr mn, TString ->
      conv_list_to_string ~depth:(depth+1) ~from:mn (cardinality e) e, false
  | _, TTup mns, TString ->
      let rec loop s i =
        if i >= Array.length mns then
          append_string s (string ")")
        else
          let s' = conv_mn ~from:mns.(i) ~to_:T.string (get_item i e) in
          let s = if i > 0 then append_string s (string ";") else s in
          loop (append_string s s') (i + 1) in
      loop (string "(") 0, false
  | _, TBool, TString ->
      if_ e ~then_:(string "true") ~else_:(string "false"), false
  | _, TUsr { name = ("Ip4" | "Ip6" | "Ip") ; _ }, TString ->
      string_of_ip e, false
  | _, TU32, TUsr { name = "Ip4" ; _ } ->
      make_usr "Ip4" [ e ], false
  | _, TUsr { name = ("Ip4" | "Ip6") ; _ }, TUsr { name = "Ip" ; _ } ->
      make_usr "Ip" [ e ], false
  | _, TU32, TUsr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip4" [ e ] ], false
  | _, TU128, TUsr { name = "Ip6" ; _ } ->
      make_usr "Ip6" [ e ], false
  | _, TU128, TUsr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip6" [ e ] ], false
  | _, TUsr { name = ("Cidr4" | "Cidr6") ; _ }, TUsr { name = "Cidr" ; _ } ->
      make_usr "Cidr" [ e ], false
  | _, TVec (d1, mn1), TVec (d2, mn2) when d1 = d2 ->
      map_items e mn1 mn2, false
  (* TODO: Also when d2 < d1 (need a truncate operator), and d2 > d1 (extending
   * with null as long as mn2 is nullable) *)
  | _, TArr mn1, TArr mn2 ->
      map_items e mn1 mn2, false
  | _, TLst mn1, TLst mn2 ->
      map_items e mn1 mn2, false
  | _, TVec (_, mn1), TArr mn2 ->
      let e = arr_of_vec e in
      map_items e mn1 mn2, false
  (* Groups are typed as lists: *)
  | _, TSet (_, mn1), TArr mn2 ->
      let e = arr_of_set e in
      map_items e mn1 mn2, false
  | _, TLst mn1, TArr mn2 ->
      let e = arr_of_lst e in
      map_items e mn1 mn2, false
  | _, TTup mns1, TTup mns2 when Array.length mns1 = Array.length mns2 ->
      (* TODO: actually we could project away fields from t_from when t_to
       * is narrower, or inject NULLs in some cases. *)
      make_tup (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~from:mns1.(i) ~to_:mns2.(i) (get_item i e))), false
  | _, TTup mns1, TVec (dim, mn2) when Array.length mns1 = dim ->
      make_vec (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~from:mns1.(i) ~to_:mn2 (get_item i e))), false
  | _, TTup mns1, TArr mn2 ->
      make_arr mn2 (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~from:mns1.(i) ~to_:mn2 (get_item i e))), false
  | _, TRec mns1, TRec mns2 when fields_of_rec mns1 = fields_of_rec mns2 ->
      Array.fold_left (fun fields (n, mn) ->
        let from = array_assoc n mns1 in
        (n, conv_mn ~from ~to_:mn (get_field n e)) :: fields
      ) [] mns2 |>
      List.rev |>
      make_rec, false
  (* TODO: other types to string *)
  (* "globals_map" is an alias for the type used by CodeGenLib to set/get
   * to/from LMDB files: *)
  | _, TExt "globals_map",
    TMap ({ typ = TString ; _ }, { typ = TString ; _ })
  | _, TMap ({ typ = TString ; _ }, { typ = TString ; _ }),
    TExt "globals_map" ->
      e, false
  | _ ->
      Printf.sprintf2 "Not implemented: Cast from %a to %a of expression %a"
        T.print from
        T.print to_
        (E.print ~max_depth:3) e |>
      failwith

and conv_list_to_string ?(depth=0) ~from length_e src =
  let_ ~name:"str_ref" (make_ref (string "[")) (fun str_ref ->
    let str = get_ref str_ref in
    let append s = set_ref str_ref (append_string str s) in
    let_ ~name:"i_ref" (make_ref (u32_of_int 0)) (fun i_ref ->
      let i = get_ref i_ref in
      let_ ~name:"src_ref" (make_ref src) (fun src_ref ->
        seq [
          while_ (lt i length_e)
            (let src = get_ref src_ref in
            seq [
              (* Append a delimiter? *)
              if_ (gt i (u32_of_int 0))
                ~then_:(append (string ";"))
                ~else_:nop ;
              (* Next value: *)
              unsafe_nth i src |>
              conv_mn ~depth:(depth+1) ~from ~to_:T.string |>
              append ;
              (* Incr i *)
              set_ref i_ref (add i (u32_of_int 1)) ]) ;
          append (string "]") ;
          str ])))

and conv_charseq_to_string ?(depth=0) ~from length_e src =
  let_ ~name:"str_ref" (make_ref (string "")) (fun str_ref ->
    let str = get_ref str_ref in
    let append s = set_ref str_ref (append_string str s) in
    let_ ~name:"i_ref" (make_ref (u32_of_int 0)) (fun i_ref ->
      let i = get_ref i_ref in
      let_ ~name:"src_ref" (make_ref src) (fun src_ref ->
        seq [
          while_ (lt i length_e)
            (let src = get_ref src_ref in
            seq [
              unsafe_nth i src |>
              (* [from] is going to be a nullable or not char but let's
               * ise conv_mn for generality: *)
              conv_mn ~depth:(depth+1) ~from ~to_:T.char |>
              string_of_char |>
              append ;
              (* Incr i *)
              set_ref i_ref (add i (u32_of_int 1)) ]) ;
          str ])))

and conv_mn ?(depth=0) ~from ~to_ e =
  if debug then (
    let indent = String.make (depth * 2) ' ' in
    Printf.eprintf "%sConverting into %a: %a\n"
      indent
      T.print_mn to_
      (E.print ?max_depth:None) e) ;
  let conv = conv ~depth:(depth+1) ~from:from.T.typ ~to_:to_.T.typ in
  let is_const_null =
    match e with T.E0 (Null _) -> true | _ -> false in
  let my_if_null def =
    if is_const_null then def else
    let_ ~name:"nullable_to_not_nullable_" e (fun e ->
      if_null e
        ~then_:def
        ~else_:(
          let e, nullable = conv (force ~what:"if_null" e) in
          if nullable then
            if_null e
              ~then_:def
              ~else_:(force e)
          else e)) in
  (* Beware that [conv] can return a nullable expression: *)
  match from.T.nullable, to_.T.nullable with
  | false, false ->
      let e, nullable = conv e in
      if nullable then force e else e
  | true, false ->
      (match to_.T.typ with
      | T.TString ->
          my_if_null (string "NULL")
      | T.TChar ->
          my_if_null (char '?')
      | _ ->
          let e, nullable =
            conv (force ~what:"conv from nullable to not nullable" e) in
          if nullable then
            force ~what:"conv from nullable to not nullable (2)" e
          else e)
  | false, true ->
      let e, nullable = conv e in
      if nullable then e else not_null e
  | true, true ->
      if is_const_null then null to_.T.typ else
      let_ ~name:"conv_mn_e" e (fun e ->
        if_null e
          ~then_:(null to_.T.typ)
          ~else_:(
            let from = T.{ from with nullable = false } in
            let e = force ~what:"conv from nullable to nullable" e in
            conv_mn ~depth:(depth+1) ~from ~to_ e))
