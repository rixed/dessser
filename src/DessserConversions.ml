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
  if match e with E.E0 (Null _) -> true | _ -> false then null to_, false else
  match from, to_ with
  (* Any cast from a user type to its implementation is a NOP, and the other
   * way around too: *)
  | Usr { def ; _ }, to_ when T.eq def to_ ->
      e, false
  | from, Usr { def ; _ } when T.eq def from ->
      e, false
  | T.Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128),
    T.Base String -> string_of_int_ e, false
  | T.Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128),
    T.Base Float -> to_float e, false
  | T.Base U8, T.Base Bool -> bool_of_u8 e, false
  | T.Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    T.Base Bool ->
        let e, nullable = conv ~depth:(depth+1) ~from ~to_:(T.Base U8) e in
        assert (not nullable) ;
        bool_of_u8 e, false
  | Base String, Base Float -> float_of_string_ e, true
  | Base String, Base Char -> nth (u8_of_int 0) e, true
  | Base String, Base I8 -> i8_of_string e, true
  | Base String, Base I16 -> i16_of_string e, true
  | Base String, Base I24 -> i24_of_string e, true
  | Base String, Base I32 -> i32_of_string e, true
  | Base String, Base I40 -> i40_of_string e, true
  | Base String, Base I48 -> i48_of_string e, true
  | Base String, Base I56 -> i56_of_string e, true
  | Base String, Base I64 -> i64_of_string e, true
  | Base String, Base I128 -> i128_of_string e, true
  | Base String, Base U8 -> u8_of_string e, true
  | Base String, Base U16 -> u16_of_string e, true
  | Base String, Base U24 -> u24_of_string e, true
  | Base String, Base U32 -> u32_of_string e, true
  | Base String, Base U40 -> u40_of_string e, true
  | Base String, Base U48 -> u48_of_string e, true
  | Base String, Base U56 -> u56_of_string e, true
  | Base String, Base U64 -> u64_of_string e, true
  | Base String, Base U128 -> u128_of_string e, true
  | Base Float, Base String -> string_of_float_ e, false
  | Base Char, Base U8 -> u8_of_char e, false
  | Base U8, Base Char -> char_of_u8 e, false
  | Base Char, Base String -> string_of_char e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I8 -> to_i8 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I16 -> to_i16 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I24 -> to_i24 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I32 -> to_i32 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I40 -> to_i40 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I48 -> to_i48 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I56 -> to_i56 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I64 -> to_i64 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I128 -> to_i128 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U8 -> to_u8 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U16 -> to_u16 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U24 -> to_u24 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U32 -> to_u32 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U40 -> to_u40 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U48 -> to_u48 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Usr { name = "Eth" ; _ } ->
      make_usr "Eth" [ to_u48 e ], false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U56 -> to_u56 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U64 -> to_u64 e, false
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U128 -> to_u128 e, false
  (* Bools can be (explicitly) converted into numbers: *)
  | Base Bool, Base U8 -> u8_of_bool e, false
  | Base Bool, Base U16 -> to_u16 (u8_of_bool e), false
  | Base Bool, Base U24 -> to_u24 (u8_of_bool e), false
  | Base Bool, Base U32 -> to_u32 (u8_of_bool e), false
  | Base Bool, Base U40 -> to_u40 (u8_of_bool e), false
  | Base Bool, Base U48 -> to_u48 (u8_of_bool e), false
  | Base Bool, Base U56 -> to_u56 (u8_of_bool e), false
  | Base Bool, Base U64 -> to_u64 (u8_of_bool e), false
  | Base Bool, Base U128 -> to_u128 (u8_of_bool e), false
  | Base Bool, Base I8 -> to_i8 (u8_of_bool e), false
  | Base Bool, Base I16 -> to_i16 (u8_of_bool e), false
  | Base Bool, Base I24 -> to_i24 (u8_of_bool e), false
  | Base Bool, Base I32 -> to_i32 (u8_of_bool e), false
  | Base Bool, Base I40 -> to_i40 (u8_of_bool e), false
  | Base Bool, Base I48 -> to_i48 (u8_of_bool e), false
  | Base Bool, Base I56 -> to_i56 (u8_of_bool e), false
  | Base Bool, Base I64 -> to_i64 (u8_of_bool e), false
  | Base Bool, Base I128 -> to_i128 (u8_of_bool e), false
  | Base Bool, Base Float -> to_float (u8_of_bool e), false
  (* A vector of 1 t into t and the other way around: *)
  | Vec (1, { nullable = false ; typ = vt1 }), vt2
    when T.eq vt1 vt2 ->
      unsafe_nth (u32_of_int 0) e, false
  | vt1, Vec (1, { typ = vt2 ; nullable })
    when T.eq vt1 vt2 ->
      make_vec [ if nullable then not_null e else e ], false
  | vt1, Arr ({ typ = vt2 ; nullable } as mn2)
    when T.eq vt1 vt2 ->
      make_arr mn2 [ if nullable then not_null e else e ], false
  (* Specialized version for arr/vec of chars that return the
   * string composed of those chars rather than an enumeration: *)
  | Vec (_, ({ typ = Base Char ; _ } as mn)), Base String
  | Arr ({ typ = Base Char ; _ } as mn), Base String ->
      conv_charseq_to_string ~depth:(depth+1) ~from:mn (cardinality e) e, false
  | Vec (_, mn), Base String
  | Arr mn, Base String ->
      conv_list_to_string ~depth:(depth+1) ~from:mn (cardinality e) e, false
  | Tup mns, Base String ->
      let rec loop s i =
        if i >= Array.length mns then
          append_string s (string ")")
        else
          let s' = conv_mn ~from:mns.(i) ~to_:T.string (get_item i e) in
          let s = if i > 0 then append_string s (string ";") else s in
          loop (append_string s s') (i + 1) in
      loop (string "(") 0, false
  | Base Bool, Base String ->
      if_ e ~then_:(string "true") ~else_:(string "false"), false
  | Usr { name = ("Ip4" | "Ip6" | "Ip") ; _ }, Base String ->
      string_of_ip e, false
  | Base U32, Usr { name = "Ip4" ; _ } ->
      make_usr "Ip4" [ e ], false
  | Usr { name = ("Ip4" | "Ip6") ; _ }, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ e ], false
  | Base U32, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip4" [ e ] ], false
  | Base U128, Usr { name = "Ip6" ; _ } ->
      make_usr "Ip6" [ e ], false
  | Base U128, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip6" [ e ] ], false
  | Usr { name = ("Cidr4" | "Cidr6") ; _ }, Usr { name = "Cidr" ; _ } ->
      make_usr "Cidr" [ e ], false
  | Vec (d1, mn1), Vec (d2, mn2) when d1 = d2 ->
      map_items e mn1 mn2, false
  | Arr mn1, Arr mn2 ->
      map_items e mn1 mn2, false
  (* TODO: Also when d2 < d1, and d2 > d1 extending with null as long as mn2 is
   * nullable *)
  | Vec (_, mn1), Arr mn2 ->
      let e = arr_of_vec e in
      map_items e mn1 mn2, false
  (* Groups are typed as lists: *)
  | Set (_, mn1), Arr mn2 ->
      let e = arr_of_set e in
      map_items e mn1 mn2, false
  | Tup mns1, Tup mns2 when Array.length mns1 = Array.length mns2 ->
      (* TODO: actually we could project away fields from t_from when t_to
       * is narrower, or inject NULLs in some cases. *)
      make_tup (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~from:mns1.(i) ~to_:mns2.(i) (get_item i e))), false
  | Tup mns1, Vec (dim, mn2) when Array.length mns1 = dim ->
      make_vec (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~from:mns1.(i) ~to_:mn2 (get_item i e))), false
  | Tup mns1, Arr mn2 ->
      make_arr mn2 (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~from:mns1.(i) ~to_:mn2 (get_item i e))), false
  | Rec mns1, Rec mns2 when fields_of_rec mns1 = fields_of_rec mns2 ->
      Array.fold_left (fun fields (n, mn) ->
        let from = array_assoc n mns1 in
        (n, conv_mn ~from ~to_:mn (get_field n e)) :: fields
      ) [] mns2 |>
      List.rev |>
      make_rec, false
  (* TODO: other types to string *)
  (* "globals_map" is an alias for the type used by CodeGenLib to set/get
   * to/from LMDB files: *)
  | Ext "globals_map",
    Map ({ typ = Base String ; _ }, { typ = Base String ; _ })
  | Map ({ typ = Base String ; _ }, { typ = Base String ; _ }),
    Ext "globals_map" ->
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
    match e with E.E0 (Null _) -> true | _ -> false in
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
      | T.(Base String) ->
          my_if_null (string "NULL")
      | T.(Base Char) ->
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
