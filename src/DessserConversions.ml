(* Converting expressions from one type into another *)
open Batteries
open Stdint

module E = DessserExpressions
module T = DessserTypes
open DessserTools
open E.Ops

let debug = false

(* Convert a non-nullable value to the given value-type.
 * Beware that the returned expression might be nullable (for instance when
 * converting a string to a number). *)
(* TODO: move in dessser.StdLib as a "cast" function *)
let rec conv ?(depth=0) ~to_ l d =
  let fields_of_rec mns =
    let f = Array.map fst mns in
    Array.fast_sort String.compare f ;
    f in
  let conv = conv ~depth:(depth+1) in
  let conv_mn = conv_mn ~depth:(depth+1) in
  let map_items d mn1 mn2 =
    map_ nop (
      E.func2 ~l T.void mn1 (fun l _void item ->
        conv_mn ~to_:mn2 l item)
      ) d in
  let from = (E.type_of l d).T.typ in
  if T.eq from to_ then d else
  (* A null can be cast to whatever. Actually, type-checking will type nulls
   * arbitrarily. *)
  if match d with E.E0 (Null _) -> true | _ -> false then null to_ else
  match from, to_ with
  (* Any cast from a user type to its implementation is a NOP, and the other
   * way around too: *)
  | Usr { def ; _ }, to_ when T.eq def to_ ->
      d
  | from, Usr { def ; _ } when T.eq def from ->
      d
  | T.Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128),
    T.Base String -> string_of_int_ d
  | T.Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128),
    T.Base Float -> to_float d
  | T.Base U8, T.Base Bool -> bool_of_u8 d
  | T.Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    T.Base Bool -> bool_of_u8 (conv ~to_:(T.Base U8) l d)
  | Base String, Base Float -> float_of_string_ d
  | Base String, Base Char -> nth (u8_of_int 0) d
  | Base String, Base I8 -> i8_of_string d
  | Base String, Base I16 -> i16_of_string d
  | Base String, Base I24 -> i24_of_string d
  | Base String, Base I32 -> i32_of_string d
  | Base String, Base I40 -> i40_of_string d
  | Base String, Base I48 -> i48_of_string d
  | Base String, Base I56 -> i56_of_string d
  | Base String, Base I64 -> i64_of_string d
  | Base String, Base I128 -> i128_of_string d
  | Base String, Base U8 -> u8_of_string d
  | Base String, Base U16 -> u16_of_string d
  | Base String, Base U24 -> u24_of_string d
  | Base String, Base U32 -> u32_of_string d
  | Base String, Base U40 -> u40_of_string d
  | Base String, Base U48 -> u48_of_string d
  | Base String, Base U56 -> u56_of_string d
  | Base String, Base U64 -> u64_of_string d
  | Base String, Base U128 -> u128_of_string d
  | Base Float, Base String -> string_of_float_ d
  | Base Char, Base U8 -> u8_of_char d
  | Base U8, Base Char -> char_of_u8 d
  | Base Char, Base String -> string_of_char d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I8 -> to_i8 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I16 -> to_i16 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I24 -> to_i24 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I32 -> to_i32 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I40 -> to_i40 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I48 -> to_i48 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I56 -> to_i56 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I64 -> to_i64 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base I128 -> to_i128 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U8 -> to_u8 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U16 -> to_u16 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U24 -> to_u24 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U32 -> to_u32 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U40 -> to_u40 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U48 -> to_u48 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Usr { name = "Eth" ; _ } ->
      make_usr "Eth" [ to_u48 d ]
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U56 -> to_u56 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U64 -> to_u64 d
  | Base (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Base U128 -> to_u128 d
  (* Bools can be (explicitly) converted into numbers: *)
  | Base Bool, Base U8 -> u8_of_bool d
  | Base Bool, Base U16 -> to_u16 (u8_of_bool d)
  | Base Bool, Base U24 -> to_u24 (u8_of_bool d)
  | Base Bool, Base U32 -> to_u32 (u8_of_bool d)
  | Base Bool, Base U40 -> to_u40 (u8_of_bool d)
  | Base Bool, Base U48 -> to_u48 (u8_of_bool d)
  | Base Bool, Base U56 -> to_u56 (u8_of_bool d)
  | Base Bool, Base U64 -> to_u64 (u8_of_bool d)
  | Base Bool, Base U128 -> to_u128 (u8_of_bool d)
  | Base Bool, Base I8 -> to_i8 (u8_of_bool d)
  | Base Bool, Base I16 -> to_i16 (u8_of_bool d)
  | Base Bool, Base I24 -> to_i24 (u8_of_bool d)
  | Base Bool, Base I32 -> to_i32 (u8_of_bool d)
  | Base Bool, Base I40 -> to_i40 (u8_of_bool d)
  | Base Bool, Base I48 -> to_i48 (u8_of_bool d)
  | Base Bool, Base I56 -> to_i56 (u8_of_bool d)
  | Base Bool, Base I64 -> to_i64 (u8_of_bool d)
  | Base Bool, Base I128 -> to_i128 (u8_of_bool d)
  | Base Bool, Base Float -> to_float (u8_of_bool d)
  (* A vector of 1 t into t and the other way around: *)
  | Vec (1, { nullable = false ; typ = vt1 }), vt2
    when T.eq vt1 vt2 ->
      unsafe_nth (u32_of_int 0) d
  | vt1, Vec (1, { typ = vt2 ; nullable })
    when T.eq vt1 vt2 ->
      make_vec [ if nullable then not_null d else d ]
  | vt1, Arr ({ typ = vt2 ; nullable } as mn2)
    when T.eq vt1 vt2 ->
      make_arr mn2 [ if nullable then not_null d else d ]
  (* Specialized version for arr/vec of chars that return the
   * string composed of those chars rather than an enumeration: *)
  | Vec (_, { typ = Base Char ; _ }), Base String
  | Arr { typ = Base Char ; _ }, Base String ->
      conv_charseq_to_string ~depth:(depth+1) (cardinality d) l d
  | Vec _, Base String
  | Arr _, Base String ->
      conv_list_to_string ~depth:(depth+1) (cardinality d) l d
  | Tup mns, Base String ->
      let to_ = T.(required (Base String)) in
      let rec loop s i =
        if i >= Array.length mns then
          append_string s (string ")")
        else
          let s' = conv_mn ~to_ l (get_item i d) in
          let s = if i > 0 then append_string s (string ";") else s in
          loop (append_string s s') (i + 1) in
      loop (string "(") 0
  | Base Bool, Base String ->
      if_ d ~then_:(string "true") ~else_:(string "false")
  | Usr { name = ("Ip4" | "Ip6" | "Ip") ; _ }, Base String ->
      string_of_ip d
  | Base U32, Usr { name = "Ip4" ; _ } ->
      make_usr "Ip4" [ d ]
  | Usr { name = ("Ip4" | "Ip6") ; _ }, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ d ]
  | Base U32, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip4" [ d ] ]
  | Base U128, Usr { name = "Ip6" ; _ } ->
      make_usr "Ip6" [ d ]
  | Base U128, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip6" [ d ] ]
  | Usr { name = ("Cidr4" | "Cidr6") ; _ }, Usr { name = "Cidr" ; _ } ->
      make_usr "Cidr" [ d ]
  | Vec (d1, mn1), Vec (d2, mn2) when d1 = d2 ->
      map_items d mn1 mn2
  | Arr mn1, Arr mn2 ->
      map_items d mn1 mn2
  (* TODO: Also when d2 < d1, and d2 > d1 extending with null as long as mn2 is
   * nullable *)
  | Vec (_, mn1), Arr mn2 ->
      let d = arr_of_vec d in
      map_items d mn1 mn2
  (* Groups are typed as lists: *)
  | Set (_, mn1), Arr mn2 ->
      let d = arr_of_set d in
      map_items d mn1 mn2
  | Tup mns1, Tup mns2 when Array.length mns1 = Array.length mns2 ->
      (* TODO: actually we could project away fields from t_from when t_to
       * is narrower, or inject NULLs in some cases. *)
      make_tup (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~to_:mns2.(i) l (get_item i d)))
  | Tup mns1, Vec (dim, mn2) when Array.length mns1 = dim ->
      make_vec (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~to_:mn2 l (get_item i d)))
  | Tup mns1, Arr mn2 ->
      make_arr mn2 (
        List.init (Array.length mns1) (fun i ->
          conv_mn ~to_:mn2 l (get_item i d)))
  | Rec mns1, Rec mns2 when fields_of_rec mns1 = fields_of_rec mns2 ->
      Array.fold_left (fun fields (n, mn) ->
        (n, conv_mn ~to_:mn l (get_field n d)) :: fields
      ) [] mns2 |>
      List.rev |>
      make_rec
  (* TODO: other types to string *)
  (* "globals_map" is an alias for the type used by CodeGenLib to set/get
   * to/from LMDB files: *)
  | Ext "globals_map",
    Map ({ typ = Base String ; _ }, { typ = Base String ; _ })
  | Map ({ typ = Base String ; _ }, { typ = Base String ; _ }),
    Ext "globals_map" ->
      d
  | _ ->
      Printf.sprintf2 "Not implemented: Cast from %a to %a of expression %a"
        T.print from
        T.print to_
        (E.print ~max_depth:3) d |>
      failwith

and conv_list_to_string ?(depth=0) length_e l src =
  let_ ~name:"str_ref" ~l (make_ref (string "[")) (fun l str_ref ->
    let str = get_ref str_ref in
    let append s = set_ref str_ref (append_string str s) in
    let_ ~name:"i_ref" ~l (make_ref (u32_of_int 0)) (fun l i_ref ->
      let i = get_ref i_ref in
      let_ ~name:"src_ref" ~l (make_ref src) (fun l src_ref ->
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
              conv_mn
                ~depth:(depth+1) ~to_:T.(required (Base String)) l |>
              append ;
              (* Incr i *)
              set_ref i_ref (add i (u32_of_int 1)) ]) ;
          append (string "]") ;
          str ])))

and conv_charseq_to_string ?(depth=0) length_e l src =
  let_ ~name:"str_ref" ~l (make_ref (string "")) (fun l str_ref ->
    let str = get_ref str_ref in
    let append s = set_ref str_ref (append_string str s) in
    let_ ~name:"i_ref" ~l (make_ref (u32_of_int 0)) (fun l i_ref ->
      let i = get_ref i_ref in
      let_ ~name:"src_ref" ~l (make_ref src) (fun l src_ref ->
        seq [
          while_ (lt i length_e)
            (let src = get_ref src_ref in
            seq [
              unsafe_nth i src |>
              conv_mn
                ~depth:(depth+1) ~to_:T.(required (Base Char)) l |>
              string_of_char |>
              append ;
              (* Incr i *)
              set_ref i_ref (add i (u32_of_int 1)) ]) ;
          str ])))

and conv_mn ?(depth=0) ~to_ l d =
  if debug then (
    let indent = String.make (depth * 2) ' ' in
    Printf.eprintf "%sConverting into %a: %a\n"
      indent
      T.print_mn to_
      (E.print ?max_depth:None) d) ;
  let conv = conv ~depth:(depth+1) ~to_:to_.T.typ in
  let from = E.type_of l d in
  let is_const_null =
    match d with E.E0 (Null _) -> true | _ -> false in
  let if_null def =
    if is_const_null then def else
    let_ ~name:"nullable_to_not_nullable_" ~l d (fun l d ->
      if_null d
        ~then_:def
        ~else_:(conv l (force ~what:"if_null" d))) in
  (* Beware that [conv] can return a nullable expression: *)
  match from.T.nullable, to_.T.nullable with
  | false, false ->
      let d' = conv l d in
      if (E.type_of l d').T.nullable then
        force ~what:"conv from not nullable to not nullable" d'
      else d'
  | true, false ->
      (match to_.T.typ with
      | T.(Base String) ->
          if_null (string "NULL")
      | T.(Base Char) ->
          if_null (char '?')
      | _ ->
          let d' =
            conv l (force ~what:"conv from nullable to not nullable" d) in
          if (E.type_of l d').T.nullable then
            force ~what:"conv from nullable to not nullable (2)" d'
          else d')
  | false, true ->
      let d' = conv l d in
      if (E.type_of l d').T.nullable then d'
                                                 else not_null d'
  | true, true ->
      if is_const_null then null to_.T.typ else
      let_ ~name:"conv_mn_x_" ~l d (fun l x ->
        if_ (is_null x)
          ~then_:(
            let x_vtyp = (E.type_of l x).T.typ in
            if T.eq x_vtyp to_.T.typ then
              x
            else
              null to_.T.typ)
          ~else_:(
            conv_mn ~depth:(depth+1) ~to_ l
              (force ~what:"conv from nullable to nullable" x)))

(* If [d] is nullable, then return it. If it's a not nullable value type,
 * then make it nullable: *)
let ensure_nullable ~l d =
  match E.type_of l d with
  | T. { nullable = false ; _ } -> not_null d
  | T. { nullable = true ; _ } -> d
