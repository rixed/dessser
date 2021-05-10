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
  let conv_maybe_nullable = conv_maybe_nullable ~depth:(depth+1) in
  let map_items d mn1 mn2 =
    map_ d (
      E.func1 ~l (T.Value mn1)
        (conv_maybe_nullable ~to_:mn2)) in
  let from = (T.mn_of_t (E.type_of l d)).T.vtyp in
  if T.value_type_eq from to_ then d else
  (* A null can be cast to whatever. Actually, type-checking will type nulls
   * arbitrarily. *)
  if match d with E.E0 (Null _) -> true | _ -> false then null to_ else
  match from, to_ with
  (* Any cast from a user type to its implementation is a NOP, and the other
   * way around too: *)
  | Usr { def ; _ }, to_ when T.value_type_eq def to_ ->
      d
  | from, Usr { def ; _ } when T.value_type_eq def from ->
      d
  | T.Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128),
    T.Mac String -> string_of_int_ d
  | T.Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128),
    T.Mac Float -> to_float d
  | T.Mac U8, T.Mac Bool -> bool_of_u8 d
  | T.Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
            U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    T.Mac Bool -> bool_of_u8 (conv ~to_:(T.Mac U8) l d)
  | Mac String, Mac Float -> float_of_string_ d
  | Mac String, Mac Char -> char_of_string (u8_of_int 0) d
  | Mac String, Mac I8 -> i8_of_string d
  | Mac String, Mac I16 -> i16_of_string d
  | Mac String, Mac I24 -> i24_of_string d
  | Mac String, Mac I32 -> i32_of_string d
  | Mac String, Mac I40 -> i40_of_string d
  | Mac String, Mac I48 -> i48_of_string d
  | Mac String, Mac I56 -> i56_of_string d
  | Mac String, Mac I64 -> i64_of_string d
  | Mac String, Mac I128 -> i128_of_string d
  | Mac String, Mac U8 -> u8_of_string d
  | Mac String, Mac U16 -> u16_of_string d
  | Mac String, Mac U24 -> u24_of_string d
  | Mac String, Mac U32 -> u32_of_string d
  | Mac String, Mac U40 -> u40_of_string d
  | Mac String, Mac U48 -> u48_of_string d
  | Mac String, Mac U56 -> u56_of_string d
  | Mac String, Mac U64 -> u64_of_string d
  | Mac String, Mac U128 -> u128_of_string d
  | Mac Float, Mac String -> string_of_float_ d
  | Mac Char, Mac U8 -> u8_of_char d
  | Mac U8, Mac Char -> char_of_u8 d
  | Mac Char, Mac String -> string_of_char d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I8 -> to_i8 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I16 -> to_i16 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I24 -> to_i24 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I32 -> to_i32 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I40 -> to_i40 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I48 -> to_i48 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I56 -> to_i56 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I64 -> to_i64 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac I128 -> to_i128 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U8 -> to_u8 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U16 -> to_u16 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U24 -> to_u24 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U32 -> to_u32 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U40 -> to_u40 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U48 -> to_u48 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Usr { name = "Eth" ; _ } ->
      make_usr "Eth" [ to_u48 d ]
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U56 -> to_u56 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U64 -> to_u64 d
  | Mac (I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128 |
         U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 | Float),
    Mac U128 -> to_u128 d
  (* Bools can be (explicitly) converted into numbers: *)
  | Mac Bool, Mac U8 -> u8_of_bool d
  | Mac Bool, Mac U16 -> to_u16 (u8_of_bool d)
  | Mac Bool, Mac U24 -> to_u24 (u8_of_bool d)
  | Mac Bool, Mac U32 -> to_u32 (u8_of_bool d)
  | Mac Bool, Mac U40 -> to_u40 (u8_of_bool d)
  | Mac Bool, Mac U48 -> to_u48 (u8_of_bool d)
  | Mac Bool, Mac U56 -> to_u56 (u8_of_bool d)
  | Mac Bool, Mac U64 -> to_u64 (u8_of_bool d)
  | Mac Bool, Mac U128 -> to_u128 (u8_of_bool d)
  | Mac Bool, Mac I8 -> to_i8 (u8_of_bool d)
  | Mac Bool, Mac I16 -> to_i16 (u8_of_bool d)
  | Mac Bool, Mac I24 -> to_i24 (u8_of_bool d)
  | Mac Bool, Mac I32 -> to_i32 (u8_of_bool d)
  | Mac Bool, Mac I40 -> to_i40 (u8_of_bool d)
  | Mac Bool, Mac I48 -> to_i48 (u8_of_bool d)
  | Mac Bool, Mac I56 -> to_i56 (u8_of_bool d)
  | Mac Bool, Mac I64 -> to_i64 (u8_of_bool d)
  | Mac Bool, Mac I128 -> to_i128 (u8_of_bool d)
  | Mac Bool, Mac Float -> to_float (u8_of_bool d)
  (* A vector of 1 t into t and the other way around: *)
  | Vec (1, { nullable = false ; vtyp = vt1 }), vt2
    when T.value_type_eq vt1 vt2 ->
      get_vec (u32_of_int 0) d
  | vt1, Vec (1, { vtyp = vt2 ; nullable })
    when T.value_type_eq vt1 vt2 ->
      make_vec [ if nullable then not_null d else d ]
  | vt1, Lst ({ vtyp = vt2 ; nullable } as mn2)
    when T.value_type_eq vt1 vt2 ->
      make_lst mn2 [ if nullable then not_null d else d ]
  (* Specialized version for lst/vec of chars that return the
   * string composed of those chars rather than an enumeration: *)
  | Vec (_, { vtyp = Mac Char ; _ }), Mac String
  | Lst { vtyp = Mac Char ; _ }, Mac String ->
      conv_charseq ~depth:(depth+1) (cardinality d) l d
  | Vec _, Mac String
  | Lst _, Mac String ->
      conv_list ~depth:(depth+1) (cardinality d) l d
  | Tup mns, Mac String ->
      let to_ = T.(required (Mac String)) in
      let rec loop s i =
        if i >= Array.length mns then
          append_string s (string ")")
        else
          let s' = conv_maybe_nullable ~to_ l (get_item i d) in
          let s = if i > 0 then append_string s (string ";") else s in
          loop (append_string s s') (i + 1) in
      loop (string "(") 0
  | Mac Bool, Mac String ->
      if_ d ~then_:(string "true") ~else_:(string "false")
  | Usr { name = ("Ip4" | "Ip6" | "Ip") ; _ }, Mac String ->
      string_of_ip d
  | Mac U32, Usr { name = "Ip4" ; _ } ->
      make_usr "Ip4" [ d ]
  | Usr { name = ("Ip4" | "Ip6") ; _ }, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ d ]
  | Mac U32, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip4" [ d ] ]
  | Mac U128, Usr { name = "Ip6" ; _ } ->
      make_usr "Ip6" [ d ]
  | Mac U128, Usr { name = "Ip" ; _ } ->
      make_usr "Ip" [ make_usr "Ip6" [ d ] ]
  | Usr { name = ("Cidr4" | "Cidr6") ; _ }, Usr { name = "Cidr" ; _ } ->
      make_usr "Cidr" [ d ]
  | Vec (d1, mn1), Vec (d2, mn2) when d1 = d2 ->
      map_items d mn1 mn2
  | Lst mn1, Lst mn2 ->
      map_items d mn1 mn2
  (* TODO: Also when d2 < d1, and d2 > d1 extending with null as long as mn2 is
   * nullable *)
  | Vec (_, mn1), Lst mn2 ->
      let d = list_of_vec d in
      map_items d mn1 mn2
  (* Groups are typed as lists: *)
  | Set (_, mn1), Lst mn2 ->
      let d = list_of_set d in
      map_items d mn1 mn2
  | Tup mns1, Tup mns2 when Array.length mns1 = Array.length mns2 ->
      (* TODO: actually we could project away fields from t_from when t_to
       * is narrower, or inject NULLs in some cases. *)
      make_tup (
        List.init (Array.length mns1) (fun i ->
          conv_maybe_nullable ~to_:mns2.(i) l (get_item i d)))
  | Tup mns1, Vec (dim, mn2) when Array.length mns1 = dim ->
      make_vec (
        List.init (Array.length mns1) (fun i ->
          conv_maybe_nullable ~to_:mn2 l (get_item i d)))
  | Tup mns1, Lst mn2 ->
      make_lst mn2 (
        List.init (Array.length mns1) (fun i ->
          conv_maybe_nullable ~to_:mn2 l (get_item i d)))
  | Rec mns1, Rec mns2 when fields_of_rec mns1 = fields_of_rec mns2 ->
      Array.fold_left (fun fields (n, mn) ->
        (n, conv_maybe_nullable ~to_:mn l (get_field n d)) :: fields
      ) [] mns2 |>
      List.rev |>
      make_rec
  (* TODO: other types to string *)
  (* "globals_map" is an alias for the type used by CodeGenLib to set/get
   * to/from LMDB files: *)
  | Ext "globals_map",
    Map ({ vtyp = Mac String ; _ }, { vtyp = Mac String ; _ })
  | Map ({ vtyp = Mac String ; _ }, { vtyp = Mac String ; _ }),
    Ext "globals_map" ->
      d
  | _ ->
      Printf.sprintf2 "Not implemented: Cast from %a to %a of expression %a"
        T.print_value_type from
        T.print_value_type to_
        (E.print ~max_depth:3) d |>
      failwith

and conv_list ?(depth=0) length_e l src =
  (* [dst] is a ref cell storing the build up string: *)
  let_ ~name:"dst_" ~l (make_vec [ string "[" ]) (fun _l dst ->
    let set v = set_ref dst v
    and get () = get_ref dst in
    let idx_t = T.(Value (required (Mac U32))) in
    let cond =
      E.func1 ~l idx_t (fun _l i -> lt i length_e)
    and body =
      E.func1 ~l idx_t (fun _l i ->
        seq [
          (* Append a delimiter? *)
          if_ (gt i (u32_of_int 0))
            ~then_:(set (append_string (get ()) (string ";")))
            ~else_:nop ;
          (* Next value: *)
          (let s =
            conv_maybe_nullable ~depth:(depth+1) ~to_:T.(required (Mac String))
                                l (get_vec i src) in
          set (append_string (get ()) s)) ;
          (* Loop: *)
          add i (u32_of_int 1) ]) in
    seq [ ignore_ (loop_while ~init:(u32_of_int 0) ~cond ~body) ;
          set (append_string (get ()) (string "]")) ;
          get () ])

and conv_charseq ?(depth=0) length_e l src =
  (* We use a one entry vector as a ref cell: *)
  let_ ~name:"dst_" ~l (make_vec [ string "" ]) (fun _l dst ->
    let set v = set_vec (u32_of_int 0) dst v
    and get () = get_vec (u32_of_int 0) dst in
    let idx_t = T.(Value (required (Mac U32))) in
    let cond =
      E.func1 ~l idx_t (fun _l i -> lt i length_e)
    and body =
      E.func1 ~l idx_t (fun _l i ->
        let s =
          conv_maybe_nullable ~depth:(depth+1) ~to_:T.(required (Mac Char))
                              l (get_vec i src) in
        seq [ set (append_string (get ()) (string_of_char s)) ;
              add i (u32_of_int 1) ]) in
    seq [ ignore_ (loop_while ~init:(u32_of_int 0) ~cond ~body) ;
          get () ])

and conv_maybe_nullable ?(depth=0) ~to_ l d =
  if debug then (
    let indent = String.make (depth * 2) ' ' in
    Printf.eprintf "%sConverting into %a: %a\n"
      indent
      T.print_maybe_nullable to_
      (E.print ?max_depth:None) d) ;
  let conv = conv ~depth:(depth+1) ~to_:to_.T.vtyp in
  let from = T.mn_of_t (E.type_of l d) in
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
      if (T.mn_of_t (E.type_of l d')).T.nullable then
        force ~what:"conv from not nullable to not nullable" d'
      else d'
  | true, false ->
      (match to_.T.vtyp with
      | T.(Mac String) ->
          if_null (string "NULL")
      | T.(Mac Char) ->
          if_null (char '?')
      | _ ->
          let d' =
            conv l (force ~what:"conv from nullable to not nullable" d) in
          if (T.mn_of_t (E.type_of l d')).T.nullable then
            force ~what:"conv from nullable to not nullable (2)" d'
          else d')
  | false, true ->
      let d' = conv l d in
      if (T.mn_of_t (E.type_of l d')).T.nullable then d'
                                                 else not_null d'
  | true, true ->
      if is_const_null then null to_.T.vtyp else
      let_ ~name:"conv_mn_x_" ~l d (fun l x ->
        if_ (is_null x)
          ~then_:(
            let x_vtyp = (T.mn_of_t (E.type_of l x)).T.vtyp in
            if T.value_type_eq x_vtyp to_.T.vtyp then
              x
            else
              null to_.T.vtyp)
          ~else_:(
            conv_maybe_nullable ~depth:(depth+1) ~to_ l
              (force ~what:"conv from nullable to nullable" x)))

(* If [d] is nullable, then return it. If it's a not nullable value type,
 * then make it nullable: *)
let ensure_nullable ~l d =
  match E.type_of l d with
  | T.Value { nullable = false ; _ } -> not_null d
  | T.Value { nullable = true ; _ } -> d
  | t -> invalid_arg ("ensure_nullable on "^ T.to_string t)
