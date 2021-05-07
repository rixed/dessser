(* Some common expressions that might be useful in various programs *)
open Stdint

open DessserTools
module C = DessserConversions
module E = DessserExpressions
module T = DessserTypes

(* [coalesce es] build an expression that selects the first non null
 * expression in [rs].
 * All items of [rs] must have the same value-type.
 * If the last alternative is nullable then the result of the coalesce is still
 * nullable.
 * The first non-nullable alternative will be the last checked item obviously. *)
let coalesce l es =
  let open E.Ops in
  if es = [] then
    invalid_arg "coalesce with no alternatives" ;
  let last_e = BatList.last es in
  let ret_nullable = T.is_nullable (E.type_of l last_e) in
  let rec loop i l = function
    | [] ->
        assert false (* because of the pre-condition *)
    | [ e ] ->
        e
    | e :: es ->
        (match E.type_of l e with
        | T.Value { nullable = true ; _ } ->
            let name = "coalesced_"^ string_of_int i in
            let_ ~name ~l e (fun l d ->
              if_null d
                ~then_:(loop (i + 1) l es)
                ~else_:(
                  if ret_nullable then d
                  else force ~what:"coalesce" d))
        | _ ->
            (* If [e] is not a nullable thing there is no point looking
             * further: *)
            if ret_nullable then not_null e else e) in
  loop 0 l es

let random_i32 =
  let open E.Ops in
  to_i32 random_u32

let rec random_slist mn =
  let open E.Ops in
  let max_list_length = i32_of_int 8 in
  let from = i32_of_int 0
  and to_ =
    force ~what:"random_slist rem"
      (rem (add (i32_of_int 1) random_i32) max_list_length)
  and body =
    E.func2 T.i32 T.(SList (Value mn)) (fun _l _idx lst ->
      cons (random mn) lst)
  and init = eol T.(Value mn) in
  repeat ~from ~to_ ~body ~init

(* [random mn] returns an expression with a (runtime) random value of
 * maybe-nullable type [mn]: *)
and random mn =
  (* this [random] is going to be shaddowed by E.Ops: *)
  let std_random = random in
  let open E.Ops in
  if mn.T.nullable then
    if_ (std_random T.(required (Mac Bool)))
      ~then_:(null mn.vtyp)
      ~else_:(not_null (std_random { mn with nullable = false }))
  else match mn.vtyp with
  | T.Unknown ->
      invalid_arg "random for unknown type"
  | Unit ->
      unit
  | Mac Float ->
      random_float
  | Mac Bool ->
      eq (u32_of_int 0) (bit_and random_u32 (u32_of_int 128))
  | Mac String ->
      (* Just 5 random letters for now: *)
      repeat
        ~from:(i32 0l) ~to_:(i32 5l)
        ~init:(string "") ~body:(E.func2 T.i32 T.string (fun _l _i s ->
          let c = std_random T.(required (Mac Char)) in
          append_string s (string_of_char_ c)))
  | Mac Char ->
      (* Just a random lowercase letter for now *)
      (char_of_u8
        (add (u8_of_char (char 'a'))
             (force ~what:"random rem"
                    (rem (std_random T.(required (Mac U8)))
                         (u8_of_int 26)))))
  | Mac U8 ->
      random_u8
  | Mac U16 ->
      to_u16 random_u32
  | Mac U24 ->
      to_u24 random_u32
  | Mac U32 ->
      random_u32
  | Mac U40 ->
      to_u40 random_u64
  | Mac U48 ->
      to_u48 random_u64
  | Mac U56 ->
      to_u56 random_u64
  | Mac U64 ->
      random_u64
  | Mac U128 ->
      random_u128
  | Mac I8 ->
      to_i8 random_u8
  | Mac I16 ->
      to_i16 random_u32
  | Mac I24 ->
      to_i24 random_u32
  | Mac I32 ->
      to_i32 random_u32
  | Mac I40 ->
      to_i40 random_u64
  | Mac I48 ->
      to_i48 random_u64
  | Mac I56 ->
      to_i56 random_u64
  | Mac I64 ->
      to_i64 random_u64
  | Mac I128 ->
      to_i128 random_u128
  | Usr ut ->
      std_random T.(required ut.def)
  | Ext n ->
      invalid_arg ("random for Ext type "^ n)
  | Vec (dim, mn) ->
      List.init dim (fun _ -> std_random mn) |>
      make_vec
  | Lst mn ->
      random_slist mn |>
      list_of_slist
  | Set (Simple, mn) ->
      random_slist mn |>
      set_of_slist
  | Set _ ->
      todo "random for non simple sets"
  | Tup mns ->
      Array.map std_random mns |>
      Array.to_list |>
      make_tup
  | Rec mns ->
      Array.map (fun (name, mn) -> name, std_random mn) mns |>
      Array.to_list |>
      make_rec
  | Sum _mns ->
      todo "random for sum types"
  | Map _ ->
      invalid_arg "random for Map type"

(*
 * Kahan sums for better accuracy when summing large number of floats:
 *)

module Kahan =
struct
  (* The state of the sum consists of the sum itself and some carry: *)
  let state_t =
    let float = T.(required (Mac Float)) in
    T.tuple [| float ; float |]

  (* Return the initial value for the state: *)
  let init =
    let open E.Ops in
    let zero = float 0. in
    make_tup [ zero ; zero ]

  (* Add [x] (a float) to [sum] ([c] is carried along and must be added too
   * eventually): *)
  let add ~l sum_c x =
    let open E.Ops in
    let_ ~name:"kahan_state" ~l sum_c (fun l sum_c ->
      let_ ~name:"kahan_x" ~l (to_float x) (fun l x ->
        let sum = get_item 0 sum_c
        and carry = get_item 1 sum_c in
        let_ ~name:"kahan_sum" ~l (add sum x) (fun _l s ->
          let carry' =
            if_ (ge (abs sum) (abs x))
              ~then_:(add (sub sum s) x)
              ~else_:(add (sub x s) sum) in
          make_tup [ s ; add carry carry' ])))

  (* In some rare cases we might want to scale the counter: *)
  let mul ~l sum_c x =
    let open E.Ops in
    let_ ~name:"kahan_state" ~l sum_c (fun l sum_c ->
      let_ ~name:"kahan_x2" ~l (to_float x) (fun _l x ->
        let sum = get_item 0 sum_c
        and carry = get_item 1 sum_c in
        make_tup [ mul sum x ; mul carry x ]))

  let finalize ~l sum_c =
    ignore l ;
    let open E.Ops in
    let_ ~name:"kahan_state" ~l sum_c (fun _l sum_c ->
      let sum = get_item 0 sum_c
      and carry = get_item 1 sum_c in
      add sum carry)
end

(*
 * Compute the percentiles [ps] of a set of values [vs].
 * [vs] and [ps] must be vectors.
 * The result is a vector of same dimension than [ps].
 *)

let percentiles ~l vs ps =
  let open E.Ops in
  comment "Compute the indices of those percentiles" (
    match E.get_item_type_err ~vec:true ~lst:true l ps with
    | Error t ->
        BatPrintf.sprintf2
          "percentiles: coefficients must be a vector/list (not %a)"
          T.print t |>
        invalid_arg
    | Ok p_t ->
        let_ ~name:"vs" ~l vs (fun l vs ->
          let ks =
            map_ ps (E.func1 ~l (T.Value p_t) (fun _l p ->
              seq [
                assert_ (and_ (ge (to_float p) (float 0.))
                              (le (to_float p) (float 100.))) ;
                mul (mul (to_float p) (float 0.01))
                    (to_float (sub (cardinality vs) (u32_of_int 1))) |>
                round |>
                to_u32 ])) in
          let_ ~name:"perc_ks" ~l ks (fun l ks ->
            seq [
              (* Sort vs: *)
              partial_sort vs ks ;
              map_ ks (E.func1 ~l T.u32 (fun _l k ->
                get_vec k vs)) ])))

(* If [e] is not nullable, [to_nullable l e] is [not_null e].
 * If [e] is nullable already then [to_nullable l e] is just [e]. *)
let to_nullable l e =
  let open E.Ops in
  if T.is_nullable (E.type_of l e) then e
  else not_null e

(* Tells if a vector/list/set/slist/data_ptr is empty, dealing with nullable: *)
let is_empty l e =
  let open E.Ops in
  let prop_null nullable e f =
    if nullable then
      if_null e
        ~then_:(null T.(Mac Bool))
        ~else_:(not_null (f (force e)))
    else
      f e in
  match E.type_of l e with
  | T.Value ({ vtyp = (Mac String) ; nullable }) ->
      prop_null nullable e (fun e ->
        eq (u32_of_int 0) (string_length e))
  | T.Value ({ vtyp = (Vec _ | Lst _ | Set _) ; nullable }) ->
      prop_null nullable e (fun e ->
        eq (u32_of_int 0) (cardinality e))
  | T.Value ({ vtyp = Usr { name = "Cidr4" ; _ } ; nullable }) ->
      prop_null nullable e (fun e ->
        lt (get_field "mask" e) (u8_of_int 32))
  | T.Value ({ vtyp = Usr { name = "Cidr6" ; _ } ; nullable }) ->
      prop_null nullable e (fun e ->
        lt (get_field "mask" e) (u8_of_int 128))
  | T.Value ({ vtyp = Usr { name = "Cidr" ; _ } ; nullable }) ->
      prop_null nullable e (fun e ->
        if_ (eq (label_of e) (u16_of_int 0))
          ~then_:(lt (get_field "mask" (get_alt "v4" e)) (u8_of_int 32))
          ~else_:(lt (get_field "mask" (get_alt "v6" e)) (u8_of_int 128)))
  | DataPtr ->
      eq (size 0) (rem_size e)
  | SList t ->
      eq e (eol t)
  | t ->
      BatPrintf.sprintf2 "is_empty for %a" T.print t |>
      invalid_arg

let exists ~l lst f =
  let open E.Ops in
  match E.get_item_type_err ~vec:true ~lst:true ~set:true l lst with
  | Error t ->
      BatPrintf.sprintf2 "exists: must pass a vector/list/set (not %a)"
        T.print t |>
      invalid_arg
  | Ok item_t ->
      (* FIXME: a way to exit the loop that iterates through a container *)
      fold
        ~init:(bool false)
        ~body:(E.func2 ~l T.bool T.(Value item_t) (fun l res item ->
          or_ res (f l item)))
        ~list:lst

let first_ip_of_cidr width all_ones cidr =
  let open E.Ops in
  let ip = get_field "ip" cidr in
  let mask = get_field "mask" cidr in
  let shf = sub (u8_of_int width) mask in
  let nm = left_shift all_ones shf in
  bit_and nm ip

let first_ip_of_cidr4 cidr =
  let open E.Ops in
  first_ip_of_cidr 32 (u32 Uint32.(sub zero one)) cidr

let first_ip_of_cidr6 cidr =
  let open E.Ops in
  first_ip_of_cidr 128 (u128 Uint128.(sub zero one)) cidr

let last_ip_of_cidr width all_ones one cidr =
  let open E.Ops in
  let ip = get_field "ip" cidr in
  let mask = get_field "mask" cidr in
  let shf = sub (u8_of_int width) mask in
  let nm = left_shift one shf in
  (* Left-shifts of more than the int width are undefined: *)
  if_ (eq (u8_of_int 0) mask)
    ~then_:all_ones
    ~else_:(sub (bit_or nm ip) one)

let last_ip_of_cidr4 cidr =
  let open E.Ops in
  last_ip_of_cidr 32 (u32 Uint32.(sub zero one)) (u32_of_int 1) cidr

let last_ip_of_cidr6 cidr =
  let open E.Ops in
  last_ip_of_cidr 128 (u128 Uint128.(sub zero one)) (u128_of_int 1) cidr

(* Tells whether [e] is in the iterable/cidr [lst].
 * Also works when [lst] is a set of sets (of sets...).
 * Also handles the case where [lst] and/or [e] is null (note than null is known
 * not to be in a (non-null) empty set).
 * Return value is nullable whenever [item] or [lst] is. *)
let rec is_in ?(l=[]) item lst =
  let open E.Ops in
  let_ ~l ~name:"lst" lst (fun l lst ->
    let_ ~l ~name:"item" item (fun l item ->
      let lst_t = E.type_of l lst
      and item_t = E.type_of l item in
      if T.is_nullable lst_t then
        if_null lst
          ~then_:(null T.(Mac Bool))
          ~else_:(
            to_nullable l (is_in ~l item (force ~what:"is_in(0)" lst)))
      else
        if_ (comment "is_in: Is List empty?"
              ((* It makes that code simpler to allow `scalar is_in scalar` and
                  treat it as meaning `scalar = scalar`, so we must accept non
                  containers as never empty: *)
                try is_empty l lst
                with Invalid_argument _ -> bool false))
          ~then_:(
            let ret = bool true in
            if T.is_nullable item_t then not_null ret else ret)
          ~else_:(
            if T.is_nullable item_t then
              if_null item
                ~then_:(null T.(Mac Bool))
                ~else_:(
                  to_nullable l (is_in ~l (force ~what:"is_in(1)" item) lst))
            else (
              let err () =
                BatPrintf.sprintf2 "is_in: invalid types (%a in %a)"
                  T.print item_t
                  T.print lst_t |>
                invalid_arg in
              (* Now that neither lst nor item are nullable, let's deal with
               * the actual question: *)
              match item_t, lst_t with
              (* Substring search: *)
              | T.Value { vtyp = Mac String ; _ },
                T.Value { vtyp = Mac String ; _ } ->
                  not_ (is_null (find_substring (bool true) item lst))
              (* In all other cases where [item] and [lst] are of the same type
               * then [in_in item lst] is just a comparison: *)
              | t1, t2 when T.eq t1 t2 ->
                  eq item lst
              (* Otherwise [lst] must be some kind of sub-set: *)
              | T.Value { vtyp = Usr { name = "Ip4" ; _ } ; _ },
                T.Value { vtyp = Usr { name = "Cidr4" ; _ } ; _ } ->
                  and_ (ge item (first_ip_of_cidr4 lst))
                       (le item (last_ip_of_cidr4 lst))
              | T.Value { vtyp = Usr { name = "Ip6" ; _ } ; _ },
                T.Value { vtyp = Usr { name = "Cidr6" ; _ } ; _ } ->
                  and_ (ge item (first_ip_of_cidr6 lst))
                       (le item (last_ip_of_cidr6 lst))
              | T.Value { vtyp = Usr { name = "Ip" ; _ } ; _ },
                T.Value { vtyp = Usr { name = "Cidr" ; _ } ; _ } ->
                  if_ (eq (label_of item) (label_of lst))
                    ~then_:(
                      if_ (eq (label_of item) (u16_of_int 0))
                        ~then_:(
                          let_ ~l ~name:"ip" (get_alt "v4" item) (fun l ip ->
                            let_ ~l ~name:"cidr" (get_alt "v4" lst) (fun l cidr ->
                              is_in ~l ip cidr)))
                        ~else_:(
                          let_ ~l ~name:"ip" (get_alt "v6" item) (fun l ip ->
                            let_ ~l ~name:"cidr" (get_alt "v6" lst) (fun l cidr ->
                              is_in ~l ip cidr))))
                    ~else_:(bool false)
              | T.Value { vtyp = item_vtyp ; _ },
                ( T.Value { vtyp = Vec (_, { vtyp = lst_vtyp ; nullable }) ; _ }
                | T.Value { vtyp = Lst { vtyp = lst_vtyp ; nullable } ; _ }) ->
                  (* If the set item type is the same as item type then perform
                   * direct comparisons with [eq], otherwise call [is_in]
                   * recursively.
                   * [is_in] cannot be merely called recursively because of the
                   * semantic with strings: "ba" is in "foobar" whereas it is
                   * not in [ "foobar" ]! *)
                  let op l =
                    if T.value_type_eq item_vtyp lst_vtyp then eq
                                                          else is_in ~l in
                  exists ~l lst (fun l i ->
                    if nullable then
                      if_null i
                        ~then_:(bool false)
                        ~else_:(op l item (force ~what:"is_in(2)" i))
                    else
                      op l item i)
              | _, T.Value lst_mn ->
                  (* If we can convert item into lst (which at this point is
                   * known not to be a list) then we can try equality: *)
                  (match C.conv_maybe_nullable ~to_:lst_mn l item with
                  | exception _ ->
                      err ()
                  | item' ->
                      eq item' lst)
              | _ ->
                  err ()))))
