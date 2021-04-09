(* Some common expressions that might be useful in various programs *)
open DessserTools
module E = DessserExpressions
module T = DessserTypes

(* [coalesce es] build an expression that selects the first non null
 * expression in [rs]. *)
let coalesce l es =
  let open E.Ops in
  let rec loop i l = function
    | [] ->
        assert false (* because of type checking *)
    | [ e ] ->
        e
    | e :: es ->
        (match E.type_of l e with
        | T.Value { nullable = true ; _ } ->
            let name = "coalesced_"^ string_of_int i in
            let_ ~name ~l e (fun l d ->
              if_
                ~cond:(is_null d)
                ~then_:(loop (i + 1) l es)
                ~else_:(force d))
        | _ ->
            (* If [e] is not a nullable thing there is no point looking
             * further: *)
            e) in
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
    if_ ~cond:(std_random T.(required (Mac Bool)))
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
      eq (u32_of_int 0) (log_and random_u32 (u32_of_int 128))
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
                    (rem (std_random T.(required (Mac mac_u8)))
                         (u8_of_int 26)))))
  | Mac (Integer (S8, Unsigned)) ->
      random_u8
  | Mac (Integer (S16, Unsigned)) ->
      to_u16 random_u32
  | Mac (Integer (S24, Unsigned)) ->
      to_u24 random_u32
  | Mac (Integer (S32, Unsigned)) ->
      random_u32
  | Mac (Integer (S40, Unsigned)) ->
      to_u40 random_u64
  | Mac (Integer (S48, Unsigned)) ->
      to_u48 random_u64
  | Mac (Integer (S56, Unsigned)) ->
      to_u56 random_u64
  | Mac (Integer (S64, Unsigned)) ->
      random_u64
  | Mac (Integer (S128, Unsigned)) ->
      random_u128
  | Mac (Integer (S8,  Signed)) ->
      to_i8 random_u8
  | Mac (Integer (S16, Signed)) ->
      to_i16 random_u32
  | Mac (Integer (S24, Signed)) ->
      to_i24 random_u32
  | Mac (Integer (S32, Signed)) ->
      to_i32 random_u32
  | Mac (Integer (S40, Signed)) ->
      to_i40 random_u64
  | Mac (Integer (S48, Signed)) ->
      to_i48 random_u64
  | Mac (Integer (S56, Signed)) ->
      to_i56 random_u64
  | Mac (Integer (S64, Signed)) ->
      to_i64 random_u64
  | Mac (Integer (S128, Signed)) ->
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
  | Set mn ->
      random_slist mn |>
      set_of_slist
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
            if_
              ~cond:(ge (abs sum) (abs x))
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
        BatPrintf.sprintf2 "percentiles: coeficients must be a vector/list (not %a)"
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
