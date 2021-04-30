(* Some common expressions that might be useful in various programs *)
open DessserTools
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
              if_
                ~cond:(is_null d)
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
