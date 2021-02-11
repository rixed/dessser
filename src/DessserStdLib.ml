(* Some common expressions that might be useful in various programs *)
open DessserTools
module E = DessserExpressions
module T = DessserTypes

(* [coalesce es] build an expression that selects the first non null
 * expression in [rs]. *)
let coalesce l es =
  let open E.Ops in
  let rec loop i = function
    | [] ->
        assert false (* because of type checking *)
    | [ e ] ->
        e
    | e :: es ->
        (match E.type_of l e with
        | T.Value { nullable = true ; _ } ->
            let name = "coalesced_"^ string_of_int i in
            let_ ~name e (fun _l d ->
              if_
                ~cond:(is_null d)
                ~then_:(loop (i + 1) es)
                ~else_:(force d))
        | _ ->
            (* If [e] is not a nullable thing there is no point looking
             * further: *)
            e) in
  loop 0 es

(* [random mn] returns an expression with a (runtime) random value of
 * maybe-nullable type [mn]: *)
let rec random mn =
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
             (force (rem (std_random T.(required (Mac U8)))
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
  | Lst _mn ->
      todo "random for lists"
  | Set _ ->
      invalid_arg "random for Set type"
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
