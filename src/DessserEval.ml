open Batteries
open Stdint

open DessserTools
module C = DessserConversions
module FloatTools = DessserFloatTools
module T = DessserTypes
module E = DessserExpressions
open E.Ops

let inline_level = ref 1

let max_inline_size () =
  match !inline_level with
  | 0 -> 0
  (* -O1 does not inflate the code but can still perform some simplification *)
  | 1 -> 1
  | 2 -> 8
  | 3 -> 16
  | 4 -> 32
  | _ -> 64

let to_u128 = function
  | T.E0 (I8 n) -> Int8.to_uint128 n
  | E0 (I16 n) -> Int16.to_uint128 n
  | E0 (I24 n) -> Int24.to_uint128 n
  | E0 (I32 n) -> Int32.to_uint128 n
  | E0 (I40 n) -> Int40.to_uint128 n
  | E0 (I48 n) -> Int48.to_uint128 n
  | E0 (I56 n) -> Int56.to_uint128 n
  | E0 (I64 n) -> Int64.to_uint128 n
  | E0 (I128 n) -> Int128.to_uint128 n
  | E0 (U8 n) -> Uint8.to_uint128 n
  | E0 (U16 n) -> Uint16.to_uint128 n
  | E0 (U24 n) -> Uint24.to_uint128 n
  | E0 (U32 n) -> Uint32.to_uint128 n
  | E0 (U40 n) -> Uint40.to_uint128 n
  | E0 (U48 n) -> Uint48.to_uint128 n
  | E0 (U56 n) -> Uint56.to_uint128 n
  | E0 (U64 n) -> Uint64.to_uint128 n
  | E0 (U128 n) -> Uint128.to_uint128 n
  | E0 (Float n) -> Uint128.of_float n
  | E0 (Size n) -> Uint128.of_int n
  | E0 (Address n) -> Uint128.of_uint64 n
  | _ -> invalid_arg "to_u128"

let to_i128 = function
  | T.E0 (I8 n) -> Int8.to_int128 n
  | E0 (I16 n) -> Int16.to_int128 n
  | E0 (I24 n) -> Int24.to_int128 n
  | E0 (I32 n) -> Int32.to_int128 n
  | E0 (I40 n) -> Int40.to_int128 n
  | E0 (I48 n) -> Int48.to_int128 n
  | E0 (I56 n) -> Int56.to_int128 n
  | E0 (I64 n) -> Int64.to_int128 n
  | E0 (I128 n) -> Int128.to_int128 n
  | E0 (U8 n) -> Uint8.to_int128 n
  | E0 (U16 n) -> Uint16.to_int128 n
  | E0 (U24 n) -> Uint24.to_int128 n
  | E0 (U32 n) -> Uint32.to_int128 n
  | E0 (U40 n) -> Uint40.to_int128 n
  | E0 (U48 n) -> Uint48.to_int128 n
  | E0 (U56 n) -> Uint56.to_int128 n
  | E0 (U64 n) -> Uint64.to_int128 n
  | E0 (U128 n) -> Uint128.to_int128 n
  | E0 (Float n) -> Int128.of_float n
  | E0 (Size n) -> Int128.of_int n
  | E0 (Address n) -> Int128.of_uint64 n
  | _ -> invalid_arg "to_u128"

let to_uint to_op e cst of_u128 =
  match to_u128 e with
  | exception Invalid_argument _ -> T.E1 (to_op, e)
  | n -> cst (of_u128 n)

let to_int to_op e cst of_i128 =
  match to_i128 e with
  | exception Invalid_argument _ -> T.E1 (to_op, e)
  | n -> cst (of_i128 n)

let float_of_num = function
  | T.E0 (Float v) -> v
  | E0 (U128 n) -> Uint128.to_float n
  | e -> Int128.to_float (to_i128 e)

let peval_to_float e =
  try float (float_of_num e)
  with _ -> to_float e

let is_nan f =
  f <> f

let nullable_of_nan f =
  if is_nan f then null TFloat else not_null (float f)

let arith2' e1 e2 op_i128 cst =
  (* All but U128 ints can be safely converted into I128.
   * And non const U128 won't be converted either, so let's just leave this
   * one aside: *)
  match e1, e2 with
  | T.E0 (U128 _), _
  | _, T.E0 (U128 _) ->
      invalid_arg "arith2'"
  | _ ->
      let n1, n2 = to_i128 e1, to_i128 e2 in
      cst (op_i128 n1 n2)

let arith2 e1 e2 op_float op_i128 op_u128 =
  match e1, e2 with
  | T.E0 (U8 _), _ -> arith2' e1 e2 op_i128 (u8 % Int128.to_uint8)
  | E0 (U16 _), _ -> arith2' e1 e2 op_i128 (u16 % Int128.to_uint16)
  | E0 (U24 _), _ -> arith2' e1 e2 op_i128 (u24 % Int128.to_uint24)
  | E0 (U32 _), _ -> arith2' e1 e2 op_i128 (u32 % Int128.to_uint32)
  | E0 (U40 _), _ -> arith2' e1 e2 op_i128 (u40 % Int128.to_uint40)
  | E0 (U48 _), _ -> arith2' e1 e2 op_i128 (u48 % Int128.to_uint48)
  | E0 (U56 _), _ -> arith2' e1 e2 op_i128 (u56 % Int128.to_uint56)
  | E0 (U64 _), _ -> arith2' e1 e2 op_i128 (u64 % Int128.to_uint64)
  | E0 (U128 a), T.E0 (U128 b) -> u128 (op_u128 a b)
  | E0 (I8 _), _ -> arith2' e1 e2 op_i128 (i8 % Int128.to_int8)
  | E0 (I16 _), _ -> arith2' e1 e2 op_i128 (i16 % Int128.to_int16)
  | E0 (I24 _), _ -> arith2' e1 e2 op_i128 (i24 % Int128.to_int24)
  | E0 (I32 _), _ -> arith2' e1 e2 op_i128 (i32 % Int128.to_int32)
  | E0 (I40 _), _ -> arith2' e1 e2 op_i128 (i40 % Int128.to_int40)
  | E0 (I48 _), _ -> arith2' e1 e2 op_i128 (i48 % Int128.to_int48)
  | E0 (I56 _), _ -> arith2' e1 e2 op_i128 (i56 % Int128.to_int56)
  | E0 (I64 _), _ -> arith2' e1 e2 op_i128 (i64 % Int128.to_int64)
  | E0 (I128 _), _ -> arith2' e1 e2 op_i128 (i128 % Int128.to_int128)
  | E0 (Size _), _ -> arith2' e1 e2 op_i128 (size % Int128.to_int)
  | E0 (Address _), _ -> arith2' e1 e2 op_i128 (address % Int128.to_uint64)
  | E0 (Float a), E0 (Float b) -> float (op_float a b)
  | _ -> invalid_arg "arith2"

let comp2 e1 e2 op_gen op_i128 op_u128 =
  match e1, e2 with
  | T.E0 (U8 a), T.E0 (U8 b) ->
      op_i128 (Uint8.to_int128 a) (Uint8.to_int128 b)
  | E0 (U16 a), E0 (U16 b) ->
      op_i128 (Uint16.to_int128 a) (Uint16.to_int128 b)
  | E0 (U24 a), E0 (U24 b) ->
      op_i128 (Uint24.to_int128 a) (Uint24.to_int128 b)
  | E0 (U32 a), E0 (U32 b) ->
      op_i128 (Uint32.to_int128 a) (Uint32.to_int128 b)
  | E0 (U40 a), E0 (U40 b) ->
      op_i128 (Uint40.to_int128 a) (Uint40.to_int128 b)
  | E0 (U48 a), E0 (U48 b) ->
      op_i128 (Uint48.to_int128 a) (Uint48.to_int128 b)
  | E0 (U56 a), E0 (U56 b) ->
      op_i128 (Uint56.to_int128 a) (Uint56.to_int128 b)
  | E0 (U64 a), E0 (U64 b)
  | E0 (Address a), E0 (Address b) ->
      op_i128 (Uint64.to_int128 a) (Uint64.to_int128 b)
  | E0 (U128 a), E0 (U128 b) ->
      op_u128 a b
  | E0 (I8 a), E0 (I8 b) ->
      op_i128 (Int8.to_int128 a) (Int8.to_int128 b)
  | E0 (I16 a), E0 (I16 b) ->
      op_i128 (Int16.to_int128 a) (Int16.to_int128 b)
  | E0 (I24 a), E0 (I24 b) ->
      op_i128 (Int24.to_int128 a) (Int24.to_int128 b)
  | E0 (I32 a), E0 (I32 b) ->
      op_i128 (Int32.to_int128 a) (Int32.to_int128 b)
  | E0 (I40 a), E0 (I40 b) ->
      op_i128 (Int40.to_int128 a) (Int40.to_int128 b)
  | E0 (I48 a), E0 (I48 b) ->
      op_i128 (Int48.to_int128 a) (Int48.to_int128 b)
  | E0 (I56 a), E0 (I56 b) ->
      op_i128 (Int56.to_int128 a) (Int56.to_int128 b)
  | E0 (I64 a), E0 (I64 b) ->
      op_i128 (Int64.to_int128 a) (Int64.to_int128 b)
  | E0 (I128 a), E0 (I128 b) ->
      op_i128 a b
  | E0 (Size a), E0 (Size b) ->
      op_i128 (Int128.of_int a) (Int128.of_int b)
  (* Although every other *constant* E0 allowed by the type checker should
   * work with the generic operators, let's prevent complex bugs to hide in
   * here in the future by explicitly enumerating all valid cases ; better
   * miss an optimisation opportunity than introduce a erroneous
   * optimisation: *)
  | E0 (Float _ | String _ | Bool _ | Char _ |
        Null _ | EndOfList _ | EmptySet _ |
        CopyField | SkipField | SetFieldNull as a),
    E0 (Float _ | String _ | Bool _ | Char _ |
        Null _ | EndOfList _ | EmptySet _ |
        CopyField | SkipField | SetFieldNull as b) ->
      op_gen a b
  | _ ->
      invalid_arg "comp2"

let arith1' e op_i128 cst =
  match e with
  | T.E0 (U128 _) ->
      invalid_arg "arith1'"
  | _ ->
      cst (op_i128 (to_i128 e))

let arith1 e op_float op_i128 op_u128 =
  match e with
  | T.E0 (U8 _) -> arith1' e op_i128 (u8 % Int128.to_uint8)
  | E0 (U16 _) -> arith1' e op_i128 (u16 % Int128.to_uint16)
  | E0 (U24 _) -> arith1' e op_i128 (u24 % Int128.to_uint24)
  | E0 (U32 _) -> arith1' e op_i128 (u32 % Int128.to_uint32)
  | E0 (U40 _) -> arith1' e op_i128 (u40 % Int128.to_uint40)
  | E0 (U48 _) -> arith1' e op_i128 (u48 % Int128.to_uint48)
  | E0 (U56 _) -> arith1' e op_i128 (u56 % Int128.to_uint56)
  | E0 (U64 _) -> arith1' e op_i128 (u64 % Int128.to_uint64)
  | E0 (U128 a) -> u128 (op_u128 a)
  | E0 (I8 _) -> arith1' e op_i128 (i8 % Int128.to_int8)
  | E0 (I16 _) -> arith1' e op_i128 (i16 % Int128.to_int16)
  | E0 (I24 _) -> arith1' e op_i128 (i24 % Int128.to_int24)
  | E0 (I32 _) -> arith1' e op_i128 (i32 % Int128.to_int32)
  | E0 (I40 _) -> arith1' e op_i128 (i40 % Int128.to_int40)
  | E0 (I48 _) -> arith1' e op_i128 (i48 % Int128.to_int48)
  | E0 (I56 _) -> arith1' e op_i128 (i56 % Int128.to_int56)
  | E0 (I64 _) -> arith1' e op_i128 (i64 % Int128.to_int64)
  | E0 (I128 _) -> arith1' e op_i128 (i128 % Int128.to_int128)
  | E0 (Float a) -> float (op_float a)
  | _ -> invalid_arg "arith1"

let known_zero e =
  e = T.E0 (Float 0.) ||
  (try E.to_cst_int e = 0
  with _ -> false)

let known_one e =
  e = T.E0 (Float 1.) ||
  (try E.to_cst_int e = 1
  with _ -> false)

let known_not_zero = function
  | T.E0 (Float n) -> n <> 0.
  | E0 (U8 n) -> n <> Uint8.zero
  | E0 (U16 n) -> n <> Uint16.zero
  | E0 (U24 n) -> n <> Uint24.zero
  | E0 (U32 n) -> n <> Uint32.zero
  | E0 (U40 n) -> n <> Uint40.zero
  | E0 (U48 n) -> n <> Uint48.zero
  | E0 (U56 n) -> n <> Uint56.zero
  | E0 (U64 n) -> n <> Uint64.zero
  | E0 (U128 n) -> n <> Uint128.zero
  | E0 (I8 n) -> n <> Int8.zero
  | E0 (I16 n) -> n <> Int16.zero
  | E0 (I24 n) -> n <> Int24.zero
  | E0 (I32 n) -> n <> Int32.zero
  | E0 (I40 n) -> n <> Int40.zero
  | E0 (I48 n) -> n <> Int48.zero
  | E0 (I56 n) -> n <> Int56.zero
  | E0 (I64 n) -> n <> Int64.zero
  | E0 (I128 n) -> n <> Int128.zero
  | _ -> false

let rec known_positive ~strict =
  let known_int_positive cmp zero n =
    (if strict then (>) else (>=)) (cmp n zero) 0 in
  function
  | T.E1 (Exp, _) -> true
  | E1 (Ceil, e) -> known_positive ~strict e
  | E1 (Floor, e) when not strict -> known_positive ~strict:false e
  | E1 (Abs, e) -> not strict || known_not_zero e
  | E1 ((StringLength | Cardinality | RemSize | Offset), _) -> not strict
  | E0 (Float n) -> (if strict then (>) else (>=)) n 0.
  | E0 (U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ |
        U56 _ | U64 _ | U128 _) as e ->
      not strict || known_not_zero e
  | E0 (I8 n) -> known_int_positive Int8.compare Int8.zero n
  | E0 (I16 n) -> known_int_positive Int16.compare Int16.zero n
  | E0 (I24 n) -> known_int_positive Int24.compare Int24.zero n
  | E0 (I32 n) -> known_int_positive Int32.compare Int32.zero n
  | E0 (I40 n) -> known_int_positive Int40.compare Int40.zero n
  | E0 (I48 n) -> known_int_positive Int48.compare Int48.zero n
  | E0 (I56 n) -> known_int_positive Int56.compare Int56.zero n
  | E0 (I64 n) -> known_int_positive Int64.compare Int64.zero n
  | E0 (I128 n) -> known_int_positive Int128.compare Int128.zero n
  | E2 (Add, e1, e2) ->
      if strict then
        known_positive ~strict:true e1 && known_positive ~strict:false e2 ||
        known_positive ~strict:false e1 && known_positive ~strict:true e2
      else
        known_positive ~strict:false e1 && known_positive ~strict:false e2
  | _ -> false

(* Returns how many time the pair identified by [name] is used with First,
 * Secnd, and in total: *)
let count_pair_uses name e =
  E.fold (0, 0, 0) (fun (fst, snd, tot as prev) -> function
    | T.E1 (GetItem 0, E0 (Identifier n)) when n = name ->
        (* Note: leave tot as is since fold is going to iterate on the
         * Identifier independently *)
        fst + 1, snd, tot
    | T.E1 (GetItem 1, E0 (Identifier n)) when n = name ->
        fst, snd + 1, tot
    | T.E2 (LetPair _, E0 (Identifier n), _) when n = name ->
        (* A LetPair using this pair count both for First and Secnd, but we
         * must also count the identifier twice in [tot] (as a double let
         * would do:): *)
        fst + 1, snd + 1, tot + 1
    | T.E0 (Identifier n) when n = name ->
        fst, snd, tot + 1
    | _ ->
        prev
  ) e

(* Extract the "final" value of an expression, in case it is hidden by
 * let bindings or at the end of a sequence: *)
let rec final_expression = function
  | T.E2 (Let _, _, e) -> final_expression e
  | E2 (LetPair _, _, e) -> final_expression e
  | E0S (Seq, l) when l <> [] -> final_expression (List.last l)
  | e -> e

(* Return [e] with its final expression replaced with [r]: *)
let rec replace_final_expression e e' =
  match e with
  | T.E2 (Let (n, r), def, body) ->
      let body' = replace_final_expression body e' in
      if E.eq body body' then e
      else T.E2 (Let (n, r), def, body')
  | E2 (LetPair (n1, r1, n2, r2), def, body) ->
      let body' = replace_final_expression body e' in
      if E.eq body body' then e
      else T.E2 (LetPair (n1, r1, n2, r2), def, body')
  | E0S (Seq, l) when l <> [] ->
      let l', last = list_split_last l in
      let last' = replace_final_expression last e' in
      if E.eq last last' then e
      else T.E0S (Seq, l' @ [ last' ])
  | _ ->
      e'

(* When combining two [replace_final_expression] calls into a single
 * expression, the names bound on each branch that needed not be unique,
 * must now be unique (one way to create non unique names in disjoint
 * expressions is to inline a value containing a let binding).
 * So new names are generated for every identifier bound within [e]: *)
let rec replace_final_expression_anonymously e e' =
  match e with
  | T.E2 (Let (n, r), def, body) ->
      let n' = E.gen_id n in
      let rep =
        E.map (function
          | T.E0 (Identifier n0) when n0 = n -> T.E0 (Identifier n')
          | e -> e) in
      let body = rep body
      and e' = rep e' in
      let body = replace_final_expression_anonymously body e' in
      T.E2 (Let (n', r), def, body)
  | E2 (LetPair (n1, r1, n2, r2), def, body) ->
      let n1' = E.gen_id n1
      and n2' = E.gen_id n2 in
      let rep =
        E.map (function
          | T.E0 (Identifier n0) when n0 = n1 -> T.E0 (Identifier n1')
          | T.E0 (Identifier n0) when n0 = n2 -> T.E0 (Identifier n2')
          | e -> e) in
      let body = rep body in
      let e' = rep e' in
      let body = replace_final_expression_anonymously body e' in
      T.E2 (LetPair (n1', r1, n2', r2), def, body)
  | E0S (Seq, l) when l <> [] ->
      let l', last = list_split_last l in
      let last' = replace_final_expression_anonymously last e' in
      if E.eq last last' then e
      else T.E0S (Seq, l' @ [ last' ])
  | _ ->
      e'

let rec peval l e =
  let is_small = E.is_smaller_than 200 in
  (* For when some operations are invalid on floats: *)
  let no_floats _ _ = assert false in
  let no_float _ = assert false in
  let p = peval l in
  (* Apply [f] to expression [e] and call once more [p] on the result,
   * unless it's no different from [e], which have gone through [p] already.
   * This is useful when a final expression (already simplified since
   * simplification happens before extraction of the final expression) has
   * to be put back in place with no modification (ie. no need for another
   * simplification pass). If that's a new expression that must be simplified
   * then it cannot be simplified before being replaced in its context (for
   * unbound identifiers) and will probably not be simplified by this
   * function so it has to be simplified explicitly after replacement. *)
  let repl f e =
    let e' = f e in
    if E.eq e e' then e else p e' in
  match e with
  | T.E0 _ ->
      e
  | E0S (op, es) ->
      (match op, List.map p es with
      | Seq, es ->
          let rec loop es = function
            | [] ->
                List.rev es
            | e :: [] ->
                (* Last item might not be void: *)
                let es =
                  if T.eq_mn (E.type_of l e) T.void &&
                     is_small e &&
                     not (E.for_any E.has_side_effect e)
                  then es else e :: es in
                loop es []
            | e :: rest ->
                (* Non last items have type Void if they type check: *)
                let es =
                  if is_small e &&
                     not (E.for_any E.has_side_effect e)
                  then es else e :: es in
                loop es rest in
          (match loop [] es with
          | [ e ] -> e
          | es -> seq es )
      (* If we build a tuple out of the very fields of another tuple, just
       * use that one. This can actually happen when manipulating pairs. *)
      | MakeTup, es ->
          let def = T.E0S (MakeTup, es) in
          (match
            List.fold_left (fun (n, src_tup, side_fx) e ->
              let f = final_expression e in
              match f with
              | E1 (GetItem n', src_tup')
                when n' = n &&
                     (src_tup = None || E.eq (Option.get src_tup) src_tup') ->
                  n + 1,
                  src_tup,
                  replace_final_expression_anonymously e side_fx
              | _ ->
                  raise Exit
            ) (0, None, nop) es with
          | exception Exit ->
              def
          | _, Some src_tup, side_fx ->
              seq [ side_fx ; src_tup ] |> p
          | _ ->
              def)
      | op, es -> E0S (op, es))
  | E0R (op, es) ->
      E0R (op, Array.map p es)
  | E1 (Function ts, body) ->
      let l = E.enter_function ~ts l in
      E1 (Function ts, peval l body)
  | E1 (Convert _, _) ->
      assert false (* Because of type_checking *)
  | E1 (op, e1) ->
      let e1 = p e1 in
      let repl1 = repl (replace_final_expression e1) in
      (match op, final_expression e1 with
      | Comment _, _ ->
          e1 (* FIXME: Would prevent further optimization *)
      | GetItem n, E0S (MakeTup, es) when n < List.length es ->
          let got = List.at es n in
          let es =
            List.fold_lefti (fun es i e ->
              if i = n then es else ignore_ e :: es
            ) [] es in
          seq (List.rev (got :: es)) |>
          replace_final_expression e1 |>
          p
      | GetField n, E0S (MakeRec, es) ->
          let rec loop got es = function
            | [] -> got, es
            | [ _ ] -> raise Exit
            | T.E0 (String n') :: v :: rest when n = n' ->
                if got = None then
                  loop (Some v) es rest
                else
                  failwith "multiple binding"
            | _ :: v :: rest ->
                loop got (ignore_ v :: es) rest
            in
          let def = T.E1 (op, e1) in
          (match loop None [] es with
          | exception _ -> def
          | None, _ -> def
          | Some got, es ->
              seq (List.rev (got :: es)) |>
              replace_final_expression e1 |> p)
      | GetAlt n, E1 (Construct (mns, i), e)
        when i < Array.length mns && fst mns.(i) = n ->
          repl1 e
      | Ignore, f1 ->
          if T.eq_mn (E.type_of l e1) T.void then repl1 f1
          (* In theory any expression of type Void and with no side effect should
           * be disposed of. Ignore is the only way to build such an expression,
           * though, so it is enough to test this case only: *)
          else if not (E.for_any E.has_side_effect e1) then nop
          else E1 (op, e1)
      | IsNull, E0 (Null _) -> repl1 true_
      | IsNull, E1 (NotNull, _) -> repl1 false_
      | IsNull, _ ->
          if not (E.type_of l e1).T.nullable then false_
          else T.E1 (IsNull, e1)
      | NotNull, E1 (Force _, e) -> repl1 e
      | NotNull, _ ->
          if (E.type_of l e1).T.nullable then e1
          else T.E1 (NotNull, e1)
      | Force _, E1 (NotNull, e) -> repl1 e
      | Force _, E2 (Div, e1, e2) -> T.E2 (UnsafeDiv, e1, e2) |> repl1
      | Force _, E2 (Rem, e1, e2) -> T.E2 (UnsafeRem, e1, e2) |> repl1
      | Force _, E2 (Pow, e1, e2) -> T.E2 (UnsafePow, e1, e2) |> repl1
      | Force m, _ ->
          if not (E.type_of l e1).T.nullable then e1
          else T.E1 (Force m, e1)
      | StringOfInt, E0 (U8 n) ->
          string (Uint8.to_string n) |> repl1
      | StringOfInt, E0 (U16 n) ->
          string (Uint16.to_string n) |> repl1
      | StringOfInt, E0 (U24 n) ->
          string (Uint24.to_string n) |> repl1
      | StringOfInt, E0 (U32 n) ->
          string (Uint32.to_string n) |> repl1
      | StringOfInt, E0 (U40 n) ->
          string (Uint40.to_string n) |> repl1
      | StringOfInt, E0 (U48 n) ->
          string (Uint48.to_string n) |> repl1
      | StringOfInt, E0 (U56 n) ->
          string (Uint56.to_string n) |> repl1
      | StringOfInt, E0 (U64 n) ->
          string (Uint64.to_string n) |> repl1
      | StringOfInt, E0 (U128 n) ->
          string (Uint128.to_string n) |> repl1
      | StringOfInt, E0 (I8 n) ->
          string (Int8.to_string n) |> repl1
      | StringOfInt, E0 (I16 n) ->
          string (Int16.to_string n) |> repl1
      | StringOfInt, E0 (I24 n) ->
          string (Int24.to_string n) |> repl1
      | StringOfInt, E0 (I32 n) ->
          string (Int32.to_string n) |> repl1
      | StringOfInt, E0 (I40 n) ->
          string (Int40.to_string n) |> repl1
      | StringOfInt, E0 (I48 n) ->
          string (Int48.to_string n) |> repl1
      | StringOfInt, E0 (I56 n) ->
          string (Int56.to_string n) |> repl1
      | StringOfInt, E0 (I64 n) ->
          string (Int64.to_string n) |> repl1
      | StringOfInt, E0 (I128 n) ->
          string (Int128.to_string n) |> repl1
      | StringOfFloat, E0 (Float f) ->
          string (FloatTools.hexstring_of_float f) |> repl1
      | DecimalStringOfFloat, E0 (Float f) ->
          string (FloatTools.string_of_float f) |> repl1
      | StringOfIp, E0 (U32 n) ->
          string (DessserIpTools.V4.to_string n) |> repl1
      | StringOfIp, E0 (U128 n) ->
          string (DessserIpTools.V6.to_string n) |> repl1
      | StringOfChar, E0 (Char c) ->
          string (String.of_char c) |> repl1
      | FloatOfString, E0 (String s) ->
          or_null_ TFloat float float_of_string s |> repl1
      | U8OfString, E0 (String s) ->
          or_null_ TU8 u8 Uint8.of_string s |> repl1
      | U16OfString, E0 (String s) ->
          or_null_ TU16 u16 Uint16.of_string s |> repl1
      | U24OfString, E0 (String s) ->
          or_null_ TU24 u24 Uint24.of_string s |> repl1
      | U32OfString, E0 (String s) ->
          or_null_ TU32 u32 Uint32.of_string s |> repl1
      | U40OfString, E0 (String s) ->
          or_null_ TU40 u40 Uint40.of_string s |> repl1
      | U48OfString, E0 (String s) ->
          or_null_ TU48 u48 Uint48.of_string s |> repl1
      | U56OfString, E0 (String s) ->
          or_null_ TU56 u56 Uint56.of_string s |> repl1
      | U64OfString, E0 (String s) ->
          or_null_ TU64 u64 Uint64.of_string s |> repl1
      | U128OfString, E0 (String s) ->
          or_null_ TU128 u128 Uint128.of_string s |> repl1
      | I8OfString, E0 (String s) ->
          or_null_ TI8 i8 Int8.of_string s |> repl1
      | I16OfString, E0 (String s) ->
          or_null_ TI16 i16 Int16.of_string s |> repl1
      | I24OfString, E0 (String s) ->
          or_null_ TI24 i24 Int24.of_string s |> repl1
      | I32OfString, E0 (String s) ->
          or_null_ TI32 i32 Int32.of_string s |> repl1
      | I40OfString, E0 (String s) ->
          or_null_ TI40 i40 Int40.of_string s |> repl1
      | I48OfString, E0 (String s) ->
          or_null_ TI48 i48 Int48.of_string s |> repl1
      | I56OfString, E0 (String s) ->
          or_null_ TI56 i56 Int56.of_string s |> repl1
      | I64OfString, E0 (String s) ->
          or_null_ TI64 i64 Int64.of_string s |> repl1
      | I128OfString, E0 (String s) ->
          or_null_ TI128 i128 Int128.of_string s |> repl1
      | BoolOfU8, E0 (U8 n) ->
          bool (Uint8.compare Uint8.zero n <> 0) |> repl1
      | BoolOfU8, E1 (U8OfBool, e) ->
          repl1 e
      | U8OfChar, E0 (Char c) ->
          u8 (Uint8.of_int (Char.code c)) |> repl1
      | U8OfChar, E1 (CharOfU8, e) ->
          repl1 e
      | U8OfBool, E0 (Bool false) ->
          u8 (Uint8.of_int 0) |> repl1
      | U8OfBool, E0 (Bool true) ->
          u8 (Uint8.of_int 1) |> repl1
      | U8OfBool, E1 (BoolOfU8, e) ->
          repl1 e
      | CharOfU8, E0 (U8 n) ->
          char (Char.chr (Uint8.to_int n)) |> repl1
      | CharOfU8, E1 (U8OfChar, e) ->
          repl1 e
      | U32OfSize, E0 (Size n) ->
          u32 (Uint32.of_int n) |> repl1
      | U32OfSize, E1 (SizeOfU32, e) ->
          repl1 e
      | SizeOfU32, E0 (U32 n) ->
          size (Uint32.to_int n) |> repl1
      | SizeOfU32, E1 (U32OfSize, e) ->
          repl1 e
      | U64OfAddress, E0 (Address n) ->
          u64 n |> repl1
      | U64OfAddress, E1 (AddressOfU64, e) ->
          repl1 e
      | AddressOfU64, E0 (U64 n) ->
          address n |> repl1
      | AddressOfU64, E1 (U64OfAddress, e) ->
          repl1 e
      | Head, E2 (Cons, e, _) ->
          repl1 e
      (* | Tail, E0 (EndOfList _) -> TODO: return Null *)
      | Tail, E2 (Cons, _, e) ->
          repl1 e |> p
      | MaskGet _, (E0 (CopyField | SkipField | SetFieldNull) as m) ->
          repl1 m
      | LabelOf, E1 (Construct (_, i), _) ->
          u16 (Uint16.of_int i) |> repl1
      | FloatOfU64, E0 (U64 n) ->
          float (BatInt64.float_of_bits (Uint64.to_int64 n)) |> repl1
      | FloatOfU64, E1 (U64OfFloat, e) ->
          repl1 e
      | U64OfFloat, E0 (Float f) ->
          u64 (Uint64.of_int64 (BatInt64.bits_of_float f)) |> repl1
      | U64OfFloat, E1 (FloatOfU64, e) ->
          repl1 e
      | Not, E0 (Bool b) ->
          bool (not b) |> repl1
      | Not, E1 (Not, e) ->
          repl1 e
      (* Shorten cascades of converters: *)
      | ToI8, E1 ((ToI8 | ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                   ToI64 | ToI128), e) ->
          T.E1 (ToI8, e) |> repl1
      | ToI16, E1 ((ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToI16, e) |> repl1
      | ToI24, E1 ((ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToI24, e) |> repl1
      | ToI32, E1 ((ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToI32, e) |> repl1
      | ToI40, E1 ((ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToI40, e) |> repl1
      | ToI48, E1 ((ToI48 | ToI56 | ToI64 | ToI128), e) ->
          T.E1 (ToI48, e) |> repl1
      | ToI56, E1 ((ToI56 | ToI64 | ToI128), e) ->
          T.E1 (ToI56, e) |> repl1
      | ToI64, E1 ((ToI64 | ToI128), e) ->
          T.E1 (ToI64, e) |> repl1
      | ToI128, E1 (ToI128, e) ->
          T.E1 (ToI128, e) |> repl1
      | ToU8, E1 ((ToU8 | ToU16 | ToU24 | ToU32 | ToU40 | ToU48 | ToU56 |
                   ToU64 | ToU128 |
                   ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                   ToI64 | ToI128), e) ->
          T.E1 (ToU8, e) |> repl1
      | ToU16, E1 ((ToU16 | ToU24 | ToU32 | ToU40 | ToU48 | ToU56 |
                    ToU64 | ToU128 |
                    ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToU16, e) |> repl1
      | ToU24, E1 ((ToU24 | ToU32 | ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToU24, e) |> repl1
      | ToU32, E1 ((ToU32 | ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToU32, e) |> repl1
      | ToU40, E1 ((ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI48 | ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToU40, e) |> repl1
      | ToU48, E1 ((ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI56 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToU48, e) |> repl1
      | ToU56, E1 ((ToU56 | ToU64 | ToU128 |
                    ToI64 | ToI128), e) ->
          T.E1 (ToU56, e) |> repl1
      | ToU64, E1 ((ToU64 | ToU128 | ToI128), e) ->
          T.E1 (ToU64, e) |> repl1
      | ToU128, E1 (ToU128, e) ->
          T.E1 (ToU128, e) |> repl1
      | ToFloat, E1 (ToFloat, e) ->
          T.E1 (ToFloat, e) |> repl1
      (* Evaluate conversions *)
      | ToI8, e ->
          to_int ToI8 e i8 Int8.of_int128 |> repl1
      | ToI16, e ->
          to_int ToI16 e i16 Int16.of_int128 |> repl1
      | ToI24, e ->
          to_int ToI24 e i24 Int24.of_int128 |> repl1
      | ToI32, e ->
          to_int ToI32 e i32 Int32.of_int128 |> repl1
      | ToI40, e ->
          to_int ToI40 e i40 Int40.of_int128 |> repl1
      | ToI48, e ->
          to_int ToI48 e i48 Int48.of_int128 |> repl1
      | ToI56, e ->
          to_int ToI56 e i56 Int56.of_int128 |> repl1
      | ToI64, e ->
          to_int ToI64 e i64 Int64.of_int128 |> repl1
      | ToI128, e ->
          to_int ToI128 e i128 Int128.of_int128 |> repl1
      | ToU8, e ->
          to_uint ToU8 e u8 Uint8.of_uint128 |> repl1
      | ToU16, e ->
          to_uint ToU16 e u16 Uint16.of_uint128 |> repl1
      | ToU24, e ->
          to_uint ToU24 e u24 Uint24.of_uint128 |> repl1
      | ToU32, e ->
          to_uint ToU32 e u32 Uint32.of_uint128 |> repl1
      | ToU40, e ->
          to_uint ToU40 e u40 Uint40.of_uint128 |> repl1
      | ToU48, e ->
          to_uint ToU48 e u48 Uint48.of_uint128 |> repl1
      | ToU56, e ->
          to_uint ToU56 e u56 Uint56.of_uint128 |> repl1
      | ToU64, e ->
          to_uint ToU64 e u64 Uint64.of_uint128 |> repl1
      | ToU128, e ->
          to_uint ToU128 e u128 Uint128.of_uint128 |> repl1
      | ToFloat, e ->
          peval_to_float e |> repl1
      | Abs, f1 ->
          (try arith1 f1 abs_float Int128.abs Uint128.abs |> repl1
          with Invalid_argument _ ->
          T.E1 (Abs, e1))
      | Neg, f1 ->
          (try arith1 f1 (~-.) Int128.neg Uint128.neg |> repl1
          with Invalid_argument _ ->
          T.E1 (Neg, e1))
      | StringOfBytes, E0 (Bytes v) ->
          string (Bytes.to_string v) |> repl1
      | StringOfBytes, E1 (BytesOfString, e) ->
          repl1 e
      | BytesOfString, E0 (String s) ->
          bytes (Bytes.of_string s) |> repl1
      | BytesOfString, E1 (StringOfBytes, e) ->
          repl1 e
      | Exp, E0 (Float n) ->
          float (exp n) |> repl1
      | Log, E0 (Float n) ->
          nullable_of_nan (log n) |> repl1
      | Log, e when known_positive ~strict:true e ->
          not_null (E1 (UnsafeLog, e)) |> repl1
      | UnsafeLog, E0 (Float n) ->
          float (log n) |> repl1
      | Log10, E0 (Float n) ->
          nullable_of_nan (log10 n) |> repl1
      | Log10, e when known_positive ~strict:true e ->
          not_null (E1 (UnsafeLog10, e)) |> repl1
      | UnsafeLog10, E0 (Float n) ->
          float (log10 n) |> repl1
      | Sqrt, E0 (Float n) ->
          nullable_of_nan (sqrt n) |> repl1
      | Sqrt, e when known_positive ~strict:false e ->
          not_null (E1 (UnsafeSqrt, e)) |> repl1
      | UnsafeSqrt, E0 (Float n) ->
          float (sqrt n) |> repl1
      | Ceil, E0 (Float n) ->
          float (ceil n) |> repl1
      | Floor, E0 (Float n) ->
          float (floor n) |> repl1
      | Round, E0 (Float n) ->
          float (Float.round n) |> repl1
      | Cos, E0 (Float n) ->
          float (cos n) |> repl1
      | Sin, E0 (Float n) ->
          float (sin n) |> repl1
      | Tan, E0 (Float n) ->
          nullable_of_nan (tan n) |> repl1
      | ACos, E0 (Float n) ->
          nullable_of_nan (acos n) |> repl1
      | ASin, E0 (Float n) ->
          nullable_of_nan (asin n) |> repl1
      | ATan, E0 (Float n) ->
          float (atan n) |> repl1
      | CosH, E0 (Float n) ->
          float (cosh n) |> repl1
      | SinH, E0 (Float n) ->
          float (sinh n) |> repl1
      | TanH, E0 (Float n) ->
          float (tanh n) |> repl1
      | Lower, E0 (String s) ->
          string (String.lowercase_ascii s) |> repl1
      | Upper, E0 (String s) ->
          string (String.uppercase_ascii s) |> repl1
      | StringLength, E0 (String s) ->
          u32_of_int (String.length s) |> repl1
      | StringLength, E1 (StringOfChar, e)
        when not (E.for_any E.has_side_effect e) ->
          u32_of_int 1 |> repl1
      | Cardinality, E0R ((MakeVec | MakeArr _), es) ->
          u32_of_int (Array.length es) |> repl1
      | Cardinality, E1 (AllocVec d, _) ->
          u32_of_int d
      | Cardinality, E0 (EndOfList _ | EmptySet _)
      | Cardinality, E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _ |
                          HashTable _ | Heap), _)
      | Cardinality, E3 (Top _, _, _, _) ->
          u32_of_int 0 |> repl1
      | Assert, E0 (Bool true) ->
          repl1 nop
      | BitNot, f1 ->
          (try arith1 f1 no_float Int128.lognot Uint128.lognot |> repl1
          with Invalid_argument _ -> T.E1 (BitNot, e1))
      | AllocVec 1, _ ->
          make_vec [ e1 ] |> p
      | op, _ ->
          T.E1 (op, e1))
  | E1S (Apply, e1, es) ->
      let e1 = p e1 in
      let es = List.map p es in
      (match final_expression e1, es with
      | E1 (Function _, body), []
        when not (E.is_recursive body) ->
          replace_final_expression e1 body |> p
      (* If [f] is constant we cannot proceed directly with variable
       * substitutions unless each variable is used only once. We can
       * turn the apply into a sequence of lets that will further
       * substitute what can be substituted: *)
      | E1 (Function _, body), es
        when not (E.is_recursive body) ->
          List.fold_lefti (fun body i e ->
            let_ e (fun e ->
              E.map ~enter_functions:false (function
                | E0 (Param i') when i' = i -> e
                | x -> x
              ) body)
          ) body es |>
          replace_final_expression e1 |>
          (* There is no more params left so no need to update the environment *)
          p
      | _, es ->
          (* In case we have no constant function (an identifier then) there is
           * nothing to simplify in the body: *)
          E1S (Apply, e1, es))
(*  Unused for now since the only E1S is Apply:
  | E1S (op, e1, es) ->
      (match op, p e1, List.map p es with
      | op, e1, es -> E1S (op, e1, es)) *)
  (*
   * Let expressions
   *)
  | E2 (Let (name, value_r), value, body) ->
      let value = p value in
      let value_t = E.get_memo_mn value_r l value in
      let l' = E.add_local name value_t l in
      let body = peval l' body in
      let def = T.E2 (Let (name, value_r), value, body) in
      (* The identifier is useless in that case: *)
      if body = T.E0 (Identifier name) then value else
      (* Also avoid introducing aliases: *)
      (match value with
      | T.E0 (Identifier alias) ->
          E.map (function
            | T.E0 (Identifier n) when n = name -> T.E0 (Identifier alias)
            | e -> e
          ) body |> p
      | _ ->
          (* If the identifier is used only once in the body, then the optimizer will
           * also prefer to have no let. This will further allow, for instance, to
           * simplify:
           *   (nth 0
           *     (let (arr (make-vec 0))
           *       (set-vec 0 arr 1)))
           * into:
           *   (nth 0
           *     (set-vec 0 (make-vec 0) 1))
           * then ultimately into:
           *   1
           *
           * This must not be done when the value has side effect or when the
           * body has and the value is sensible to side effects.
           *)
          let use_count =
            (* TODO: early exit *)
            E.fold 0 (fun c -> function
              | E0 (Identifier n) when n = name -> c + 1
              | _ -> c
            ) body in
          let value_has_effects = E.for_any E.has_side_effect value
          and body_has_effects = E.for_any E.has_side_effect body in
          if use_count = 0 then
            if not value_has_effects then body
            else p (seq [ ignore_ value ; body ])
          else if value_has_effects && E.for_any E.depends_on_side_effect body ||
                  body_has_effects && E.for_any E.depends_on_side_effect value then
            def
          else if use_count = 1 ||
                  is_small value && E.for_all E.can_duplicate value &&
                  (use_count - 1) * E.size value <= max_inline_size ()
               then
            E.map (function
              | E0 (Identifier n) when n = name -> value
              | e -> e
            ) body |> p
          (* If the let binds a pair, and this binding appears only in Fst or Snd
           * expression, then use a LetPair instead and save the intermediary
           * bindings: *)
          else (match value_t with
          (* FIXME: would work as well if Tup had any number of items and was only
           * ever used with GetItem. *)
          | T.{ typ = TTup [| mn1 ; mn2 |] ; nullable = false } ->
              (* Also count how many times the identifier is used to find out if
               * the identifier is used outside of fst/snd: *)
              let fst_count, snd_count, tot_count = count_pair_uses name body in
              if tot_count > fst_count + snd_count then def else
              let n1 = "fst_"^ name and n2 = "snd_"^ name in
              let body =
                E.map (function
                  | E1 (GetItem 0, E0 (Identifier n)) when n = name ->
                      E0 (Identifier n1)
                  | E1 (GetItem 1, E0 (Identifier n)) when n = name ->
                      E0 (Identifier n2)
                  | E2 (LetPair _ as letpair, E0 (Identifier n), body)
                    when n = name ->
                      (* This will be further simplified: *)
                      let tup = T.[ E0 (Identifier n1) ; E0 (Identifier n2) ] in
                      T.E2 (letpair, E0S (MakeTup, tup), body)
                  | e -> e
                ) body in
              T.E2 (LetPair (n1, ref (Some mn1), n2, ref (Some mn2)),
                            value, body) |> p
          | _ ->
              def))
  | E2 (LetPair (n1, r1, n2, r2), value, body) ->
      let value = p value in
      let mn1 = E.get_memo_mn r1 l (first value)
      and mn2 = E.get_memo_mn r2 l (secnd value) in
      let l = E.add_local n1 mn1 l |>
              E.add_local n2 mn2 in
      let body = peval l body in
      let def = T.E2 (LetPair (n1, r1, n2, r2), value, body) in
      let fst_count, snd_count =
        E.fold (0, 0) (fun (c1, c2 as prev) -> function
          | T.E0 (Identifier n) ->
              let c1 = if n = n1 then c1 + 1 else c1 in
              let c2 = if n = n2 then c2 + 1 else c2 in
              c1, c2
          | _ ->
              prev
        ) body in
      if fst_count = 0 then
        if snd_count = 0 then body
        else T.E2 (Let (n2, ref (Some mn2)), secnd value, body) |> p
      else if snd_count = 0 then
        T.E2 (Let (n1, ref (Some mn1)), first value, body) |> p
      else (match value with
        | T.E0S (MakeTup, [ e1 ; e2 ]) ->
            T.E2 (Let (n1, ref (Some mn1)), e1,
              E2 (Let (n2, ref (Some mn2)), e2, body)) |> p
        | _ ->
            (* The value could still result in a MakeTup after some
             * intermediary let/let-pair definitions. In that case, if the
             * identifiers n1 and n2 are used only a few times then it is
             * beneficial to inline the MakeTup arguments into the body,
             * despite we cannot split the pair as easily as above. *)
            match final_expression value with
            | T.E0S (MakeTup, [ e1 ; e2 ]) as final_make_pair ->
                (* So this MakeTup would be replaced with the body, where
                 * each occurrences of n1/n2 have been replaced by the full
                 * e1/e2: *)
                let body_swap =
                  E.map (function
                    | T.E0 (Identifier n) when n = n1 -> e1
                    | T.E0 (Identifier n) when n = n2 -> e2
                    | e -> e
                  ) body in
                (* The new expression would then be [value], where that final
                 * MakeTup is replaced by that new body, and the initial
                 * let-pair is gone: *)
                let new_e =
                  E.map (fun e ->
                    if e == final_make_pair then body_swap else e
                  ) value |>
                  p in
                (* Is that worth it? *)
                let def_sz = E.size def
                and new_sz = E.size new_e in
                if new_sz <= def_sz + max_inline_size () then
                  new_e
                else
                  def
            | _ ->
                (* never mind then *)
                def)
  | E2 (ForEach (name, item_r), lst, body) ->
      let lst = p lst in
      let item_t = E.get_memo_item_mn item_r l lst in
      let l' = E.add_local name item_t l in
      let body = peval l' body in
      (match final_expression lst with
      (* Loop over empty lst: that's a nop,  but for the side effect of lst: *)
      | E0R ((MakeVec | MakeArr _), [||])
      | E1 (AllocVec 0, _)
      | E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _ |
             HashTable _ | Heap), _)
      | E3 (Top _, _, _, _) ->
          replace_final_expression_anonymously lst nop
      (* Loop over a single item: no need for the loop *)
      | E0R ((MakeVec | MakeArr _), [| item |])
      | E1 (AllocVec 1, item) ->
          E2 (Let (name, item_r), replace_final_expression lst item, body)
      | _ ->
          T.E2 (ForEach (name, item_r), lst, body)
      (* TODO: ForEach of AllocVec/AllocArr not building the vector/array *))
  | E2 (NullMap _, _, _) ->
      assert false (* Because of type_checking *)
  | E2 (op, e1, e2) ->
      let e1 = p e1
      and e2 = p e2 in
      let repl1 = repl (replace_final_expression e1)
      and repl2 = repl (replace_final_expression e2) in
      let keep1 () = repl (replace_final_expression_anonymously e2) e1
      and keep2 () = repl (replace_final_expression_anonymously e1) e2 in
      (* Keep the side effects that may hide in [repl1], and use [p] to
       * simplify. Beware that this could create shadowing so use the
       * special `replace_final_expression_anonymously`. *)
      let repl12 =
        repl (replace_final_expression_anonymously e1 %
              replace_final_expression_anonymously e2) in
      (match op, final_expression e1, final_expression e2 with
      | Nth, f1, E0R ((MakeVec | MakeArr _), es) ->
          let def = T.E2 (Nth, e1, e2) in
          (match E.to_cst_int f1 with
          | exception _ ->
              def
          | idx when idx < Array.length es ->
              es.(idx) |>
              (* Identifiers may be needed in [es], ie [e2] *)
              replace_final_expression e2 |>
              replace_final_expression_anonymously e1 |>
              not_null |>
              p
          | _ ->
              def)
      | Nth, _, E1 (AllocVec d, init) ->
          if_ (lt (to_u32 e1) (u32_of_int d))
            (* Avoid building the array unless e1 is bogus: *)
            ~then_:(replace_final_expression e2 init |> not_null)
            ~else_:(
              (* This needs to crash at runtime, after having performed the
               * side effects in e1 and e2. Also, it must have the same type
               * as init and to not loop forever in peval: *)
              seq [ replace_final_expression_anonymously e2 false_ |>
                    replace_final_expression_anonymously e1 |>
                    assert_ ;
                    init ]) |>
          p (* Will simplify further if e1 is known *)
      | Nth, idx, E0 (String s) ->
          let def = T.E2 (Nth, e1, e2) in
          if String.length s = 0 then null TChar |> repl12
          else (match E.to_cst_int idx with
          | exception _ -> def
          | idx when idx < String.length s -> not_null (char s.[idx]) |> repl12
          | _ -> def)
      | Nth, idx, E0 (Bytes s) ->
          let def = T.E2 (Nth, e1, e2) in
          if Bytes.length s = 0 then null TU8 |> repl12
          else (match E.to_cst_int idx with
          | exception _ -> def
          | idx when idx < Bytes.length s ->
              not_null (u8_of_const_char (Bytes.get s idx)) |> repl12
          | _ -> def)
      (* Peel away some common wrappers: *)
      | Eq, E1 (NotNull, f1), E1 (NotNull, f2)
      | Eq, E1 (Force _, f1), E1 (Force _, f2) ->
          eq (repl1 f1) (repl2 f2) |> p
      (* Another easy case of practical importance: comparison of a null with
       * anything that's NotNull: *)
      | Eq, E0 (Null _), E1 (NotNull, _)
      | Eq, E1 (NotNull, _), E0 (Null _) ->
          repl12 false_
      | Gt, f1, f2 ->
          (try
            comp2 f1 f2 (fun a b -> bool (a > b))
                        (fun a b -> bool (Int128.compare a b > 0))
                        (fun a b -> bool (Uint128.compare a b > 0))
          with Invalid_argument _ ->
            T.E2 (Gt, e1, e2))
      | Ge, f1, f2 ->
          (try
            comp2 f1 f2 (fun a b -> bool (a >= b))
                        (fun a b -> bool (Int128.compare a b >= 0))
                        (fun a b -> bool (Uint128.compare a b >= 0))
          with Invalid_argument _ ->
            T.E2 (Ge, e1, e2))
      | Eq, f1, f2 ->
          (try
            comp2 f1 f2 (fun a b -> bool (a = b))
                        (fun a b -> bool (Int128.compare a b = 0))
                        (fun a b -> bool (Uint128.compare a b = 0))
          with Invalid_argument _ ->
            T.E2 (Eq, e1, e2))
      | Add, f1, _ when known_zero f1 -> keep2 ()
      | (Add | Sub), _, f2 when known_zero f2 -> keep1 ()
      | Sub, f1, f2 when known_zero f1 ->
          replace_final_expression e2 (neg f2) |>
          replace_final_expression_anonymously e1 |>
          p
      | Mul, f1, _ when known_one f1 -> keep2 ()
      | Mul, _, f2 when known_one f2 -> keep1 ()
      | Mul, f1, _ when known_zero f1 -> keep1 ()
      | Mul, _, f2 when known_zero f2 -> keep2 ()
      | Add, f1, E2 (op, e2_1, e2_2) ->
          let def = T.E2 (Add, e1, e2) in
          (match op, final_expression e2_1, final_expression e2_2 with
          | Add, f2_0, f2_1 ->
              (try T.E2 (Add, arith2 f1 f2_0 (+.) Int128.add Uint128.add, e2_2) |>
                   replace_final_expression_anonymously e2_1 |>
                   repl12
              with Invalid_argument _ ->
                (try T.E2 (Add, e2_1, arith2 f1 f2_1 (+.) Int128.add Uint128.add) |>
                     replace_final_expression_anonymously e2_2 |>
                     repl12
                with Invalid_argument _ ->
                  def))
            | _ -> (* TODO: other operators that we can combine with Add *)
                def)
      | Add, f1, f2 ->
          (try arith2 f1 f2 (+.) Int128.add Uint128.add |> repl12
          with Invalid_argument _ -> T.E2 (Add, e1, e2))
      | Sub, f1, f2 ->
          (try arith2 f1 f2 (-.) Int128.sub Uint128.sub |> repl12
          with Invalid_argument _ -> T.E2 (Sub, e1, e2))
      | Mul, f1, f2 ->
          (try arith2 f1 f2 ( *.) Int128.mul Uint128.mul |> repl12
          with Invalid_argument _ -> T.E2 (Mul, e1, e2))
      | (Div | Rem as op), E0 (Float a), E0 (Float b) ->
          (try
            let v = if op = Div then a /. b else Stdlib.Float.rem a b in
            nullable_of_nan v
          with Division_by_zero -> null TFloat) |> repl12
      | (Div | Rem as op), f1, f2 ->
          (match arith2 f1 f2
                        (if op = Div then (/.) else (mod_float))
                        (if op = Div then Int128.div else Int128.rem)
                        (if op = Div then Uint128.div else Uint128.rem) with
          | exception Invalid_argument _ ->
              T.E2 (op, e1, e2)
          | exception Division_by_zero ->
              (* Tried to compute, but could not: *)
              null (E.type_of l e1).T.typ |> repl12
          | e ->
              (* Did replace by the result, make it nullable: *)
              not_null e |> repl12)
      | (UnsafeDiv | UnsafeRem as op), E0 (Float a), E0 (Float b) ->
          let def = T.E2 (op, e1, e2) in
          (try
            let v = if op = Div then a /. b else Stdlib.Float.rem a b in
            if is_nan v then def else repl12 (float v)
          with Division_by_zero -> def)
      | (UnsafeDiv | UnsafeRem as op), f1, f2 ->
          (try arith2 f1 f2
                      (if op = Div then (/.) else (mod_float))
                      (if op = Div then Int128.div else Int128.rem)
                      (if op = Div then Uint128.div else Uint128.rem) |> repl12
          with Invalid_argument _ | Division_by_zero -> T.E2 (op, e1, e2))
      | Pow, E0 (Float a), E0 (Float b) ->
          nullable_of_nan (a ** b) |> repl12
      | Pow, E0 (I32 a), E0 (I32 b) ->
          (try not_null (i32 (BatInt32.pow a b))
          with Invalid_argument _ -> null TI32) |> repl12
      | Pow, E0 (I64 a), E0 (I64 b) ->
          (try not_null (i64 (BatInt64.pow a b))
          with Invalid_argument _ -> null TI64) |> repl12
      | Pow, f1, f2 ->
          (match float_of_num f1, float_of_num f2 with
          | exception _ ->
              E2 (Pow, e1, e2)
          | a, b ->
              let from = T.TFloat in
              let to_ = T.(E.type_of l e1).typ in
              (try not_null (fst (C.conv ~from ~to_ (float (a ** b))))
              with _ -> null to_ |> repl12))
      | UnsafePow, E0 (Float a), E0 (Float b) ->
          let v = a ** b in
          if is_nan v then T.E2 (UnsafePow, e1, e2) else repl12 (float v)
      | UnsafePow, E0 (I32 a), E0 (I32 b) ->
          (try i32 (BatInt32.pow a b) |> repl12
          with Invalid_argument _ -> E2 (UnsafePow, e1, e2))
      | UnsafePow, E0 (I64 a), E0 (I64 b) ->
          (try i64 (BatInt64.pow a b) |> repl12
          with Invalid_argument _ -> E2 (UnsafePow, e1, e2))
      | UnsafePow, f1, f2 ->
          let def = T.E2 (UnsafePow, e1, e2) in
          (match float_of_num f1, float_of_num f2 with
          | exception _ ->
              def
          | a, b ->
              let from = T.TFloat in
              let to_ = T.(E.type_of l e1).typ in
              (try fst (C.conv ~from ~to_ (float (a ** b))) |> repl12
              with _ -> def))
      | PtrAdd, E2 (PtrAdd, e1_1, e1_2), _ ->
          T.E2 (PtrAdd, e1_1, T.E2 (Add, e1_2, e2)) |>
          repl (replace_final_expression_anonymously e1)
      | PtrAdd, _, E0 (Size 0) ->
          replace_final_expression_anonymously e2 e1
      | Rewind, E2 (Rewind, e1_1, e1_2), _ ->
          T.E2 (Rewind, e1_1, T.E2 (Add, e1_2, e2)) |>
          repl (replace_final_expression_anonymously e1)
      | Rewind, _, E0 (Size 0) ->
          replace_final_expression_anonymously e2 e1
      | BitAnd, f1, f2 ->
          (try arith2 f1 f2 no_floats Int128.logand Uint128.logand |> repl12
          with Invalid_argument _ -> T.E2 (BitAnd, e1, e2))
      | BitOr, f1, f2 ->
          (try arith2 f1 f2 no_floats Int128.logor Uint128.logor |> repl12
          with Invalid_argument _ -> T.E2 (BitOr, e1, e2))
      | BitXor, f1, f2 ->
          (try arith2 f1 f2 no_floats Int128.logxor Uint128.logxor |> repl12
          with Invalid_argument _ -> T.E2 (BitXor, e1, e2))
      | LeftShift, E0 (U128 x), E0 (U8 n) ->
          let n = Uint8.to_int n in
          u128 (Uint128.shift_left x n) |> repl12
      | LeftShift, f1, E0 (U8 n) ->
          (match to_i128 f1 with
          | exception _ -> E2 (LeftShift, e1, e2)
          | x ->
              let n = Uint8.to_int n in
              let from = T.TI128 in
              let to_ = T.(E.type_of l e1).typ in
              fst (C.conv ~from ~to_ (i128 (Int128.shift_left x n))) |> repl12)
      | RightShift, E0 (U128 x), E0 (U8 n) ->
          let n = Uint8.to_int n in
          u128 (Uint128.shift_right x n) |> repl12
      | RightShift, f1, E0 (U8 n) ->
          (match to_i128 f1 with
          | exception _ -> E2 (RightShift, e1, e2)
          | x ->
              let n = Uint8.to_int n in
              let from = T.TI128 in
              let to_ = T.(E.type_of l e1).typ in
              fst (C.conv ~from ~to_ (i128 (Int128.shift_right x n))) |> repl12)
      | Join, E0 (String s1), E0R (MakeVec, ss) ->
          (try
            (* TODO: we could join only some of the strings *)
            Array.enum ss /@
            (function
              | T.E0 (String s) -> s
              | _ -> raise Exit) |>
            List.of_enum |>
            String.join s1 |>
            string |> repl12
          with Exit ->
            E2 (Join, e1, e2))
      | And, E0 (Bool true), _ -> keep2 ()
      | And, _, E0 (Bool true) -> keep1 ()
      | And, E0 (Bool false), _ -> bool false |> repl1 (* [e2] not evaluated *)
      (* Cannot ignore [e1] even if e2 is demonstrably false because of its
       * possible side effects! *)
      | Or, E0 (Bool false), _ -> keep2 ()
      | Or, _, E0 (Bool false) -> keep1 ()
      | Or, E0 (Bool true), _ -> bool true |> repl1 (* [e2] not evaluated *)
      (* Cannot ignore [e1] event if e2 is demonstrably true because if its
       * possible side effects! *)
      (* Those are created empty: *)
      | Member, _, E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _
                       | HashTable _ | Heap), _) ->
          bool false |> repl12
      | AllocArr, f1, _ ->
          let def = T.E2 (AllocArr, e1, e2) in
          let mn = E.type_of l e2 in
          (match E.to_cst_int f1 with
          | exception _ ->
              def
          | 0 ->
              make_arr mn [] |> repl12
(*          | 1 ->
              make_arr mn [ e2 ] |> repl (replace_final_expression_anonymously e1) *)
          | _ ->
              def)
      | PartialSort, _, E0R ((MakeVec | MakeArr _), [||]) ->
          keep1 () (* result is the original, already "sorted" array *)
      | PartialSort, E0R ((MakeVec | MakeArr _), [||]), _ ->
          keep1 () (* result is the original empty array *)
      | SplitBy, E0 (String s1), E0 (String s2) ->
          String.split_on_string s1 s2 |>
          List.map string |>
          make_arr T.string |>
          repl12
      | SplitAt, f1, E0 (String s) ->
          let def = T.E2 (SplitAt, e1, e2) in
          (try
            let i = E.to_cst_int f1 in
            make_tup
              [ string (String.sub s 0 i) ;
                string (String.sub s i (String.length s - i)) ] |> repl12
          with _ ->
            def)
      | AppendBytes, E0 (Bytes b1), E0 (Bytes b2) ->
          bytes (Bytes.cat b1 b2) |> repl12
      | AppendBytes, E0 (Bytes b), _ when Bytes.length b = 0 -> keep2 ()
      | AppendBytes, _, E0 (Bytes b) when Bytes.length b = 0 -> keep1 ()
      | AppendString, E0 (String s1), E0 (String s2) ->
          string (s1 ^ s2) |> repl12
      | AppendString, E0 (String ""), _ -> keep2 ()
      | AppendString, _, E0 (String "") -> keep1 ()
      | StartsWith, E0 (String s1), E0 (String s2) ->
          bool (String.starts_with s1 s2) |> repl12
      | EndsWith, E0 (String s1), E0 (String s2) ->
          bool (String.ends_with s1 s2) |> repl12
      | Cons, E1 (Head, l1), E1 (Tail, l2)
        when E.eq l1 l2 && not (E.for_any E.has_side_effect l1) ->
          repl12 l1
      (* Cannot be truncated further: *)
      | ChopBegin, E0R (MakeArr _, [||]), _ ->
          keep1 ()
      | ChopBegin, E0R (MakeArr mn, items), n ->
          (match E.to_cst_int n with
          | exception _ ->
              T.E2 (ChopBegin, e1, e2)
          | n ->
              let len = Array.length items in
              (if n >= len then
                T.E0R (MakeArr mn, [||]) |> repl12
              else
                T.E0R (MakeArr mn, Array.sub items n (len - n)) |>
                replace_final_expression e1 |>
                repl (replace_final_expression_anonymously e2)))
      | ChopBegin, _, n ->
          let def = T.E2 (ChopBegin, e1, e2) in
          (match E.to_cst_int n with
          | exception _ -> def
          | 0 -> keep1 ()
          | _ -> def)
      (* Cannot be truncated further: *)
      | ChopEnd, E0R (MakeArr _, [||]), _ ->
          keep1 ()
      | ChopEnd, E0R (MakeArr mn, items), n ->
          let def = T.E2 (ChopEnd, e1, e2) in
          (match E.to_cst_int n with
          | exception _ -> def
          | n ->
              let len = Array.length items in
              (if n >= len then
                T.E0R (MakeArr mn, [||]) |> repl12
              else
                T.E0R (MakeArr mn, Array.sub items 0 (len - n)) |>
                replace_final_expression e1 |>
                repl (replace_final_expression_anonymously e2)))
      | ChopEnd, _, n ->
          let def = T.E2 (ChopEnd, e1, e2) in
          (match E.to_cst_int n with
          | exception _ -> def
          | 0 -> keep1 ()
          | _ -> def)
      | While, E0 (Bool false), _ ->
          replace_final_expression_anonymously e2 nop |>
          repl (replace_final_expression_anonymously e1)
      | Index, E0 (Char c), E0 (String s) ->
          (try not_null (u32_of_int (String.index s c))
          with Not_found -> null TU32) |>
          repl12
      | op, _, _ -> T.E2 (op, e1, e2))
  | E3 (op, e1, e2, e3) ->
      let e1 = p e1
      and e2 = p e2
      and e3 = p e3 in
      let repl1 = repl (replace_final_expression e1)
      and repl3 = repl (replace_final_expression e3) in
      let repl123 =
        repl (replace_final_expression_anonymously e1 %
              replace_final_expression_anonymously e2 %
              replace_final_expression_anonymously e3) in
      (match op, final_expression e1, final_expression e2,
             final_expression e3 with
      | If, E0 (Bool true), _, _ ->
          replace_final_expression_anonymously e1 e2
      | If, E0 (Bool false), _, _ ->
          replace_final_expression_anonymously e1 e3
      | If, E1 (Not, e), _, _ ->
          p (E3 (If, repl1 e, e3, e2))
      | If, _ , then_, else_
        when E.eq then_ else_ &&
             not (E.for_any E.has_side_effect e1) &&
             not (E.for_any E.has_side_effect e2) &&
             not (E.for_any E.has_side_effect e3) ->
          (* Then we ever need to execute one of the alternative: *)
          e2
      (* Propagate knowledge about truthness of an If condition in its
       * branches: *)
      | If, cond, f2, f3 ->
          let repl ~by src =
            E.map (fun e ->
              if E.eq e cond then by else e
            ) src in
          (* Replace the condition by true / false in the branches: *)
          let f2' = repl ~by:true_ f2
          and f3' = repl ~by:false_ f3 in
          if E.eq f2 f2' && E.eq f3 f3' then
            T.E3 (If, e1, e2, e3)
          else
            T.E3 (If, e1, replace_final_expression e2 f2',
                          replace_final_expression e3 f3') |> p
      | Map, _, f, lst ->
          (match lst with
          | E0R (MakeVec, [| e |]) ->
              (* Unearthing the MakeVec might makes further optimisations
               * possible: *)
              make_vec [ apply e2 [ e1 ; repl3 e ] ] |> p
          | E1 (AllocVec d, init) ->
              alloc_vec d (apply e2 [ repl3 init ])
          | _ ->
              if E.is_identity 0 f then
                replace_final_expression_anonymously e2 e3 |>
                replace_final_expression_anonymously e1
              else E3 (Map, e1, e2, e3))
      (* Do nothing if blitting nothing: *)
      | BlitByte, _, _, E0 (Size 0) ->
          replace_final_expression_anonymously e2 e1 |>
          replace_final_expression_anonymously e3
      | FindSubstring, _, E0 (String s1), E0 (String s2) ->
          (try
            (* Let [p] optimize away this condition if the bool is known: *)
            let then_ = u24 (Uint24.of_int (String.find s2 s1))
            and else_ = u24 (Uint24.of_int (String.rfind s2 s1)) in
            not_null (if_ e1 ~then_ ~else_) |>
            replace_final_expression_anonymously e3 |>
            replace_final_expression_anonymously e2 |> p
          with Not_found ->
            null T.TU24 |> repl123)
      | SetVec, _, (T.E0R ((MakeVec | MakeArr _), _) |
                    T.E1 (AllocVec _, _) |
                    T.E2 (AllocArr, _, _)), _ ->
          Printf.eprintf "Warning: vector is lost after modification in:%s"
            (E.to_pretty_string ~max_depth:4 e) ;
          T.E3 (SetVec, e1, e2, e3)
      | op, _, _, _ -> E3 (op, e1, e2, e3))

(*$inject
  module P = DessserParser

  let test_peval opt_lvl s =
    let lvl = !inline_level in
    inline_level := opt_lvl ;
    let e = peval E.no_env (P.expr_of_string s) |> E.to_string ?max_depth:None in
    inline_level := lvl ;
    e
*)
(*$= test_peval & ~printer:BatPervasives.identity
  "(make-tup (size 4) (size 0))" \
    (test_peval 3 \
      "(let \"useless\" (make-tup (add (size 0) (size 0)) (size 0)) \
          (make-tup (add (size 4) (fst (identifier \"useless\"))) \
          (snd (identifier \"useless\"))))")

  "(make-tup (size 8) (size 0))" \
    (test_peval 3 "(make-tup (add (size 4) (size 4)) (size 0))")

  "(let \"a\" (u8 1) (let \"b\" (u8 2) (dump (add (add (identifier \"a\") (identifier \"b\")) (add (identifier \"a\") (identifier \"b\"))))))" \
    (test_peval 0 "(let-pair \"a\" \"b\" (make-tup (u8 1) (u8 2)) \
                     (dump (add (add (identifier \"a\") (identifier \"b\")) \
                                (add (identifier \"a\") (identifier \"b\")))))")

  "(dump (u8 6))" \
    (test_peval 1 "(let-pair \"a\" \"b\" (make-tup (u8 1) (u8 2)) \
                     (dump (add (add (identifier \"a\") (identifier \"b\")) \
                                (add (identifier \"a\") (identifier \"b\")))))")

  "(fun (\"FLOAT\") (unsafe-log (add (float 0x1p+0) (abs (param 0)))))" \
    (test_peval 3 "(fun (\"float\") \
                     (force (log (add (force (sqrt (float 1))) \
                                      (abs (param 0))))))")

  "(bool true)" \
    (test_peval 3 \
      "(apply \
        (fun (\"U32\") \
          (gt (cardinality (make-vec (char \"f\") (char \"o\") (char \"o\"))) \
              (param 0))) \
          (u32 2))")

  "(fun (\"U16\") (add (u16 1) (param 0)))" \
    (test_peval 3 \
      "(fun (\"U16\") \
        (let \"p\" (make-tup (u8 1) (param 0)) \
          (let-pair \"a\" \"b\" (identifier \"p\") \
            (add (to-u16 (identifier \"a\")) \
                 (identifier \"b\")))))")

  "(fun (\"Ptr\" \"Ptr\") (let-pair \"inner1\" \"innerPtr\" (read-u32 little-endian (param 0)) (make-tup (identifier \"inner1\") (ptr-add (identifier \"innerPtr\") (size 4)))))" \
    (test_peval 3 \
      "(fun (\"Ptr\" \"Ptr\") \
        (let-pair \"outer1\" \"outerPtr\" \
          (let-pair \"inner1\" \"innerPtr\" \
            (read-u32 little-endian (param 0)) \
            (make-tup (identifier \"inner1\") (identifier \"innerPtr\"))) \
          (make-tup (identifier \"outer1\") \
                     (ptr-add (identifier \"outerPtr\") (size 4)))))")

  "(let \"a\" (random-u8) (add (identifier \"a\") (identifier \"a\")))" \
    (test_peval 0 \
      "(fst (let \"a\" (random-u8) \
              (make-tup (add (identifier \"a\") (identifier \"a\")) (u8 0))))")

  "(random-u8)" \
    (test_peval 3 \
      "(force (nth (u8 1) \
         (let \"a\" (random-u8) \
           (make-vec (identifier \"a\") (identifier \"a\")))))")

  "(null \"FLOAT\")" \
    (test_peval 3 "(float-of-string (string \"POISON\"))")

  "(fun (\"U8?\") (if (is-null (param 0)) (u8 49) (add (force (param 0)) (u8 1))))" \
    (test_peval 3 \
      "(fun (\"U8?\") \
         (if (is-null (param 0)) \
             (add (if (is-null (param 0)) (u8 42) (force (param 0))) \
                  (u8 7)) \
             (add (force (param 0)) \
                  (u8 1))))")
*)
(* This one is but a wish for now:
  "(fun (\"u32\") \
    (repeat (u32_of_int 0) (param 0) \
      (fun (\"u32\" \"string\") \
        (append-string (param 1 1) (string \"x\"))) \
      (string \"\")))" \
    (test_peval 3 \
      "(fun (\"u32\") \
        (get-item 1 \
          (loop-while \
            (fun (\"(U32;String;U32)\") \
              (lt (get-item 0 (param 1 0)) (get-item 2 (param 1 0)))) \
            (fun (\"(U32;String;U32)\") \
              (make-tup (get-item 0 (param 1 0)) \
                        (append-string (get-item 1 (param 1 0)) (string \"x\")) \
                        (get-item 2 (param 1 0)))) \
            (make-tup (u32 0) (string \"\") (param 0)))))")
*)
