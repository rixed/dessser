open Batteries
open Stdint

open DessserTools
open DessserFloatTools
module C = DessserConversions
module T = DessserTypes
module E = DessserExpressions
open E.Ops

let inline_level = ref 1

let max_inline_size () =
  match !inline_level with
  | 0 -> 0
  | 1 -> 4
  | 2 -> 8
  | 3 -> 16
  | 4 -> 32
  | _ -> 64

let to_u128 = function
  | E.E0 (I8 n) -> Int8.to_uint128 n
  | E0 (I16 n) -> Int16.to_uint128 n
  | E0 (I24 n) -> Int24.to_uint128 n
  | E0 (I32 n) -> Int32.to_uint128 n
  | E0 (I40 n) -> Int40.to_uint128 n
  | E0 (I48 n) -> Int48.to_uint128 n
  | E0 (I56 n) -> Int56.to_uint128 n
  | E0 (I64 n) -> Int64.to_uint128 n
  | E0 (I128 n) -> Int128.to_uint128 n
  | E0 (U8 n | Byte n) -> Uint8.to_uint128 n
  | E0 (U16 n| Word n) -> Uint16.to_uint128 n
  | E0 (U24 n) -> Uint24.to_uint128 n
  | E0 (U32 n| DWord n) -> Uint32.to_uint128 n
  | E0 (U40 n) -> Uint40.to_uint128 n
  | E0 (U48 n) -> Uint48.to_uint128 n
  | E0 (U56 n) -> Uint56.to_uint128 n
  | E0 (U64 n | QWord n) -> Uint64.to_uint128 n
  | E0 (U128 n | OWord n) -> Uint128.to_uint128 n
  | E0 (Float n) -> Uint128.of_float n
  | E0 (Size n) -> Uint128.of_int n
  | E0 (Address n) -> Uint128.of_uint64 n
  | _ -> invalid_arg "to_u128"

let to_i128 = function
  | E.E0 (I8 n) -> Int8.to_int128 n
  | E0 (I16 n) -> Int16.to_int128 n
  | E0 (I24 n) -> Int24.to_int128 n
  | E0 (I32 n) -> Int32.to_int128 n
  | E0 (I40 n) -> Int40.to_int128 n
  | E0 (I48 n) -> Int48.to_int128 n
  | E0 (I56 n) -> Int56.to_int128 n
  | E0 (I64 n) -> Int64.to_int128 n
  | E0 (I128 n) -> Int128.to_int128 n
  | E0 (U8 n | Byte n) -> Uint8.to_int128 n
  | E0 (U16 n | Word n) -> Uint16.to_int128 n
  | E0 (U24 n) -> Uint24.to_int128 n
  | E0 (U32 n | DWord n) -> Uint32.to_int128 n
  | E0 (U40 n) -> Uint40.to_int128 n
  | E0 (U48 n) -> Uint48.to_int128 n
  | E0 (U56 n) -> Uint56.to_int128 n
  | E0 (U64 n | QWord n) -> Uint64.to_int128 n
  | E0 (U128 n | OWord n) -> Uint128.to_int128 n
  | E0 (Float n) -> Int128.of_float n
  | E0 (Size n) -> Int128.of_int n
  | E0 (Address n) -> Int128.of_uint64 n
  | _ -> invalid_arg "to_u128"

let to_uint to_op e cst of_u128 =
  match to_u128 e with
  | exception Invalid_argument _ -> E.E1 (to_op, e)
  | n -> cst (of_u128 n)

let to_int to_op e cst of_i128 =
  match to_i128 e with
  | exception Invalid_argument _ -> E.E1 (to_op, e)
  | n -> cst (of_i128 n)

let float_of_num = function
  | E.E0 (Float v) -> v
  | E0 (U128 n) -> Uint128.to_float n
  | e -> Int128.to_float (to_i128 e)

let peval_to_float e =
  try float (float_of_num e)
  with _ -> to_float e

let is_nan f =
  f <> f

let nullable_of_nan f =
  if is_nan f then null (Base Float) else not_null (float f)

let arith2'' op e1 e2 op_i128 cst =
  (* All but U128 ints can be safely converted into I128.
   * And non const U128 won't be converted either, so let's just leave this
   * one aside: *)
  try
    match e1, e2 with
    | E.E0 (U128 _), _
    | _, E.E0 (U128 _) ->
        invalid_arg "arith2''"
    | _ ->
        let n1, n2 = to_i128 e1, to_i128 e2 in
        cst (op_i128 n1 n2)
  with Invalid_argument _ ->
    E.E2 (op, e1, e2)

let arith2' op e1 e2 op_float op_i128 op_u128 =
  match e1, e2 with
  | E.E0 (U8 _), _ -> arith2'' op e1 e2 op_i128 (u8 % Int128.to_uint8)
  | E0 (U16 _), _ -> arith2'' op e1 e2 op_i128 (u16 % Int128.to_uint16)
  | E0 (U24 _), _ -> arith2'' op e1 e2 op_i128 (u24 % Int128.to_uint24)
  | E0 (U32 _), _ -> arith2'' op e1 e2 op_i128 (u32 % Int128.to_uint32)
  | E0 (U40 _), _ -> arith2'' op e1 e2 op_i128 (u40 % Int128.to_uint40)
  | E0 (U48 _), _ -> arith2'' op e1 e2 op_i128 (u48 % Int128.to_uint48)
  | E0 (U56 _), _ -> arith2'' op e1 e2 op_i128 (u56 % Int128.to_uint56)
  | E0 (U64 _), _ -> arith2'' op e1 e2 op_i128 (u64 % Int128.to_uint64)
  | E0 (U128 a), E.E0 (U128 b) -> u128 (op_u128 a b)
  | E0 (I8 _), _ -> arith2'' op e1 e2 op_i128 (i8 % Int128.to_int8)
  | E0 (I16 _), _ -> arith2'' op e1 e2 op_i128 (i16 % Int128.to_int16)
  | E0 (I24 _), _ -> arith2'' op e1 e2 op_i128 (i24 % Int128.to_int24)
  | E0 (I32 _), _ -> arith2'' op e1 e2 op_i128 (i32 % Int128.to_int32)
  | E0 (I40 _), _ -> arith2'' op e1 e2 op_i128 (i40 % Int128.to_int40)
  | E0 (I48 _), _ -> arith2'' op e1 e2 op_i128 (i48 % Int128.to_int48)
  | E0 (I56 _), _ -> arith2'' op e1 e2 op_i128 (i56 % Int128.to_int56)
  | E0 (I64 _), _ -> arith2'' op e1 e2 op_i128 (i64 % Int128.to_int64)
  | E0 (I128 _), _ -> arith2'' op e1 e2 op_i128 (i128 % Int128.to_int128)
  | E0 (Byte _), _ -> arith2'' op e1 e2 op_i128 (byte % Int128.to_uint8)
  | E0 (Word _), _ -> arith2'' op e1 e2 op_i128 (word % Int128.to_uint16)
  | E0 (DWord _), _ -> arith2'' op e1 e2 op_i128 (dword % Int128.to_uint32)
  | E0 (QWord _), _ -> arith2'' op e1 e2 op_i128 (qword % Int128.to_uint64)
  | E0 (OWord _), _ -> arith2'' op e1 e2 op_i128 (oword % Int128.to_uint128)
  | E0 (Size _), _ -> arith2'' op e1 e2 op_i128 (size % Int128.to_int)
  | E0 (Address _), _ -> arith2'' op e1 e2 op_i128 (address % Int128.to_uint64)
  | E0 (Float a), E0 (Float b) -> float (op_float a b)
  | _ -> E.E2 (op, e1, e2)

let arith2 op e1 e2 =
  match op with
  | E.Add -> arith2' op e1 e2 (+.) Int128.add Uint128.add
  | Sub -> arith2' op e1 e2 (-.) Int128.sub Uint128.sub
  | Mul -> arith2' op e1 e2 ( *.) Int128.mul Uint128.mul
  | _ -> E.E2 (op, e1, e2)

let comp2' op e1 e2 op_gen op_i128 op_u128 =
  match e1, e2 with
  | E.E0 (U8 a), E.E0 (U8 b)
  | E0 (Byte a), E0 (Byte b) ->
      op_i128 (Uint8.to_int128 a) (Uint8.to_int128 b)
  | E0 (U16 a), E0 (U16 b)
  | E0 (Word a), E0 (Word b) ->
      op_i128 (Uint16.to_int128 a) (Uint16.to_int128 b)
  | E0 (U24 a), E0 (U24 b) ->
      op_i128 (Uint24.to_int128 a) (Uint24.to_int128 b)
  | E0 (U32 a), E0 (U32 b)
  | E0 (DWord a), E0 (DWord b) ->
      op_i128 (Uint32.to_int128 a) (Uint32.to_int128 b)
  | E0 (U40 a), E0 (U40 b) ->
      op_i128 (Uint40.to_int128 a) (Uint40.to_int128 b)
  | E0 (U48 a), E0 (U48 b) ->
      op_i128 (Uint48.to_int128 a) (Uint48.to_int128 b)
  | E0 (U56 a), E0 (U56 b) ->
      op_i128 (Uint56.to_int128 a) (Uint56.to_int128 b)
  | E0 (U64 a), E0 (U64 b)
  | E0 (QWord a), E0 (QWord b)
  | E0 (Address a), E0 (Address b) ->
      op_i128 (Uint64.to_int128 a) (Uint64.to_int128 b)
  | E0 (U128 a), E0 (U128 b)
  | E0 (OWord a), E0 (OWord b) ->
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
  | E0 (Float _ | String _ | Bool _ | Char _ | Bit _ |
        Null _ | EndOfList _ | EmptySet _ | Unit |
        CopyField | SkipField | SetFieldNull as a),
    E0 (Float _ | String _ | Bool _ | Char _ | Bit _ |
        Null _ | EndOfList _ | EmptySet _ | Unit |
        CopyField | SkipField | SetFieldNull as b) ->
      op_gen a b
  | _ ->
      E.E2 (op, e1, e2)

let comp2 op e1 e2 =
  match op with
  | E.Gt ->
      comp2' op e1 e2 (fun a b -> bool (a > b))
                      (fun a b -> bool (Int128.compare a b > 0))
                      (fun a b -> bool (Uint128.compare a b > 0))
  | Ge ->
      comp2' op e1 e2 (fun a b -> bool (a >= b))
                      (fun a b -> bool (Int128.compare a b >= 0))
                      (fun a b -> bool (Uint128.compare a b >= 0))
  | Eq ->
      comp2' op e1 e2 (fun a b -> bool (a = b))
                      (fun a b -> bool (Int128.compare a b = 0))
                      (fun a b -> bool (Uint128.compare a b = 0))
  | _ ->
      E.E2 (op, e1, e2)

let arith1'' op e op_i128 cst =
  try
    match e with
    | E.E0 (U128 _) ->
        invalid_arg "arith2''"
    | _ ->
        cst (op_i128 (to_i128 e))
  with Invalid_argument _ ->
    E.E1 (op, e)

let arith1' op e op_float op_i128 op_u128 =
  match e with
  | E.E0 (U8 _) -> arith1'' op e op_i128 (u8 % Int128.to_uint8)
  | E0 (U16 _) -> arith1'' op e op_i128 (u16 % Int128.to_uint16)
  | E0 (U24 _) -> arith1'' op e op_i128 (u24 % Int128.to_uint24)
  | E0 (U32 _) -> arith1'' op e op_i128 (u32 % Int128.to_uint32)
  | E0 (U40 _) -> arith1'' op e op_i128 (u40 % Int128.to_uint40)
  | E0 (U48 _) -> arith1'' op e op_i128 (u48 % Int128.to_uint48)
  | E0 (U56 _) -> arith1'' op e op_i128 (u56 % Int128.to_uint56)
  | E0 (U64 _) -> arith1'' op e op_i128 (u64 % Int128.to_uint64)
  | E0 (U128 a) -> u128 (op_u128 a)
  | E0 (I8 _) -> arith1'' op e op_i128 (i8 % Int128.to_int8)
  | E0 (I16 _) -> arith1'' op e op_i128 (i16 % Int128.to_int16)
  | E0 (I24 _) -> arith1'' op e op_i128 (i24 % Int128.to_int24)
  | E0 (I32 _) -> arith1'' op e op_i128 (i32 % Int128.to_int32)
  | E0 (I40 _) -> arith1'' op e op_i128 (i40 % Int128.to_int40)
  | E0 (I48 _) -> arith1'' op e op_i128 (i48 % Int128.to_int48)
  | E0 (I56 _) -> arith1'' op e op_i128 (i56 % Int128.to_int56)
  | E0 (I64 _) -> arith1'' op e op_i128 (i64 % Int128.to_int64)
  | E0 (I128 _) -> arith1'' op e op_i128 (i128 % Int128.to_int128)
  | E0 (Float a) -> float (op_float a)
  | _ -> E.E1 (op, e)

let known_zero e =
  e = E.E0 (Float 0.) ||
  (try E.to_cst_int e = 0
  with _ -> false)

let known_one e =
  e = E.E0 (Float 1.) ||
  (try E.to_cst_int e = 1
  with _ -> false)

let known_not_zero = function
  | E.E0 (Float n) -> n <> 0.
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
  | E.E1 (Exp, _) -> true
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
    | E.E1 (Fst, E0 (Identifier n)) when n = name -> fst + 1, snd, tot
    | E.E1 (Snd, E0 (Identifier n)) when n = name -> fst, snd + 1, tot
    | E.E0 (Identifier n) when n = name -> fst, snd, tot + 1
    | _ -> prev
  ) e

let count_id_uses name e =
  E.fold 0 (fun count -> function
    | E.E0 (Identifier n) when n = name -> count + 1
    | _ -> count
  ) e

let rec peval l e =
  (* For when some operations are invalid on floats: *)
  let no_floats _ _ = assert false in
  let no_float _ = assert false in
  let p = peval l in
  match e with
  | E.E0 _ ->
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
                  if E.type_of l e = T.Void && not (E.has_side_effect e) then es
                  else e :: es in
                loop es []
            | e :: rest ->
                (* Non last items have type Void if they type check: *)
                let es = if not (E.has_side_effect e) then es else e :: es in
                loop es rest in
          (match loop [] es with
          | [ e ] -> e
          | es -> seq es )
      | op, es -> E0S (op, es))
  | E1 (Function (fid, ts), body) ->
      let l = E.enter_function fid ts l in
      E1 (Function (fid, ts), peval l body)
  | E1 (op, e1) ->
      (match op, p e1 with
      | Comment _, e1 -> e1 (* FIXME: Would prevent further optimization *)
      | GetItem n, E0S (MakeTup, es) when n < List.length es ->
          let got = List.at es n in
          let es =
            List.fold_lefti (fun es i e ->
              if i = n then es else ignore_ e :: es
            ) [] es in
          seq (List.rev (got :: es)) |> p
      | GetField n, (E0S (MakeRec, es) as e1) ->
          let def = E.E1 (op, e1) in
          let rec loop got es = function
            | [] -> got, es
            | [ _ ] -> raise Exit
            | E.E0 (String n') :: v :: rest when n = n' ->
                if got = None then
                  loop (Some v) es rest
                else
                  failwith "multiple binding"
            | _ :: v :: rest ->
                loop got (ignore_ v :: es) rest
            in
          (match loop None [] es with
          | exception _ -> def
          | None, _ -> def
          | Some got, es -> seq (List.rev (got :: es)) |> p)
      | GetAlt n, E1 (Construct (mns, i), e)
        when i < Array.length mns && fst mns.(i) = n ->
          e
      | Ignore, e ->
          if E.type_of l e = T.Void then e
          (* In theory any expression of type Void and with no side effect should
           * be disposed of. Ignore is the only way to build such an expression,
           * though, so it is enough to test this case only: *)
          else if not (E.has_side_effect e) then seq []
          else E1 (op, e)
      | IsNull, E0 (Null _) -> true_
      | IsNull, E1 (NotNull, _) -> false_
      | NotNull, E1 (Force _, e) -> e
      | Force _, E1 (NotNull, e) -> e
      | Force _, E2 (Div, e1, e2) -> E.E2 (UnsafeDiv, e1, e2) |> p
      | Force _, E2 (Rem, e1, e2) -> E.E2 (UnsafeRem, e1, e2) |> p
      | Force _, E2 (Pow, e1, e2) -> E.E2 (UnsafePow, e1, e2) |> p
      | StringOfInt, E0 (U8 n) -> string (Uint8.to_string n)
      | StringOfInt, E0 (U16 n) -> string (Uint16.to_string n)
      | StringOfInt, E0 (U24 n) -> string (Uint24.to_string n)
      | StringOfInt, E0 (U32 n) -> string (Uint32.to_string n)
      | StringOfInt, E0 (U40 n) -> string (Uint40.to_string n)
      | StringOfInt, E0 (U48 n) -> string (Uint48.to_string n)
      | StringOfInt, E0 (U56 n) -> string (Uint56.to_string n)
      | StringOfInt, E0 (U64 n) -> string (Uint64.to_string n)
      | StringOfInt, E0 (U128 n) -> string (Uint128.to_string n)
      | StringOfInt, E0 (I8 n) -> string (Int8.to_string n)
      | StringOfInt, E0 (I16 n) -> string (Int16.to_string n)
      | StringOfInt, E0 (I24 n) -> string (Int24.to_string n)
      | StringOfInt, E0 (I32 n) -> string (Int32.to_string n)
      | StringOfInt, E0 (I40 n) -> string (Int40.to_string n)
      | StringOfInt, E0 (I48 n) -> string (Int48.to_string n)
      | StringOfInt, E0 (I56 n) -> string (Int56.to_string n)
      | StringOfInt, E0 (I64 n) -> string (Int64.to_string n)
      | StringOfInt, E0 (I128 n) -> string (Int128.to_string n)
      | StringOfFloat, E0 (Float f) -> string (hexstring_of_float f)
      | StringOfIp, E0 (U32 n) -> string (DessserIpTools.V4.to_string n)
      | StringOfIp, E0 (U128 n) -> string (DessserIpTools.V6.to_string n)
      | StringOfChar, E0 (Char c) -> string (String.of_char c)
      | FloatOfString, E0 (String s) -> or_null_ (Base Float) float float_of_string s
      | U8OfString, E0 (String s) -> or_null_ (Base U8) u8 Uint8.of_string s
      | U16OfString, E0 (String s) -> or_null_ (Base U16) u16 Uint16.of_string s
      | U24OfString, E0 (String s) -> or_null_ (Base U24) u24 Uint24.of_string s
      | U32OfString, E0 (String s) -> or_null_ (Base U32) u32 Uint32.of_string s
      | U40OfString, E0 (String s) -> or_null_ (Base U40) u40 Uint40.of_string s
      | U48OfString, E0 (String s) -> or_null_ (Base U48) u48 Uint48.of_string s
      | U56OfString, E0 (String s) -> or_null_ (Base U56) u56 Uint56.of_string s
      | U64OfString, E0 (String s) -> or_null_ (Base U64) u64 Uint64.of_string s
      | U128OfString, E0 (String s) -> or_null_ (Base U128) u128 Uint128.of_string s
      | I8OfString, E0 (String s) -> or_null_ (Base I8) i8 Int8.of_string s
      | I16OfString, E0 (String s) -> or_null_ (Base I16) i16 Int16.of_string s
      | I24OfString, E0 (String s) -> or_null_ (Base I24) i24 Int24.of_string s
      | I32OfString, E0 (String s) -> or_null_ (Base I32) i32 Int32.of_string s
      | I40OfString, E0 (String s) -> or_null_ (Base I40) i40 Int40.of_string s
      | I48OfString, E0 (String s) -> or_null_ (Base I48) i48 Int48.of_string s
      | I56OfString, E0 (String s) -> or_null_ (Base I56) i56 Int56.of_string s
      | I64OfString, E0 (String s) -> or_null_ (Base I64) i64 Int64.of_string s
      | I128OfString, E0 (String s) -> or_null_ (Base I128) i128 Int128.of_string s
      | ByteOfU8, E0 (U8 n) -> byte n
      | BoolOfU8, E0 (U8 n) -> bool (Uint8.compare Uint8.zero n <> 0)
      | BoolOfU8, E1 (U8OfBool, e) -> e
      | WordOfU16, E0 (U16 n) -> word n
      | WordOfU16, E1 (U16OfWord, e) -> e
      | DWordOfU32, E0 (U32 n) -> dword n
      | DWordOfU32, E1 (U32OfDWord, e) -> e
      | QWordOfU64, E0 (U64 n) -> qword n
      | QWordOfU64, E1 (U64OfQWord, e) -> e
      | OWordOfU128, E0 (U128 n) -> oword n
      | OWordOfU128, E1 (U128OfOWord, e) -> e
      | U8OfByte, E0 (Byte n) -> u8 n
      | U8OfByte, E1 (ByteOfU8, e) -> e
      | U8OfChar, E0 (Char c) -> u8 (Uint8.of_int (Char.code c))
      | U8OfChar, E1 (CharOfU8, e) -> e
      | U8OfBool, E0 (Bool false) -> u8 (Uint8.of_int 0)
      | U8OfBool, E0 (Bool true) -> u8 (Uint8.of_int 1)
      | U8OfBool, E1 (BoolOfU8, e) -> e
      | BoolOfBit, E0 (Bit b) -> bool b
      | BoolOfBit, E1 (BitOfBool, e) -> e
      | BitOfBool, E0 (Bool b) -> bit b
      | BitOfBool, E1 (BoolOfBit, e) -> e
      | CharOfU8, E0 (U8 n) -> char (Char.chr (Uint8.to_int n))
      | CharOfU8, E1 (U8OfChar, e) -> e
      | U32OfSize, E0 (Size n) -> u32 (Uint32.of_int n)
      | U32OfSize, E1 (SizeOfU32, e) -> e
      | SizeOfU32, E0 (U32 n) -> size (Uint32.to_int n)
      | SizeOfU32, E1 (U32OfSize, e) -> e
      | U64OfAddress, E0 (Address n) -> u64 n
      | U64OfAddress, E1 (AddressOfU64, e) -> e
      | AddressOfU64, E0 (U64 n) -> address n
      | AddressOfU64, E1 (U64OfAddress, e) -> e
      | Fst, E2 (MakePair, e, _) -> e
      | Snd, E2 (MakePair, _, e) -> e
      | Head, E2 (Cons, e, _) -> e
      (* | Tail, E0 (EndOfList _) -> TODO: return Null *)
      | Tail, E2 (Cons, _, e) -> e |> p
      | LabelOf, E1 (Construct (_, i), _) -> u16 (Uint16.of_int i)
      | FloatOfQWord, E0 (QWord n) ->
          float (BatInt64.float_of_bits (Uint64.to_int64 n))
      | FloatOfQWord, E1 (QWordOfFloat, e) -> e
      | QWordOfFloat, E0 (Float f) ->
          qword (Uint64.of_int64 (BatInt64.bits_of_float f))
      | QWordOfFloat, E1 (FloatOfQWord, e) -> e
      | Not, E0 (Bool b) -> bool (not b)
      | Not, E1 (Not, e) -> e
      (* Shorten cascades of converters: *)
      | ToI8, E1 ((ToI8 | ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                   ToI64 | ToI128), e) -> E1 (ToI8, e)
      | ToI16, E1 ((ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToI16, e)
      | ToI24, E1 ((ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToI24, e)
      | ToI32, E1 ((ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToI32, e)
      | ToI40, E1 ((ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToI40, e)
      | ToI48, E1 ((ToI48 | ToI56 | ToI64 | ToI128), e) -> E1 (ToI48, e)
      | ToI56, E1 ((ToI56 | ToI64 | ToI128), e) -> E1 (ToI56, e)
      | ToI64, E1 ((ToI64 | ToI128), e) -> E1 (ToI64, e)
      | ToI128, E1 (ToI128, e) -> E1 (ToI128, e)
      | ToU8, E1 ((ToU8 | ToU16 | ToU24 | ToU32 | ToU40 | ToU48 | ToU56 |
                   ToU64 | ToU128 |
                   ToI16 | ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                   ToI64 | ToI128), e) -> E1 (ToU8, e)
      | ToU16, E1 ((ToU16 | ToU24 | ToU32 | ToU40 | ToU48 | ToU56 |
                    ToU64 | ToU128 |
                    ToI24 | ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToU16, e)
      | ToU24, E1 ((ToU24 | ToU32 | ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI32 | ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToU24, e)
      | ToU32, E1 ((ToU32 | ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI40 | ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToU32, e)
      | ToU40, E1 ((ToU40 | ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI48 | ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToU40, e)
      | ToU48, E1 ((ToU48 | ToU56 | ToU64 | ToU128 |
                    ToI56 |
                    ToI64 | ToI128), e) -> E1 (ToU48, e)
      | ToU56, E1 ((ToU56 | ToU64 | ToU128 |
                    ToI64 | ToI128), e) -> E1 (ToU56, e)
      | ToU64, E1 ((ToU64 | ToU128 | ToI128), e) -> E1 (ToU64, e)
      | ToU128, E1 (ToU128, e) -> E1 (ToU128, e)
      | ToFloat, E1 (ToFloat, e) -> E1 (ToFloat, e)
      (* Evaluate conversions *)
      | ToI8, e -> to_int ToI8 e i8 Int8.of_int128
      | ToI16, e -> to_int ToI16 e i16 Int16.of_int128
      | ToI24, e -> to_int ToI24 e i24 Int24.of_int128
      | ToI32, e -> to_int ToI32 e i32 Int32.of_int128
      | ToI40, e -> to_int ToI40 e i40 Int40.of_int128
      | ToI48, e -> to_int ToI48 e i48 Int48.of_int128
      | ToI56, e -> to_int ToI56 e i56 Int56.of_int128
      | ToI64, e -> to_int ToI64 e i64 Int64.of_int128
      | ToI128, e -> to_int ToI128 e i128 Int128.of_int128
      | ToU8, e -> to_uint ToU8 e u8 Uint8.of_uint128
      | ToU16, e -> to_uint ToU16 e u16 Uint16.of_uint128
      | ToU24, e -> to_uint ToU24 e u24 Uint24.of_uint128
      | ToU32, e -> to_uint ToU32 e u32 Uint32.of_uint128
      | ToU40, e -> to_uint ToU40 e u40 Uint40.of_uint128
      | ToU48, e -> to_uint ToU48 e u48 Uint48.of_uint128
      | ToU56, e -> to_uint ToU56 e u56 Uint56.of_uint128
      | ToU64, e -> to_uint ToU64 e u64 Uint64.of_uint128
      | ToU128, e -> to_uint ToU128 e u128 Uint128.of_uint128
      | ToFloat, e -> peval_to_float e
      | Abs, e1 -> arith1' Abs e1 abs_float Int128.abs Uint128.abs
      | Neg, e1 -> arith1' Neg e1 (~-.) Int128.neg Uint128.neg
      | StringOfBytes, E0 (Bytes v) -> string (Bytes.to_string v)
      | StringOfBytes, E1 (BytesOfString, e) -> e
      | BytesOfString, E0 (String s) -> bytes (Bytes.of_string s)
      | BytesOfString, E1 (StringOfBytes, e) -> e
      | Exp, E0 (Float n) -> float (exp n)
      | Log, E0 (Float n) -> nullable_of_nan (log n)
      | Log, e when known_positive ~strict:true e ->
          not_null (E1 (UnsafeLog, e)) |> p
      | UnsafeLog, E0 (Float n) -> float (log n)
      | Log10, E0 (Float n) -> nullable_of_nan (log10 n)
      | Log10, e when known_positive ~strict:true e ->
          not_null (E1 (UnsafeLog10, e)) |> p
      | UnsafeLog10, E0 (Float n) -> float (log10 n)
      | Sqrt, E0 (Float n) -> nullable_of_nan (sqrt n)
      | Sqrt, e when known_positive ~strict:false e ->
          not_null (E1 (UnsafeSqrt, e)) |> p
      | UnsafeSqrt, E0 (Float n) -> float (sqrt n)
      | Ceil, E0 (Float n) -> float (ceil n)
      | Floor, E0 (Float n) -> float (floor n)
      | Round, E0 (Float n) -> float (Float.round n)
      | Cos, E0 (Float n) -> float (cos n)
      | Sin, E0 (Float n) -> float (sin n)
      | Tan, E0 (Float n) -> nullable_of_nan (tan n)
      | ACos, E0 (Float n) -> nullable_of_nan (acos n)
      | ASin, E0 (Float n) -> nullable_of_nan (asin n)
      | ATan, E0 (Float n) -> float (atan n)
      | CosH, E0 (Float n) -> float (cosh n)
      | SinH, E0 (Float n) -> float (sinh n)
      | TanH, E0 (Float n) -> float (tanh n)
      | Lower, E0 (String s) -> string (String.lowercase_ascii s)
      | Upper, E0 (String s) -> string (String.uppercase_ascii s)
      | U16OfWord, E0 (Word w) -> u16 w
      | U16OfWord, E1 (WordOfU16, e) -> e
      | U32OfDWord, E0 (DWord d) -> u32 d
      | U32OfDWord, E1 (DWordOfU32, e) -> e
      | U64OfQWord, E0 (QWord q) -> u64 q
      | U64OfQWord, E1 (QWordOfU64, e) -> e
      | U128OfOWord, E0 (OWord o) -> u128 o
      | U128OfOWord, E1 (OWordOfU128, e) -> e
      | StringLength, E0 (String s) -> u32_of_int (String.length s)
      | StringLength, E1 (StringOfChar, e) when not (E.has_side_effect e) ->
          u32_of_int 1
      | Cardinality, E0S ((MakeVec | MakeLst _), es) ->
          u32_of_int (List.length es)
      | Cardinality, E0 (EndOfList _ | EmptySet _)
      | Cardinality, E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _ |
                          HashTable _ | Heap), _)
      | Cardinality, E3 (Top _, _, _, _) ->
          u32_of_int 0
      | Assert, E0 (Bool true) -> nop
      | BitNot, e ->
          arith1' BitNot e no_float Int128.lognot Uint128.lognot
      | op, e1 -> E1 (op, e1))
  | E1S (Apply, e1, es) ->
      (match p e1, List.map p es with
      | E1 (Function _, body), [] -> body
      (* If [f] is constant we cannot proceed directly with variable
       * substitutions unless each variable is used only once. We can
       * turn the apply into a sequence of lets that will further
       * substitute what can be substituted: *)
      | E1 (Function (fid, _), body), es ->
          List.fold_lefti (fun body i e ->
            let_ ~l e (fun _l e ->
              E.map (function
                | E0 (Param (fid', i')) when fid' = fid && i' = i -> e
                | x -> x
              ) body)
          ) body es |>
          (* There is no more params left so no need to update the environment: *)
          p
      | e1, es ->
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
  | E2 (Let (name, value_t), value, body) ->
      let value = p value in
      (* Best effort, as sometime we cannot provide the environment but
       * do not need it in the [body]: *)
      let l' = E.add_local name value_t l in
      let body = peval l' body in
      let def = E.E2 (Let (name, value_t), value, body) in
      (* The identifier is useless in  that case: *)
      if body = E.E0 (Identifier name) then value else
      (* Also avoid introducing aliases: *)
      (match value with
      | E.E0 (Identifier alias) ->
          E.map (function
            | E.E0 (Identifier n) when n = name -> E.E0 (Identifier alias)
            | e -> e
          ) body |> p
      | _ ->
          (* If the identifier is used only once in the body, then the optimizer will
           * also prefer to have no let. This will further allow, for instance, to
           * simplify:
           *   (get-vec 0
           *     (let (arr (make-vec 0))
           *       (set-vec 0 arr 1)))
           * into:
           *   (get-vec 0
           *     (set-vec 0 (make-vec 0) 1))
           * then ultimately into:
           *   1
           *)
          let use_count =
            (* TODO: early exit *)
            E.fold 0 (fun c -> function
              | E0 (Identifier n) when n = name -> c + 1
              | _ -> c
            ) body in
          if use_count = 0 then
            if not (E.has_side_effect value) then body
            else p (seq [ ignore_ value ; body ])
          else if use_count = 1 ||
                  E.can_duplicate value &&
                  (use_count - 1) * E.size value < max_inline_size ()
               then
            E.map (function
              | E0 (Identifier n) when n = name -> value
              | e -> e
            ) body |> p
          (* If the let binds a pair, and this binding appears only in Fst or Snd
           * expression, then use a LetPair instead and save the intermediary
           * bindings: *)
          else match value_t with
          | T.Pair (t1, t2) ->
              (* Also count how many times the identifier is used to find out if
               * the identifier is used outside of fst/snd: *)
              let fst_count, snd_count, tot_count = count_pair_uses name body in
              if tot_count > fst_count + snd_count then def else
              let n1 = "fst_"^ name and n2 = "snd_"^ name in
              let body =
                E.map (function
                  | E1 (Fst, E0 (Identifier n)) when n = name -> E0 (Identifier n1)
                  | E1 (Snd, E0 (Identifier n)) when n = name -> E0 (Identifier n2)
                  | e -> e
                ) body in
              E.E2 (LetPair (n1, t1, n2, t2), value, body) |> p
          | _ ->
              def)
  | E2 (LetPair (n1, t1, n2, t2), value, body) ->
      let value = p value in
      let l = E.add_local n1 t1 l |>
              E.add_local n2 t2 in
      let body = peval l body in
      let fst_count = count_id_uses n1 body
      and snd_count = count_id_uses n2 body in
      if fst_count = 0 then
        if snd_count = 0 then body
        else E.E2 (Let (n2, t2), secnd value, body) |> p
      else if snd_count = 0 then
        E.E2 (Let (n1, t1), first value, body) |> p
      else (match value with
        | E.E2 (MakePair, e1, e2) ->
            E.E2 (Let (n1, t1), e1, E2 (Let (n2, t2), e2, body)) |> p
        | _ ->
            E.E2 (LetPair (n1, t1, n2, t2), value, body))
   | E2 (op, e1, e2) ->
      (match op, p e1, p e2 with
      | Nth, e1, (E0S ((MakeVec | MakeLst _), es) as e2) ->
          let def = E.E2 (Nth, e1, e2) in
          (match E.to_cst_int e1 with
          | exception _ -> def
          | idx when idx < List.length es -> List.at es idx
          | _ -> def)
      (* Peel away some common wrappers: *)
      | Eq, E1 (NotNull, e1), E1 (NotNull, e2)
      | Eq, E1 (Force _, e1), E1 (Force _, e2) ->
          eq e1 e2 |> p
      (* Another easy case of practical importance: comparison of a null with
       * anything that's NotNull: *)
      | Eq, E0 (Null _), E1 (NotNull, _)
      | Eq, E1 (NotNull, _), E0 (Null _) ->
          false_
      | (Gt | Ge | Eq as op), e1, e2 -> comp2 op e1 e2
      | Add, e1, e2 when known_zero e1 -> e2
      | (Add | Sub), e1, e2 when known_zero e2 -> e1
      | Sub, e1, e2 when known_zero e1 -> neg e2 |> p
      | Mul, e1, e2 when known_one e1 -> e2
      | Mul, e1, e2 when known_one e2 -> e1
      | Mul, e1, _ when known_zero e1 -> e1
      | Mul, _, e2 when known_zero e2 -> e2
      | (Add | Sub | Mul as op), e1, e2 -> arith2 op e1 e2
      | (Div | Rem as op), E0 (Float a), E0 (Float b) ->
          (try
            let v = if op = Div then a /. b else Stdlib.Float.rem a b in
            nullable_of_nan v
          with Division_by_zero -> null (Base Float))
      | (Div | Rem as op), e1, e2 ->
          (match arith2' op e1 e2
                         (if op = Div then (/.) else (mod_float))
                         (if op = Div then Int128.div else Int128.rem)
                         (if op = Div then Uint128.div else Uint128.rem) with
          | exception Division_by_zero ->
              (* Tried to compute, but could not: *)
              null (T.value_of_t (E.type_of l e1))
          | E2 ((Div | Rem), _, _) as e ->
              (* Could not replace: keep as is *)
              e
          | e ->
              (* Did replace by the result, make it nullable: *)
              not_null e)
      | (UnsafeDiv | UnsafeRem as op), (E0 (Float a) as e1), (E0 (Float b) as e2) ->
          let def = E.E2 (op, e1, e2) in
          (try
            let v = if op = Div then a /. b else Stdlib.Float.rem a b in
            if is_nan v then def else float v
          with Division_by_zero -> def)
      | (UnsafeDiv | UnsafeRem as op), e1, e2 ->
          let def = E.E2 (op, e1, e2) in
          (try arith2' op e1 e2
                       (if op = Div then (/.) else (mod_float))
                       (if op = Div then Int128.div else Int128.rem)
                       (if op = Div then Uint128.div else Uint128.rem)
          with Division_by_zero -> def)
      | Pow, E0 (Float a), E0 (Float b) ->
          nullable_of_nan (a ** b)
      | Pow, E0 (I32 a), E0 (I32 b) ->
          (try not_null (i32 (BatInt32.pow a b))
          with Invalid_argument _ -> null (Base I32))
      | Pow, E0 (I64 a), E0 (I64 b) ->
          (try not_null (i64 (BatInt64.pow a b))
          with Invalid_argument _ -> null (Base I64))
      | Pow, e1, e2 ->
          (match float_of_num e1, float_of_num e2 with
          | exception _ ->
              E2 (Pow, e1, e2)
          | a, b ->
              let to_ = T.(mn_of_t (E.type_of l e1)).vtyp in
              (try not_null (C.conv ~to_ l (float (a ** b)))
              with _ -> null to_))
      | UnsafePow, (E0 (Float a) as e1), (E0 (Float b) as e2) ->
          let def = E.E2 (UnsafePow, e1, e2) in
          let v = a ** b in
          if is_nan v then def else float a
      | UnsafePow, (E0 (I32 a) as e1), (E0 (I32 b) as e2) ->
          (try i32 (BatInt32.pow a b)
          with Invalid_argument _ -> E2 (UnsafePow, e1, e2))
      | UnsafePow, (E0 (I64 a) as e1), (E0 (I64 b) as e2) ->
          (try i64 (BatInt64.pow a b)
          with Invalid_argument _ -> E2 (UnsafePow, e1, e2))
      | UnsafePow, e1, e2 ->
          let def = E.E2 (UnsafePow, e1, e2) in
          (match float_of_num e1, float_of_num e2 with
          | exception _ ->
              def
          | a, b ->
              let to_ = T.(mn_of_t (E.type_of l e1)).vtyp in
              (try C.conv ~to_ l (float (a ** b))
              with _ -> def))
      | BitAnd, e1, e2 ->
          arith2' BitAnd e1 e2 no_floats Int128.logand Uint128.logand
      | BitOr, e1, e2 ->
          arith2' BitOr e1 e2 no_floats Int128.logor Uint128.logor
      | BitXor, e1, e2 ->
          arith2' BitXor e1 e2 no_floats Int128.logxor Uint128.logxor
      | LeftShift, E0 (U128 x), E0 (U8 n) ->
          let n = Uint8.to_int n in
          u128 (Uint128.shift_left x n)
      | LeftShift, e1, (E0 (U8 n) as e2) ->
          (match to_i128 e1 with
          | exception _ -> E2 (LeftShift, e1, e2)
          | x ->
              let n = Uint8.to_int n in
              let to_ = T.(mn_of_t (E.type_of l e1)).vtyp in
              C.conv ~to_ l (i128 (Int128.shift_left x n)) |> p)
      | RightShift, E0 (U128 x), E0 (U8 n) ->
          let n = Uint8.to_int n in
          u128 (Uint128.shift_right x n)
      | RightShift, e1, (E0 (U8 n) as e2) ->
          (match to_i128 e1 with
          | exception _ -> E2 (RightShift, e1, e2)
          | x ->
              let n = Uint8.to_int n in
              let to_ = T.(mn_of_t (E.type_of l e1)).vtyp in
              C.conv ~to_ l (i128 (Int128.shift_right x n)) |> p)
      | Join, (E0 (String s1) as e1), (E0S (MakeVec, ss) as e2) ->
          (try
            (* TODO: we could join only some of the strings *)
            List.map (function
              | E.E0 (String s) -> s
              | _ -> raise Exit
            ) ss |>
            String.join s1 |>
            string
          with Exit ->
            E2 (Join, e1, e2))
      | CharOfString, idx, (E0 (String s) as str) ->
          if String.length s = 0 then null (Base Char)
          else (match E.to_cst_int idx with
          | exception _ -> E2 (CharOfString, idx, str)
          | idx when idx < String.length s -> not_null (char s.[idx])
          | _ -> E2 (CharOfString, idx, str))
      | And, E0 (Bool true), e2 -> e2
      | And, e1, E0 (Bool true) -> e1
      | And, E0 (Bool false), _ -> bool false  (* [e2] not evaluated *)
      (* Cannot ignore [e1] even if e2 is demonstrably false because of its
       * possible side effects! *)
      | Or, E0 (Bool false), e2 -> e2
      | Or, e1, E0 (Bool false) -> e1
      | Or, E0 (Bool true), _ -> bool true  (* [e2] not evaluated *)
      (* Cannot ignore [e1] event if e2 is demonstrably true because if its
       * possible side effects! *)
      | MakePair, E1 (Fst, a), E1 (Snd, b) when E.eq a b -> a
      (* Those are created empty: *)
      | Member, _, E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _
                       | HashTable _ | Heap), _) -> bool false
      | AllocLst, e1, e2 ->
          (match E.type_of l e2 with
          | T.Data mn ->
              (match E.to_cst_int e1 with
              | exception _ -> E2 (AllocLst, e1, e2)
              | 0 -> make_lst mn [] |> p
              | _ -> E2 (AllocLst, e1, e2))
          | _ -> E2 (AllocLst, e1, e2))
      | PartialSort, e1, E0S ((MakeVec | MakeLst _), [])
      | PartialSort, (E0S ((MakeVec | MakeLst _), []) as e1), _ -> e1
      | SplitBy, E0 (String s1), E0 (String s2) ->
          String.split_on_string s1 s2 |>
          List.map string |>
          make_lst T.(required (Base String)) |>
          p
      | SplitAt, e1, (E0 (String s) as e2) ->
          (match E.to_cst_int e1 with
          | exception _ ->
              E2 (SplitAt, e1, e2)
          | i ->
              make_tup
                [ string (String.sub s 0 i) ;
                  string (String.sub s i (String.length s - i)) ] |> p)
      | AppendBytes, E0 (Bytes b1), E0 (Bytes b2) -> bytes (Bytes.cat b1 b2)
      | AppendBytes, E0 (Bytes b), e2 when Bytes.length b = 0 -> e2
      | AppendBytes, e1, E0 (Bytes b) when Bytes.length b = 0 -> e1
      | AppendString, E0 (String s1), E0 (String s2) -> string (s1 ^ s2)
      | AppendString, E0 (String ""), e2 -> e2
      | AppendString, e1, E0 (String "") -> e1
      | StartsWith, E0 (String s1), E0 (String s2) ->
          bool (String.starts_with s1 s2)
      | EndsWith, E0 (String s1), E0 (String s2) ->
          bool (String.ends_with s1 s2)
      | GetVec, e1, (E0S ((MakeLst _ | MakeVec), es) as e2) ->
          let def = E.E2 (GetVec, e1, e2) in
          (match E.to_cst_int e1 with
          | exception _ -> def
          | idx -> (try List.at es idx with _ -> def))
      | Cons, E1 (Head, l1), E1 (Tail, l2)
        when E.eq l1 l2 && not (E.has_side_effect l1) ->
          l1
      (* Cannot be truncated further: *)
      | ChopBegin, (E0S (MakeLst _, []) as lst), _ -> lst
      | ChopBegin, (E0S (MakeLst mn, items) as lst), n ->
          (match E.to_cst_int n with
          | exception _ -> E.E2 (ChopBegin, lst, n)
          | n -> E.E0S (MakeLst mn, List.drop n items) |> p)
      | ChopBegin, lst, n ->
          (match E.to_cst_int n with
          | exception _ -> E.E2 (ChopBegin, lst, n)
          | 0 -> lst
          | _ -> E2 (ChopBegin, lst, n))
      (* Cannot be truncated further: *)
      | ChopEnd, (E0S (MakeLst _, []) as lst), _ -> lst
      | ChopEnd, (E0S (MakeLst mn, items) as lst), n ->
          (match E.to_cst_int n with
          | exception _ -> E2 (ChopEnd, lst, n)
          | n ->
              let l = List.length items in
              (if n >= l then
                E.E0S (MakeLst mn, [])
              else
                E.E0S (MakeLst mn, List.take (l - n) items)) |> p)
      | ChopEnd, lst, n ->
          (match E.to_cst_int n with
          | exception _ -> E.E2 (ChopEnd, lst, n)
          | 0 -> lst
          | _ -> E.E2 (ChopEnd, lst, n))
      | op, e1, e2 -> E.E2 (op, e1, e2))
  | E3 (op, e1, e2, e3) ->
      (match op, p e1, p e2, p e3 with
      | If, E0 (Bool true), then_, _ -> then_
      | If, E0 (Bool false), _, else_ -> else_
      | If, E1 (Not, e), then_, else_ -> p (E3 (If, e, else_, then_))
      | If, cond, then_, else_
        when E.eq then_ else_ && not (E.has_side_effect cond) -> then_
      | LoopUntil, body, E0 (Bool false), init -> apply body [ init ] |> p
      | LoopWhile, E0 (Bool false), _, init -> init
      | Fold, init, body, E0S ((MakeVec | MakeLst _), [ e ]) ->
          apply body [ e ; init ] |> p
      | Map, init, f, lst ->
          (match lst with
          | E0S (MakeVec, [ e ]) ->
              (* Unearth the MakeVec might makes further optimisations possible: *)
              make_vec [ apply f [ init ; e ] ] |> p
          | _ ->
              if E.is_identity 0 f then lst
              else E3 (Map, init, f, lst))
      (* Do nothing if blitting nothing: *)
      | BlitByte, ptr, _, E0 (Size 0) -> ptr
      | FindSubstring, from_start, E0 (String s1), E0 (String s2) ->
          (try
            (* Let [p] optimize away this condition if the bool is known: *)
            let then_ = u24 (Uint24.of_int (String.find s2 s1))
            and else_ = u24 (Uint24.of_int (String.rfind s2 s1)) in
            not_null (if_ from_start ~then_ ~else_) |> p
          with Not_found ->
            null T.(Base U24))
      | SetVec, e1, e2, e3 ->
          (match E.to_cst_int e1 with
          | exception _ ->
              E3 (SetVec, e1, e2, e3)
          | i ->
              (match e2 with
              | E0S (MakeVec, es) ->
                  List.mapi (fun j e -> if i = j then e3 else e) es |>
                  make_vec |> p
              | _ ->
                  E3 (SetVec, e1, e2, e3)))
      | op, e1, e2, e3 -> E3 (op, e1, e2, e3))
  | E4 (op, e1, e2, e3, e4) ->
      (match op, p e1, p e2, p e3, p e4 with
      | ReadWhile, E1 (Function _, E0 (Bool false)), _, init, pos -> make_pair init pos
      | Repeat, (E0 (I32 f) as from), (E0 (I32 t) as to_), body, init ->
          let c = Int32.compare f t in
          if c >= 0 then init
          else if 0 = Int32.(compare t (succ f)) then
            apply body [ from ; init ] |>
            p
          else E4 (Repeat, from, to_, body, init)
      | op, e1, e2, e3, e4 -> E4 (op, e1, e2, e3, e4))

(*$inject
  let test_peval opt_lvl s =
    let lvl = !inline_level in
    inline_level := opt_lvl ;
    let e = peval E.no_env (E.of_string s) |> E.to_string ?max_depth:None in
    inline_level := lvl ;
    e
*)
(*$= test_peval & ~printer:BatPervasives.identity
  "(make-pair (size 4) (size 0))" \
    (test_peval 3 \
      "(let \"useless\" \"(SIZE * SIZE)\" (make-pair (add (size 0) (size 0)) (size 0)) \
          (make-pair (add (size 4) (fst (identifier \"useless\"))) \
          (snd (identifier \"useless\"))))")

  "(make-pair (size 8) (size 0))" \
    (test_peval 3 "(make-pair (add (size 4) (size 4)) (size 0))")

  "(let \"a\" \"U8\" (u8 1) (let \"b\" \"U8\" (u8 2) (dump (add (add (identifier \"a\") (identifier \"b\")) (add (identifier \"a\") (identifier \"b\"))))))" \
    (test_peval 0 "(let-pair \"a\" \"U8\" \"b\" \"U8\" (make-pair (u8 1) (u8 2)) \
                     (dump (add (add (identifier \"a\") (identifier \"b\")) \
                                (add (identifier \"a\") (identifier \"b\")))))")

  "(dump (u8 6))" \
    (test_peval 1 "(let-pair \"a\" \"U8\" \"b\" \"U8\" (make-pair (u8 1) (u8 2)) \
                     (dump (add (add (identifier \"a\") (identifier \"b\")) \
                                (add (identifier \"a\") (identifier \"b\")))))")

  "(fun 0 \"FLOAT\" (unsafe-log (add (float 0x1p+0) (abs (param 0 0)))))" \
    (test_peval 3 "(fun 0 \"float\" \
                     (force \"\" (log (add (force \"\" (sqrt (float 1))) \
                                           (abs (param 0 0))))))")
*)
