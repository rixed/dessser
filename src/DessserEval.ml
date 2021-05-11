open Batteries
open Stdint

open DessserTools
open DessserFloatTools
module T = DessserTypes

module E = DessserExpressions
open E.Ops

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
  | _ -> invalid_arg "to_u128"

let to_uint to_op e cst of_u128 =
  match to_u128 e with
  | exception Invalid_argument _ -> E.E1 (to_op, e)
  | n -> cst (of_u128 n)

let to_int to_op e cst of_i128 =
  match to_i128 e with
  | exception Invalid_argument _ -> E.E1 (to_op, e)
  | n -> cst (of_i128 n)

let peval_to_float e =
  match e with
  | E.E0 (Float _) -> e
  | E0 (U128 n) -> float (Uint128.to_float n)
  | _ ->
      (match to_i128 e with
      | exception _ -> to_float e
      | n -> float (Int128.to_float n))

let nullable_of_nan f =
  if f <> f then null (Mac Float) else not_null (float f)

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

let arith2' op e1 e2 op_i128 op_u128 =
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
  | _ -> E.E2 (op, e1, e2)

let arith2 op e1 e2 =
  match op with
  | E.Add -> arith2' op e1 e2 Int128.add Uint128.add
  | Sub -> arith2' op e1 e2 Int128.sub Uint128.sub
  | Mul -> arith2' op e1 e2 Int128.mul Uint128.mul
  | _ -> E.E2 (op, e1, e2)

let arith1'' op e op_i128 cst =
  try
    match e with
    | E.E0 (U128 _) ->
        invalid_arg "arith2''"
    | _ ->
        cst (op_i128 (to_i128 e))
  with Invalid_argument _ ->
    E.E1 (op, e)

let arith1' op e op_i128 op_u128 =
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
  | _ -> E.E1 (op, e)

let arith1 op e =
  match op with
  | E.Neg -> arith1' op e Int128.neg Uint128.neg
  | _ -> E.E1 (op, e)

let is_zero e =
  e = E.E0 (Float 0.) ||
  (try E.to_cst_int e = 0
  with _ -> false)

let is_one e =
  e = E.E0 (Float 1.) ||
  (try E.to_cst_int e = 1
  with _ -> false)

let can_inline _e =
  true (* TODO *)

let rec peval l e =
  let p = peval l in
  match e with
  | E.E0 _ ->
      e
  | E0S (op, es) ->
      (match op, List.map p es with
      | Seq, es ->
          let es = List.filter (fun e -> e <> E.E0S (Seq, [])) es in
          (match es with
          | [ e ] -> e
          | es -> seq es )
      | op, es -> E0S (op, es))
  | E1 (op, e1) ->
      (match op, p e1 with
      | Comment _, e1 -> e1 (* FIXME: Would prevent further optimization *)
      | IsNull, E0 (Null _) -> true_
      | IsNull, E1 (NotNull, _) -> false_
      | NotNull, E1 (Force _, e) -> p e
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
      | FloatOfString, E0 (String s) -> or_null_ (Mac Float) float float_of_string s
      | U8OfString, E0 (String s) -> or_null_ (Mac U8) u8 Uint8.of_string s
      | U16OfString, E0 (String s) -> or_null_ (Mac U16) u16 Uint16.of_string s
      | U24OfString, E0 (String s) -> or_null_ (Mac U24) u24 Uint24.of_string s
      | U32OfString, E0 (String s) -> or_null_ (Mac U32) u32 Uint32.of_string s
      | U40OfString, E0 (String s) -> or_null_ (Mac U40) u40 Uint40.of_string s
      | U48OfString, E0 (String s) -> or_null_ (Mac U48) u48 Uint48.of_string s
      | U56OfString, E0 (String s) -> or_null_ (Mac U56) u56 Uint56.of_string s
      | U64OfString, E0 (String s) -> or_null_ (Mac U64) u64 Uint64.of_string s
      | U128OfString, E0 (String s) -> or_null_ (Mac U128) u128 Uint128.of_string s
      | I8OfString, E0 (String s) -> or_null_ (Mac I8) i8 Int8.of_string s
      | I16OfString, E0 (String s) -> or_null_ (Mac I16) i16 Int16.of_string s
      | I24OfString, E0 (String s) -> or_null_ (Mac I24) i24 Int24.of_string s
      | I32OfString, E0 (String s) -> or_null_ (Mac I32) i32 Int32.of_string s
      | I40OfString, E0 (String s) -> or_null_ (Mac I40) i40 Int40.of_string s
      | I48OfString, E0 (String s) -> or_null_ (Mac I48) i48 Int48.of_string s
      | I56OfString, E0 (String s) -> or_null_ (Mac I56) i56 Int56.of_string s
      | I64OfString, E0 (String s) -> or_null_ (Mac I64) i64 Int64.of_string s
      | I128OfString, E0 (String s) -> or_null_ (Mac I128) i128 Int128.of_string s
      | ByteOfU8, E0 (U8 n) -> byte n
      | BoolOfU8, E0 (U8 n) -> bool (Uint8.compare Uint8.zero n <> 0)
      | WordOfU16, E0 (U16 n) -> word n
      | DWordOfU32, E0 (U32 n) -> dword n
      | QWordOfU64, E0 (U64 n) -> qword n
      | OWordOfU128, E0 (U128 n) -> oword n
      | U8OfByte, E0 (Byte n) -> u8 n
      | U8OfChar, E0 (Char c) -> u8 (Uint8.of_int (Char.code c))
      | U8OfBool, E0 (Bool false) -> u8 (Uint8.of_int 0)
      | U8OfBool, E0 (Bool true) -> u8 (Uint8.of_int 1)
      | BoolOfBit, E0 (Bit b) -> bool b
      | BitOfBool, E0 (Bool b) -> bit b
      | CharOfU8, E0 (U8 n) -> char (Char.chr (Uint8.to_int n))
      | U32OfSize, E0 (Size n) -> u32 (Uint32.of_int n)
      | SizeOfU32, E0 (U32 n) -> size (Uint32.to_int n)
      | Fst, E2 (Pair, e, _) -> e
      | Snd, E2 (Pair, _, e) -> e
      | Head, E2 (Cons, e, _) -> e
      (* | Tail, E0 (EndOfList _) -> TODO: return Null *)
      | Tail, E2 (Cons, e, E0 (EndOfList _)) -> e
      | Tail, E2 (Cons, _, e) -> tail e |> p
      | FloatOfQWord, E0 (QWord n) ->
          float (BatInt64.float_of_bits (Uint64.to_int64 n))
      | QWordOfFloat, E0 (Float f) ->
          qword (Uint64.of_int64 (BatInt64.bits_of_float f))
      | Not, E0 (Bool b) -> bool (not b)
      | Not, E1 (Not, e) -> e
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
      | Neg, e1 -> arith1 Neg e1
      | StringOfBytes, E0 (Bytes v) -> string (Bytes.to_string v)
      | Exp, E0 (Float n) -> float (exp n)
      | Log, E0 (Float n) -> nullable_of_nan (log n)
      | Log10, E0 (Float n) -> nullable_of_nan (log10 n)
      | Sqrt, E0 (Float n) -> nullable_of_nan (sqrt n)
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
      | U32OfDWord, E0 (DWord d) -> u32 d
      | U64OfQWord, E0 (QWord q) -> u64 q
      | U128OfOWord, E0 (OWord o) -> u128 o
      | StringLength, E0 (String s) -> u32_of_int (String.length s)
      | Cardinality, E0S ((MakeVec | MakeLst _), es) ->
          u32_of_int (List.length es)
      | Force _, E1 (NotNull, e) -> e
      | Assert, E0 (Bool true) -> nop
      | op, e1 -> E1 (op, e1))
  | E1S (op, e1, es) ->
      (match op, p e1, List.map p es with
      | Apply, E1 (Function _, body), [] when can_inline body -> body
      (* If [f] is constant we cannot proceed directly with variable
       * substitutions unless each variable is used only once. We can
       * turn the apply into a sequence of lets that will further
       * substitute what can be substituted: *)
      | Apply, E1 (Function (fid, _), body), es when can_inline body ->
          List.fold_lefti (fun body i e ->
            let_ ~l e (fun _l e ->
              E.map (function
                | E0 (Param (fid', i')) when fid' = fid && i' = i -> e
                | x -> x
              ) body)
          ) body es |>
          p
      | op, e1, es -> E1S (op, e1, es))
  (*
   * Let expressions
   *)
  | E2 (Let n, def, body) ->
      let def = p def in
      (* Best effort, as sometime we cannot provide the environment but
       * do not need it in the [body]: *)
      let l =
        try (E.E0 (Identifier n), E.type_of l def) :: l
        with E.Unbound_identifier _ | E.Unbound_parameter _ -> l in
      let body = peval l body in
      if body = E0 (Identifier n) then (
        (* The identifier is then useless: *)
        def
      ) else (
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
            | E0 (Identifier n') when n' = n -> c + 1
            | _ -> c
          ) body in
        assert (use_count <> 8765) ;
        if use_count = 0 && not (E.has_side_effect def) then
          body
        else if use_count = 1 then
          E.map (function
            | E0 (Identifier n') when n' = n -> def
            | e -> e
          ) body |> p
        else
          E2 (Let n, def, body))
  | E2 (op, e1, e2) ->
      (match op, p e1, p e2 with
      | Add, e1, e2 when is_zero e1 -> e2
      | (Add | Sub), e1, e2 when is_zero e2 -> e1
      | Sub, e1, e2 when is_zero e1 -> neg e2 |> p
      | Mul, e1, e2 when is_one e1 -> e2
      | Mul, e1, e2 when is_one e2 -> e1
      | Mul, e1, _ when is_zero e1 -> e1
      | Mul, _, e2 when is_zero e2 -> e2
      | (Add | Sub | Mul as op), e1, e2 -> arith2 op e1 e2
      | Div, E0 (Float a), E0 (Float b) ->
          (try float (a /. b)
          with Division_by_zero -> null (Mac Float))
      | Div, e1, e2 ->
          (try arith2' Div e1 e2 Int128.div Uint128.div
          with Division_by_zero -> null (T.vtyp_of_t (E.type_of l e1)))
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
          if String.length s = 0 then null (Mac Char)
          else (match E.to_cst_int idx with
          | exception _ -> E2 (CharOfString, idx, str)
          | idx when idx < String.length s -> not_null (char s.[idx])
          | _ -> E2 (CharOfString, idx, str))
      | Eq, E0 Null _, E0 Null _
      | Eq, E0 (EndOfList _), E0 (EndOfList _)
      | Eq, E0 (EmptySet _), E0 (EmptySet _)
      | Eq, E0 Unit, E0 Unit
      | Eq, E0 CopyField, E0 CopyField
      | Eq, E0 SkipField, E0 SkipField
      | Eq, E0 SetFieldNull, E0 SetFieldNull ->
          true_
      (* None other combination of those can be equal: *)
      | Eq, E0 (Null _ | EndOfList _ | EmptySet _ | Unit
           | CopyField | SkipField | SetFieldNull),
        E0 (Null _ | EndOfList _ | EmptySet _ | Unit
           | CopyField | SkipField | SetFieldNull) ->
          false_
      (* Another easy case of practical importance: comparison of a null with
       * a NotNull: *)
      | Eq, E0 (Null _), E1 (NotNull, _)
      | Eq, E1 (NotNull, _), E0 (Null _) ->
          false_
      (* Peel away some common wrappers: *)
      | Eq, E1 (NotNull, e1), E1 (NotNull, e2)
      | Eq, E1 (Force _, e1), E1 (Force _, e2) ->
          eq e1 e2
      (* Compare numerical constant (only if of the same type (TODO)): *)
      | Eq, E0 (Float v1), E0 (Float v2) -> bool (v1 = v2)
      | Eq, E0 (String v1), E0 (String v2) -> bool (v1 = v2)
      | Eq, E0 (Bool v1), E0 (Bool v2) -> bool (v1 = v2)
      | Eq, E0 (Char v1), E0 (Char v2) -> bool (v1 = v2)
      | Eq, E0 (U8 v1), E0 (U8 v2) -> bool (Uint8.compare v1 v2 = 0)
      | Eq, E0 (U16 v1), E0 (U16 v2) -> bool (Uint16.compare v1 v2 = 0)
      | Eq, E0 (U24 v1), E0 (U24 v2) -> bool (Uint24.compare v1 v2 = 0)
      | Eq, E0 (U32 v1), E0 (U32 v2) -> bool (Uint32.compare v1 v2 = 0)
      | Eq, E0 (U40 v1), E0 (U40 v2) -> bool (Uint40.compare v1 v2 = 0)
      | Eq, E0 (U48 v1), E0 (U48 v2) -> bool (Uint48.compare v1 v2 = 0)
      | Eq, E0 (U56 v1), E0 (U56 v2) -> bool (Uint56.compare v1 v2 = 0)
      | Eq, E0 (U64 v1), E0 (U64 v2) -> bool (Uint64.compare v1 v2 = 0)
      | Eq, E0 (U128 v1), E0 (U128 v2) -> bool (Uint128.compare v1 v2 = 0)
      | Eq, E0 (I8 v1), E0 (I8 v2) -> bool (Int8.compare v1 v2 = 0)
      | Eq, E0 (I16 v1), E0 (I16 v2) -> bool (Int16.compare v1 v2 = 0)
      | Eq, E0 (I24 v1), E0 (I24 v2) -> bool (Int24.compare v1 v2 = 0)
      | Eq, E0 (I32 v1), E0 (I32 v2) -> bool (Int32.compare v1 v2 = 0)
      | Eq, E0 (I40 v1), E0 (I40 v2) -> bool (Int40.compare v1 v2 = 0)
      | Eq, E0 (I48 v1), E0 (I48 v2) -> bool (Int48.compare v1 v2 = 0)
      | Eq, E0 (I56 v1), E0 (I56 v2) -> bool (Int56.compare v1 v2 = 0)
      | Eq, E0 (I64 v1), E0 (I64 v2) -> bool (Int64.compare v1 v2 = 0)
      | Eq, E0 (I128 v1), E0 (I128 v2) -> bool (Int128.compare v1 v2 = 0)
      | Eq, E0 (Bit v1), E0 (Bit v2) -> bool (v1 = v2)
      | Eq, E0 (Size v1), E0 (Size v2) -> bool (v1 = v2)
      | Eq, E0 (Byte v1), E0 (Byte v2) -> bool (Uint8.compare v1 v2 = 0)
      | Eq, E0 (Word v1), E0 (Word v2) -> bool (Uint16.compare v1 v2 = 0)
      | Eq, E0 (DWord v1), E0 (DWord v2) -> bool (Uint32.compare v1 v2 = 0)
      | Eq, E0 (QWord v1), E0 (QWord v2) -> bool (Uint64.compare v1 v2 = 0)
      | Eq, E0 (OWord v1), E0 (OWord v2) -> bool (Uint128.compare v1 v2 = 0)
      | Eq, E0 (Bytes v1), E0 (Bytes v2) -> bool (v1 = v2)
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
      (* Those are created empty: *)
      | Member, _, E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _
                       | HashTable _ | Heap), _) -> bool false
      | AllocLst, e1, e2 ->
          (match E.type_of l e2 with
          | T.Value mn ->
              (match E.to_cst_int e1 with
              | exception _ -> E2 (AllocLst, e1, e2)
              | 0 -> make_lst mn [] |> p
              | _ -> E2 (AllocLst, e1, e2))
          | _ -> E2 (AllocLst, e1, e2))
      | PartialSort, e1, E0S ((MakeVec | MakeLst _), [])
      | PartialSort, (E0S ((MakeVec | MakeLst _), []) as e1), _ -> e1
      | GetVec, e1, e2 ->
          (match E.to_cst_int e1 with
          | exception _ ->
              E2 (GetVec, e1, e2)
          | i ->
              (match e2 with
              | E0S (MakeVec, es) ->
                  (try List.at es i
                  with Invalid_argument _ -> E2 (GetVec, e1, e2))
              | _ -> E2 (GetVec, e1, e2)))
      | Map, lst, f ->
          (match lst with
          | E0S (MakeVec, [ e ]) ->
              (* Unearth the MakeVec might makes further optimisations possible: *)
              make_vec [ apply f [ e ] ] |> p
          | _ ->
              if E.is_identity f then lst
              else E2 (Map, lst, f))
      | SplitBy, E0 (String s1), E0 (String s2) ->
          String.split_on_string s1 s2 |>
          List.map string |>
          make_lst T.(required (Mac String)) |>
          p
      | SplitAt, e1, E0 (String s) ->
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
      (* TODO: Cons! *)
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
      (* Do nothing if blitting nothing: *)
      | BlitByte, ptr, _, E0 (Size 0) -> ptr
      | FindSubstring, from_start, E0 (String s1), E0 (String s2) ->
          (* Let [p] optimize away this condition if the bool is known: *)
          let then_ =
            try not_null (u24 (Uint24.of_int (String.find s2 s1)))
            with Not_found -> null T.(Mac U24)
          and else_ =
            try not_null (u24 (Uint24.of_int (String.rfind s2 s1)))
            with Not_found -> null T.(Mac U24) in
          if_ from_start ~then_ ~else_ |> p
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
      | ReadWhile, E0 (Bool false), _, init, pos -> pair init pos
      | Repeat, (E0 (I32 f) as from), (E0 (I32 t) as to_), body, init ->
          let c = Int32.compare f t in
          if c >= 0 then init
          else if 0 = Int32.(compare t (succ f)) then
            apply body [ from ; init ] |>
            p
          else E4 (Repeat, from, to_, body, init)
      | op, e1, e2, e3, e4 -> E4 (op, e1, e2, e3, e4))
