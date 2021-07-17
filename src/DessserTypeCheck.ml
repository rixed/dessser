open Batteries
open Stdint

module E = DessserExpressions
module C = DessserConversions
module T = DessserTypes

(*$inject
  open Stdint
  module T = DessserTypes *)

(* [l] is the stack of expr * type *)
let rec type_check l =
  E.map_env l (fun l e0 ->
    let check_void l e =
      match E.type_of l e |> T.develop1 with
      | T.{ typ = Void ; nullable = false } -> ()
      | t -> raise (E.Type_error (e0, e, t, "be Void")) in
    let check_nullable b l e =
      match E.type_of l e |> T.develop1 with
      | { nullable ; _ } when nullable = b -> ()
      | t -> raise (E.Type_error (e0, e, t, "be a "^ (if b then "" else "not ") ^
                                          "nullable value")) in
    let rec is_comparable = function
      | T.{
          typ =
            (Size | Address | Mask | Bytes
            | Base (
                Float | String | Bool | Char |
                U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
                I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128)) ;
          nullable = false } ->
          true
      | { typ = Sum mns ; nullable = false } ->
          Array.for_all (fun (_, mn) -> is_comparable mn) mns
      | { nullable = true ; typ } ->
          is_comparable { nullable = false ; typ }
      | _ ->
          false in
    let check_comparable l e =
      let t = E.type_of l e |> T.develop_mn in
      if not (is_comparable t) then
        raise (E.Type_error (e0, e, t, "be comparable")) in
    let check_numeric ?(only_base=false) l e =
      match E.type_of l e |> T.develop1 with
      | T.{ typ = Size | Address ;
          nullable = false } when not only_base ->
          ()
      | { typ = Base (
            Float |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
            I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128) ;
          nullable = false } -> ()
      | t -> raise (E.Type_error (e0, e, t, "be numeric")) in
    let check_integer l e =
      match E.type_of l e |> T.develop1 with
      | T.{ typ =
            (Size | Address
            | Base (
                U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
                I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128)) ;
          nullable = false } -> ()
      | t -> raise (E.Type_error (e0, e, t, "be an integer")) in
    let is_unsigned = function
      | T.{ typ = (Size | Address |
                   Base (U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128)) ;
            nullable = false } ->
          true
      | _ ->
          false in
    let check_unsigned l e =
      let t = E.type_of l e |> T.develop1 in
      if not (is_unsigned t) then
        raise (E.Type_error (e0, e, t, "be an unsigned integer")) in
    let check_eq l e exp =
      let act = E.type_of l e in
      if not (T.eq_mn act exp) then
        let expected = T.mn_to_string exp in
        raise (E.Type_error (e0, e, act, "be a "^ expected)) in
    let check_same_types l e1 e2 =
      let t1 = E.type_of l e1 in
      check_eq l e2 t1 in
    let check_all_same_types l e1 e2s =
      List.iter (check_same_types l e1) e2s in
    let check_vector l e =
      ignore (E.get_item_type ~vec:true e0 l e) in
    let check_set l e =
      ignore (E.get_item_type ~set:true e0 l e) in
    let check_arr l e =
      ignore (E.get_item_type ~arr:true e0 l e) in
    let check_ordered_lst l e =
      ignore (E.get_item_type ~arr:true ~vec:true ~lst:true ~str:true ~bytes:true
                            e0 l e) in
    let check_any_lst l e =
      ignore (E.get_item_type ~arr:true ~vec:true ~set:true ~lst:true e0 l e) in
    let check_lst l e =
      match E.type_of l e |> T.develop1 with
      | T.{ typ = Lst _ ; nullable = false } -> ()
      | t -> raise (E.Type_error (e0, e, t, "be a lst")) in
    let check_lst_same_type e1 l e =
      match E.type_of l e |> T.develop1 with
      | T.{ typ = Lst mn ; nullable = false } -> check_eq l e1 mn
      | t -> raise (E.Type_error (e0, e, t, "be a lst")) in
    let check_sum l e =
      match E.type_of l e |> T.develop1 with
      | { typ = Sum _ ; nullable = false } -> ()
      | t -> raise (E.Type_error (e0, e, t, "be a union")) in
    (* Check that [f] signature correspond to the array of parameters *)
    let check_fun_sign = E.check_fun_sign e0 in
    let rec check_ip ?(rec_=false) l e t =
      (* Any 32 or 128 unsigned integer will do, or any sum of such thing,
       * but do not allow recursion in the sum type because code generator
       * won't deal with that. *)
      match t |> T.develop1 with
      | { typ = Base (U32 | U128) ; nullable = false } ->
          ()
      | { typ = Sum mns ; nullable = false } when rec_ = false ->
          Array.iter (fun (_, mn) -> check_ip ~rec_:true l e mn) mns
      | t -> raise (E.Type_error (e0, e, t, "be an ip")) in
    (* Convert and NullMap are the only operations that returns a different
     * expression: *)
    match e0 with
    | E1 (Convert to_, e1) ->
        let from = E.type_of l e1 |> T.develop_mn in
        let res = C.conv_mn ~from ~to_ e1 in
        type_check l res
    | E2 (NullMap (n, r), x, body) ->
        let res =
          let open E.Ops in
          if (E.type_of l x).T.nullable then
            let t = E.get_memo_mn r l (force x) in
            let l' = E.add_local n t l in
            let res_typ = (E.type_of l' body).T.typ in
            let_ ~name:"null_map" x (fun x ->
              if_null x
                ~then_:(null res_typ)
                ~else_:(E.E2 (Let (n, r), force x, not_null body)))
          else
            E.E2 (Let (n, r), x, body) in
        type_check l res
    | e0 ->
        (match e0 with
        | E1 (Convert _, _)
        | E2 (NullMap _, _, _) ->
            assert false (* Handled right above *)
        | E0 (Null _ | Myself _ | EndOfList _ | EmptySet _ | Now
             | RandomFloat | RandomU8 | RandomU32 | RandomU64 | RandomU128
             | Float _ | String _ | Bool _ | Char _
             | U8 _ | U16 _ | U24 _ | U32 _ | U40 _
             | U48 _ | U56 _ | U64 _ | U128 _
             | I8 _ | I16 _ | I24 _ | I32 _ | I40 _
             | I48 _ | I56 _ | I64 _ | I128 _
             | Size _ | Address _
             | Bytes _ | Identifier _ | ExtIdentifier _
             | Param _ | CopyField | SkipField | SetFieldNull)
        | E0S (Verbatim _, _)
        | E1 ((Comment _ | NotNull | Force _ | Dump | Identity | Ignore
              | Function _ | Hash | AllocVec _), _)
        | E2 ((Let _ | LetPair _), _, _) ->
            (* Subexpressions will be type checked recursively already *)
            ()
        | E0S (Seq, es) ->
            let rec loop = function
              | [] | [_] -> ()
              | e::es -> check_void l e ; loop es in
            loop es
        | E0S (MakeVec, []) ->
            raise (E.Struct_error (e0, "vector dimension must be > 0"))
        | E0S (MakeVec, e1 :: e2s) ->
            check_all_same_types l e1 e2s
        | E0S (MakeArr mn, e1s) ->
            List.iter (fun e1 -> check_eq l e1 mn) e1s
        | E0S (MakeTup, es) ->
            if List.compare_length_with es 2 < 0 then
              raise (E.Struct_error (e0, "tuple dimension must be ≥ 2"))
        | E0S (MakeRec, es) ->
            let len = List.length es in
            if len mod 2 <> 0 then
              raise (E.Struct_error (e0,
                "record expressions must have an even number of values")) ;
            if len < 2 then
              raise (E.Struct_error (e0, "record dimension must be ≥ 1")) ;
            List.iteri (fun i e ->
              if i mod 2 = 0 then ignore (E.field_name_of_expr e)
            ) es
        | E0S (MakeUsr name, es) ->
            ignore (E.apply_constructor e0 l name es)
        | E1S (Apply, f, es) ->
            check_fun_sign l f es
        | E1 (IsNull, e) ->
            check_nullable true l e
        | E2 ((Nth | UnsafeNth), e1, e2) ->
            check_integer l e1 ;
            check_ordered_lst l e2
        | E2 ((Gt | Ge | Eq | Min | Max), e1, e2) ->
            (* TODO: For Eq, also accept sets? *)
            check_comparable l e1 ;
            check_same_types l e1 e2
        | E2 ((Add | Sub | Mul), e1, e2) ->
            check_numeric l e1 ;
            check_same_types l e1 e2
        | E2 ((Div | UnsafeDiv | Rem | UnsafeRem | Pow | UnsafePow), e1, e2) ->
            check_numeric ~only_base:true l e1 ;
            check_same_types l e1 e2
        | E2 (Member, e1, e2) ->
            (match E.type_of l e2 |> T.develop1 with
            | { typ = (Vec (_, t) | Arr t | Set (_, t)) ; nullable = false } ->
                check_eq l e1 t
            | t ->
                raise (E.Type_error (e0, e2, t, "be a vector, array or set")))
        | E2 ((BitAnd | BitOr | BitXor), e1, e2) ->
            check_integer l e1 ;
            check_same_types l e1 e2
        | E2 ((LeftShift | RightShift), e1, e2) ->
            check_integer l e1 ;
            check_eq l e2 T.u8
        | E1 ((BitNot | StringOfInt), e) ->
            check_integer l e
        | E1 ((StringOfChar | U8OfChar), e) ->
            check_eq l e T.char
        | E1 (StringOfIp, e) ->
            check_ip l e (E.type_of l e)
        | E1 ((FloatOfString | U8OfString | U16OfString
             | U24OfString | U32OfString | U40OfString | U48OfString
             | U56OfString | U64OfString | U128OfString | I8OfString
             | I16OfString | I24OfString | I32OfString | I40OfString
             | I48OfString | I56OfString | I64OfString | I128OfString
             | StringLength | BytesOfString), e) ->
            check_eq l e T.string
        | E1 ((FloatOfPtr | CharOfPtr | U8OfPtr | U16OfPtr
             | U24OfPtr | U32OfPtr | U40OfPtr | U48OfPtr
             | U56OfPtr | U64OfPtr | U128OfPtr | I8OfPtr
             | I16OfPtr | I24OfPtr | I32OfPtr | I40OfPtr
             | I48OfPtr | I56OfPtr | I64OfPtr | I128OfPtr), e) ->
            check_eq l e T.ptr
        | E1 (FloatOfU64, e) ->
            check_eq l e T.u64
        | E1 ((U64OfFloat | StringOfFloat), e) ->
            check_eq l e T.float
        | E1 ((CharOfU8 | BoolOfU8), e) ->
            check_eq l e T.u8
        | E1 ((ToU8 | ToI8 | ToI16 | ToU16 | ToI24 | ToU24 | ToI32 | ToU32
             | ToI40 | ToU40 | ToI48 | ToU48 | ToI56 | ToU56 | ToI64 | ToU64
             | ToI128 | ToU128 | ToFloat), e) ->
            check_numeric l e
        | E1 (SizeOfU32, e) ->
            check_eq l e T.u32
        | E1 (AddressOfU64, e) ->
            check_eq l e T.u64
        | E1 (U32OfSize, e) ->
            check_eq l e T.size
        | E1 (U64OfAddress, e) ->
            check_eq l e T.address
        | E1 ((U8OfBool | Not | Assert), e) ->
            check_eq l e T.bool
        | E1 ((Abs | Neg), e) ->
            check_numeric l e
        | E1 ((Exp | Log | UnsafeLog | Log10 | UnsafeLog10 | Sqrt | UnsafeSqrt |
               Ceil | Floor | Round |
               Cos | Sin | Tan | ACos | ASin | ATan | CosH | SinH | TanH), e) ->
            check_eq l e T.float
        | E1 ((Lower | Upper), e) ->
            check_eq l e T.string
        | E1 ((ArrOfLst | ArrOfLstRev | SetOfLst), e) ->
            check_lst l e
        | E1 (ArrOfVec, e) ->
            check_vector l e
        | E1 (ArrOfSet, e) ->
            check_set l e
        | E1 (PtrOfString, e) ->
            check_eq l e T.string
        | E1 (PtrOfBuffer, e) ->
            check_eq l e T.size
        | E2 (PtrOfAddress, e1, e2) ->
            check_eq l e1 T.address ;
            check_eq l e2 T.size
        | E2 (While, cond, body) ->
            check_eq l cond T.bool ;
            check_eq l body T.void
        | E2 (ForEach _, lst, body) ->
            check_any_lst l lst ;
            check_eq l body T.void
        | E2 (Index, chr, str) ->
            check_eq l chr T.char ;
            check_eq l str T.string
        | E1 (GetEnv, e) ->
            check_eq l e T.string
        | E1 (GetMin, e) ->
            check_set l e
        | E2 (AppendByte, e1, e2) ->
            check_eq l e1 T.bytes ;
            check_eq l e2 T.u8
        | E2 (AppendBytes, e1, e2) ->
            check_eq l e1 T.bytes ;
            check_eq l e2 T.bytes
        | E2 ((AppendString | StartsWith | EndsWith | SplitBy), e1, e2) ->
            check_eq l e1 T.string ;
            check_eq l e2 T.string
        | E1 (StringOfBytes, e) ->
            check_eq l e T.bytes
        | E1 (Cardinality, e) ->
            check_any_lst l e
        | E2 (GetBit, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.size
        | E2 (ScaleWeights, set, d) ->
            check_set l set ;
            check_numeric l d
        | E2 (Strftime, fmt, time) ->
            check_eq l fmt T.string ;
            check_numeric l time
        | E3 (SetBit, e1, e2, e3) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.size ;
            check_eq l e3 T.bool
        | E3 (SetVec, e1, e2, e3) ->
            check_integer l e1 ;
            (match E.type_of l e2 |> T.develop1 with
            | { typ = (T.Vec (_, mn) | T.Arr mn) ; nullable = false } ->
                check_eq l e3 mn
            | t ->
                raise (E.Type_error (e0, e1, t, "be a vector")))
        | E1 ((ReadU8 | ReadU16 _ | ReadU32 _ | ReadU64 _ | ReadU128 _), e) ->
            check_eq l e T.ptr
        | E2 ((ReadBytes | PeekU8 | PeekU16 _ | PeekU32 _ | PeekU64 _
             | PeekU128 _), e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.size
        | E2 ((WriteU8 | PokeU8), e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.u8
        | E2 (WriteU16 _, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.u16
        | E2 (WriteU32 _, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.u32
        | E2 (WriteU64 _, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.u64
        | E2 (WriteU128 _, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.u128
        | E2 (WriteBytes, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.bytes
        | E3 (BlitByte, e1, e2, e3) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.u8 ;
            check_eq l e3 T.size
        | E2 (PtrAdd, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.size
        | E2 (PtrSub, e1, e2) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.ptr
        | E1 ((RemSize | Offset), e) ->
            check_eq l e T.ptr
        | E3 (PtrOfPtr, e1, e2, e3) ->
            check_eq l e1 T.ptr ;
            check_eq l e2 T.size ;
            check_eq l e3 T.size
        | E3 (FindSubstring, e1, e2, e3) ->
            check_eq l e1 T.bool ;
            check_eq l e2 T.string ;
            check_eq l e3 T.string
        | E2 ((And | Or), e1, e2) ->
            check_eq l e1 T.bool ;
            check_eq l e2 T.bool
        | E1 (GetItem _, _)
        | E1 (GetField _, _)
        | E1 (GetAlt _, _) ->
            (* everything checks already performed in [type_of] *)
            ignore (E.type_of l e0)
        | E1 (Construct (mns, i), e) ->
              let max_lbl = Array.length mns - 1 in
              if i < 0 || i > max_lbl then (
                let msg =
                  Printf.sprintf "Constructor (%d) must not be greater than %d"
                    i max_lbl in
                raise (E.Struct_error (e0, msg))
              ) ;
              check_eq l e (snd mns.(i))
        | E1 (Head, e) ->
            check_lst l e
        | E1 (Tail, e) ->
            check_lst l e
        | E2 (Cons, e1, e2) ->
            check_lst_same_type e1 l e2
        | E3 (Map, init, f, set) ->
            (match E.type_of l f |> T.develop1 with
            | T.{ typ = Function ([| init_t ; item_t |], _) ; nullable = false }
              as f_t ->
                check_eq l init init_t ;
                let check_fun_type_with mn =
                  (* FIXME: why isn't map allowed to change the item type?! *)
                  if not (T.eq_mn mn item_t) then (
                    let err = "be a function of "^ T.mn_to_string mn in
                    raise (E.Type_error (e0, f, f_t, err))) in
                (match E.type_of l set |> T.develop1 with
                | T.{ typ = Vec (_, mn) ; nullable = false } ->
                    check_fun_type_with mn
                | T.{ typ = Arr mn ; nullable = false } ->
                    check_fun_type_with mn
                | T.{ typ = Set (_, mn) ; nullable = false } ->
                    check_fun_type_with mn
                | T.{ typ = Lst mn ; nullable = false } ->
                    check_fun_type_with mn
                | t ->
                    raise (E.Type_error (e0, set, t, "be an iterable")))
            | { typ = Function _ ; nullable = false } as f_t ->
                raise (E.Type_error (e0, f, f_t, "be a function of one argument"))
            | t -> raise (E.Type_error (e0, f, t, "be a function")))
        | E3 (If, e1, e2, e3) ->
            check_eq l e1 T.bool ;
            check_same_types l e2 e3
        | E1 (MaskGet _, e1) ->
            check_eq l e1 T.mask
        | E1 (LabelOf, e1) ->
            check_sum l e1
        | E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _
              | HashTable _), e1) ->
            check_unsigned l e1
        | E1 (Heap, cmp) ->
            let cmp_t = E.type_of l cmp in
            let err msg =
              raise (E.Comparator_error (cmp, cmp_t, msg)) in
            (* TODO: We could also accept a Null comparison function if the items
             * are readily comparable (as in [is_comparable]). *)
            (match cmp_t with
            | T.{ typ = Function (ts, _) ; nullable = false } ->
                let ts_len = Array.length ts in
                if ts_len <> 2 then
                  err "must have two parameters" ;
                if not (T.eq_mn ts.(0) ts.(1)) then
                  err "parameters must have the same type"
            | _ ->
                err "must be a function")
        | E2 (Insert, set, x) ->
            (match E.type_of l set |> T.develop1 with
            | { typ = T.Set (_, mn) ; nullable = false } ->
                check_eq l x mn
            | t ->
                raise (E.Type_error (e0, set, t, "be a set")))
        | E2 (DelMin, set, n) ->
            check_set l set ;
            check_unsigned l n
        | E2 (SplitAt, e1, e2) ->
            check_unsigned l e1 ;
            check_eq l e2 T.string
        | E2 (Join, e1, e2) ->
            check_eq l e1 T.string ;
            let item_t = E.get_item_type ~arr:true ~vec:true e0 l e2 in
            if item_t <> T.(required (Base String)) then
              let msg = "be a list or vector of strings" in
              raise (E.Type_error (e0, e2, E.type_of l e2, msg))
        | E2 (AllocArr, e1, _) ->
            check_unsigned l e1
        | E2 (PartialSort, e1, e2) ->
            let item_t1 = E.get_item_type ~arr:true ~vec:true e0 l e1 in
            if not (is_comparable item_t1) then
              raise (E.Type_error (e0, e1, item_t1,
                                 "be a list or vector of comparable items")) ;
            let item_t2 = E.get_item_type ~arr:true ~vec:true e0 l e2 in
            if not (is_unsigned item_t2) then
              raise (E.Type_error (e0, e2, item_t2,
                                 "be a list or vector of unsigned integers"))
        | E2 ((ChopBegin | ChopEnd), arr, len) ->
            check_arr l arr ;
            check_unsigned l len
        | E3 (Top _, size, max_size, sigmas) ->
            check_unsigned l size ;
            check_unsigned l max_size ;
            check_numeric l sigmas
        | E3 (InsertWeighted, set, w, x) ->
            (match E.type_of l set |> T.develop1 with
            | { typ = T.Set (_, mn) ; nullable = false } ->
                check_eq l w T.float ;
                check_eq l x mn
            | t ->
                raise (E.Type_error (e0, set, t, "be a set")))
        | E3 (Substring, str, start, stop) ->
            check_eq l str T.string ;
            check_integer l start ;
            check_integer l stop
        ) ;
        e0
  )

(*$inject
  let pass_type_check s =
    let e = Parser.expr s |> List.hd in
    try ignore (type_check no_env e) ; true
    with _ -> false

  let sum_times ps =
    Unix.(ps.tms_utime +. ps.tms_stime +. ps.tms_cutime +. ps.tms_cstime)

  let tot_time f =
    let t1 = Unix.times () in
    f () ;
    let t2 = Unix.times () in
    sum_times t2 -. sum_times t1
*)

(*$T pass_type_check
   not (pass_type_check "(get-item 2 (read-u64 big-endian (u64 17)))")
   not (pass_type_check "(get-alt \"pejh\" (random-float))")
   not (pass_type_check "(apply (string \"blabla\") (bool false))")
*)

(*$T type_check
  tot_time (fun () -> \
    let z = Ops.u8_of_int 0 in \
    let rec loop n e = \
      if n <= 0 then e else \
      let_ (loop (n-1) z) (fun v -> \
        Ops.add v (loop (n-1) e)) in \
    let e = loop 10 z in \
    ignore (type_check no_env e)) < 10.
*)
