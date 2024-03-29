open Batteries
open Stdint

open DessserMiscTypes
open DessserTools
module T = DessserTypes

(*$inject
  open Stdint
  module T = DessserTypes *)

(* Controls whether [debug] translates into Dump or Ignore: *)
let dump_debug = ref false

type t = T.expr

let rec e0_eq e1 e2 =
  match e1, e2 with
  | T.Null t1, T.Null t2 ->
      T.eq t1 t2
  | EndOfList mnt1, EndOfList mnt2 ->
      T.eq_mn mnt1 mnt2
  | EmptySet mnt1, EmptySet mnt2 ->
      T.eq_mn mnt1 mnt2
  | e1, e2 ->
      (* Assuming here and below that when the constructors are different
       * the generic equality does not look t the fields and therefore won't
       * encounter functional values: *)
      e1 = e2

and e0s_eq e1 e2 = e1 = e2

and e0r_eq e1 e2 = e1 = e2

and e1_eq e1 e2 =
  match e1, e2 with
  | T.Function typ1, T.Function typ2 ->
      array_for_all2_no_exc T.eq_mn typ1 typ2
  | e1, e2 -> e1 = e2

and e1s_eq e1 e2 = e1 = e2

and e2_eq e1 e2 = e1 = e2

and e3_eq e1 e2 = e1 = e2

and e4_eq e1 e2 = e1 = e2

and eq e1 e2 =
  let eq_lst e1s e2s =
    try List.for_all2 eq e1s e2s
    with Invalid_argument _ -> false in
  match e1, e2 with
  | T.E0 op1, T.E0 op2 ->
      e0_eq op1 op2
  | E0S (op1, e1s), E0S (op2, e2s) ->
      e0s_eq op1 op2 &&
      (try List.for_all2 (eq) e1s e2s
      with Invalid_argument _ -> false)
  | E0R (op1, e1s), E0R (op2, e2s) ->
      e0r_eq op1 op2 &&
      (try Array.for_all2 (eq) e1s e2s
      with Invalid_argument _ -> false)
  | E1 (op1, e11), E1 (op2, e21) ->
      e1_eq op1 op2 && eq e11 e21
  | E1S (op1, e11, e1s), E1S (op2, e21, e2s) ->
      e1s_eq op1 op2 && eq e11 e21 && eq_lst e1s e2s
  | E2 (op1, e11, e12), E2 (op2, e21, e22) ->
      e2_eq op1 op2 && eq e11 e21 && eq e12 e22
  | E3 (op1, e11, e12, e13), E3 (op2, e21, e22, e23) ->
      e3_eq op1 op2 && eq e11 e21 && eq e12 e22 && eq e13 e23
  | _ -> false

(* [i] a set of identifiers (names) that can.
 * [has_function_body] is set whenever we enter a function body, and imply
 * all parameters are known. *)
let rec can_precompute ?(has_function_body=false) i = function
  | T.E0 (Now | RandomFloat | RandomU8 | RandomU32 | RandomU64 | RandomU128) ->
      false
  | E0 (Null _ | EndOfList _ | EmptySet _ | Float _ | String _ | Bool _
       | U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _
       | I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _
       | Char _ | Size _ | Address _
       | Bytes _ | CopyField | SkipField | SetFieldNull | NoReturn _
       | ExtIdentifierUnmanaged _ | ExtIdentifierManaged _) ->
      true
  | E0 (Param _) ->
      has_function_body
  | E0 (Myself _) ->
      true
  | E0 (Identifier n) ->
      List.mem n i
  | E0S (_, es) ->
      List.for_all (can_precompute ~has_function_body i) es
  | E0R (_, es) ->
      Array.for_all (can_precompute ~has_function_body i) es
  | E1 (Function _, body) ->
      can_precompute ~has_function_body i body
  | E1 ((Dump | Assert | MaskGet _), _) ->
      false
  | E1 (_, e) -> can_precompute ~has_function_body i e
  | E1S (Apply, E1 (Function _, e1), e2s)
  | E1S ((CopyRec | CopyTup), e1, e2s) ->
      List.for_all (can_precompute ~has_function_body i) e2s &&
      can_precompute ~has_function_body:true i e1
  | E1S (Apply, _, _) ->
      false
  | E2 (Let (n, _), e1, e2) ->
      can_precompute ~has_function_body i e1 &&
      can_precompute ~has_function_body (n :: i) e2
  | E2 (LetPair (n1, _, n2, _), e1, e2) ->
      can_precompute ~has_function_body i e1 &&
      can_precompute ~has_function_body (n1 :: n2 :: i) e2
  | E2 (ForEach (n, _), e1, e2) ->
      can_precompute ~has_function_body i e1 &&
      can_precompute ~has_function_body (n :: i) e2
  | E2 (_, e1, e2) ->
      can_precompute ~has_function_body i e1 &&
      can_precompute ~has_function_body i e2
  | E3 (Top _, _, _, _) ->
      false (* TODO *)
  | E3 (_, e1, e2, e3) ->
      can_precompute ~has_function_body i e1 &&
      can_precompute ~has_function_body i e2 &&
      can_precompute ~has_function_body i e3

(*
 * User-defined constructors for user-defined types.
 *
 * The constructor expression must be a non empty list of functions, all
 * returning the same type but with distinct signature.
 * [make-usr "foo" x y] is like [apply foo_constr x y] matching the type for
 * [x y] but returns the user type "foo" instead of the underlying type.
 *)

let user_constructors : (string, t list) Hashtbl.t = Hashtbl.create 50

let is_const_null = function
  | T.E0 (Null _) -> true
  | _ -> false

(* Given a type, returns the simplest expression of that type - suitable
 * whenever a default value is required. *)
let rec default ?(allow_null=true) t =
  let default = default ~allow_null
  and default_mn = default_mn ~allow_null in
  match t with
  | T.TUnknown | TExt _ | TPtr | TAddress ->
      Printf.sprintf2 "No default value for type %a"
        T.print t |>
      invalid_arg
  | TNamed (_, t) ->
      default t
  | TThis n ->
      let t = T.find_this n in
      default t
  | TVoid ->
      T.E0S (Seq, [])
  | TFloat ->
      E0 (Float 0.)
  | TString ->
      E0 (String "")
  | TBool ->
      E0 (Bool false)
  | TChar ->
      E0 (Char '\000')
  | TI8 ->
      E0 (I8 Int8.zero)
  | TI16 ->
      E0 (I16 Int16.zero)
  | TI24 ->
      E0 (I24 Int24.zero)
  | TI32 ->
      E0 (I32 Int32.zero)
  | TI40 ->
      E0 (I40 Int40.zero)
  | TI48 ->
      E0 (I48 Int48.zero)
  | TI56 ->
      E0 (I56 Int56.zero)
  | TI64 ->
      E0 (I64 Int64.zero)
  | TI128 ->
      E0 (I128 Int128.zero)
  | TU8 ->
      E0 (U8 Uint8.zero)
  | TU16 ->
      E0 (U16 Uint16.zero)
  | TU24 ->
      E0 (U24 Uint24.zero)
  | TU32 ->
      E0 (U32 Uint32.zero)
  | TU40 ->
      E0 (U40 Uint40.zero)
  | TU48 ->
      E0 (U48 Uint48.zero)
  | TU56 ->
      E0 (U56 Uint56.zero)
  | TU64 ->
      E0 (U64 Uint64.zero)
  | TU128 ->
      E0 (U128 Uint128.zero)
  | TUsr nn ->
      default_mn T.(required nn.def)
  | TTup mns ->
      E0S (
        MakeTup,
        Array.map default_mn mns |>
        Array.to_list)
  | TRec mns ->
      E0S (
        MakeRec,
        Array.fold_left (fun fields (fn, mn) ->
          T.E0 (String fn) :: default_mn mn :: fields
        ) [] mns)
  | TSum mns ->
      assert (Array.length mns > 0) ;
      T.E1 (
        Construct (mns, 0),
        default_mn (snd mns.(0)))
  | TVec (dim, mn) ->
      T.E0R (
        MakeVec,
        Array.init dim (fun _ -> default_mn mn))
  | TArr mn ->
      T.E0R (MakeArr mn, [||])
  | TSet (Simple, mn) ->
      T.E0 (EmptySet mn)
  | TSet (Sliding, mn) ->
      T.E1 (SlidingWindow mn, E0 (U8 Uint8.zero))
  | TSet (Tumbling, mn) ->
      T.E1 (TumblingWindow mn, E0 (U8 Uint8.zero))
  | TSet (Sampling, mn) ->
      T.E1 (Sampling mn, E0 (U8 Uint8.zero))
  | TSet (HashTable, mn) ->
      T.E1 (HashTable mn, E0 (U8 Uint8.zero))
  | TSet (Heap, mn) ->
      let cmp = T.E0S (Seq, [ E1 (Ignore, (E0 (Param 0))) ;
                              E1 (Ignore, (E0 (Param 1))) ]) in
      T.E1 (Heap, E1 (Function [| mn ; mn |], cmp))
  | TSet (Top, mn) ->
      let size = T.E0 (U8 Uint8.one) in
      let max_size = size
      and sigmas = T.E0 (Float 0.) in
      T.E3 (Top mn, size, max_size, sigmas)
  | TMap _ ->
      assert false (* no value of map type *)
  | TBytes ->
      E0 (Bytes (Bytes.create 0))
  | TMask ->
      E0 SkipField
  | TLst mn ->
      E0 (EndOfList mn)
  | TSize ->
      E0 (Size 0)
  | TFunction _ ->
      todo "default functions"

(* Given a type, returns the simplest expression of that type - suitable
 * whenever a default value is required. *)
and default_mn ?(allow_null=true) mn =
  (* In some places we want the whole tree of values to be populated. *)
  match mn.nullable, allow_null with
  | true, true -> E0 (Null mn.typ)
  | true, false -> E1 (NotNull, default ~allow_null mn.typ)
  | false, _ -> default ~allow_null mn.typ

(* Display in a single line to help with tests. *)
let print = T.print_expr

let to_string ?max_depth e =
  IO.to_string (print ?max_depth) e

let rec pretty_print ?max_depth fmt e =
  if Option.map_default (fun m -> m <= 0) false max_depth then
    Format.fprintf fmt "…"
  else
    let max_depth = Option.map pred max_depth in
    let p s es =
      Format.fprintf fmt "@[<hov 2>(%s" s ;
      List.iter (fun e ->
        Format.fprintf fmt "@ %a" (pretty_print ?max_depth) e
      ) es ;
      Format.fprintf fmt ")@]" in
    match e with
    | T.E0 op ->
        p (T.string_of_e0 op) []
    | E0S (Seq, []) ->
        p "nop" []
    | E0S (op, es) ->
        p (T.string_of_e0s op) es
    | E0R (op, es) ->
        p (T.string_of_e0r op) (Array.to_list es)
    | E1 (op, e1) ->
        p (T.string_of_e1 op) [ e1 ]
    | E1S (op, e1, es) ->
        p (T.string_of_e1s op) (e1 :: es)
    | E2 (op, e1, e2) ->
        p (T.string_of_e2 op) [ e1 ; e2 ]
    | E3 (op, e1, e2, e3) ->
        p (T.string_of_e3 op) [ e1 ; e2 ; e3 ]

let to_pretty_string ?max_depth e =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  (* Seems to do nothing but still: *)
  Format.pp_set_max_indent fmt 240 ;
  Format.pp_set_margin fmt 230 ;
  Format.fprintf fmt "@.    @[<hov 4>%a@]@." (pretty_print ?max_depth) e ;
  Buffer.contents buf

let to_cst_int =
  let m = max_int in
  function
  | T.E0 (U8 n) -> Uint8.to_int n
  | E0 (U16 n) -> Uint16.to_int n
  | E0 (U24 n) -> Uint24.to_int n
  | E0 (U32 n) when Uint64.(compare (of_uint32 n) (of_int m) <= 0) -> Uint32.to_int n
  | E0 (U40 n) when Uint64.(compare (of_uint40 n) (of_int m) <= 0) -> Uint40.to_int n
  | E0 (U48 n) when Uint64.(compare (of_uint48 n) (of_int m) <= 0) -> Uint48.to_int n
  | E0 (U56 n) when Uint64.(compare (of_uint56 n) (of_int m) <= 0) -> Uint56.to_int n
  | E0 (U64 n) when Uint64.(compare n (of_int m) <= 0) -> Uint64.to_int n
  | E0 (U128 n) when Uint128.(compare n (of_int m) <= 0) -> Uint128.to_int n
  | E0 (I8 n) -> Int8.to_int n
  | E0 (I16 n) -> Int16.to_int n
  | E0 (I24 n) -> Int24.to_int n
  | E0 (I32 n) when Int64.(compare (of_int32 n) (of_int m) <= 0) -> Int32.to_int n
  | E0 (I40 n) when Int64.(compare (of_int40 n) (of_int m) <= 0) -> Int40.to_int n
  | E0 (I48 n) when Int64.(compare (of_int48 n) (of_int m) <= 0) -> Int48.to_int n
  | E0 (I56 n) when Int64.(compare (of_int56 n) (of_int m) <= 0) -> Int56.to_int n
  | E0 (I64 n) when Int64.(compare n (of_int m) <= 0) -> Int64.to_int n
  | E0 (I128 n) when Int128.(compare n (of_int m) <= 0) -> Int128.to_int n
  | E0 (Size n) -> n
  | E0 (Address n) when Uint64.(compare n (of_int m) <= 0) -> Uint64.to_int n
  | _ -> invalid_arg "to_cst_int"

let has_side_effect = function
  | T.E0 (RandomFloat | RandomU8 | RandomU32 | RandomU64 | RandomU128)
  | E1 ((Dump | ReadU8 | ReadU16 _ |
         ReadU32 _ | ReadU64 _ |ReadU128 _ | Assert |
         FloatOfPtr | CharOfPtr | U8OfPtr | U16OfPtr |
         U24OfPtr | U32OfPtr | U40OfPtr | U48OfPtr |
         U56OfPtr | U64OfPtr | U128OfPtr | I8OfPtr |
         I16OfPtr | I24OfPtr | I32OfPtr | I40OfPtr |
         I48OfPtr | I56OfPtr | I64OfPtr | I128OfPtr |
         AllocVec _), _)
  | E1S (Apply, E0 (Identifier _ |
         ExtIdentifierUnmanaged _ | ExtIdentifierManaged _), _)
  | E2 ((ReadBytes | WriteU8 | WriteBytes | WriteU16 _ | WriteU32 _ |
         WriteU64 _ | WriteU128 _ | PokeU8 | PtrAdd |
         Insert | DelMin | AllocArr | PartialSort), _, _)
  | E3 ((SetBit | SetVec | BlitByte | InsertWeighted), _, _, _) ->
      true
  | _ ->
      false

(* Some expressions that have no side effect still depends on side effects,
 * sur as when reading a container that can be modified. those should not be
 * reordered with side effectful operations.
 * The simplifying assumption is made that every side effectful operation also
 * depends on side effects. *)
let depends_on_side_effect e =
  has_side_effect e ||
  match e with
  | T.E0R ((MakeVec | MakeArr _), es) when Array.length es > 0 ->
      (* Because those are mutable: *)
      true
  | E2 ((Nth | UnsafeNth), _, _) (* Only GetVec really (FIXME) *)
  | E2 ((PeekU8 | PeekU16 _ | PeekU32 _ | PeekU64 _ | PeekU128 _), _, _) ->
      true
  | _ ->
      false

let can_duplicate e =
  not (has_side_effect e) &&
  match e with
  (* Although not exactly a side effect, those functions produce a copy of
   * a given pointer that are then mutable and which address is used in
   * comparisons (exception: empty things are not mutable): *)
  | T.E0R ((MakeVec | MakeArr _), es) when Array.length es > 0 ->
      false
  | E0S ((MakeTup | MakeRec | MakeUsr _), _::_)
  | E1 ((PtrOfString | PtrOfBuffer), _)
  | E2 (PtrOfAddress, _, _)
  | E3 (PtrOfPtr, _, _, _)
  (* Similarly, sets and vec are mutable: *)
  | E0 (EmptySet _)
  | E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _ | HashTable _ |
         Heap | DecimalStringOfFloat), _)
  | E3 (Top _, _, _, _)
  (* Expensive: *)
  | E1 ((ArrOfLst | ArrOfLstRev | SetOfLst | ArrOfVec | ArrOfSet |
         Truncate _), _)
  | E2 ((While | ForEach _), _, _)
  | E3 ((FindSubstring | SubString), _, _, _) ->
      false
  | _ ->
      true

(* depth last, pass the list of bound identifiers along the way: *)
let rec fold u f e =
  let u = f u e in
  match e with
  | T.E0 _ ->
      u
  | E0S (_, es) ->
      List.fold_left (fun u e1 -> fold u f e1) u es
  | E0R (_, es) ->
      Array.fold_left (fun u e1 -> fold u f e1) u es
  | E1 (_, e1) ->
      fold u f e1
  | E1S (_, e1, es) ->
      let u = fold u f e1 in
      List.fold_left (fun u e1 -> fold u f e1) u es
  | E2 (_, e1, e2) ->
      fold (fold u f e1) f e2
  | E3 (_, e1, e2, e3) ->
      fold (fold (fold u f e1) f e2) f e3

let iter f e =
  fold () (fun () e -> f e) e

let size e =
  fold 0 (fun c _ -> c + 1) e

let is_smaller_than n e =
  try
    fold 0 (fun c _ ->
      if c >= n then raise Exit
      else c + 1
    ) e |> ignore ;
    true
  with Exit ->
    false

let for_any p e =
  try
    iter (fun e ->
      if p e then raise Exit
    ) e ;
    false
  with Exit ->
    true

let for_all p e =
  not (for_any (not % p) e)

(* Global and local environment. Variables of that type are usually called "l".
 * Notice that since there are no closures, the local environment is emptied
 * at function entry. *)
type binding =
  { def : T.expr option ;
    mn : T.mn }

let make_binding def mn =
  { def ; mn }

type env =
  { global : (t * binding) list ;
    local : (t * binding) list ;
    (* Name of the current function, so backends can implement "Myself" to emit
     * a recursive call: *)
    name : string option }

let no_env = { global = [] ; local = [] ; name = None }

exception Type_error of t * t * T.mn * string
exception Type_error_param of t * t * int * T.mn * string
exception Struct_error of t * string
exception Apply_error of t * string
exception Comparator_error of t * T.mn * string
exception Unbound_identifier of t * env
exception Unbound_parameter of t * int * env
exception Invalid_expression of t * string
exception Redefinition of string
exception Invalid_truncate of t * string

(* expr must be a plain string: *)
let field_name_of_expr = function
  | T.E0 (String s) -> s
  | e -> raise (Struct_error (e, "record names must be constant strings"))

let defined n l =
  let def =
    List.exists (function
      | T.E0 (Identifier n' | ExtIdentifierUnmanaged n'), _
        when n' = n ->
          true
      | _ ->
          false) in
  def l.local || def l.global

let print_environment oc l =
  let p oc (id, binding) =
    Printf.fprintf oc "%a:%a"
      (print ~max_depth:2) id
      T.print_mn binding.mn in
  pretty_list_print p oc (l.global @ l.local)

let rec enter_function ?name ?ts ?es l =
  if ts = None && es = None then invalid_arg "enter_function" ;
  let ts =
    match ts with
    | Some ts -> ts
    | None -> List.enum (Option.get es) /@ type_of l |> Array.of_enum in
  { l with local = Array.fold_lefti (fun l i mn ->
                     (
                       T.E0 (Param i),
                       make_binding (Option.map (fun es -> List.nth es i) es) mn
                     ) :: l
                   ) [] ts ;
           name }

and add_local n ?e mn l =
  (* Make sure there is no shadowing: *)
  if defined n l then raise (Redefinition n) ;
  { l with local = (E0 (Identifier n), make_binding e mn) :: l.local }

(* Returns the type of [e0]. [l] is the environment.
 * Will try hard to find a type, even in the presence of unbound identifiers.
 * This is how recursive functions are typed. *)
and type_of l e0 =
  let find_id l e =
    List.find_map (fun (id, binding) ->
      if eq id e then Some binding.mn else None
    ) l in
  let find_id_type l e =
    try find_id l.local e
    with Not_found ->
      try find_id l.global e
      with Not_found ->
        raise (Unbound_identifier (e, l)) in
  let check_get_item n max_n =
    if n < 0 || n >= max_n then
      raise (Struct_error (e0, "no item #"^ string_of_int n ^" (only "^
                               string_of_int max_n ^" items)")) in
  let no_such_field name names =
    let msg =
      Printf.sprintf2 "no field named %s (have %a)"
        name
        (pretty_enum_print String.print) names in
    raise (Struct_error (e0, msg)) in
  let either e1 e2 =
    try type_of l e1
    with Unbound_parameter _ -> type_of l e2
  in
  match e0 with
  | T.E0 (Null typ) -> T.(optional typ)
  | E0 (Myself out) ->
      let num_params =
        List.fold_left (fun n (e, _) ->
          match e with T.E0 (Param _) -> n + 1 | _ -> n
        ) 0 l.local in
      let ins = Array.make num_params T.void in
      List.iter (function
        | T.E0 (Param n), binding -> ins.(n) <- binding.mn
        | _ -> ()
      ) l.local ;
      T.(required (TFunction (ins, out)))
  | E0 (EndOfList t) -> T.required (T.lst t)
  | E0 (EmptySet mn) -> T.required (T.set Simple mn)
  | E0 Now -> T.float
  | E0 RandomFloat -> T.float
  | E0 RandomU8 -> T.u8
  | E0 RandomU32 -> T.u32
  | E0 RandomU64 -> T.u64
  | E0 RandomU128 -> T.u128
  | E0 (Float _) -> T.float
  | E0 (String _) -> T.string
  | E0 (Bool _) -> T.bool
  | E0 (Char _) -> T.char
  | E0 (U8 _) -> T.u8
  | E0 (U16 _) -> T.u16
  | E0 (U24 _) -> T.u24
  | E0 (U32 _) -> T.u32
  | E0 (U40 _) -> T.u40
  | E0 (U48 _) -> T.u48
  | E0 (U56 _) -> T.u56
  | E0 (U64 _) -> T.u64
  | E0 (U128 _) -> T.u128
  | E0 (I8 _) -> T.i8
  | E0 (I16 _) -> T.i16
  | E0 (I24 _) -> T.i24
  | E0 (I32 _) -> T.i32
  | E0 (I40 _) -> T.i40
  | E0 (I48 _) -> T.i48
  | E0 (I56 _) -> T.i56
  | E0 (I64 _) -> T.i64
  | E0 (I128 _) -> T.i128
  | E0 (Size _) -> T.size
  | E0 (Address _) -> T.address
  | E0 (Bytes _) -> T.bytes
  | E0S (Seq, []) | E1 ((Dump | Ignore), _) ->
      T.void
  | E0S (Seq, es) ->
      type_of l (List.last es)
  | E0R (MakeVec, [||]) ->
      raise (Struct_error (e0, "vector dimension must be > 1"))
  | E0R (MakeVec, es) ->
      T.required (TVec (Array.length es, type_of l es.(0)))
  | E0R (MakeArr mn, _) ->
      T.required (TArr mn)
  | E0S (MakeTup, es) ->
      T.required (TTup (List.enum es /@ type_of l |> Array.of_enum))
  | E0S (MakeRec, es) ->
      let prev_name, mns =
        List.fold_left (fun (prev_name, mns) e ->
          match prev_name with
          | None ->
              Some (field_name_of_expr e), mns
          | Some name ->
              None, (name, type_of l e) :: mns
        ) (None, []) es in
      if prev_name <> None then
        raise (Struct_error (e0,
          "record expressions must have an even number of values")) ;
      let mns = List.rev mns in
      T.required (TRec (Array.of_list mns))
  | E0S (MakeUsr n, _) ->
      T.(required (get_user_type n))
  | E0S (Verbatim (_, t), _) -> t
  | E1S (Apply, f, _) ->
      (match type_of l f with
      | T.{ typ = TFunction (_, mn) ; nullable = false ; _ } -> mn
      | t -> raise (Type_error (e0, f, t, "be a function")))
  | E1S (CopyRec, r, with_) ->
      (match type_of l r |> T.develop1 with
      | T. { typ = TRec mns ; nullable = false ; _ } as mn ->
          let mns' =
            Array.map (fun (name, mn) ->
              name,
              match list_find_after (fun e ->
                      field_name_of_expr e = name
                    ) with_ with
              | exception Not_found -> mn
              | new_val -> type_of l new_val
            ) mns in
          { mn with typ = TRec mns' }
      | t -> raise (Type_error (e0, r, t, "be a record")))
  | E1S (CopyTup, r, with_) ->
      (match type_of l r |> T.develop1 with
      | T. { typ = TTup mns ; nullable = false ; _ } as mn ->
          let mns' =
            Array.mapi (fun i mn ->
              match list_find_after (fun e ->
                      to_cst_int e = i
                    ) with_ with
              | exception Not_found -> mn
              | new_val -> type_of l new_val
            ) mns in
          { mn with typ = TTup mns' }
      | t -> raise (Type_error (e0, r, t, "be a record")))
  | E1 (GetItem n, E0S (MakeTup, es)) -> (* Shortcut: *)
      check_get_item n (List.length es) ;
      type_of l (List.nth es n)
  | E1 (GetItem n, e1) ->
      (match type_of l e1 |> T.develop1 with
      | { typ = TTup mns ; nullable = false ; _ } ->
          let num_n = Array.length mns in
          check_get_item n num_n ;
          mns.(n)
      | t ->
          raise (Type_error (e0, e1, t, "be a tuple")))
  | E1 (GetField name, E0S (MakeRec, es)) ->
      let rec loop = function
        | T.E0 (String n) :: e :: rest ->
            if n = name then e else loop rest
        | _ ->
            let names =
              (List.enum es |> Enum.mapi (fun i e -> i, e)) //@
              (fun (i, e) ->
                if i mod 2 = 0 then Some (field_name_of_expr e) else None) in
            no_such_field name names in
      type_of l (loop es)
  | E1 (GetField name, e1) ->
      (match type_of l e1 |> T.develop1 with
      | { typ = TRec mns ; nullable = false } ->
          (try array_assoc name mns
           with Not_found ->
              no_such_field name (Array.enum mns /@ fst))
      | t ->
          raise (Type_error (e0, e1, t, "be a record")))
  | E1 ((GetAlt name), e1) ->
      (match type_of l e1 |> T.develop1 with
      | { typ = TSum mns ; nullable = false } ->
          (try array_assoc name mns
           with Not_found ->
              raise (Struct_error (e0, "no alternative named "^ name)))
      | t -> raise (Type_error (e0, e1, t, "be a union")))
  | E1 ((Construct (mns, _)), _) ->
      T.required (TSum mns)
  | E2 (Nth, _, e2) ->
      get_item_type ~arr:true ~vec:true ~set:true ~lst:true ~bytes:true
                    ~str:true e0 l e2 |>
      T.to_nullable
  | E2 (UnsafeNth, _, e2) ->
      get_item_type ~arr:true ~vec:true ~set:true ~lst:true ~bytes:true
                    ~str:true e0 l e2
  | E2 ((Add | Sub | Mul | BitAnd | BitOr | BitXor |
         UnsafeDiv | UnsafeRem | UnsafePow), e1, e2) ->
      either e1 e2
  | E1 (Comment _, e)
  | E2 ((LeftShift | RightShift), e, _) ->
      type_of l e
  | E1 (BitNot, e) ->
      type_of l e
  | E2 ((Div | Rem | Pow), e1, e2) ->
      T.to_nullable (either e1 e2)
  | E1 (NotNull, e) ->
      T.to_nullable (type_of l e)
  | E1 (Force _, e) ->
      T.force (type_of l e)
  | E1 (IsNull, _) -> T.bool
  | E2 (Gt, _, _) -> T.bool
  | E2 (Ge, _, _) -> T.bool
  | E2 (Eq, _, _) -> T.bool
  | E1 (StringOfFloat, _)
  | E1 (DecimalStringOfFloat, _)
  | E1 (StringOfChar, _)
  | E1 (StringOfInt, _) -> T.string
  | E1 (StringOfIp, _) -> T.string
  | E1 (FloatOfString, _) -> T.nfloat
  | E1 (U8OfString, _) -> T.nu8
  | E1 (U16OfString, _) -> T.nu16
  | E1 (U24OfString, _) -> T.nu24
  | E1 (U32OfString, _) -> T.nu32
  | E1 (U40OfString, _) -> T.nu40
  | E1 (U48OfString, _) -> T.nu48
  | E1 (U56OfString, _) -> T.nu56
  | E1 (U64OfString, _) -> T.nu64
  | E1 (U128OfString, _) -> T.nu128
  | E1 (I8OfString, _) -> T.ni8
  | E1 (I16OfString, _) -> T.ni16
  | E1 (I24OfString, _) -> T.ni24
  | E1 (I32OfString, _) -> T.ni32
  | E1 (I40OfString, _) -> T.ni40
  | E1 (I48OfString, _) -> T.ni48
  | E1 (I56OfString, _) -> T.ni56
  | E1 (I64OfString, _) -> T.ni64
  | E1 (I128OfString, _) -> T.ni128
  | E1 (CharOfPtr, _) -> T.pair T.char T.ptr
  | E1 (FloatOfPtr, _) -> T.pair T.float T.ptr
  | E1 (U8OfPtr, _) -> T.pair T.u8 T.ptr
  | E1 (U16OfPtr, _) -> T.pair T.u16 T.ptr
  | E1 (U24OfPtr, _) -> T.pair T.u24 T.ptr
  | E1 (U32OfPtr, _) -> T.pair T.u32 T.ptr
  | E1 (U40OfPtr, _) -> T.pair T.u40 T.ptr
  | E1 (U48OfPtr, _) -> T.pair T.u48 T.ptr
  | E1 (U56OfPtr, _) -> T.pair T.u56 T.ptr
  | E1 (U64OfPtr, _) -> T.pair T.u64 T.ptr
  | E1 (U128OfPtr, _) -> T.pair T.u128 T.ptr
  | E1 (I8OfPtr, _) -> T.pair T.i8 T.ptr
  | E1 (I16OfPtr, _) -> T.pair T.i16 T.ptr
  | E1 (I24OfPtr, _) -> T.pair T.i24 T.ptr
  | E1 (I32OfPtr, _) -> T.pair T.i32 T.ptr
  | E1 (I40OfPtr, _) -> T.pair T.i40 T.ptr
  | E1 (I48OfPtr, _) -> T.pair T.i48 T.ptr
  | E1 (I56OfPtr, _) -> T.pair T.i56 T.ptr
  | E1 (I64OfPtr, _) -> T.pair T.i64 T.ptr
  | E1 (I128OfPtr, _) -> T.pair T.i128 T.ptr
  | E1 (FloatOfU64, _) -> T.float
  | E1 (U64OfFloat, _) -> T.u64
  | E1 (U8OfChar, _) -> T.u8
  | E1 (CharOfU8, _) -> T.char
  | E1 (SizeOfU32, _) -> T.size
  | E1 (U32OfSize, _) -> T.u32
  | E1 (AddressOfU64, _) -> T.address
  | E1 (U64OfAddress, _) -> T.u64
  | E1 ((ArrOfLst | ArrOfLstRev), e) ->
      (match type_of l e |> T.develop1 with
      | T.{ typ = TLst mn ; nullable = false ; _ } ->
          T.(required (arr mn))
      | t -> raise (Type_error (e0, e, t, "be a lst")))
  | E1 (SetOfLst, e) ->
      (match type_of l e |> T.develop1 with
      | T.{ typ = TLst mn ; nullable = false ; _ } ->
          T.(required (set Simple mn))
      | t -> raise (Type_error (e0, e, t, "be a lst")))
  | E1 (ArrOfVec, e) ->
      (match type_of l e |> T.develop1 with
      | T.{ typ = TVec (_, mn) ; nullable = false ; _ } ->
          T.(required (arr mn))
      | t ->
          raise (Type_error (e0, e, t, "be a vec")))
  | E1 (ArrOfSet, e) ->
      (match type_of l e |> T.develop1 with
      | T.{ typ = TSet (_, mn) ; nullable = false ; _ } ->
          T.(required (arr mn))
      | t ->
          raise (Type_error (e0, e, t, "be a set")))
  | E1 (U8OfBool, _) -> T.u8
  | E1 (BoolOfU8, _) -> T.bool
  | E2 (AppendByte, _, _) -> T.bytes
  | E2 (AppendBytes, _, _) -> T.bytes
  | E2 (AppendString, _, _) -> T.string
  | E2 ((StartsWith | EndsWith), _, _) -> T.bool
  | E1 (StringLength, _) -> T.u32
  | E1 (BytesLength, _) -> T.size
  | E1 (StringOfBytes, _) -> T.string
  | E1 (BytesOfString, _) -> T.bytes
  | E1 (Cardinality, _) -> T.u32
  | E3 (PtrOfPtr, _, _, _) -> T.ptr
  | E3 (FindSubstring, _, _, _) -> T.nu24
  | E2 (GetBit, _, _) -> T.bool
  | E3 ((SetBit | SetVec), _, _, _) -> T.void
  | E1 (ReadU8, _) -> T.pair T.u8 T.ptr
  | E1 (ReadU16 _, _) -> T.pair T.u16 T.ptr
  | E1 (ReadU32 _, _) -> T.pair T.u32 T.ptr
  | E1 (ReadU64 _, _) -> T.pair T.u64 T.ptr
  | E1 (ReadU128 _, _) -> T.pair T.u128 T.ptr
  | E1 (Assert, _) -> T.void
  | E2 (ReadBytes, _, _) -> T.pair T.bytes T.ptr
  | E2 (PeekU8, _, _) -> T.u8
  | E2 (PeekU16 _, _, _) -> T.u16
  | E2 (PeekU32 _ , _, _)-> T.u32
  | E2 (PeekU64 _, _, _) -> T.u64
  | E2 (PeekU128 _, _, _) -> T.u128
  | E2 (WriteU8, _, _) -> T.ptr
  | E2 (WriteU16 _, _, _) -> T.ptr
  | E2 (WriteU32 _, _, _) -> T.ptr
  | E2 (WriteU64 _, _, _) -> T.ptr
  | E2 (WriteU128 _, _, _) -> T.ptr
  | E2 (WriteBytes, _, _) -> T.ptr
  | E2 (PokeU8, _, _) -> T.ptr
  | E3 (BlitByte, _, _, _) -> T.ptr
  | E2 (PtrAdd, _, _) -> T.ptr
  | E2 (PtrSub, _, _) -> T.size
  | E2 (Rewind, _, _) -> T.ptr
  | E1 (RemSize, _) -> T.size
  | E1 (Offset, _) -> T.size
  | E2 (And, _, _) -> T.bool
  | E2 (Or, _, _) -> T.bool
  | E1 (Not, _) -> T.bool
  | E1 ((Identity | Abs | Neg), e1) -> type_of l e1
  | E1 ((Exp | Ceil | Floor | Round |
         Cos | Sin | Tan | ACos | ASin | ATan | CosH | SinH | TanH), _) ->
      T.float
  | E1 ((Log | Log10 | Sqrt), _) ->
      T.to_nullable T.float
  | E1 ((UnsafeLog | UnsafeLog10 | UnsafeSqrt), _) -> T.float
  | E1 ((Lower | Upper), _) -> T.string
  | E1 (Hash, _) -> T.u64
  | E1 (ToU8, _) -> T.u8
  | E1 (ToI8, _) -> T.i8
  | E1 (ToU16, _) -> T.u16
  | E1 (ToI16, _) -> T.i16
  | E1 (ToU24, _) -> T.u24
  | E1 (ToI24, _) -> T.i24
  | E1 (ToU32, _) -> T.u32
  | E1 (ToI32, _) -> T.i32
  | E1 (ToU40, _) -> T.u40
  | E1 (ToI40, _) -> T.i40
  | E1 (ToU48, _) -> T.u48
  | E1 (ToI48, _) -> T.i48
  | E1 (ToU56, _) -> T.u56
  | E1 (ToI56, _) -> T.i56
  | E1 (ToU64, _) -> T.u64
  | E1 (ToI64, _) -> T.i64
  | E1 (ToU128, _) -> T.u128
  | E1 (ToI128, _) -> T.i128
  | E1 (ToFloat, _) -> T.float
  | E1 (PtrOfString, _) -> T.ptr
  | E1 (PtrOfBuffer, _) -> T.ptr
  | E2 (PtrOfAddress, _, _) -> T.ptr
  | E2 (While, _, _) -> T.void
  | E2 (ForEach _, _, _) -> T.void
  | E2 (NullMap (n, r), e1, e2) ->
      let t = get_memo_mn r l (T.E1 (Force "NullMap", e1)) in
      let l = add_local n t l in
      let mn1 = type_of l e1 in
      let mn = type_of l e2 in
      if mn1.nullable then T.to_nullable mn else mn
  | E2 (Index, _, _) -> T.nu32
  | E1 (GetEnv, _) -> T.nstring
  | E1 (GetMin, e) ->
      (match type_of l e |> T.develop1 with
      | T.{ typ = TSet (Heap, mn) ; nullable = false ; _ } -> mn
      | t -> raise (Type_error (e0, e, t, "be a heap")))
  | E1 (AllocVec d, init) ->
      let item_mn = type_of l init in
      T.(required (vec d item_mn))
  | E1 (Convert mn, _) -> mn
  | E1 (Truncate (_, len), e1) ->
      let item_mn = get_item_type ~vec:true e0 l e1 in
      T.(required (vec len item_mn))
  | E2 (Cons, e1, _e2) ->
      T.(required (lst (type_of l e1)))
  (* Shortcut: *)
  | E1 (Head, E2 (Cons, e, _)) ->
      T.to_nullable (type_of l e)
  | E1 (Head, e) ->
      (match type_of l e |> T.develop1 with
      | T.{ typ = TLst mn ; nullable = false ; _ } -> T.to_nullable mn
      | t -> raise (Type_error (e0, e, t, "be a lst")))
  (* Shortcuts: *)
  | E1 (Tail, E2 (Cons, _, E0 (EndOfList mn))) ->
      T.to_nullable mn
  | E1 (Tail, e) ->
      T.to_nullable (type_of l e)
  | E2 ((Min | Max), e1, e2) ->
      either e1 e2
  | E2 (Member, _, _) -> T.bool
  | E0 (Identifier _ | ExtIdentifierUnmanaged _) as e ->
      find_id_type l e
  | E0 (ExtIdentifierManaged { type_name ; meth = SerWithMask enc }) ->
      let sptr = T.sptr_of_enc enc in
      T.func3 T.mask T.(required (ext type_name)) sptr sptr
  | E0 (ExtIdentifierManaged { type_name ; meth = SerNoMask enc }) ->
      let sptr = T.sptr_of_enc enc in
      T.func2 T.(required (ext type_name)) sptr sptr
  | E0 (ExtIdentifierManaged { type_name ; meth = DesNoMask enc }) ->
      let dptr = T.dptr_of_enc enc in
      T.func1 dptr T.(pair (required (ext type_name)) dptr)
  | E0 (ExtIdentifierManaged { type_name ; meth = SSizeWithMask _ }) ->
      T.func2 T.mask T.(required (ext type_name)) T.size
  | E0 (ExtIdentifierManaged { type_name ; meth = SSizeNoMask _ }) ->
      T.func1 T.(required (ext type_name)) T.size
  | E0 (ExtIdentifierManaged { meth = Convert (senc, denc) }) ->
      let sptr = T.sptr_of_enc senc
      and dptr = T.dptr_of_enc denc in
      T.func2 sptr dptr T.(pair sptr dptr)
  | E0 (CopyField|SkipField|SetFieldNull) ->
      T.mask
  | E0 (NoReturn mn) ->
      mn
  | E2 (Let (n, r), e1, e2) ->
      let t = get_memo_mn r l e1 in
      let l = add_local n t l in
      type_of l e2
  | E2 (LetPair (n1, r1, n2, r2), e1, e2) ->
      let t1 = get_memo_mn r1 l (E1 (GetItem 0, e1))
      and t2 = get_memo_mn r2 l (E1 (GetItem 1, e1)) in
      let l = add_local n1 t1 l |>
              add_local n2 t2 in
      type_of l e2
  | E1 (Function ts, e) ->
      let l = enter_function ~ts l in
      T.func ts (type_of l e)
  | E0 (Param p) as e ->
      (try find_id l.local e
      with Not_found ->
        raise (Unbound_parameter (e0, p, l)))
  | E3 (If, _e1, e2, e3) ->
      either e2 e3
  | E3 (Map, _, f, set) ->
      (match type_of l f |> T.develop1 with
      | T.{ typ = TFunction (_, ot) ; nullable = false ; _ } ->
          let map_mn g = T.required (g ot) in
          (match type_of l set |> T.develop1 with
          | T.{ typ = TVec (n, _) ; _ } -> map_mn (fun mn -> TVec (n, mn))
          | T.{ typ = TArr _ ; _ } -> map_mn (fun mn -> TArr mn)
          | T.{ typ = TSet (st, _) ; _ } -> map_mn (fun mn -> TSet (st, mn))
          | T.{ typ = TLst _ ; _ } -> map_mn (fun mn -> TLst mn)
          | t -> raise (Type_error (e0, set, t, "be an iterable")))
      | t ->
          raise (Type_error (e0, f, t, "be a function")))
  | E1 (MaskGet _, _) ->
      T.mask
  | E1 (LabelOf, _) ->
      T.u16
  | E1 (SlidingWindow mn, _) ->
      T.(required (set Sliding mn))
  | E1 (TumblingWindow mn, _) ->
      T.(required (set Tumbling mn))
  | E1 (Sampling mn, _) ->
      T.(required (set Sampling mn))
  | E1 (HashTable mn, _) ->
      T.(required (set HashTable mn))
  | E1 (Heap, cmp) ->
      let item_t = get_compared_type l cmp in
      T.(required (set Heap item_t))
  | E2 ((Insert | DelMin), _, _) ->
      T.void
  | E2 (SplitBy, _, _) ->
      T.(required (arr string))
  | E2 (SplitAt, _, _) ->
      T.(required (tup [| string ; string |]))
  | E2 (Join, _, _) ->
      T.string
  | E2 (AllocArr, _, e2) ->
      let item_mn = type_of l e2 in
      T.(required (arr item_mn))
  | E2 (PartialSort, _, _) ->
      T.void
  | E2 ((ChopBegin | ChopEnd), arr, _) ->
      type_of l arr
  | E2 (ScaleWeights, _, _) ->
      T.void
  | E3 (Top mn, _, _, _) ->
      T.(required (set Top mn))
  | E3 (InsertWeighted, _, _, _) ->
      T.void
  | E3 (SubString, _, _, _)
  | E2 (Strftime, _, _) ->
      T.string

and get_item_type_err ?(vec=false) ?(arr=false) ?(set=false) ?(lst=false)
                      ?(str=false) ?(bytes=false) l e =
  match type_of l e |> T.develop1 with
  | { typ = TVec (_, mn) ; nullable = false ; _ } when vec -> Ok mn
  | { typ = TArr mn ; nullable = false ; _ } when arr -> Ok mn
  | { typ = TSet (_, mn) ; nullable = false ; _ } when set -> Ok mn
  | { typ = TLst mn ; nullable = false ; _ } when lst -> Ok mn
  | { typ = TString ; nullable = false ; _ } when str -> Ok T.char
  | { typ = TBytes ; nullable = false ; _ } when bytes -> Ok T.u8
  | t -> Error t

(* Return the element type or fail: *)
and get_item_type ?(vec=false) ?(arr=false) ?(set=false) ?(lst=false)
                  ?(str=false) ?(bytes=false) e0 l e =
  match get_item_type_err ~vec ~arr ~set ~lst ~str ~bytes l e with
  | Ok t -> t
  | Error t ->
      let acceptable = if vec then [ "vector" ] else [] in
      let acceptable = if arr then "array" :: acceptable else acceptable in
      let acceptable = if set then "set" :: acceptable else acceptable in
      let acceptable = if lst then "list" :: acceptable else acceptable in
      let acceptable = if str then "string" :: acceptable else acceptable in
      let acceptable = if bytes then "bytes" :: acceptable else acceptable in
      raise (Type_error (e0, e, t, "be a "^ String.join " or " acceptable))

and get_compared_type l cmp =
  match type_of l cmp with
  | T.{ typ = TFunction ([| item_t ; _ |], _) ; nullable = false ; _ } ->
      item_t
  | cmp_t ->
      let err = "should be a function of two values" in
      raise (Comparator_error (cmp, cmp_t, err))

(* Registering the constructor also register the type: *)
and register_user_constructor name out_vt def =
  (* Add identity to the passed definitions (aka "copy constructor"): *)
  let out_t = T.required out_vt in
  let id = T.E1 (Function [| out_t |], E0 (Param (0))) in
  let def = id :: def in
  (* Check constructors' signatures: *)
  let _ =
    List.fold_left (fun prev f ->
      match type_of no_env f with
      | T.{ typ = TFunction (ins, out_t') ; nullable = false ; _ } ->
          if not (T.eq_mn out_t' out_t) then
            Printf.sprintf2 "register_user_constructor: constructors must \
                             output type %a (not %a)"
              T.print_mn out_t
              T.print_mn out_t' |>
            invalid_arg ;
          (match prev with
          | None ->
              Some ([ ins ])
          | Some prev_ins ->
              let same_input_than ins' =
                try Array.for_all2 T.eq_mn ins ins'
                with Invalid_argument _ -> false in
              if List.exists same_input_than prev_ins then
                Printf.sprintf2 "register_user_constructor: constructors \
                                 signature %a appears more than once"
                  (Array.print T.print_mn) ins |>
                invalid_arg ;
              Some (ins :: prev_ins))
      | mn ->
          Printf.sprintf2 "register_user_constructor: constructors must be \
                           functions (not %a)"
            T.print_mn mn |>
          invalid_arg
    ) None def in
  T.register_user_type name out_vt ;
  Hashtbl.modify_opt name (function
    | None ->
        Some def
    | Some _ ->
        Printf.sprintf "register_user_constructor: name %S not unique"
          name |>
        invalid_arg
  ) user_constructors

and check_fun_sign e0 l f ps =
  match type_of l f with
  | T.{ typ = TFunction (ts, _) ; nullable = false ; _ } ->
      let lf = Array.length ts
      and lp = List.length ps in
      if lf <> lp then (
        let err = string_of_int lp ^" parameter(s) but function expects "^
                  string_of_int lf in
        raise (Apply_error (e0, err))) ;
      List.iteri (fun i p ->
        let act = type_of l p in
        if not (T.eq_mn act ts.(i)) then (
          (match act with
          | { typ = T.TThis n ; _ } ->
              Printf.eprintf "Arg %d of type %a instead of %a\n"
                i
                T.print_mn act
                T.print_mn ts.(i) ;
              Printf.eprintf "This %S is: %a\n"
                n
                T.print (T.find_this n)
          | _ -> ()) ;
          let expected = T.mn_to_string ts.(i) in
          raise (Type_error (e0, p, act, "be a "^ expected)))
      ) ps
  | t ->
      raise (Type_error (e0, f, t, "be a function"))

and memoize_type r f =
  match !r with
  | Some mn ->
      mn
  | None ->
      let mn = f () in
      r := Some mn ;
      mn

and get_memo_mn r l e =
  memoize_type r (fun () -> type_of l e)

and get_memo_item_mn r l e =
  memoize_type r (fun () ->
    match get_item_type_err ~vec:true ~arr:true ~set:true ~lst:true
                            ~str:true ~bytes:true l e with
    | Ok mn ->
        mn
    | Error mn ->
        Printf.sprintf2 "argument must be a vector/array/list/set, not %a"
          T.print_mn mn |>
        invalid_arg)

let apply_constructor e0 l name ins =
  match Hashtbl.find user_constructors name with
  | exception Not_found ->
      raise (Invalid_expression (e0, "unregistered user type "^
                                       String.quote name))
  | [ c ] ->
      T.E1S (Apply, c, ins)
  | cs ->
      (match
        List.find (fun c ->
          try check_fun_sign e0 l c ins ; true
          with _ -> false
        ) cs with
      | exception Not_found ->
          let msg =
            Printf.sprintf2 "none of the constructors (%a) match input types %a"
              (List.print (fun oc e -> T.print_mn oc (type_of l e))) cs
              (List.print (fun oc e -> T.print_mn oc (type_of l e))) ins in
          raise (Invalid_expression (e0, msg))
      | c ->
          E1S (Apply, c, ins))

let expand_verbatim backend_id temps ins =
  match List.assoc backend_id temps with
  | exception Not_found ->
      Printf.sprintf2 "No implementation provided for %s (only %a)"
        (string_of_backend backend_id)
        (pretty_list_print (fun oc (b, _) ->
          String.print oc (string_of_backend b))) temps |>
      failwith
  | temp ->
      List.fold_lefti (fun s i in_ ->
        let sub = "%" ^ string_of_int (i + 1) in
        String.nreplace ~str:s ~sub ~by:in_
      ) temp ins

(* Top to bottom: *)
(* Folding a tree of 100M nodes takes ~30s :-< *)
let rec fold_env u l f e =
  let u = f u l e in
  match e with
  | T.E0 _ ->
      u
  | E0S (_, es) ->
      List.fold_left (fun u e1 -> fold_env u l f e1) u es
  | E0R (_, es) ->
      Array.fold_left (fun u e1 -> fold_env u l f e1) u es
  | E1 (Function ts, e1) ->
      let l = enter_function ~ts l in
      fold_env u l f e1
  | E1 (_, e1) ->
      fold_env u l f e1
  | E1S (_, e1, es) ->
      let u = fold_env u l f e1 in
      List.fold_left (fun u e1 -> fold_env u l f e1) u es
  | E2 (Let (n, r), e1, e2) ->
      let t = get_memo_mn r l e1 in
      let l' = add_local n t l in
      fold_env (fold_env u l f e1) l' f e2
  | E2 (LetPair (n1, r1, n2, r2), e1, e2) ->
      let t1 = get_memo_mn r1 l (E1 (GetItem 0, e1))
      and t2 = get_memo_mn r2 l (E1 (GetItem 1, e1)) in
      let l' = add_local n1 t1 l |>
               add_local n2 t2 in
      fold_env (fold_env u l f e1) l' f e2
  | E2 (ForEach (n, r), e1, e2) ->
      let mn = get_memo_item_mn r l e1 in
      let l' = add_local n mn l in
      fold_env (fold_env u l f e1) l' f e2
  | E2 (NullMap (n, r), e1, e2) ->
      let mn = get_memo_mn r l (E1 (Force "fold_env", e1)) in
      let l' = add_local n mn l in
      fold_env (fold_env u l f e1) l' f e2
  | E2 (_, e1, e2) ->
      fold_env (fold_env u l f e1) l f e2
  | E3 (_, e1, e2, e3) ->
      fold_env (fold_env (fold_env u l f e1) l f e2) l f e3

let iter_env l f e =
  fold_env () l (fun () l e -> f l e) e

(* Depth first expression transformation.
 * It is important that when [f] returns the same [e] then the expression
 * is not rebuild, both for performance and to allow the use of the ==
 * operator, for instance in DessserEval.ml: *)
let rec map ?(enter_functions=true) f e =
  let same = List.for_all2 (==) in
  match e with
  | T.E0 _ ->
      f e
  | E0S (op, es) ->
      let es' = List.map (map f) es in
      if same es' es then f e else
      f (E0S (op, es'))
  | E0R (op, es) ->
      let es' = Array.map (map f) es in
      if Array.for_all2 (==) es' es then f e else
      f (E0R (op, es'))
  | E1 (Function _, _) when not enter_functions ->
      f e
  | E1 (op, e1) ->
      let e1' = map f e1 in
      if e1 == e1' then f e else
      f (E1 (op, e1'))
  | E1S (op, e1, es) ->
      let e1' = map f e1
      and es' = List.map (map f) es in
      if e1' == e1 && same es' es then f e else
      f (E1S (op, e1', es'))
  | E2 (op, e1, e2) ->
      let e1' = map f e1
      and e2' = map f e2 in
      if e1' == e1 && e2' == e2 then f e else
      f (E2 (op, e1', e2'))
  | E3 (op, e1, e2, e3) ->
      let e1' = map f e1
      and e2' = map f e2
      and e3' = map f e3 in
      if e1' == e1 && e2' == e2 && e3' == e3 then f e else
      f (E3 (op, e1', e2', e3'))

(* Call [f] bottom to top. [f] is not allowed to change the type of the passed
 * expression. *)
let rec map_env l f e =
  match e with
  | T.E0 _ ->
      f l e
  | E0S (op, es) ->
      let es = List.map (map_env l f) es in
      f l (E0S (op, es))
  | E0R (op, es) ->
      let es = Array.map (map_env l f) es in
      f l (E0R (op, es))
  | E1 (Function ts, e1) ->
      let l' = enter_function ~ts l in
      let e1 = map_env l' f e1 in
      f l (E1 (Function ts, e1))
  | E1 (op, e1) ->
      let e1 = map_env l f e1 in
      f l (E1 (op, e1))
  | E1S (op, e1, es) ->
      let e1 = map_env l f e1
      and es = List.map (map_env l f) es in
      f l (E1S (op, e1, es))
  | E2 (Let (n, r), e1, e2) ->
      let e1 = map_env l f e1 in
      let t = get_memo_mn r l e1 in
      let l' = add_local n t l in
      let e2 = map_env l' f e2 in
      f l (E2 (Let (n, r), e1, e2))
  | E2 (LetPair (n1, r1, n2, r2), e1, e2) ->
      let e1 = map_env l f e1 in
      let t1 = get_memo_mn r1 l (E1 (GetItem 0, e1))
      and t2 = get_memo_mn r2 l (E1 (GetItem 1, e1)) in
      let l' = add_local n1 t1 l |>
               add_local n2 t2 in
      let e2 = map_env l' f e2 in
      f l (E2 (LetPair (n1, r1, n2, r2), e1, e2))
  | E2 (ForEach (n, r), e1, e2) ->
      let e1 = map_env l f e1 in
      let t = get_memo_item_mn r l e1 in
      let l' = add_local n t l in
      let e2 = map_env l' f e2 in
      f l (E2 (ForEach (n, r), e1, e2))
  | E2 (NullMap (n, r), e1, e2) ->
      let e1 = map_env l f e1 in
      let t = get_memo_mn r l (E1 (Force "null_map", e1)) in
      let l' = add_local n t l in
      let e2 = map_env l' f e2 in
      f l (E2 (NullMap (n, r), e1, e2))
  | E2 (op, e1, e2) ->
      let e1 = map_env l f e1
      and e2 = map_env l f e2 in
      f l (E2 (op, e1, e2))
  | E3 (op, e1, e2, e3) ->
      let e1 = map_env l f e1
      and e2 = map_env l f e2
      and e3 = map_env l f e3 in
      f l (E3 (op, e1, e2, e3))

let size_of_expr e =
  fold 0 (fun n _e0 -> n + 1) e

let () =
  let max_depth = 5 in
  Printexc.register_printer (function
    | Type_error (e0, e, t, s) ->
        Some (
          Printf.sprintf2
            "Type Error: In expression\
             %s\
             expression\
             %s\
             should %s but is a %a"
            (to_pretty_string ~max_depth e0)
            (to_pretty_string ~max_depth e)
            s
            T.print_mn t)
    | Type_error_param (e0, e, n, t, s) ->
        Some (
          Printf.sprintf2
            "Type Error: In expression\
             %s\
             %s of function\
             %s\
             should %s but is a %a"
            (to_pretty_string ~max_depth e0)
            (if n >= 0 then "parameter "^ string_of_int n else "return type")
            (to_pretty_string ~max_depth e)
            s
            T.print_mn t)
    | Struct_error (e0, s) ->
        Some (
          Printf.sprintf2
            "Invalid type structure: In expression\
             %s\
             %s"
            (to_pretty_string ~max_depth e0)
            s)
    | Apply_error (e0, s) ->
        Some (
          Printf.sprintf2
            "Invalid function application: In expression\
             %s\
             %s"
            (to_pretty_string ~max_depth e0)
            s)
    | Comparator_error (e0, mn, s) ->
        Some (
          Printf.sprintf2
            "Invalid comparator function:\
             %s\
             %s but has type %a"
            (to_pretty_string ~max_depth e0)
            s
            T.print_mn mn)
    | Unbound_identifier (e0, l) ->
        Some (
          Printf.sprintf2
            "Unbound identifier:\
             %s\
             environment is %a"
            (to_pretty_string ~max_depth e0)
            print_environment l)
    | Unbound_parameter (e0, p, l) ->
        Some (
          Printf.sprintf2
            "Unbound parameter #%d: In expression\
             %s\
             environment is %a"
            p
            (to_pretty_string ~max_depth e0)
            print_environment l)
    | Invalid_expression (e0, msg) ->
        Some (
          Printf.sprintf2
            "Invalid expression\
             %s\
             %s"
            (to_pretty_string ~max_depth e0)
            msg)
    | Redefinition n ->
        Some (
          "Identifier "^ String.quote n ^" shadows a previous definition")
    | Invalid_truncate (e0, what) ->
        Some (
          Printf.sprintf2 "Invalid %s in truncate operation %s"
            what
            (to_pretty_string ~max_depth e0))
    | _ ->
        None)

(*
 * Some helpers to deal with expressions:
 *)

let gen_id =
  let seq = ref (-1) in
  fun prefix ->
    incr seq ;
    prefix ^"_"^ string_of_int !seq

let let_ ?name value f =
  match value with
  (* If [value] is already an identifier (or a param) there is no need for a
   * new one: *)
  | T.E0 (Param _ | Identifier _)
  (* Also, if it's a constant then the optimizer will work better if it's
   * not hidden behind an identifier: *)
  | E0 (Null _ | EndOfList _ | EmptySet _ | Float _ | Bool _ | Char _
       | U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _
       | I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _
       | Size _ | Address _
       | CopyField | SkipField | SetFieldNull | NoReturn _)
  | E0S (Seq, []) ->
      f value
  | _ ->
      let n = match name with Some n -> gen_id n | None -> gen_id "gen" in
      T.E2 (Let (n, ref None), value, f (E0 (Identifier n)))

let let_pair ?n1 ?n2 value f =
  let name = function Some n -> gen_id n | None -> gen_id "gen" in
  let n1 = name n1 and n2 = name n2 in
  let id n = T.E0 (Identifier n) in
  T.E2 (LetPair (n1, ref None, n2, ref None), value, f (id n1) (id n2))

(* Do not use a function to avoid leaking function parameters *)
let with_sploded_pair what e f =
  let n1 = what ^"_fst"
  and n2 = what ^"_snd" in
  let_ ~name:what e (fun p ->
    let_pair ~n1 ~n2 p f)

(* Tells is a function just return its [p]th argument: *)
let is_identity p = function
  | T.E1 (Function _, E0 (Param p')) ->
      p = p'
  | _ ->
      false

let is_recursive e =
  try
    iter (function
      | T.E0 (Myself _) -> raise Exit
      | _ -> ()
    ) e ;
    false
  with Exit ->
    true

(*$< DessserTypes *)
(*$= type_of & ~printer:(T.mn_to_string)
  (T.pair T.u24 T.ptr) \
    (type_of no_env Ops.(make_pair (to_u24 (i32 42l)) (ptr_of_string (string ""))))
*)

(*
 * Simplified notation:
 *)

module Ops =
struct
  (* Create a function expression: *)
  let func ts f =
    T.E1 (Function ts, f ())

  (* Specialized to a given arity: *)

  let func0 f =
    func [||] f

  let func1 t1 f =
    func [| t1 |] (fun () ->
      let p1 = T.E0 (Param 0) in
      f p1)

  let func2 t1 t2 f =
    func [| t1 ; t2 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1) in
      f p1 p2)

  let func3 t1 t2 t3 f =
    func [| t1 ; t2 ; t3 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2) in
      f p1 p2 p3)

  let func4 t1 t2 t3 t4 f =
    func [| t1 ; t2 ; t3 ; t4 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2)
      and p4 = T.E0 (Param 3) in
      f p1 p2 p3 p4)

  let func5 t1 t2 t3 t4 t5 f =
    func [| t1 ; t2 ; t3 ; t4 ; t5 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2)
      and p4 = T.E0 (Param 3)
      and p5 = T.E0 (Param 4) in
      f p1 p2 p3 p4 p5)

  let func6 t1 t2 t3 t4 t5 t6 f =
    func [| t1 ; t2 ; t3 ; t4 ; t5 ; t6 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2)
      and p4 = T.E0 (Param 3)
      and p5 = T.E0 (Param 4)
      and p6 = T.E0 (Param 5) in
      f p1 p2 p3 p4 p5 p6)

  let func7 t1 t2 t3 t4 t5 t6 t7 f =
    func [| t1 ; t2 ; t3 ; t4 ; t5 ; t6 ; t7 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2)
      and p4 = T.E0 (Param 3)
      and p5 = T.E0 (Param 4)
      and p6 = T.E0 (Param 5)
      and p7 = T.E0 (Param 6) in
      f p1 p2 p3 p4 p5 p6 p7)

  let func8 t1 t2 t3 t4 t5 t6 t7 t8 f =
    func [| t1 ; t2 ; t3 ; t4 ; t5 ; t6 ; t7 ; t8 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2)
      and p4 = T.E0 (Param 3)
      and p5 = T.E0 (Param 4)
      and p6 = T.E0 (Param 5)
      and p7 = T.E0 (Param 6)
      and p8 = T.E0 (Param 7) in
      f p1 p2 p3 p4 p5 p6 p7 p8)

  let func9 t1 t2 t3 t4 t5 t6 t7 t8 t9 f =
    func [| t1 ; t2 ; t3 ; t4 ; t5 ; t6 ; t7 ; t8 ; t9 |] (fun () ->
      let p1 = T.E0 (Param 0)
      and p2 = T.E0 (Param 1)
      and p3 = T.E0 (Param 2)
      and p4 = T.E0 (Param 3)
      and p5 = T.E0 (Param 4)
      and p6 = T.E0 (Param 5)
      and p7 = T.E0 (Param 6)
      and p8 = T.E0 (Param 7)
      and p9 = T.E0 (Param 8) in
      f p1 p2 p3 p4 p5 p6 p7 p8 p9)

  let identity e1 = T.E1 (Identity, e1)

  let ignore_ e1 = T.E1 (Ignore, e1)

  let dump e1 = T.E1 (Dump, e1)

  let debug e1 =
    T.E1 ((if !dump_debug then Dump else Ignore), e1)

  let debugs es = T.E0S (Seq, List.map debug es)

  let bool n = T.E0 (Bool n)

  let false_ = bool false

  let true_ = bool true

  let bit n = T.E0 (Bool n)

  let i8 n = T.E0 (I8 n)

  let u8 n = T.E0 (U8 n)

  let i16 n = T.E0 (I16 n)

  let u16 n = T.E0 (U16 n)

  let i24 n = T.E0 (I24 n)

  let u24 n = T.E0 (U24 n)

  let i32 n = T.E0 (I32 n)

  let u32 n = T.E0 (U32 n)

  let i40 n = T.E0 (I40 n)

  let u40 n = T.E0 (U40 n)

  let i48 n = T.E0 (I48 n)

  let u48 n = T.E0 (U48 n)

  let i56 n = T.E0 (I56 n)

  let u56 n = T.E0 (U56 n)

  let i64 n = T.E0 (I64 n)

  let u64 n = T.E0 (U64 n)

  let i128 n = T.E0 (I128 n)

  let u128 n = T.E0 (U128 n)

  let char n = T.E0 (Char n)

  let float n = T.E0 (Float n)

  let string n = T.E0 (String n)

  let u8 n = T.E0 (U8 n)

  let size n = T.E0 (Size n)

  let address n = T.E0 (Address n)

  let u16 n = T.E0 (U16 n)

  let u32 n = T.E0 (U32 n)

  let u64 n = T.E0 (U64 n)

  let u128 n = T.E0 (U128 n)

  let bytes s = T.E0 (Bytes s)

  let char_of_int n = char (Char.chr n)

  let i8_of_int n = i8 (Int8.of_int n)

  let u8_of_int n = u8 (Uint8.of_int n)

  let i16_of_int n = i16 (Int16.of_int n)

  let u16_of_int n = u16 (Uint16.of_int n)

  let i24_of_int n = i24 (Int24.of_int n)

  let u24_of_int n = u24 (Uint24.of_int n)

  let i32_of_int n = i32 (Int32.of_int n)

  let u32_of_int n = u32 (Uint32.of_int n)

  let i40_of_int n = i40 (Int40.of_int n)

  let u40_of_int n = u40 (Uint40.of_int n)

  let i48_of_int n = i48 (Int48.of_int n)

  let u48_of_int n = u48 (Uint48.of_int n)

  let i56_of_int n = i56 (Int56.of_int n)

  let u56_of_int n = u56 (Uint56.of_int n)

  let i64_of_int n = i64 (Int64.of_int n)

  let u64_of_int n = u64 (Uint64.of_int n)

  let i128_of_int n = i128 (Int128.of_int n)

  let u128_of_int n = u128 (Uint128.of_int n)

  let is_null e = T.E1 (IsNull, e)

  let nth i v = T.E2 (Nth, i, v)

  let unsafe_nth i v = T.E2 (UnsafeNth, i, v)

  let read_u8 e1 = T.E1 (ReadU8, e1)

  let read_u16 en e1 = T.E1 (ReadU16 en, e1)

  let read_u32 en e1 = T.E1 (ReadU32 en, e1)

  let read_u64 en e1 = T.E1 (ReadU64 en, e1)

  let read_u128 en e1 = T.E1 (ReadU128 en, e1)

  let peek_u16 en e1 e2 = T.E2 (PeekU16 en, e1, e2)

  let peek_u32 en e1 e2 = T.E2 (PeekU32 en, e1, e2)

  let peek_u64 en e1 e2 = T.E2 (PeekU64 en, e1, e2)

  let peek_u128 en e1 e2 = T.E2 (PeekU128 en, e1, e2)

  let read_bytes e1 e2 = T.E2 (ReadBytes, e1, e2)

  let peek_u8 e1 e2 = T.E2 (PeekU8, e1, e2)

  let write_bytes e1 e2 = T.E2 (WriteBytes, e1, e2)

  let write_u8 e1 e2 = T.E2 (WriteU8, e1, e2)

  let write_u16 en e1 e2 = T.E2 (WriteU16 en, e1, e2)

  let write_u32 en e1 e2 = T.E2 (WriteU32 en, e1, e2)

  let write_u64 en e1 e2 = T.E2 (WriteU64 en, e1, e2)

  let write_u128 en e1 e2 = T.E2 (WriteU128 en, e1, e2)

  let insert set x = T.E2 (Insert, set, x)

  let insert_weighted set w x = T.E3 (InsertWeighted, set, w, x)

  let substring str start stop = T.E3 (SubString, str, start, stop)

  let del_min set n = T.E2 (DelMin, set, n)

  let get_min set = T.E1 (GetMin, set)

  let scale_weights set d = T.E2 (ScaleWeights, set, d)

  let join e1 e2 = T.E2 (Join, e1, e2)

  let bytes_of_string e1 = T.E1 (BytesOfString, e1)

  let string_of_int_ e = T.E1 (StringOfInt, e)

  let string_of_float_ e = T.E1 (StringOfFloat, e)

  let decimal_string_of_float e = T.E1 (DecimalStringOfFloat, e)

  let string_of_ip e = T.E1 (StringOfIp, e)

  let null vt = T.E0 (Null vt)

  let strftime fmt time = T.E2 (Strftime, fmt, time)

  let string_of_char e = T.E1 (StringOfChar, e)

  let not_null e = T.E1 (NotNull, e)

  let or_null_ vt op conv s =
    try not_null (op (conv s)) with _ -> null vt

  let float_of_string_ e = T.E1 (FloatOfString, e)

  let u8_of_string e = T.E1 (U8OfString, e)

  let u16_of_string e = T.E1 (U16OfString, e)

  let u24_of_string e = T.E1 (U24OfString, e)

  let u32_of_string e = T.E1 (U32OfString, e)

  let u40_of_string e = T.E1 (U40OfString, e)

  let u48_of_string e = T.E1 (U48OfString, e)

  let u56_of_string e = T.E1 (U56OfString, e)

  let u64_of_string e = T.E1 (U64OfString, e)

  let u128_of_string e = T.E1 (U128OfString, e)

  let i8_of_string e = T.E1 (I8OfString, e)

  let i16_of_string e = T.E1 (I16OfString, e)

  let i24_of_string e = T.E1 (I24OfString, e)

  let i32_of_string e = T.E1 (I32OfString, e)

  let i40_of_string e = T.E1 (I40OfString, e)

  let i48_of_string e = T.E1 (I48OfString, e)

  let i56_of_string e = T.E1 (I56OfString, e)

  let i64_of_string e = T.E1 (I64OfString, e)

  let i128_of_string e = T.E1 (I128OfString, e)

  let float_of_ptr e = T.E1 (FloatOfPtr, e)

  let char_of_ptr e = T.E1 (CharOfPtr, e)

  let u8_of_ptr e = T.E1 (U8OfPtr, e)

  let u16_of_ptr e = T.E1 (U16OfPtr, e)

  let u24_of_ptr e = T.E1 (U24OfPtr, e)

  let u32_of_ptr e = T.E1 (U32OfPtr, e)

  let u40_of_ptr e = T.E1 (U40OfPtr, e)

  let u48_of_ptr e = T.E1 (U48OfPtr, e)

  let u56_of_ptr e = T.E1 (U56OfPtr, e)

  let u64_of_ptr e = T.E1 (U64OfPtr, e)

  let u128_of_ptr e = T.E1 (U128OfPtr, e)

  let i8_of_ptr e = T.E1 (I8OfPtr, e)

  let i16_of_ptr e = T.E1 (I16OfPtr, e)

  let i24_of_ptr e = T.E1 (I24OfPtr, e)

  let i32_of_ptr e = T.E1 (I32OfPtr, e)

  let i40_of_ptr e = T.E1 (I40OfPtr, e)

  let i48_of_ptr e = T.E1 (I48OfPtr, e)

  let i56_of_ptr e = T.E1 (I56OfPtr, e)

  let i64_of_ptr e = T.E1 (I64OfPtr, e)

  let i128_of_ptr e = T.E1 (I128OfPtr, e)

  let bool_of_u8 e = T.E1 (BoolOfU8, e)

  let u8_of_char e = T.E1 (U8OfChar, e)

  let u8_of_const_char c = T.E1 (U8OfChar, E0 (Char c))

  let u8_of_bool e = T.E1 (U8OfBool, e)

  let char_of_u8 e = T.E1 (CharOfU8, e)

  let u32_of_size e = T.E1 (U32OfSize, e)

  let size_of_u32 e = T.E1 (SizeOfU32, e)

  let u64_of_address e = T.E1 (U64OfAddress, e)

  let address_of_u64 e = T.E1 (AddressOfU64, e)

  let eol t = T.E0 (EndOfList t)

  let end_of_list = eol

  let sliding_window mn e1 =
    let mn = T.shrink_mn mn in
    T.E1 (SlidingWindow mn, e1)

  let tumbling_window mn e1 =
    let mn = T.shrink_mn mn in
    T.E1 (TumblingWindow mn, e1)

  let sampling mn e1 =
    let mn = T.shrink_mn mn in
    T.E1 (Sampling mn, e1)

  let hash_table mn e1 =
    let mn = T.shrink_mn mn in
    T.E1 (HashTable mn, e1)

  let heap cmp = T.E1 (Heap, cmp)

  let empty_set mn =
    let mn = T.shrink_mn mn in
    T.E0 (EmptySet mn)

  let top mn size max_size sigmas =
    let mn = T.shrink_mn mn in
    T.E3 (Top mn, size, max_size, sigmas)

  let now = T.E0 Now

  let random_float = T.E0 RandomFloat

  let random_u8 = T.E0 RandomU8

  let random_u32 = T.E0 RandomU32

  let random_u64 = T.E0 RandomU64

  let random_u128 = T.E0 RandomU128

  let make_pair e1 e2 = T.E0S (MakeTup, [ e1 ; e2 ])

  let first e = T.E1 (GetItem 0, e)

  let secnd e = T.E1 (GetItem 1, e)

  let cons e1 e2 = T.E2 (Cons, e1, e2)

  let head e = T.E1 (Head, e)

  let tail e = T.E1 (Tail, e)

  let if_ cond ~then_ ~else_ = T.E3 (If, cond, then_, else_)

  let if_null d ~then_ ~else_ = if_ (is_null d) ~then_ ~else_

  let float_of_u64 e = T.E1 (FloatOfU64, e)

  let u64_of_float e = T.E1 (U64OfFloat, e)

  let comment n e1 = T.E1 (Comment n, e1)

  let ge e1 e2 = T.E2 (Ge, e1, e2)

  let gt e1 e2 = T.E2 (Gt, e1, e2)

  let le e1 e2 = ge e2 e1

  let lt e1 e2 = gt e2 e1

  let eq e1 e2 = T.E2 (Eq, e1, e2)

  let not_ e = T.E1 (Not, e)

  let abs e1 = T.E1 (Abs, e1)

  let ne e1 e2 = not_ (eq e1 e2)

  let param n = T.E0 (Param n)

  let myself mn =
    let mn = T.shrink_mn mn in
    T.E0 (Myself mn)

  let add e1 e2 = T.E2 (Add, e1, e2)

  let sub e1 e2 = T.E2 (Sub, e1, e2)

  let mul e1 e2 = T.E2 (Mul, e1, e2)

  let div e1 e2 = T.E2 (Div, e1, e2)

  let unsafe_div e1 e2 = T.E2 (UnsafeDiv, e1, e2)

  let rem e1 e2 = T.E2 (Rem, e1, e2)

  let unsafe_rem e1 e2 = T.E2 (UnsafeRem, e1, e2)

  let pow e1 e2 = T.E2 (Pow, e1, e2)

  let unsafe_pow e1 e2 = T.E2 (UnsafePow, e1, e2)

  let left_shift e1 e2 = T.E2 (LeftShift, e1, e2)

  let right_shift e1 e2 = T.E2 (RightShift, e1, e2)

  let bit_and e1 e2 = T.E2 (BitAnd, e1, e2)

  let bit_or e1 e2 = T.E2 (BitOr, e1, e2)

  let bit_xor e1 e2 = T.E2 (BitXor, e1, e2)

  let bit_not e = T.E1 (BitNot, e)

  let and_ e1 e2 = T.E2 (And, e1, e2)

  let or_ e1 e2 = T.E2 (Or, e1, e2)

  let let_ = let_

  let let_pair = let_pair

  let for_each ?name lst f =
    let n = match name with Some n -> gen_id n | None -> gen_id "for_each" in
    T.E2 (ForEach (n, ref None), lst, f (T.E0 (Identifier n)))

  let null_map ?name x f =
    let n = match name with Some n -> gen_id n | None -> gen_id "null_map" in
    T.E2 (NullMap (n, ref None), x, f (T.E0 (Identifier n)))

  let identifier n = T.E0 (Identifier n)

  let ext_identifier n = T.E0 (ExtIdentifierUnmanaged n)

  let type_method type_name meth =
    T.E0 (ExtIdentifierManaged { type_name ; meth })

  let to_i8 e = T.E1 (ToI8, e)
  let to_i16 e = T.E1 (ToI16, e)
  let to_i24 e = T.E1 (ToI24, e)
  let to_i32 e = T.E1 (ToI32, e)
  let to_i40 e = T.E1 (ToI40, e)
  let to_i48 e = T.E1 (ToI48, e)
  let to_i56 e = T.E1 (ToI56, e)
  let to_i64 e = T.E1 (ToI64, e)
  let to_i128 e = T.E1 (ToI128, e)
  let to_u8 e = T.E1 (ToU8, e)
  let to_u16 e = T.E1 (ToU16, e)
  let to_u24 e = T.E1 (ToU24, e)
  let to_u32 e = T.E1 (ToU32, e)
  let to_u40 e = T.E1 (ToU40, e)
  let to_u48 e = T.E1 (ToU48, e)
  let to_u56 e = T.E1 (ToU56, e)
  let to_u64 e = T.E1 (ToU64, e)
  let to_u128 e = T.E1 (ToU128, e)
  let to_float e = T.E1 (ToFloat, e)

  let seq es = T.E0S (Seq, es)

  let nop = seq []

  let void = nop

  let apply f es = T.E1S (Apply, f, es)

  (* Flatten an association list, transforming even items with to_expr: *)
  let flatten_assoc to_expr xs =
    List.fold_left (fun lst (n, v) -> v :: to_expr n :: lst) [] xs |>
    List.rev

  let copy_rec ~with_ e =
    let es = flatten_assoc string with_ in
    T.E1S (CopyRec, e, es)

  let copy_tup ~with_ e =
    let es = flatten_assoc u16_of_int with_ in
    T.E1S (CopyTup, e, es)

  let while_ cond ~do_ = T.E2 (While, cond, do_)

  let string_of_bytes e = T.E1 (StringOfBytes, e)

  let rem_size e = T.E1 (RemSize, e)

  let offset e = T.E1 (Offset, e)

  let neg e = T.E1 (Neg, e)

  let exp_ e = T.E1 (Exp, e)

  let log_ e = T.E1 (Log, e)

  let unsafe_log e = T.E1 (UnsafeLog, e)

  let log10_ e = T.E1 (Log10, e)

  let unsafe_log10 e = T.E1 (UnsafeLog10, e)

  let sqrt_ e = T.E1 (Sqrt, e)

  let unsafe_sqrt e = T.E1 (UnsafeSqrt, e)

  let ceil_ e = T.E1 (Ceil, e)

  let floor_ e = T.E1 (Floor, e)

  let round e = T.E1 (Round, e)

  let cos_ e = T.E1 (Cos, e)

  let sin_ e = T.E1 (Sin, e)

  let tan_ e = T.E1 (Tan, e)

  let acos_ e = T.E1 (ACos, e)

  let asin_ e = T.E1 (ASin, e)

  let atan_ e = T.E1 (ATan, e)

  let cosh_ e = T.E1 (CosH, e)

  let sinh_ e = T.E1 (SinH, e)

  let tanh_ e = T.E1 (TanH, e)

  let lower e = T.E1 (Lower, e)

  let upper e = T.E1 (Upper, e)

  let hash e = T.E1 (Hash, e)

  let ptr_add e1 e2 = T.E2 (PtrAdd, e1, e2)

  let ptr_sub e1 e2 = T.E2 (PtrSub, e1, e2)

  let rewind e1 e2 = T.E2 (Rewind, e1, e2)

  let ptr_of_string e = T.E1 (PtrOfString, e)

  let ptr_of_buffer e = T.E1 (PtrOfBuffer, e)

  let ptr_of_address e1 e2 = T.E2 (PtrOfAddress, e1, e2)

  let ptr_of_ptr e1 e2 e3 = T.E3 (PtrOfPtr, e1, e2, e3)

  let string_length e = T.E1 (StringLength, e)

  let bytes_length e = T.E1 (BytesLength, e)

  let cardinality e = T.E1 (Cardinality, e)

  let blit_byte e1 e2 e3 = T.E3 (BlitByte, e1, e2, e3)

  let set_bit e1 e2 e3 = T.E3 (SetBit, e1, e2, e3)

  let get_bit e1 e2 = T.E2 (GetBit, e1, e2)

  let force ?(what="") e = T.E1 (Force what, e)

  let find_substring from_start haystack needle =
    T.E3 (FindSubstring, from_start, haystack, needle)

  let get_item n e = T.E1 (GetItem n, e)

  let get_field s e = T.E1 (GetField s, e)

  let get_alt s e = T.E1 (GetAlt s, e)

  let construct mns i e = T.E1 (Construct (mns, i), e)

  let min_ e1 e2 = T.E2 (Min, e1, e2)

  let max_ e1 e2 = T.E2 (Max, e1, e2)

  let member e1 e2 = T.E2 (Member, e1, e2)

  let make_vec es = T.E0R (MakeVec, Array.of_list es)

  let alloc_vec d init =
    assert (d >= 0) ;
    T.E1 (AllocVec d, init)

  let convert mn e1 =
    let mn = T.shrink_mn mn in
    T.E1 (Convert mn, e1)

  let truncate o l e1 =
    T.E1 (Truncate (o, l), e1)

  let make_arr mn es =
    let mn = T.shrink_mn mn in
    T.E0R (MakeArr mn, Array.of_list es)

  let alloc_arr len init = T.E2 (AllocArr, len, init)

  let partial_sort vs ks = T.E2 (PartialSort, vs, ks)

  let assert_ e = T.E1 (Assert, e)

  let set_vec e1 e2 e3 = T.E3 (SetVec, e1, e2, e3)

  let map_ init f lst = T.E3 (Map, init, f, lst)

  let arr_of_lst e1 = T.E1 (ArrOfLst, e1)

  let arr_of_lst_rev e1 = T.E1 (ArrOfLstRev, e1)

  let set_of_lst e1 = T.E1 (SetOfLst, e1)

  let arr_of_vec e1 = T.E1 (ArrOfVec, e1)

  let arr_of_set e1 = T.E1 (ArrOfSet, e1)

  let split_by e1 e2 = T.E2 (SplitBy, e1, e2)

  (* It might be easier for users to accept also 0 or 1 expressions and turn
   * them into what's expected: *)
  let make_tup = function
    | [] -> nop
    | [ x ] -> x
    | es -> T.E0S (MakeTup, es)

  let make_rec = function
    | [] -> void
    | es ->
        (* Flatten the list to comply with E0S structure.
         * Although field order does not matter in records, it's nice to keep
         * user order when parsing/printing: *)
        let es = flatten_assoc string es in
        T.E0S (MakeRec, es)

  let make_usr name es =
    T.E0S (MakeUsr name, es)

  let verbatim temps out_t ins =
    T.E0S (Verbatim (temps, out_t), ins)

  let split_at e1 e2 = T.E2 (SplitAt, e1, e2)

  let append_byte e1 e2 = T.E2 (AppendByte, e1, e2)

  let append_bytes e1 e2 = T.E2 (AppendBytes, e1, e2)

  let append_string e1 e2 = T.E2 (AppendString, e1, e2)

  let starts_with e1 e2 = T.E2 (StartsWith, e1, e2)

  let ends_with e1 e2 = T.E2 (EndsWith, e1, e2)

  let mask_get i m = T.E1 (MaskGet i, m)

  let label_of e = T.E1 (LabelOf, e)

  let copy_field = T.E0 CopyField

  let skip_field = T.E0 SkipField

  let set_field_null = T.E0 SetFieldNull

  let no_return mn = T.E0 (NoReturn mn)

  let getenv e = T.E1 (GetEnv, e)

  let string_of_char_ e = T.E1 (StringOfChar, e)

  let make_ref e = T.E0R (MakeVec, [| e |])

  let get_ref e = unsafe_nth (u8_of_int 0) e

  let set_ref e x = T.E3 (SetVec, u8_of_int 0, e, x)

  let chop_begin arr n = T.E2 (ChopBegin, arr, n)

  let chop_end arr n = T.E2 (ChopEnd, arr, n)

  let index c s = T.E2 (Index, c, s)
end

(* User constructors for the example user types: *)

let () =
  let open Ops in
  register_user_constructor "Date" TFloat [] ;
  register_user_constructor "Eth" TU48 [] ;
  register_user_constructor "Ip4" TU32 [] ;
  register_user_constructor "Ip6" TU128 [] ;
  let ip4_t = T.required (T.get_user_type "Ip4")
  and ip6_t = T.required (T.get_user_type "Ip6") in
  let ip_mns = [| "v4", ip4_t ; "v6", ip6_t |] in
  register_user_constructor "Ip" (TSum ip_mns)
    [ func1 ip4_t (fun x -> construct ip_mns 0 x) ;
      func1 ip6_t (fun x -> construct ip_mns 1 x) ] ;
  register_user_constructor "Cidr4"
    (TRec [| "ip", ip4_t ; "mask", T.required TU8 |])
    [ func2 ip4_t T.u8 (fun ip mask ->
        make_rec [ "ip", ip ; "mask", mask ]) ] ;
  register_user_constructor "Cidr6"
    (TRec [| "ip", ip6_t ; "mask", T.required TU8 |])
    [ func2 ip6_t T.u8 (fun ip mask ->
        make_rec [ "ip", ip ; "mask", mask ]) ] ;
  let cidr4_t = T.required (T.get_user_type "Cidr4")
  and cidr6_t = T.required (T.get_user_type "Cidr6") in
  let cidr_mns = [| "v4", cidr4_t ; "v6", cidr6_t |] in
  register_user_constructor "Cidr" (TSum cidr_mns)
    [ func1 cidr4_t (fun x -> construct cidr_mns 0 x) ;
      func1 cidr6_t (fun x -> construct cidr_mns 1 x) ]
