open Batteries
open Stdint
open DessserTools
open DessserFloatTools
module T = DessserTypes

(*$inject
  open Stdint
  module T = DessserTypes *)

(* Controls whether [debug] translates into Dump or Ignore: *)
let dump_debug = ref false

(* Controls whether optimisation is enabled: *)
let optimize = ref true

type endianness = LittleEndian | BigEndian

let string_of_endianness = function
  | LittleEndian -> "little-endian"
  | BigEndian -> "big-endian"

let print_endianness oc en =
  string_of_endianness en |> String.print oc

let endianness_of_string = function
  | "little-endian" -> LittleEndian
  | "big-endian" -> BigEndian
  | en -> invalid_arg ("endianness_of_string "^ en)

type param_id = int (* function id *) * int (* param number *)

let param_print oc (f, n) =
  Printf.fprintf oc "%d:%d" f n

type e0 =
  | Param of param_id
  (* Identifier are set with `Let` expressions, or obtained from the code
   * generators in exchange for an expression: *)
  | Identifier of string
  (* Contrary to identifiers which name can be arbitrary, an external identifier
   * name is used verbatim by the backend and must therefore correspond to a
   * valid object. *)
  | ExtIdentifier of string
  | Null of T.value_type
  | EndOfList of T.t (* T.t being the type of list items *)
  | EmptySet of T.maybe_nullable (* just an unsophisticated set *)
  | Now
  | RandomFloat
  | RandomU8
  | RandomU32
  | RandomU64
  | RandomU128
  | Unit
  | Float of float
  | String of string
  | Bool of bool
  | Char of char
  | U8 of Uint8.t
  | U16 of Uint16.t
  | U24 of Uint24.t
  | U32 of Uint32.t
  | U40 of Uint40.t
  | U48 of Uint48.t
  | U56 of Uint56.t
  | U64 of Uint64.t
  | U128 of Uint128.t
  | I8 of Int8.t
  | I16 of Int16.t
  | I24 of Int24.t
  | I32 of Int32.t
  | I40 of Int40.t
  | I48 of Int48.t
  | I56 of Int56.t
  | I64 of Int64.t
  | I128 of Int128.t
  | Bit of bool
  | Size of int
  | Byte of Uint8.t
  | Word of Uint16.t
  | DWord of Uint32.t
  | QWord of Uint64.t
  | OWord of Uint128.t
  | Bytes of Bytes.t
  | DataPtrOfString of string
  (* Constant mask actions: *)
  | CopyField
  | SkipField
  | SetFieldNull

type e0s =
  | Seq
  (* Data constructors: *)
  | MakeVec
  | MakeLst of T.maybe_nullable
  | MakeTup
  (* For convenience, MakeRec is handled like an E0S but it is constrained to
   * have an even number of arguments, the field names being forced to be
   * constant strings *)
  | MakeRec

type e1 =
  | Function of (*function id*) int * (*args*) T.t array
  | Comment of string
  | GetItem of int (* for tuples *)
  | GetField of string (* For records *)
  | GetAlt of string (* Destruct a sum type (See LabelOf) *)
  | Construct of (string * T.maybe_nullable) array (* type of the resulting sum *)
               * int (* Which alternative is constructed *)
  | Dump
  | Identity  (* Useful as a default function *)
  | Ignore
  | IsNull
  (* Turn e into a nullable: *)
  | NotNull
  (* Turn e into a not-nullable (or fail with the given message): *)
  | Force of string
  (* Convert from/to string for all base value types: *)
  | StringOfFloat
  | StringOfChar
  | StringOfInt
  | StringOfIp
  | FloatOfString
  | CharOfString
  | U8OfString
  | U16OfString
  | U24OfString
  | U32OfString
  | U40OfString
  | U48OfString
  | U56OfString
  | U64OfString
  | U128OfString
  | I8OfString
  | I16OfString
  | I24OfString
  | I32OfString
  | I40OfString
  | I48OfString
  | I56OfString
  | I64OfString
  | I128OfString
  (* Faster versions of the above, returning a pair with value and next pointer.
   * No test for error, input must be a valid number. *)
  | FloatOfPtr
  | CharOfPtr
  | U8OfPtr
  | U16OfPtr
  | U24OfPtr
  | U32OfPtr
  | U40OfPtr
  | U48OfPtr
  | U56OfPtr
  | U64OfPtr
  | U128OfPtr
  | I8OfPtr
  | I16OfPtr
  | I24OfPtr
  | I32OfPtr
  | I40OfPtr
  | I48OfPtr
  | I56OfPtr
  | I64OfPtr
  | I128OfPtr
  (* Integers and floats can be cast upon others regardless of sign or
   * width: *)
  | ToU8
  | ToU16
  | ToU24
  | ToU32
  | ToU40
  | ToU48
  | ToU56
  | ToU64
  | ToU128
  | ToI8
  | ToI16
  | ToI24
  | ToI32
  | ToI40
  | ToI48
  | ToI56
  | ToI64
  | ToI128
  | ToFloat
  | LogNot
  | FloatOfQWord
  | QWordOfFloat
  | U8OfByte
  | ByteOfU8
  | U16OfWord
  | WordOfU16
  | U32OfDWord
  | DWordOfU32
  | U64OfQWord
  | QWordOfU64
  | U128OfOWord
  | OWordOfU128
  | U8OfChar
  | CharOfU8
  | SizeOfU32
  | U32OfSize
  | BitOfBool
  | BoolOfBit
  | ListOfSList
  | ListOfSListRev
  | SetOfSList
  | ListOfVec
  | ListOfSet
  (* à la C: *)
  | U8OfBool
  | BoolOfU8
  | StringLength
  | StringOfBytes
  | BytesOfString
  | Cardinality (* of lists, vectors or sets *)
  | ReadByte
  | DataPtrPush
  | DataPtrPop
  | RemSize
  | DataPtrOffset
  | Not
  | Abs
  | Neg
  | Exp
  | Log
  | Log10
  | Sqrt
  | Ceil
  | Floor
  | Round
  | Cos
  | Sin
  | Tan
  | ACos
  | ASin
  | ATan
  | CosH
  | SinH
  | TanH
  | Lower
  | Upper
  | Hash (* Turns anything into an u64 *)
  (* WARNING: never use Fst and Snd on the same expression or that expression
   * will be computed twice!
   * Instead, use MapPair or Let *)
  | Fst
  | Snd
  | Head
  | Tail
  | ReadWord of endianness
  | ReadDWord of endianness
  | ReadQWord of endianness
  | ReadOWord of endianness
  | Assert
  (* For tuples, int is the index of the item in the tuple.
   * For records, the index of the field in definition order: *)
  | MaskGet of int
  (* Given a value of a sum type, return the integer label associated with its
   * constructor, as an u16: *)
  | LabelOf
  (* Various set implementations, configured with their max size: *)
  | SlidingWindow of T.maybe_nullable (* Sliding window of the last N added items *)
  | TumblingWindow of T.maybe_nullable (* Tumbling window *)
  | Sampling of T.maybe_nullable (* Reservoir sampling of N items *)
  (* A set with an O(1) implementation of Member, N is the initial size: *)
  | HashTable of T.maybe_nullable
  (* A set that order items according to a given comparison function
   * (given as first and only argument, this function also provides the
   * set elements' type): *)
  | Heap
  | DataPtrOfBuffer
  | GetEnv
  (* Get the minimal value of a set (heap): *)
  | GetMin

type e1s =
  | Apply

type e2 =
  | Let of string
  (* Deconstructor for vectors/lists: *)
  | Nth
  (* Comparators: *)
  | Gt
  | Ge
  | Eq
  | Ne
  (* Arithmetic operators returning same type as their inputs, which must
   * be of the same type (namely, any numeric). *)
  | Add
  | Sub
  | Mul
  | Div (* Fails with Null *)
  | Rem (* Fails with Null *)
  | Pow (* Fails with Null *)
  | LogAnd
  | LogOr
  | LogXor
  | LeftShift
  | RightShift
  | AppendByte
  | AppendBytes
  | AppendString
  | StartsWith
  | EndsWith
  | GetBit
  | GetVec (* first the index then the vector or list (as in Getfield) *)
  | ReadBytes
  | PeekByte
  | WriteByte
  | WriteBytes
  | PokeByte
  | DataPtrAdd
  | DataPtrSub
  | And
  | Or
  | Cons
  | Pair
  (* MapPair takes the pair then the function of 2 arguments returning whatever.
   * Note: the result is whatever returns the function, not necessarily a
   * pair! FIXME: that name is misleading ; also, should be part of stdlib *)
  | MapPair
  | Map (* Map over any iterable (list, slist, vector) *)
  | Min
  | Max
  (* Membership test for vectors, lists and sets; Not for CIDRs nor strings.
   * Args are: item, container *)
  | Member
  | PeekWord of endianness
  | PeekDWord of endianness
  | PeekQWord of endianness
  | PeekOWord of endianness
  | WriteWord of endianness
  | WriteDWord of endianness
  | WriteQWord of endianness
  | WriteOWord of endianness
  | Insert (* args are: set, item *)
  (* Not implemented for all types of sets. args are: set, how many.
   * Will merely empty the set if number of deleted item is greater than
   * cardinality. *)
  | DelMin
  | SplitBy
  | SplitAt
  (* Parameters are separator and list/vector: *)
  | Join
  (* Parameters are size and item initial value *)
  | AllocLst
  (* Sort (inplace) the 1st parameter (vector or list) until the indices given
   * in the 2sn parameter have reached their final location. Other part of the
   * array might not be sorted. *)
  | PartialSort
  (* Remove the first items from a list (args are the list and the length to
   * remove): *)
  | ChopBegin
  (* Truncate a list at the end (args are the list and the length to
   * remove): *)
  | ChopEnd
  (* Scale the weight of a weighted set (ie. top) *)
  | ScaleWeights

type e3 =
  | SetBit
  (* Similarly to GetVec: first the index, then the vector or list, then
   * the value *)
  | SetVec
  | BlitByte
  | If (* Condition * Consequent * Alternative *)
  | LoopWhile (* Condition ('a->bool) * Loop body ('a->'a) * Initial value *)
  (* Unline LoopWhile, LoopUntil executes the body at least once *)
  | LoopUntil (* Loop body ('a->'a) * Condition ('a->bool) * Initial value *)
  | Fold (* args are: init, function, list/vector/set *)
  (* Get a slice from a pointer, starting at given offset and shortened to
   * given length: *)
  | DataPtrOfPtr
  (* bool to indicate the search direction (true = from start), then the needle
   * and finally the haystack. *)
  | FindSubstring
  (* Parameters are: size, max_size and sigmas.
   * Tops can make use of the insert_weighted to specify the weight to be used
   * for each item, and the downscale operators to decay old entries (first
   * inflate the weight in time, and periodically downscale the whole set) *)
  | Top of T.maybe_nullable
  (* Insert with an explicit weight. Args are: the set, the weight and the
   * value. *)
  | InsertWeighted


type e4 =
  | ReadWhile
      (* Cond (byte->bool) * Reducer ('a->byte->'a) * Init ('a) * Start pos ->
           Result ('a*ptr)
        Read whenever cond returns true, or the input stream is exhausted *)
  | Repeat (* From * To * body (idx->'a->'a) * Init value *)

type t =
  | E0 of e0
  | E0S of e0s * t list
  | E1 of e1 * t
  | E1S of e1s * t * t list
  | E2 of e2 * t * t
  | E3 of e3 * t * t * t
  | E4 of e4 * t * t * t * t

let rec e0_eq e1 e2 =
  match e1, e2 with
  | Null vt1, Null vt2 ->
      T.value_type_eq vt1 vt2
  | EndOfList t1, EndOfList t2 ->
      T.eq t1 t2
  | EmptySet t1, EmptySet t2 ->
      T.maybe_nullable_eq t1 t2
  | e1, e2 ->
      (* Assuming here and below that when the constructors are different
       * the generic equality does not look t the fields and therefore won't
       * encounter functional values: *)
      e1 = e2

and e0s_eq e1 e2 = e1 = e2

and e1_eq e1 e2 =
  match e1, e2 with
  | Function (fid1, typ1), Function (fid2, typ2) ->
      fid1 = fid2 && Array.for_all2 T.eq typ1 typ2
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
  | E0 op1, E0 op2 ->
      e0_eq op1 op2
  | E0S (op1, e1s), E0S (op2, e2s) ->
      e0s_eq op1 op2 &&
      (try List.for_all2 (eq) e1s e2s
      with Invalid_argument _ -> false)
  | E1 (op1, e11), E1 (op2, e21) ->
      e1_eq op1 op2 && eq e11 e21
  | E1S (op1, e11, e1s), E1S (op2, e21, e2s) ->
      e1s_eq op1 op2 && eq e11 e21 && eq_lst e1s e2s
  | E2 (op1, e11, e12), E2 (op2, e21, e22) ->
      e2_eq op1 op2 && eq e11 e21 && eq e12 e22
  | E3 (op1, e11, e12, e13), E3 (op2, e21, e22, e23) ->
      e3_eq op1 op2 && eq e11 e21 && eq e12 e22 && eq e13 e23
  | E4 (op1, e11, e12, e13, e14), E4 (op2, e21, e22, e23, e24) ->
      e4_eq op1 op2 && eq e11 e21 && eq e12 e22 && eq e13 e23 && eq e14 e24
  | _ -> false

(* Variables of that type are usually called "l" *)
type env = (t * T.t) list

(* Note re. Apply: even if the function can be precomputed (which it usually
 * can) and its parameters as well, the application can be precomputed only
 * if the function body can in a context where the parameters can.
 * Here, p is a list of parameters (function id) that can be precomputed
 * and i a set of identifiers (names) that can. *)
let rec can_precompute l i = function
  | E0 (Now | RandomFloat | RandomU8 | RandomU32 | RandomU64 | RandomU128) ->
      false
  | E0 (Null _ | EndOfList _ | EmptySet _ | Unit | Float _ | String _ | Bool _
       | U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _
       | I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _
       | Char _ | Bit _ | Size _ | Byte _ | Word _ | DWord _ | QWord _ | OWord _
       | Bytes _ | DataPtrOfString _
       | CopyField | SkipField | SetFieldNull) ->
      true
  | E0 (Param (fid, _)) ->
      List.mem fid l
  | E0 (Identifier n | ExtIdentifier n) ->
      List.mem n i
  | E0S (_, es) ->
      List.for_all (can_precompute l i) es
  | E1 (Function _, body) ->
      can_precompute l i body
  | E1 ((Dump | DataPtrPush | DataPtrPop | Assert | MaskGet _), _) ->
      false
  | E1 (_, e) -> can_precompute l i e
  | E1S (Apply, E1 (Function (fid, _), body), e2s) ->
      List.for_all (can_precompute l i) e2s &&
      can_precompute (fid :: l) i body
  | E1S (Apply, _, _) ->
      false
  | E2 (Let n, e1, e2) ->
      can_precompute l i e1 &&
      can_precompute l (n :: i) e2
  | E2 (MapPair, _, _) ->
      false (* TODO *)
  | E2 (Map, e1, e2) ->
      can_precompute l i e1 &&
      can_precompute l i e2
  | E2 (_, e1, e2) ->
      can_precompute l i e1 &&
      can_precompute l i e2
  | E3 ((LoopWhile | LoopUntil | Fold), _, _, _) ->
      false (* TODO *)
  | E3 (Top _, _, _, _) ->
      false
  | E3 (_, e1, e2, e3) ->
      can_precompute l i e1 &&
      can_precompute l i e2 &&
      can_precompute l i e3
  | E4 ((ReadWhile | Repeat), _, _, _, _) ->
      false

let is_const_null = function
  | E0 (Null _) -> true
  | _ -> false

(* Given a type, returns the simplest expression of that type - suitable
 * whenever a default value is required. *)
let rec default_value ?(allow_null=true) = function
  | T.{ vtyp ; nullable = true } ->
      (* In some places we want the whole tree of values to be populated. *)
      if allow_null then
        E0 (Null vtyp)
      else
        default_value ~allow_null { vtyp ; nullable = false }
  | { vtyp = T.Unknown ; _ }
  | { vtyp = T.Ext _ ; _ } ->
      invalid_arg "default_value"
  | { vtyp = T.Unit ; _ } ->
      E0 Unit
  | { vtyp = Mac Float ; _ } ->
      E0 (Float 0.)
  | { vtyp = Mac String ; _ } ->
      E0 (String "")
  | { vtyp = Mac Bool ; _ } ->
      E0 (Bool false)
  | { vtyp = Mac Char ; _ } ->
      E0 (Char '\000')
  | { vtyp = Mac I8 ; _ } ->
      E0 (I8 Int8.zero)
  | { vtyp = Mac I16 ; _ } ->
      E0 (I16 Int16.zero)
  | { vtyp = Mac I24 ; _ } ->
      E0 (I24 Int24.zero)
  | { vtyp = Mac I32 ; _ } ->
      E0 (I32 Int32.zero)
  | { vtyp = Mac I40 ; _ } ->
      E0 (I40 Int40.zero)
  | { vtyp = Mac I48 ; _ } ->
      E0 (I48 Int48.zero)
  | { vtyp = Mac I56 ; _ } ->
      E0 (I56 Int56.zero)
  | { vtyp = Mac I64 ; _ } ->
      E0 (I64 Int64.zero)
  | { vtyp = Mac I128 ; _ } ->
      E0 (I128 Int128.zero)
  | { vtyp = Mac U8 ; _ } ->
      E0 (U8 Uint8.zero)
  | { vtyp = Mac U16 ; _ } ->
      E0 (U16 Uint16.zero)
  | { vtyp = Mac U24 ; _ } ->
      E0 (U24 Uint24.zero)
  | { vtyp = Mac U32 ; _ } ->
      E0 (U32 Uint32.zero)
  | { vtyp = Mac U40 ; _ } ->
      E0 (U40 Uint40.zero)
  | { vtyp = Mac U48 ; _ } ->
      E0 (U48 Uint48.zero)
  | { vtyp = Mac U56 ; _ } ->
      E0 (U56 Uint56.zero)
  | { vtyp = Mac U64 ; _ } ->
      E0 (U64 Uint64.zero)
  | { vtyp = Mac U128 ; _ } ->
      E0 (U128 Uint128.zero)
  | { vtyp = Usr nn ; _ } ->
      default_value ~allow_null { vtyp = nn.def ; nullable = false }
  | { vtyp = Tup mns ; _ } ->
      E0S (
        MakeTup,
        Array.map (default_value ~allow_null) mns |>
        Array.to_list)
  | { vtyp = Rec mns ; _ } ->
      E0S (
        MakeRec,
        Array.fold_left (fun fields (fn, mn) ->
          E0 (String fn) :: default_value mn :: fields
        ) [] mns)
  | { vtyp = Sum mns ; _ } ->
      assert (Array.length mns > 0) ;
      E1 (
        Construct (mns, 0),
        default_value (snd mns.(0)))
  | { vtyp = Vec (dim, mn) ; _ } ->
      E0S (
        MakeVec,
        List.init dim (fun _ -> default_value mn))
  | { vtyp = Lst mn ; _ } ->
      E0S (MakeLst mn, [])
  | { vtyp = Set (Simple, mn) ; _ } ->
      E0 (EmptySet mn)
  | { vtyp = Set (Sliding, mn) ; _ } ->
      E1 (SlidingWindow mn, E0 (U8 Uint8.zero))
  | { vtyp = Set (Tumbling, mn) ; _ } ->
      E1 (TumblingWindow mn, E0 (U8 Uint8.zero))
  | { vtyp = Set (Sampling, mn) ; _ } ->
      E1 (Sampling mn, E0 (U8 Uint8.zero))
  | { vtyp = Set (HashTable, mn) ; _ } ->
      E1 (HashTable mn, E0 (U8 Uint8.zero))
  | { vtyp = Set (Heap, mn) ; _ } ->
      let cmp = E0S (Seq, [ E1 (Ignore, (E0 (Param (0, 0)))) ;
                            E1 (Ignore, (E0 (Param (0, 1)))) ]) in
      E1 (Heap, E1 (Function (0, [| T.Value mn ; T.Value mn |]), cmp))
  | { vtyp = Set (Top, mn) ; _ } ->
      let size = E0 (U8 Uint8.one) in
      let max_size = size
      and sigmas = E0 (Float 0.) in
      E3 (Top mn, size, max_size, sigmas)
  | { vtyp = Map _ ; _ } ->
      assert false (* no value of map type *)

let string_of_path = IO.to_string T.print_path

let string_of_e0s = function
  | Seq -> "seq"
  | MakeVec -> "make-vec"
  | MakeLst mn -> "make-lst "^ String.quote (T.string_of_maybe_nullable mn)
  | MakeTup -> "make-tup"
  | MakeRec -> "make-rec"

let string_of_e1s = function
  | Apply -> "apply"

let string_of_e1 = function
  | Function (fid, typs) ->
      "fun "^ string_of_int fid ^
      (if typs = [||] then "" else
       IO.to_string (Array.print ~first:" " ~sep:" " ~last:"" (fun oc t ->
         Printf.fprintf oc "%S" (IO.to_string T.print t))) typs)
  | Comment s -> "comment "^ String.quote s
  | GetItem n -> "get-item "^ string_of_int n
  | GetField s -> "get-field "^ String.quote s
  | GetAlt s -> "get-alt "^ String.quote s
  | Construct (mns, i) ->
      "construct "^ String.quote (IO.to_string T.print_value_type (Sum mns))
                  ^" "^ string_of_int i
  | Dump -> "dump"
  | Identity -> "identity"
  | Ignore -> "ignore"
  | IsNull -> "is-null"
  | NotNull -> "not-null"
  | Force what -> "force "^ String.quote what
  | StringOfFloat -> "string-of-float"
  | StringOfChar -> "string-of-char"
  | StringOfInt -> "string-of-int"
  | StringOfIp -> "string-of-ip"
  | FloatOfString -> "float-of-string"
  | CharOfString -> "char-of-string"
  | U8OfString -> "u8-of-string"
  | U16OfString -> "u16-of-string"
  | U24OfString -> "u24-of-string"
  | U32OfString -> "u32-of-string"
  | U40OfString -> "u40-of-string"
  | U48OfString -> "u48-of-string"
  | U56OfString -> "u56-of-string"
  | U64OfString -> "u64-of-string"
  | U128OfString -> "u128-of-string"
  | I8OfString -> "i8-of-string"
  | I16OfString -> "i16-of-string"
  | I24OfString -> "i24-of-string"
  | I32OfString -> "i32-of-string"
  | I40OfString -> "i40-of-string"
  | I48OfString -> "i48-of-string"
  | I56OfString -> "i56-of-string"
  | I64OfString -> "i64-of-string"
  | I128OfString -> "i128-of-string"
  | FloatOfPtr -> "float-of-ptr"
  | CharOfPtr -> "char-of-ptr"
  | U8OfPtr -> "u8-of-ptr"
  | U16OfPtr -> "u16-of-ptr"
  | U24OfPtr -> "u24-of-ptr"
  | U32OfPtr -> "u32-of-ptr"
  | U40OfPtr -> "u40-of-ptr"
  | U48OfPtr -> "u48-of-ptr"
  | U56OfPtr -> "u56-of-ptr"
  | U64OfPtr -> "u64-of-ptr"
  | U128OfPtr -> "u128-of-ptr"
  | I8OfPtr -> "i8-of-ptr"
  | I16OfPtr -> "i16-of-ptr"
  | I24OfPtr -> "i24-of-ptr"
  | I32OfPtr -> "i32-of-ptr"
  | I40OfPtr -> "i40-of-ptr"
  | I48OfPtr -> "i48-of-ptr"
  | I56OfPtr -> "i56-of-ptr"
  | I64OfPtr -> "i64-of-ptr"
  | I128OfPtr -> "i128-of-ptr"
  | ToU8 -> "to-u8"
  | ToU16 -> "to-u16"
  | ToU24 -> "to-u24"
  | ToU32 -> "to-u32"
  | ToU40 -> "to-u40"
  | ToU48 -> "to-u48"
  | ToU56 -> "to-u56"
  | ToU64 -> "to-u64"
  | ToU128 -> "to-u128"
  | ToI8 -> "to-i8"
  | ToI16 -> "to-i16"
  | ToI24 -> "to-i24"
  | ToI32 -> "to-i32"
  | ToI40 -> "to-i40"
  | ToI48 -> "to-i48"
  | ToI56 -> "to-i56"
  | ToI64 -> "to-i64"
  | ToI128 -> "to-i128"
  | ToFloat -> "to-float"
  | LogNot -> "log-not"
  | FloatOfQWord -> "float-of-qword"
  | QWordOfFloat -> "qword-of-float"
  | U8OfByte -> "u8-of-byte"
  | ByteOfU8 -> "byte-of-u8"
  | U16OfWord -> "u16-of-word"
  | WordOfU16 -> "word-of-u16"
  | U32OfDWord -> "u32-of-dword"
  | DWordOfU32 -> "dword-of-u32"
  | U64OfQWord -> "u64-of-qword"
  | QWordOfU64 -> "qword-of-u64"
  | U128OfOWord -> "u128-of-oword"
  | OWordOfU128 -> "oword-of-u128"
  | U8OfChar -> "u8-of-char"
  | CharOfU8 -> "char-of-u8"
  | SizeOfU32 -> "size-of-u32"
  | U32OfSize -> "u32-of-size"
  | BitOfBool -> "bit-of-bool"
  | BoolOfBit -> "bool-of-bit"
  | ListOfSList -> "list-of-slist"
  | ListOfSListRev -> "list-of-slist-rev"
  | SetOfSList -> "set-of-slist"
  | ListOfVec -> "list-of-vec"
  | ListOfSet -> "list-of-set"
  | U8OfBool -> "u8-of-bool"
  | BoolOfU8 -> "bool-of-u8"
  | StringLength -> "string-length"
  | StringOfBytes -> "string-of-bytes"
  | BytesOfString -> "bytes-of-string"
  | Cardinality -> "cardinality"
  | ReadByte -> "read-byte"
  | DataPtrPush -> "data-ptr-push"
  | DataPtrPop -> "data-ptr-pop"
  | RemSize -> "rem-size"
  | DataPtrOffset -> "data-ptr-offset"
  | Not -> "not"
  | Abs -> "abs"
  | Neg -> "neg"
  | Exp -> "exp"
  | Log -> "log"
  | Log10 -> "log10"
  | Sqrt -> "sqrt"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Round -> "round"
  | Cos -> "cos"
  | Sin -> "sin"
  | Tan -> "tan"
  | ACos -> "acos"
  | ASin -> "asin"
  | ATan -> "atan"
  | CosH -> "cosh"
  | SinH -> "sinh"
  | TanH -> "tanh"
  | Lower -> "lower"
  | Upper -> "upper"
  | Hash -> "hash"
  | Fst -> "fst"
  | Snd -> "snd"
  | Head -> "head"
  | Tail -> "tail"
  | ReadWord en -> "read-word "^ string_of_endianness en
  | ReadDWord en -> "read-dword "^ string_of_endianness en
  | ReadQWord en -> "read-qword "^ string_of_endianness en
  | ReadOWord en -> "read-oword "^ string_of_endianness en
  | Assert -> "assert"
  | MaskGet d -> "mask-get "^ string_of_int d
  | LabelOf -> "label-of"
  | SlidingWindow mn ->
      "sliding-window "^ String.quote (T.string_of_maybe_nullable mn)
  | TumblingWindow mn ->
      "tumbling-window "^ String.quote (T.string_of_maybe_nullable mn)
  | Sampling mn ->
      "sampling "^ String.quote (T.string_of_maybe_nullable mn)
  | HashTable mn ->
      "hash-table "^ String.quote (T.string_of_maybe_nullable mn)
  | Heap ->
      "heap"
  | DataPtrOfBuffer -> "data-ptr-of-buffer"
  | GetEnv -> "getenv"
  | GetMin -> "get-min"

let string_of_e2 = function
  | Let s -> "let "^ String.quote s
  | Nth -> "nth"
  | Gt -> "gt"
  | Ge -> "ge"
  | Eq -> "eq"
  | Ne -> "ne"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Rem -> "rem"
  | Pow -> "pow"
  | LogAnd -> "log-and"
  | LogOr -> "log-or"
  | LogXor -> "log-xor"
  | LeftShift -> "left-shift"
  | RightShift -> "right-shift"
  | AppendByte -> "append-byte"
  | AppendBytes -> "append-bytes"
  | AppendString -> "append-string"
  | StartsWith -> "starts-with"
  | EndsWith -> "ends-with"
  | GetBit -> "get-bit"
  | GetVec -> "get-vec"
  | ReadBytes -> "read-bytes"
  | PeekByte -> "peek-byte"
  | WriteByte -> "write-byte"
  | WriteBytes -> "write-bytes"
  | PokeByte -> "poke-byte"
  | DataPtrAdd -> "data-ptr-add"
  | DataPtrSub -> "data-ptr-sub"
  | And -> "and"
  | Or -> "or"
  | Cons -> "cons"
  | Pair -> "pair"
  | MapPair -> "map-pair"
  | Map -> "map"
  | Min -> "min"
  | Max -> "max"
  | Member -> "mem"
  | PeekWord en -> "peek-word "^ string_of_endianness en
  | PeekDWord en -> "peek-dword "^ string_of_endianness en
  | PeekQWord en -> "peek-qword "^ string_of_endianness en
  | PeekOWord en -> "peek-oword "^ string_of_endianness en
  | WriteWord en -> "write-word "^ string_of_endianness en
  | WriteDWord en -> "write-dword "^ string_of_endianness en
  | WriteQWord en -> "write-qword "^ string_of_endianness en
  | WriteOWord en -> "write-oword "^ string_of_endianness en
  | Insert -> "insert"
  | DelMin -> "del-min"
  | SplitBy -> "split-on"
  | SplitAt -> "split-at"
  | Join -> "join"
  | AllocLst -> "alloc-lst"
  | PartialSort -> "partial-sort"
  | ChopBegin -> "chop-begin"
  | ChopEnd -> "chop-end"
  | ScaleWeights -> "scale-weights"

let string_of_e3 = function
  | SetBit -> "set-bit"
  | SetVec -> "set-vec"
  | BlitByte -> "blit-byte"
  | If -> "if"
  | LoopWhile -> "loop-while"
  | LoopUntil -> "loop-until"
  | Fold -> "fold"
  | DataPtrOfPtr -> "data-ptr-of-ptr"
  | FindSubstring -> "find-substring"
  | Top mn -> "top "^ String.quote (T.string_of_maybe_nullable mn)
  | InsertWeighted -> "insert-weighted"

let string_of_e4 = function
  | ReadWhile -> "read-while"
  | Repeat -> "repeat"

let pp = Printf.fprintf

let rec string_of_e0 = function
  | Param (fid, n) -> "param "^ string_of_int fid ^" "^ string_of_int n
  | Null vt -> "null "^ String.quote (IO.to_string T.print_value_type vt)
  | EndOfList t -> "end-of-list "^ String.quote (IO.to_string T.print t)
  | EmptySet mn -> "empty-set "^ String.quote (T.string_of_maybe_nullable mn)
  | Now -> "now"
  | RandomFloat -> "random-float"
  | RandomU8 -> "random-u8"
  | RandomU32 -> "random-u32"
  | RandomU64 -> "random-u64"
  | RandomU128 -> "random-u128"
  | Float f -> "float "^ hexstring_of_float f
  | Unit -> "()"
  | String s -> "string "^ String.quote s
  | Bool b -> "bool "^ Bool.to_string b
  | Char c -> "char "^ String.quote (String.of_char c)
  | U8 n -> "u8 "^ Uint8.to_string n
  | U16 n -> "u16 "^ Uint16.to_string n
  | U24 n -> "u24 "^ Uint24.to_string n
  | U32 n -> "u32 "^ Uint32.to_string n
  | U40 n -> "u40 "^ Uint40.to_string n
  | U48 n -> "u48 "^ Uint48.to_string n
  | U56 n -> "u56 "^ Uint56.to_string n
  | U64 n -> "u64 "^ Uint64.to_string n
  | U128 n -> "u128 "^ Uint128.to_string n
  | I8 n -> "i8 "^ Int8.to_string n
  | I16 n -> "i16 "^ Int16.to_string n
  | I24 n -> "i24 "^ Int24.to_string n
  | I32 n -> "i32 "^ Int32.to_string n
  | I40 n -> "i40 "^ Int40.to_string n
  | I48 n -> "i48 "^ Int48.to_string n
  | I56 n -> "i56 "^ Int56.to_string n
  | I64 n -> "i64 "^ Int64.to_string n
  | I128 n -> "i128 "^ Int128.to_string n
  | Bit b -> "bit "^ Bool.to_string b
  | Size n -> "size "^ string_of_int n
  | Byte n -> "byte "^ Uint8.to_string n
  | Word n -> "word "^ Uint16.to_string n
  | DWord n -> "dword "^ Uint32.to_string n
  | QWord n -> "qword "^ Uint64.to_string n
  | OWord n -> "oword "^ Uint128.to_string n
  | Bytes s -> "bytes "^ String.quote (Bytes.to_string s)
  | DataPtrOfString s -> "data-ptr-of-string "^ String.quote s
  | Identifier s -> "identifier "^ String.quote s
  | ExtIdentifier s -> "ext-identifier "^ String.quote s
  | CopyField -> "copy-field"
  | SkipField -> "skip-field"
  | SetFieldNull -> "set-field-null"

(* Display in a single line to help with tests. *)
and print ?max_depth oc e =
  if Option.map_default (fun m -> m <= 0) false max_depth then
    pp oc "…"
  else
    let max_depth = Option.map pred max_depth in
    let p = print ?max_depth in
    match e with
    | E0 op ->
        pp oc "(%s)" (string_of_e0 op)
    | E0S (op, es) ->
        pp oc "(%s%s%a)"
          (string_of_e0s op)
          (if es = [] then "" else " ")
          (List.print ~first:"" ~last:"" ~sep:" " p) es
    | E1 (op, e1) ->
        pp oc "(%s %a)" (string_of_e1 op) p e1
    | E1S (op, e1, es) ->
        pp oc "(%s %a%s%a)"
          (string_of_e1s op)
          p e1
          (if es = [] then "" else " ")
          (List.print ~first:"" ~last:"" ~sep:" " p) es
    | E2 (op, e1, e2) ->
        pp oc "(%s %a %a)" (string_of_e2 op) p e1 p e2
    | E3 (op, e1, e2, e3) ->
        pp oc "(%s %a %a %a)" (string_of_e3 op) p e1 p e2 p e3
    | E4 (op, e1, e2, e3, e4) ->
        pp oc "(%s %a %a %a %a)" (string_of_e4 op) p e1 p e2 p e3 p e4

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
    | E0 op ->
        p (string_of_e0 op) []
    | E0S (op, es) ->
        p (string_of_e0s op) es
    | E1 (op, e1) ->
        p (string_of_e1 op) [ e1 ]
    | E1S (op, e1, es) ->
        p (string_of_e1s op) (e1 :: es)
    | E2 (op, e1, e2) ->
        p (string_of_e2 op) [ e1 ; e2 ]
    | E3 (op, e1, e2, e3) ->
        p (string_of_e3 op) [ e1 ; e2 ; e3 ]
    | E4 (op, e1, e2, e3, e4) ->
        p (string_of_e4 op) [ e1 ; e2 ; e3 ; e4 ]

let to_pretty_string ?max_depth e =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "@.    @[<hov 4>%a@]@." (pretty_print ?max_depth) e ;
  Buffer.contents buf

let to_cst_int =
  let m = max_int in
  function
  | E0 (U8 n) -> Uint8.to_int n
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
  | _ -> invalid_arg "to_cst_int"

module Parser =
struct
  (* String representation of expressions are mere s-expressions.
   * strings are represented as OCaml quoted strings. *)
  type context = Blank | Enter | Leave | Symbol | String

  (* Returns both the tokens and the number of characters read from [str]: *)
  let rec tok str res i =
    let can_be_symbol c =
      (* Notice that numbers are also seen as symbols from the sexpr parser point
       * of view *)
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'A') ||
      (c >= '0' && c <= '9') ||
      c = '_' || c = '-' || c = '+' || c = '.' in
    let ctx = match res with (ctx, _)::_ -> ctx | [] -> Blank in
    if i >= String.length str then List.rev res, i
    else if Char.is_whitespace str.[i] then
      let res =
        if ctx = Blank || ctx = String then res
        else (Blank, i) :: res in
      tok str res (i + 1)
    else if str.[i] = '(' then
      let res =
        if ctx = String then res
        else (Enter, i) :: res in
      tok str res (i + 1)
    else if str.[i] = ')' then
      let res =
        if ctx = String then res
        else (Leave, i) :: res in
      tok str res (i + 1)
    else if str.[i] = '"' then
      let res =
        (* String start and stop with the double quotes, included: *)
        if ctx = String then (Blank, i + 1) :: res
        else (String, i) :: res in
      tok str res (i + 1)
    else if str.[i] = '\\' && ctx = String && i < String.length str - 1 then
      tok str res (i + 2)
    else if str.[i] = ';' && ctx <> String then
      let i' =
        match String.index_from str i '\n' with
        | exception Not_found -> String.length str
        | nl_pos -> nl_pos + 1 in
      let res = if ctx = Blank then res else (Blank, i) :: res in
      tok str res i'
    else
      if ctx = String then
        tok str res (i + 1)
      else if can_be_symbol str.[i] then
        if ctx = Symbol then
          tok str res (i + 1)
        else tok str ((Symbol, i) :: res) (i + 1)
      else
        (* The end *)
        List.rev res, i

  type sexpr =
    | Sym of string
    | Str of string
    | Lst of sexpr list

  let print_sexpr oc =
    let rec loop indent sep oc = function
      | Sym s ->
          Printf.fprintf oc "%s%s" sep s
      | Str s ->
          Printf.fprintf oc "%s%S" sep s
      | Lst lst ->
          let indent = indent ^"  " in
          Printf.fprintf oc "\n%s(" indent ;
          List.iteri (fun i x ->
            loop indent (if i > 0 then " " else "") oc x ;
          ) lst ;
          Printf.fprintf oc ")" in
    loop "" "" oc

  exception Unknown_expression of sexpr
  exception Extraneous_expressions of int
  exception Garbage_after of int
  exception Must_be_integer of sexpr * string

  let () =
    Printexc.register_printer (function
      | Unknown_expression x ->
          Some (Printf.sprintf2 "Unknown expression: %a" print_sexpr x)
      | Extraneous_expressions i ->
          Some ("Extraneous expressions at position "^ string_of_int i)
      | Garbage_after i ->
          Some ("Cannot parse expressions after position "^ string_of_int i)
      | Must_be_integer (x, d) ->
          Some (Printf.sprintf2 "%S must be an integer in %a" d print_sexpr x)
      | _ ->
          None)

  let sexpr_of_toks toks str =
    let add_sym sta sto lst =
      Sym (String.sub str sta (sto-sta)) :: lst in
    let add_str sta sto lst =
      Str (Scanf.sscanf (String.sub str sta (sto-sta)) "%S" identity) :: lst in
    let add_blk _sta _sto lst = lst in
    let rec loop lst adder = function
      | [] ->
          let sto = String.length str in
          List.rev (adder sto lst),
          []
      | (Symbol, i) :: rest -> loop (adder i lst) (add_sym i) rest
      | (String, i) :: rest -> loop (adder i lst) (add_str i) rest
      | (Blank, i) :: rest -> loop (adder i lst) (add_blk i) rest
      | (Enter, i) :: rest ->
          let lst = adder i lst in
          let sublst, rest = loop [] (add_blk i) rest in
          loop (Lst sublst :: lst) (add_blk i) rest
      | (Leave, i) :: rest ->
          List.rev (adder i lst),
          rest
    in
    let sublst, rest = loop [] (add_blk 0) toks in
    (match rest with
    | (_, i) :: _ -> raise (Extraneous_expressions i)
    | [] -> ()) ;
    sublst

  let sexpr_of_string str =
    let toks, i = tok str [] 0 in
    if i < String.length str then raise (Garbage_after i) ;
    sexpr_of_toks toks str

  (*$< Parser *)

  (*$= sexpr_of_string & ~printer:(BatIO.to_string (BatList.print print_sexpr))
    [ Sym "glop" ] (sexpr_of_string "glop")
    [ Str "glop" ] (sexpr_of_string "\"glop\"")
    [ Lst [ Sym "pas" ; Sym "glop" ] ] (sexpr_of_string "(pas glop)")
    [ Lst [ Sym "pas" ; Sym "glop" ] ] (sexpr_of_string " (pas   glop ) ")
    [ Lst [ Lst [ Sym "pas" ; Str "glop" ] ; Lst [ Sym "glop" ] ] ] \
      (sexpr_of_string "((pas \"glop\") (glop))")
    [ Lst [ Sym "null" ; Str "u8" ] ] (sexpr_of_string "(null \"u8\")")
    [ Sym "glop" ] (sexpr_of_string "glop ; comment")
  *)

  let int_of_symbol x d =
    try int_of_string d
    with _ -> raise (Must_be_integer (x, d))

  let rec e = function
    (* e0 *)
    | Lst [ Sym "param" ; Sym fid ; Sym n ] ->
        E0 (Param (int_of_string fid, int_of_string n))
    | Lst [ Sym "null" ; Str vt ] ->
        E0 (Null (T.value_type_of_string vt))
    | Lst [ Sym ("end-of-list" | "eol") ; Str t ] ->
        E0 (EndOfList (T.Parser.of_string t))
    | Lst [ Sym "empty-set" ; Str mn ] ->
        E0 (EmptySet (T.maybe_nullable_of_string mn))
    | Lst [ Sym "now" ] -> E0 Now
    | Lst [ Sym "random-float" ] -> E0 RandomFloat
    | Lst [ Sym "random-u8" ] -> E0 RandomU8
    | Lst [ Sym "random-u32" ] -> E0 RandomU32
    | Lst [ Sym "random-u64" ] -> E0 RandomU64
    | Lst [ Sym "random-u128" ] -> E0 RandomU128
    | Lst [ Sym "float" ; Sym f ] -> E0 (Float (float_of_anystring f))
    | Lst [] -> E0 (Unit)
    | Lst [ Sym "string" ; Str s ] -> E0 (String s)
    | Lst [ Sym "bool" ; Sym b ] -> E0 (Bool (Bool.of_string b))
    | Lst [ Sym "char" ; Str c ] -> assert (String.length c = 1) ; E0 (Char c.[0])
    | Lst [ Sym "u8" ; Sym n ] -> E0 (U8 (Uint8.of_string n))
    | Lst [ Sym "u16" ; Sym n ] -> E0 (U16 (Uint16.of_string n))
    | Lst [ Sym "u24" ; Sym n ] -> E0 (U24 (Uint24.of_string n))
    | Lst [ Sym "u32" ; Sym n ] -> E0 (U32 (Uint32.of_string n))
    | Lst [ Sym "u40" ; Sym n ] -> E0 (U40 (Uint40.of_string n))
    | Lst [ Sym "u48" ; Sym n ] -> E0 (U48 (Uint48.of_string n))
    | Lst [ Sym "u56" ; Sym n ] -> E0 (U56 (Uint56.of_string n))
    | Lst [ Sym "u64" ; Sym n ] -> E0 (U64 (Uint64.of_string n))
    | Lst [ Sym "u128" ; Sym n ] -> E0 (U128 (Uint128.of_string n))
    | Lst [ Sym "i8" ; Sym n ] -> E0 (I8 (Int8.of_string n))
    | Lst [ Sym "i16" ; Sym n ] -> E0 (I16 (Int16.of_string n))
    | Lst [ Sym "i24" ; Sym n ] -> E0 (I24 (Int24.of_string n))
    | Lst [ Sym "i32" ; Sym n ] -> E0 (I32 (Int32.of_string n))
    | Lst [ Sym "i40" ; Sym n ] -> E0 (I40 (Int40.of_string n))
    | Lst [ Sym "i48" ; Sym n ] -> E0 (I48 (Int48.of_string n))
    | Lst [ Sym "i56" ; Sym n ] -> E0 (I56 (Int56.of_string n))
    | Lst [ Sym "i64" ; Sym n ] -> E0 (I64 (Int64.of_string n))
    | Lst [ Sym "i128" ; Sym n ] -> E0 (I128 (Int128.of_string n))
    | Lst [ Sym "bit" ; Sym b ] -> E0 (Bit (Bool.of_string b))
    | Lst [ Sym "size" ; Sym n ] -> E0 (Size (int_of_string n))
    | Lst [ Sym "byte" ; Sym n ] -> E0 (Byte (Uint8.of_string n))
    | Lst [ Sym "word" ; Sym n ] -> E0 (Word (Uint16.of_string n))
    | Lst [ Sym "dword" ; Sym n ] -> E0 (DWord (Uint32.of_string n))
    | Lst [ Sym "qword" ; Sym n ] -> E0 (QWord (Uint64.of_string n))
    | Lst [ Sym "oword" ; Sym n ] -> E0 (OWord (Uint128.of_string n))
    | Lst [ Sym "bytes" ; Str s ] -> E0 (Bytes (Bytes.of_string s))
    | Lst [ Sym "data-ptr-of-string" ; Str s ] -> E0 (DataPtrOfString s)
    | Lst [ Sym "identifier" ; Str s ] -> E0 (Identifier s)
    | Lst [ Sym "ext-identifier" ; Str s ] -> E0 (ExtIdentifier s)
    | Lst [ Sym "copy-field" ] -> E0 CopyField
    | Lst [ Sym "skip-field" ] -> E0 SkipField
    | Lst [ Sym "set-field-null" ] -> E0 SetFieldNull
    (* e0s *)
    | Lst (Sym "seq" :: xs) -> E0S (Seq, List.map e xs)
    | Lst (Sym "make-vec" :: xs) -> E0S (MakeVec, List.map e xs)
    | Lst (Sym "make-lst" :: Str mn :: xs) ->
        E0S (MakeLst (T.maybe_nullable_of_string mn), List.map e xs)
    | Lst (Sym "make-tup" :: xs) -> E0S (MakeTup, List.map e xs)
    | Lst (Sym "make-rec" :: xs) -> E0S (MakeRec, List.map e xs)
    (* e1 *)
    | Lst (Sym ("function" | "fun") :: Sym fid :: (_ :: _ :: _ as tail)) ->
        (* Syntax for functions is:
         *    (fun id "type arg 1" "type arg 2" ... body)
         * where:
         *   - id is an integer used to identify this function when using param
         *   - body is an expression *)
        let typs, x = list_split_last tail in
        let typs =
          Array.of_list typs |>
          Array.map (function
            | Str s -> T.Parser.of_string s
            | x ->
                Printf.sprintf2 "Need a type (in string) not %a" print_sexpr x |>
                failwith
          ) in
        E1 (Function (int_of_string fid, typs), e x)
    | Lst [ Sym "comment" ; Str s ; x ] ->
        E1 (Comment s, e x)
    | Lst [ Sym "get-item" ; Sym n ; x ] ->
        E1 (GetItem (int_of_string n), e x)
    | Lst [ Sym "get-field" ; Str s ; x ] ->
        E1 (GetField s, e x)
    | Lst [ Sym "get-alt" ; Str s ; x ] ->
        E1 (GetAlt s, e x)
    | Lst [ Sym "construct" ; Str mn ; Sym i ; x ] ->
        let i = int_of_string i in
        (match T.maybe_nullable_of_string mn with
        | { vtyp = Sum mns ; nullable = false } ->
            let max_lbl = Array.length mns - 1 in
            if i > max_lbl then
              Printf.sprintf "Sum type %S has no label %d" mn i |>
              failwith ;
            E1 (Construct (mns, i), e x)
        | _ ->
            Printf.sprintf2 "Not a sum type: %S" mn |>
            failwith)
    | Lst [ Sym "dump" ; x ] -> E1 (Dump, e x)
    | Lst [ Sym "identity" ; x ] -> E1 (Identity, e x)
    | Lst [ Sym "ignore" ; x ] -> E1 (Ignore, e x)
    | Lst [ Sym "is-null" ; x ] -> E1 (IsNull, e x)
    | Lst [ Sym "not-null" ; x ] -> E1 (NotNull, e x)
    | Lst [ Sym "force" ; Str w ; x ] -> E1 (Force w, e x)
    | Lst [ Sym "string-of-float" ; x ] -> E1 (StringOfFloat, e x)
    | Lst [ Sym "string-of-char" ; x ] -> E1 (StringOfChar, e x)
    | Lst [ Sym "string-of-int" ; x ] -> E1 (StringOfInt, e x)
    | Lst [ Sym "string-of-ip" ; x ] -> E1 (StringOfIp, e x)
    | Lst [ Sym "float-of-string" ; x ] -> E1 (FloatOfString, e x)
    | Lst [ Sym "char-of-string" ; x ] -> E1 (CharOfString, e x)
    | Lst [ Sym "u8-of-string" ; x ] -> E1 (U8OfString, e x)
    | Lst [ Sym "u16-of-string" ; x ] -> E1 (U16OfString, e x)
    | Lst [ Sym "u24-of-string" ; x ] -> E1 (U24OfString, e x)
    | Lst [ Sym "u32-of-string" ; x ] -> E1 (U32OfString, e x)
    | Lst [ Sym "u40-of-string" ; x ] -> E1 (U40OfString, e x)
    | Lst [ Sym "u48-of-string" ; x ] -> E1 (U48OfString, e x)
    | Lst [ Sym "u56-of-string" ; x ] -> E1 (U56OfString, e x)
    | Lst [ Sym "u64-of-string" ; x ] -> E1 (U64OfString, e x)
    | Lst [ Sym "u128-of-string" ; x ] -> E1 (U128OfString, e x)
    | Lst [ Sym "i8-of-string" ; x ] -> E1 (I8OfString, e x)
    | Lst [ Sym "i16-of-string" ; x ] -> E1 (I16OfString, e x)
    | Lst [ Sym "i24-of-string" ; x ] -> E1 (I24OfString, e x)
    | Lst [ Sym "i32-of-string" ; x ] -> E1 (I32OfString, e x)
    | Lst [ Sym "i40-of-string" ; x ] -> E1 (I40OfString, e x)
    | Lst [ Sym "i48-of-string" ; x ] -> E1 (I48OfString, e x)
    | Lst [ Sym "i56-of-string" ; x ] -> E1 (I56OfString, e x)
    | Lst [ Sym "i64-of-string" ; x ] -> E1 (I64OfString, e x)
    | Lst [ Sym "i128-of-string" ; x ] -> E1 (I128OfString, e x)
    | Lst [ Sym "float-of-ptr" ; x ] -> E1 (FloatOfPtr, e x)
    | Lst [ Sym "char-of-ptr" ; x ] -> E1 (CharOfPtr, e x)
    | Lst [ Sym "u8-of-ptr" ; x ] -> E1 (U8OfPtr, e x)
    | Lst [ Sym "u16-of-ptr" ; x ] -> E1 (U16OfPtr, e x)
    | Lst [ Sym "u24-of-ptr" ; x ] -> E1 (U24OfPtr, e x)
    | Lst [ Sym "u32-of-ptr" ; x ] -> E1 (U32OfPtr, e x)
    | Lst [ Sym "u40-of-ptr" ; x ] -> E1 (U40OfPtr, e x)
    | Lst [ Sym "u48-of-ptr" ; x ] -> E1 (U48OfPtr, e x)
    | Lst [ Sym "u56-of-ptr" ; x ] -> E1 (U56OfPtr, e x)
    | Lst [ Sym "u64-of-ptr" ; x ] -> E1 (U64OfPtr, e x)
    | Lst [ Sym "u128-of-ptr" ; x ] -> E1 (U128OfPtr, e x)
    | Lst [ Sym "i8-of-ptr" ; x ] -> E1 (I8OfPtr, e x)
    | Lst [ Sym "i16-of-ptr" ; x ] -> E1 (I16OfPtr, e x)
    | Lst [ Sym "i24-of-ptr" ; x ] -> E1 (I24OfPtr, e x)
    | Lst [ Sym "i32-of-ptr" ; x ] -> E1 (I32OfPtr, e x)
    | Lst [ Sym "i40-of-ptr" ; x ] -> E1 (I40OfPtr, e x)
    | Lst [ Sym "i48-of-ptr" ; x ] -> E1 (I48OfPtr, e x)
    | Lst [ Sym "i56-of-ptr" ; x ] -> E1 (I56OfPtr, e x)
    | Lst [ Sym "i64-of-ptr" ; x ] -> E1 (I64OfPtr, e x)
    | Lst [ Sym "i128-of-ptr" ; x ] -> E1 (I128OfPtr, e x)
    | Lst [ Sym "to-u8" ; x ] -> E1 (ToU8, e x)
    | Lst [ Sym "to-u16" ; x ] -> E1 (ToU16, e x)
    | Lst [ Sym "to-u24" ; x ] -> E1 (ToU24, e x)
    | Lst [ Sym "to-u32" ; x ] -> E1 (ToU32, e x)
    | Lst [ Sym "to-u40" ; x ] -> E1 (ToU40, e x)
    | Lst [ Sym "to-u48" ; x ] -> E1 (ToU48, e x)
    | Lst [ Sym "to-u56" ; x ] -> E1 (ToU56, e x)
    | Lst [ Sym "to-u64" ; x ] -> E1 (ToU64, e x)
    | Lst [ Sym "to-u128" ; x ] -> E1 (ToU128, e x)
    | Lst [ Sym "to-i8" ; x ] -> E1 (ToI8, e x)
    | Lst [ Sym "to-i16" ; x ] -> E1 (ToI16, e x)
    | Lst [ Sym "to-i24" ; x ] -> E1 (ToI24, e x)
    | Lst [ Sym "to-i32" ; x ] -> E1 (ToI32, e x)
    | Lst [ Sym "to-i40" ; x ] -> E1 (ToI40, e x)
    | Lst [ Sym "to-i48" ; x ] -> E1 (ToI48, e x)
    | Lst [ Sym "to-i56" ; x ] -> E1 (ToI56, e x)
    | Lst [ Sym "to-i64" ; x ] -> E1 (ToI64, e x)
    | Lst [ Sym "to-i128" ; x ] -> E1 (ToI128, e x)
    | Lst [ Sym "to-float" ; x ] -> E1 (ToFloat, e x)
    | Lst [ Sym "log-not" ; x ] -> E1 (LogNot, e x)
    | Lst [ Sym "float-of-qword" ; x ] -> E1 (FloatOfQWord, e x)
    | Lst [ Sym "qword-of-float" ; x ] -> E1 (QWordOfFloat, e x)
    | Lst [ Sym "u8-of-byte" ; x ] -> E1 (U8OfByte, e x)
    | Lst [ Sym "byte-of-u8" ; x ] -> E1 (ByteOfU8, e x)
    | Lst [ Sym "u16-of-word" ; x ] -> E1 (U16OfWord, e x)
    | Lst [ Sym "word-of-u16" ; x ] -> E1 (WordOfU16, e x)
    | Lst [ Sym "u32-of-dword" ; x ] -> E1 (U32OfDWord, e x)
    | Lst [ Sym "dword-of-u32" ; x ] -> E1 (DWordOfU32, e x)
    | Lst [ Sym "u64-of-qword" ; x ] -> E1 (U64OfQWord, e x)
    | Lst [ Sym "qword-of-u64" ; x ] -> E1 (QWordOfU64, e x)
    | Lst [ Sym "u128-of-oword" ; x ] -> E1 (U128OfOWord, e x)
    | Lst [ Sym "oword-of-u128" ; x ] -> E1 (OWordOfU128, e x)
    | Lst [ Sym "u8-of-char" ; x ] -> E1 (U8OfChar, e x)
    | Lst [ Sym "char-of-u8" ; x ] -> E1 (CharOfU8, e x)
    | Lst [ Sym "size-of-u32" ; x ] -> E1 (SizeOfU32, e x)
    | Lst [ Sym "u32-of-size" ; x ] -> E1 (U32OfSize, e x)
    | Lst [ Sym "bit-of-bool" ; x ] -> E1 (BitOfBool, e x)
    | Lst [ Sym "bool-of-bit" ; x ] -> E1 (BoolOfBit, e x)
    | Lst [ Sym "list-of-slist" ; x ] -> E1 (ListOfSList, e x)
    | Lst [ Sym "list-of-slist-rev" ; x ] -> E1 (ListOfSListRev, e x)
    | Lst [ Sym "set-of-slist" ; x ] -> E1 (SetOfSList, e x)
    | Lst [ Sym "list-of-vec" ; x ] -> E1 (ListOfVec, e x)
    | Lst [ Sym "list-of-set" ; x ] -> E1 (ListOfSet, e x)
    | Lst [ Sym "u8-of-bool" ; x ] -> E1 (U8OfBool, e x)
    | Lst [ Sym "bool-of-u8" ; x ] -> E1 (BoolOfU8, e x)
    | Lst [ Sym "string-length" ; x ] -> E1 (StringLength, e x)
    | Lst [ Sym "string-of-bytes" ; x ] -> E1 (StringOfBytes, e x)
    | Lst [ Sym "bytes-of-string" ; x ] -> E1 (BytesOfString, e x)
    | Lst [ Sym "cardinality" ; x ] -> E1 (Cardinality, e x)
    | Lst [ Sym "read-byte" ; x ] -> E1 (ReadByte, e x)
    | Lst [ Sym "data-ptr-push" ; x ] -> E1 (DataPtrPush, e x)
    | Lst [ Sym "data-ptr-pop" ; x ] -> E1 (DataPtrPop, e x)
    | Lst [ Sym "rem-size" ; x ] -> E1 (RemSize, e x)
    | Lst [ Sym "data-ptr-offset" ; x ] -> E1 (DataPtrOffset, e x)
    | Lst [ Sym "not" ; x ] -> E1 (Not, e x)
    | Lst [ Sym "abs" ; x ] -> E1 (Abs, e x)
    | Lst [ Sym "neg" ; x ] -> E1 (Neg, e x)
    | Lst [ Sym "exp" ; x ] -> E1 (Exp, e x)
    | Lst [ Sym "log" ; x ] -> E1 (Log, e x)
    | Lst [ Sym "log10" ; x ] -> E1 (Log10, e x)
    | Lst [ Sym "sqrt" ; x ] -> E1 (Sqrt, e x)
    | Lst [ Sym "ceil" ; x ] -> E1 (Ceil, e x)
    | Lst [ Sym "floor" ; x ] -> E1 (Floor, e x)
    | Lst [ Sym "round" ; x ] -> E1 (Round, e x)
    | Lst [ Sym "cos" ; x ] -> E1 (Cos, e x)
    | Lst [ Sym "sin" ; x ] -> E1 (Sin, e x)
    | Lst [ Sym "tan" ; x ] -> E1 (Tan, e x)
    | Lst [ Sym "acos" ; x ] -> E1 (ACos, e x)
    | Lst [ Sym "asin" ; x ] -> E1 (ASin, e x)
    | Lst [ Sym "atan" ; x ] -> E1 (ATan, e x)
    | Lst [ Sym "cosh" ; x ] -> E1 (CosH, e x)
    | Lst [ Sym "sinh" ; x ] -> E1 (SinH, e x)
    | Lst [ Sym "tanh" ; x ] -> E1 (TanH, e x)
    | Lst [ Sym "lower" ; x ] -> E1 (Lower, e x)
    | Lst [ Sym "upper" ; x ] -> E1 (Upper, e x)
    | Lst [ Sym "hash" ; x ] -> E1 (Hash, e x)
    | Lst [ Sym "fst" ; x ] -> E1 (Fst, e x)
    | Lst [ Sym "snd" ; x ] -> E1 (Snd, e x)
    | Lst [ Sym "head" ; x ] -> E1 (Head, e x)
    | Lst [ Sym "tail" ; x ] -> E1 (Tail, e x)
    | Lst [ Sym "read-word" ; Sym en ; x ] ->
        E1 (ReadWord (endianness_of_string en), e x)
    | Lst [ Sym "read-dword" ; Sym en ; x ] ->
        E1 (ReadDWord (endianness_of_string en), e x)
    | Lst [ Sym "read-qword" ; Sym en ; x ] ->
        E1 (ReadQWord (endianness_of_string en), e x)
    | Lst [ Sym "read-oword" ; Sym en ; x ] ->
        E1 (ReadOWord (endianness_of_string en), e x)
    | Lst [ Sym "assert" ; x1 ] -> E1 (Assert, e x1)
    | Lst [ Sym "mask-get" ; Sym d ; x1 ] as x ->
        E1 (MaskGet (int_of_symbol x d), e x1)
    | Lst [ Sym "label-of" ; x ] -> E1 (LabelOf, e x)
    | Lst [ Sym "sliding-window" ; Str mn ; x ] ->
        E1 (SlidingWindow (T.maybe_nullable_of_string mn), e x)
    | Lst [ Sym "tumbling-window" ; Str mn ; x ] ->
        E1 (TumblingWindow (T.maybe_nullable_of_string mn), e x)
    | Lst [ Sym "sampling" ; Str mn ; x ] ->
        E1 (Sampling (T.maybe_nullable_of_string mn), e x)
    | Lst [ Sym "hash-table" ; Str mn ; x ] ->
        E1 (HashTable (T.maybe_nullable_of_string mn), e x)
    | Lst [ Sym "heap" ; x ] ->
        E1 (Heap, e x)
    | Lst [ Sym "data-ptr-of-buffer" ; x ] ->
        E1 (DataPtrOfBuffer, e x)
    | Lst [ Sym "getenv" ; x ] ->
        E1 (GetEnv, e x)
    | Lst [ Sym "get-min" ; x ] ->
        E1 (GetMin, e x)
    (* e1s *)
    | Lst (Sym "apply" :: x1 :: xs) -> E1S (Apply, e x1, List.map e xs)
    (* e2 *)
    | Lst [ Sym "let" ; Str s ; x1 ; x2 ] -> E2 (Let s, e x1, e x2)
    | Lst [ Sym "nth" ; x1 ; x2 ] -> E2 (Nth, e x1, e x2)
    | Lst [ Sym "gt" ; x1 ; x2 ] -> E2 (Gt, e x1, e x2)
    | Lst [ Sym "ge" ; x1 ; x2 ] -> E2 (Ge, e x1, e x2)
    | Lst [ Sym "eq" ; x1 ; x2 ] -> E2 (Eq, e x1, e x2)
    | Lst [ Sym "ne" ; x1 ; x2 ] -> E2 (Ne, e x1, e x2)
    | Lst [ Sym "add" ; x1 ; x2 ] -> E2 (Add, e x1, e x2)
    | Lst [ Sym "sub" ; x1 ; x2 ] -> E2 (Sub, e x1, e x2)
    | Lst [ Sym "mul" ; x1 ; x2 ] -> E2 (Mul, e x1, e x2)
    | Lst [ Sym "div" ; x1 ; x2 ] -> E2 (Div, e x1, e x2)
    | Lst [ Sym "rem" ; x1 ; x2 ] -> E2 (Rem, e x1, e x2)
    | Lst [ Sym "pow" ; x1 ; x2 ] -> E2 (Pow, e x1, e x2)
    | Lst [ Sym "log-and" ; x1 ; x2 ] -> E2 (LogAnd, e x1, e x2)
    | Lst [ Sym "log-or" ; x1 ; x2 ] -> E2 (LogOr, e x1, e x2)
    | Lst [ Sym "log-xor" ; x1 ; x2 ] -> E2 (LogXor, e x1, e x2)
    | Lst [ Sym "left-shift" ; x1 ; x2 ] -> E2 (LeftShift, e x1, e x2)
    | Lst [ Sym "right-shift" ; x1 ; x2 ] -> E2 (RightShift, e x1, e x2)
    | Lst [ Sym "append-byte" ; x1 ; x2 ] -> E2 (AppendByte, e x1, e x2)
    | Lst [ Sym "append-bytes" ; x1 ; x2 ] -> E2 (AppendBytes, e x1, e x2)
    | Lst [ Sym "append-string" ; x1 ; x2 ] -> E2 (AppendString, e x1, e x2)
    | Lst [ Sym "starts-with" ; x1 ; x2 ] -> E2 (StartsWith, e x1, e x2)
    | Lst [ Sym "ends-with" ; x1 ; x2 ] -> E2 (EndsWith, e x1, e x2)
    | Lst [ Sym "get-bit" ; x1 ; x2 ] -> E2 (GetBit, e x1, e x2)
    | Lst [ Sym "get-vec" ; x1 ; x2 ] -> E2 (GetVec, e x1, e x2)
    | Lst [ Sym "read-bytes" ; x1 ; x2 ] -> E2 (ReadBytes, e x1, e x2)
    | Lst [ Sym "peek-byte" ; x1 ; x2 ] -> E2 (PeekByte, e x1, e x2)
    | Lst [ Sym "write-byte" ; x1 ; x2 ] -> E2 (WriteByte, e x1, e x2)
    | Lst [ Sym "write-bytes" ; x1 ; x2 ] -> E2 (WriteBytes, e x1, e x2)
    | Lst [ Sym "poke-byte" ; x1 ; x2 ] -> E2 (PokeByte, e x1, e x2)
    | Lst [ Sym "data-ptr-add" ; x1 ; x2 ] -> E2 (DataPtrAdd, e x1, e x2)
    | Lst [ Sym "data-ptr-sub" ; x1 ; x2 ] -> E2 (DataPtrSub, e x1, e x2)
    | Lst [ Sym "and" ; x1 ; x2 ] -> E2 (And, e x1, e x2)
    | Lst [ Sym "or" ; x1 ; x2 ] -> E2 (Or, e x1, e x2)
    | Lst [ Sym "cons" ; x1 ; x2 ] -> E2 (Cons, e x1, e x2)
    | Lst [ Sym "pair" ; x1 ; x2 ] -> E2 (Pair, e x1, e x2)
    | Lst [ Sym "map-pair" ; x1 ; x2 ] -> E2 (MapPair, e x1, e x2)
    | Lst [ Sym "map" ; x1 ; x2 ] -> E2 (Map, e x1, e x2)
    | Lst [ Sym "min" ; x1 ; x2 ] -> E2 (Min, e x1, e x2)
    | Lst [ Sym "max" ; x1 ; x2 ] -> E2 (Max, e x1, e x2)
    | Lst [ Sym "mem" ; x1 ; x2 ] -> E2 (Member, e x1, e x2)
    | Lst [ Sym "peek-word" ; Sym en ; x1 ; x2 ] ->
        E2 (PeekWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "peek-dword" ; Sym en ; x1 ; x2 ] ->
        E2 (PeekDWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "peek-qword" ; Sym en ; x1 ; x2 ] ->
        E2 (PeekQWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "peek-oword" ; Sym en ; x1 ; x2 ] ->
        E2 (PeekOWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "write-word" ; Sym en ; x1 ; x2 ] ->
        E2 (WriteWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "write-dword" ; Sym en ; x1 ; x2 ] ->
        E2 (WriteDWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "write-qword" ; Sym en ; x1 ; x2 ] ->
        E2 (WriteQWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "write-oword" ; Sym en ; x1 ; x2 ] ->
        E2 (WriteOWord (endianness_of_string en), e x1, e x2)
    | Lst [ Sym "insert" ; x1 ; x2 ] ->
        E2 (Insert, e x1, e x2)
    | Lst [ Sym "del-min" ; x1 ; x2 ] ->
        E2 (DelMin, e x1, e x2)
    | Lst [ Sym "split-on" ; x1 ; x2 ] ->
        E2 (SplitBy, e x1, e x2)
    | Lst [ Sym "split-at" ; x1 ; x2 ] ->
        E2 (SplitAt, e x1, e x2)
    | Lst [ Sym "join" ; x1 ; x2 ] ->
        E2 (Join, e x1, e x2)
    | Lst [ Sym "alloc-lst" ; x1 ; x2 ] ->
        E2 (AllocLst, e x1, e x2)
    | Lst [ Sym "partial-sort" ; x1 ; x2 ] ->
        E2 (PartialSort, e x1, e x2)
    | Lst [ Sym "chop-begin" ; x1 ; x2 ] ->
        E2 (ChopBegin, e x1, e x2)
    | Lst [ Sym "chop-end" ; x1 ; x2 ] ->
        E2 (ChopEnd, e x1, e x2)
    | Lst [ Sym "scale-weights" ; set ; d ] ->
        E2 (ScaleWeights, e set, e d)
    (* e3 *)
    | Lst [ Sym "set-bit" ; x1 ; x2 ; x3 ] -> E3 (SetBit, e x1, e x2, e x3)
    | Lst [ Sym "set-vec" ; x1 ; x2 ; x3 ] -> E3 (SetVec, e x1, e x2, e x3)
    | Lst [ Sym "blit-byte" ; x1 ; x2 ; x3 ] -> E3 (BlitByte, e x1, e x2, e x3)
    | Lst [ Sym "if" ; x1 ; x2 ; x3 ] -> E3 (If, e x1, e x2, e x3)
    | Lst [ Sym "loop-while" ; x1 ; x2 ; x3 ] -> E3 (LoopWhile, e x1, e x2, e x3)
    | Lst [ Sym "loop-until" ; x1 ; x2 ; x3 ] -> E3 (LoopUntil, e x1, e x2, e x3)
    | Lst [ Sym "fold" ; x1 ; x2 ; x3 ] -> E3 (Fold, e x1, e x2, e x3)
    | Lst [ Sym "data-ptr-of-ptr" ; x1 ; x2 ; x3 ] ->
        E3 (DataPtrOfPtr, e x1, e x2, e x3)
    | Lst [ Sym "find-substring" ; x1 ; x2 ; x3 ] ->
        E3 (FindSubstring, e x1, e x2, e x3)
    | Lst [ Sym "top" ; Str mn ; x1 ; x2 ; x3 ] ->
        E3 (Top (T.maybe_nullable_of_string mn), e x1, e x2, e x3)
    | Lst [ Sym "insert-weighted" ; x1 ; x2 ; x3 ] ->
        E3 (InsertWeighted, e x1, e x2, e x3)
    (* e4 *)
    | Lst [ Sym "read-while" ; x1 ; x2 ; x3 ; x4 ] ->
        E4 (ReadWhile, e x1, e x2, e x3, e x4)
    | Lst [ Sym "repeat" ; x1 ; x2 ; x3 ; x4 ] ->
        E4 (Repeat, e x1, e x2, e x3, e x4)

    | x -> raise (Unknown_expression x)

  let expr_of_toks toks str =
    List.map e (sexpr_of_toks toks str)

  let expr str =
    List.map e (sexpr_of_string str)

  (*$= expr & ~printer:(BatIO.to_string (BatList.print print))
    [ Ops.u8 (Uint8.of_int 42) ] (expr "(u8 42)")
    [ Ops.float 1. ] (expr "(float 1.0)")
    [ Ops.char '\019' ] (expr "(char \"\\019\")")
    [ Ops.null T.(Mac String) ] (expr "(null \"string\")")
    [ Ops.i56 (Int56.of_string "-36028797018963967") ] (expr "(i56 -36028797018963967)")
    [ Ops.i128 (Int128.of_string "-1213949874624120272") ] \
      (expr "(i128 -1213949874624120272)")
    [ Ops.false_ ] (expr "(bool false)")
    [ Ops.u64 (Uint64.of_int 8) ] (expr "(u64 8)")
    [ Ops.seq [ Ops.u16 (Uint16.of_int 45134) ; Ops.u64 (Uint64.of_int 6)] ] \
      (expr "(seq (u16 45134) (u64 6))")
    [ Ops.comment "foo" (Ops.u32 (Uint32.of_int 2)) ] \
      (expr "(comment \"foo\" (u32 2))")
    [ Ops.(make_vec [ u8 Uint8.one ; u8 (Uint8.of_int 2) ]) ] \
      (expr "(make-vec (u8 1) (u8 2))")
  *)

  (*$>*)
end

let of_string s =
  match Parser.expr s with
  | [ e ] -> e
  | _ ->
      Printf.sprintf2 "Cannot parse %S as a single expression" s |>
      failwith

exception Type_error of t * t * T.t * string
exception Type_error_param of t * t * int * T.t * string
exception Type_error_path of t * t * T.path * string
exception Struct_error of t * string
exception Apply_error of t * string
exception Comparator_error of t * T.t * string
exception Unbound_identifier of t * bool * string * (t * T.t) list
exception Unbound_parameter of t * param_id * (t * T.t) list
exception Invalid_expression of t * string

(* expr must be a plain string: *)
let field_name_of_expr = function
  | E0 (String s) -> s
  | e -> raise (Struct_error (e, "record names must be constant strings"))

(* Returns the type of [e0].
 * [l] is an association list of bound identifiers.
 * [e0] must have been type checked already: *)
(* FIXME: merge with type_check? *)
let rec type_of l e0 =
  let maybe_nullable_of l e =
    let t = type_of l e in
    try
      T.to_maybe_nullable t
    with Invalid_argument _ ->
      raise (Type_error (e0, e, t, "be a possibly nullable value type")) in
  match e0 with
  | E0S (Seq, [])
  | E1 ((Dump | Ignore), _) ->
      T.void
  | E0S (Seq, es) ->
      type_of l (List.last es)
  | E0S (MakeVec, []) ->
      raise (Struct_error (e0, "vector dimension must be > 1"))
  | E0S (MakeVec, (e0::_ as es)) ->
      Value (T.make (Vec (List.length es, maybe_nullable_of l e0)))
  | E0S (MakeLst mn, _) ->
      Value (T.make (Lst mn))
  | E0S (MakeTup, es) ->
      Value (T.make (Tup (List.map (maybe_nullable_of l) es |>
                            Array.of_list)))
  | E0S (MakeRec, es) ->
      let prev_name, mns =
        List.fold_left (fun (prev_name, mns) e ->
          match prev_name with
          | None ->
              Some (field_name_of_expr e), mns
          | Some name ->
              None, (name, maybe_nullable_of l e) :: mns
        ) (None, []) es in
      if prev_name <> None then
        raise (Struct_error (e0,
          "record expressions must have an even number of values")) ;
      let mns = List.rev mns in
      Value (T.make (Rec (Array.of_list mns)))
  | E1S (Apply, f, _) ->
      (match type_of l f with
      | Function (_, t) -> t
      | t -> raise (Type_error (e0, f, t, "be a function")))
  | E1 (GetItem n, e1) ->
      (match type_of l e1 |> T.develop_user_types with
      | Value { vtyp = Tup mns ; nullable = false } ->
          let num_n = Array.length mns in
          if n < 0 || n >= num_n then
            raise (Struct_error (e0, "no item #"^ string_of_int n ^" (only "^
                                     string_of_int num_n ^" items)")) ;
          Value mns.(n)
      | t ->
          raise (Type_error (e0, e1, t, "be a tuple")))
  | E1 ((GetField name), e1) ->
      (match type_of l e1 |> T.develop_user_types with
      | Value { vtyp = Rec mns ; nullable = false } ->
          (match array_assoc name mns with
          | exception Not_found ->
              let msg =
                Printf.sprintf2 "no field named %s (have %a)"
                  name
                  (pretty_enum_print String.print) (Array.enum mns /@ fst) in
              raise (Struct_error (e0, msg))
          | mn ->
              Value mn)
      | t ->
          raise (Type_error (e0, e1, t, "be a record")))
  | E1 ((GetAlt name), e1) ->
      (match type_of l e1 |> T.develop_user_types with
      | Value { vtyp = Sum mns ; nullable = false } ->
          (match array_assoc name mns with
          | exception Not_found ->
              raise (Struct_error (e0, "no alternative named "^ name))
          | mn -> Value mn)
      | t -> raise (Type_error (e0, e1, t, "be a union")))
  | E1 ((Construct (mns, _)), _) -> Value (T.make (Sum mns))
  | E2 (Nth, _, e2) ->
      (match type_of l e2 |> T.develop_user_types with
      | Value { vtyp = (Vec (_, mn) | Lst mn) ; nullable = false } ->
          Value mn
      | t ->
          raise (Type_error (e0, e2, t, "be a vector or list")))
  | E1 (Comment _, e)
  | E2 ((Add | Sub | Mul | LogAnd | LogOr | LogXor |
         LeftShift | RightShift), e, _) ->
      type_of l e
  | E1 (LogNot, e) ->
      type_of l e
  | E2 ((Div | Rem | Pow), e, _) ->
      (* TODO: make it nullable only if it cannot be ascertained from e1 and e2
       * that the result will never be null *)
      T.to_nullable (type_of l e)
  | E1 (NotNull, e) ->
      T.to_nullable (type_of l e)
  | E1 (Force _, e) ->
      T.force (type_of l e)
  | E1 (IsNull, _) -> T.bool
  | E0 (Null vt) -> Value { vtyp = vt ; nullable = true }
  | E0 (EndOfList t) -> SList t
  | E0 (EmptySet mn) -> Value (T.make (T.Set (Simple, mn)))
  | E0 Now -> T.float
  | E0 RandomFloat -> T.float
  | E0 RandomU8 -> T.u8
  | E0 RandomU32 -> T.u32
  | E0 RandomU64 -> T.u64
  | E0 RandomU128 -> T.u128
  | E0 (Float _) -> T.float
  | E0 Unit -> T.unit
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
  | E0 (Bit _) -> T.bit
  | E0 (Size _) -> T.size
  | E0 (Byte _) -> T.byte
  | E0 (Word _) -> T.word
  | E0 (DWord _) -> T.dword
  | E0 (QWord _) -> T.qword
  | E0 (OWord _) -> T.oword
  | E0 (Bytes _) -> T.bytes
  | E2 (Gt, _, _) -> T.bool
  | E2 (Ge, _, _) -> T.bool
  | E2 (Eq, _, _) -> T.bool
  | E2 (Ne, _, _) -> T.bool
  | E1 (StringOfFloat, _)
  | E1 (StringOfChar, _)
  | E1 (StringOfInt, _) -> T.string
  | E1 (StringOfIp, _) -> T.string
  | E1 (CharOfString, _) -> T.char
  | E1 (FloatOfString, _) -> T.(Value (optional (Mac Float)))
  | E1 (U8OfString, _) -> T.(Value (optional (Mac U8)))
  | E1 (U16OfString, _) -> T.(Value (optional (Mac U16)))
  | E1 (U24OfString, _) -> T.(Value (optional (Mac U24)))
  | E1 (U32OfString, _) -> T.(Value (optional (Mac U32)))
  | E1 (U40OfString, _) -> T.(Value (optional (Mac U40)))
  | E1 (U48OfString, _) -> T.(Value (optional (Mac U48)))
  | E1 (U56OfString, _) -> T.(Value (optional (Mac U56)))
  | E1 (U64OfString, _) -> T.(Value (optional (Mac U64)))
  | E1 (U128OfString, _) -> T.(Value (optional (Mac U128)))
  | E1 (I8OfString, _) -> T.(Value (optional (Mac I8)))
  | E1 (I16OfString, _) -> T.(Value (optional (Mac I16)))
  | E1 (I24OfString, _) -> T.(Value (optional (Mac I24)))
  | E1 (I32OfString, _) -> T.(Value (optional (Mac I32)))
  | E1 (I40OfString, _) -> T.(Value (optional (Mac I40)))
  | E1 (I48OfString, _) -> T.(Value (optional (Mac I48)))
  | E1 (I56OfString, _) -> T.(Value (optional (Mac I56)))
  | E1 (I64OfString, _) -> T.(Value (optional (Mac I64)))
  | E1 (I128OfString, _) -> T.(Value (optional (Mac I128)))
  | E1 (CharOfPtr, _) -> T.pair T.char T.dataptr
  | E1 (FloatOfPtr, _) -> T.pair T.float T.dataptr
  | E1 (U8OfPtr, _) -> T.pair T.u8 T.dataptr
  | E1 (U16OfPtr, _) -> T.pair T.u16 T.dataptr
  | E1 (U24OfPtr, _) -> T.pair T.u24 T.dataptr
  | E1 (U32OfPtr, _) -> T.pair T.u32 T.dataptr
  | E1 (U40OfPtr, _) -> T.pair T.u40 T.dataptr
  | E1 (U48OfPtr, _) -> T.pair T.u48 T.dataptr
  | E1 (U56OfPtr, _) -> T.pair T.u56 T.dataptr
  | E1 (U64OfPtr, _) -> T.pair T.u64 T.dataptr
  | E1 (U128OfPtr, _) -> T.pair T.u128 T.dataptr
  | E1 (I8OfPtr, _) -> T.pair T.i8 T.dataptr
  | E1 (I16OfPtr, _) -> T.pair T.i16 T.dataptr
  | E1 (I24OfPtr, _) -> T.pair T.i24 T.dataptr
  | E1 (I32OfPtr, _) -> T.pair T.i32 T.dataptr
  | E1 (I40OfPtr, _) -> T.pair T.i40 T.dataptr
  | E1 (I48OfPtr, _) -> T.pair T.i48 T.dataptr
  | E1 (I56OfPtr, _) -> T.pair T.i56 T.dataptr
  | E1 (I64OfPtr, _) -> T.pair T.i64 T.dataptr
  | E1 (I128OfPtr, _) -> T.pair T.i128 T.dataptr
  | E1 (FloatOfQWord, _) -> T.float
  | E1 (QWordOfFloat, _) -> T.qword
  | E1 (U8OfByte, _) -> T.u8
  | E1 (ByteOfU8, _) -> T.byte
  | E1 (U16OfWord, _) -> T.u16
  | E1 (WordOfU16, _) -> T.word
  | E1 (U32OfDWord, _) -> T.u32
  | E1 (DWordOfU32, _) -> T.dword
  | E1 (U64OfQWord, _) -> T.u64
  | E1 (QWordOfU64, _) -> T.qword
  | E1 (U128OfOWord, _) -> T.u128
  | E1 (OWordOfU128, _) -> T.oword
  | E1 (U8OfChar, _) -> T.u8
  | E1 (CharOfU8, _) -> T.char
  | E1 (SizeOfU32, _) -> T.size
  | E1 (U32OfSize, _) -> T.u32
  | E1 (BitOfBool, _) -> T.bit
  | E1 (BoolOfBit, _) -> T.bool
  | E1 ((ListOfSList | ListOfSListRev), e) ->
      (match type_of l e |> T.develop_user_types with
      | SList (Value mn) -> Value (T.make (Lst mn))
      | SList _ as t ->
          raise (Type_error (e0, e, t, "be a slist of maybe nullable values"))
      | t -> raise (Type_error (e0, e, t, "be a slist")))
  | E1 (SetOfSList, e) ->
      (match type_of l e |> T.develop_user_types with
      | SList (Value mn) -> Value (T.make (Set (Simple, mn)))
      | SList _ as t ->
          raise (Type_error (e0, e, t, "be a slist of maybe nullable values"))
      | t -> raise (Type_error (e0, e, t, "be a slist")))
  | E1 (ListOfVec, e) ->
      (match type_of l e |> T.develop_user_types with
      | T.Value ({ vtyp = Vec (_, mn) ; nullable = false }) ->
          Value (T.make (Lst mn))
      | t ->
          raise (Type_error (e0, e, t, "be a vec")))
  | E1 (ListOfSet, e) ->
      (match type_of l e |> T.develop_user_types with
      | T.Value ({ vtyp = Set (_, mn) ; nullable = false }) ->
          Value (T.make (Lst mn))
      | t ->
          raise (Type_error (e0, e, t, "be a set")))
  | E1 (U8OfBool, _) -> T.u8
  | E1 (BoolOfU8, _) -> T.bool
  | E2 (AppendByte, _, _) -> T.bytes
  | E2 (AppendBytes, _, _) -> T.bytes
  | E2 (AppendString, _, _) -> T.string
  | E2 ((StartsWith | EndsWith), _, _) -> T.bool
  | E1 (StringLength, _) -> T.u32
  | E1 (StringOfBytes, _) -> T.string
  | E1 (BytesOfString, _) -> T.bytes
  | E1 (Cardinality, _) -> T.u32
  | E0 (DataPtrOfString _) -> T.dataptr
  | E3 (DataPtrOfPtr, _, _, _) -> T.dataptr
  | E3 (FindSubstring, _, _, _) -> T.(Value (optional (Mac U24)))
  | E2 (GetBit, _, _) -> T.bit
  | E2 (GetVec, _, e1) -> T.Value (get_item_type ~lst:true ~vec:true e0 l e1)
  | E3 ((SetBit | SetVec), _, _, _) -> T.void
  | E1 (ReadByte, _) -> T.pair T.byte T.dataptr
  | E1 (ReadWord _, _) -> T.pair T.word T.dataptr
  | E1 (ReadDWord _, _) -> T.pair T.dword T.dataptr
  | E1 (ReadQWord _, _) -> T.pair T.qword T.dataptr
  | E1 (ReadOWord _, _) -> T.pair T.oword T.dataptr
  | E1 (Assert, _) -> T.void
  | E2 (ReadBytes, _, _) -> T.pair T.bytes T.dataptr
  | E2 (PeekByte, _, _) -> T.byte
  | E2 (PeekWord _, _, _) -> T.word
  | E2 (PeekDWord _ , _, _)-> T.dword
  | E2 (PeekQWord _, _, _) -> T.qword
  | E2 (PeekOWord _, _, _) -> T.oword
  | E2 (WriteByte, _, _) -> T.dataptr
  | E2 (WriteWord _, _, _) -> T.dataptr
  | E2 (WriteDWord _, _, _) -> T.dataptr
  | E2 (WriteQWord _, _, _) -> T.dataptr
  | E2 (WriteOWord _, _, _) -> T.dataptr
  | E2 (WriteBytes, _, _) -> T.dataptr
  | E2 (PokeByte, _, _) -> T.dataptr
  | E3 (BlitByte, _, _, _) -> T.dataptr
  | E2 (DataPtrAdd, _, _) -> T.dataptr
  | E2 (DataPtrSub, _, _) -> T.size
  | E1 (DataPtrPush, _) -> T.dataptr
  | E1 (DataPtrPop, _) -> T.dataptr
  | E1 (RemSize, _) -> T.size
  | E1 (DataPtrOffset, _) -> T.size
  | E2 (And, _, _) -> T.bool
  | E2 (Or, _, _) -> T.bool
  | E1 (Not, _) -> T.bool
  | E1 ((Identity | Abs | Neg), e1) -> type_of l e1
  | E1 ((Exp | Ceil | Floor | Round |
         Cos | Sin | Tan | ACos | ASin | ATan | CosH | SinH | TanH), _) ->
      T.float
  | E1 ((Log | Log10 | Sqrt), _) ->
      (* TODO: make it nullable only if it cannot be ascertained from e1
       * that the result will never be null *)
      T.to_nullable T.float
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
  | E1 (DataPtrOfBuffer, _) -> T.dataptr
  | E1 (GetEnv, _) -> T.(Value (optional (Mac String)))
  | E1 (GetMin, e) ->
      (match type_of l e |> T.develop_user_types with
      | T.Value { vtyp = Set (Heap, mn) ; nullable = false } -> T.Value mn
      | t -> raise (Type_error (e0, e, t, "be a heap")))
  | E2 (Cons, e1, _e2) ->
      T.slist (type_of l e1)
  | E2 (Pair, e1, e2) ->
      T.pair (type_of l e1) (type_of l e2)
  | E1 (Fst, e) ->
      (match type_of l e |> T.develop_user_types with
      | Pair (t, _) -> t
      | t -> raise (Type_error (e0, e, t, "be a pair")))
  | E1 (Snd, e) ->
      (match type_of l e |> T.develop_user_types with
      | Pair (_, t) -> t
      | t -> raise (Type_error (e0, e, t, "be a pair")))
  | E1 (Head, e) ->
      (match type_of l e |> T.develop_user_types with
      | SList t -> t
      | t -> raise (Type_error (e0, e, t, "be a slist")))
  | E1 (Tail, e) ->
      (match type_of l e |> T.develop_user_types with
      | SList _ as t -> t
      | t -> raise (Type_error (e0, e, t, "be a slist")))
  | E2 (MapPair, _, e2) ->
      (match type_of l e2 |> T.develop_user_types with
      | Function (_, ot) -> ot
      | t2 -> raise (Type_error (e0, e2, t2, "be a function")))
  | E2 (Map, set, f) ->
      (match type_of l f |> T.develop_user_types with
      | Function (_, ot) as f_t ->
          let map_mn g =
            match ot with
            | T.Value mn -> T.(Value (required (g mn)))
            | _ ->
                let err = "be a function of a value type" in
                raise (Type_error (e0, f, f_t, err)) in
          (match type_of l set |> T.develop_user_types with
          | T.Value { vtyp = Vec (n, _) ; _ } -> map_mn (fun mn -> Vec (n, mn))
          | T.Value { vtyp = Lst _ ; _ } -> map_mn (fun mn -> Lst mn)
          | T.Value { vtyp = Set (st, _) ; _ } -> map_mn (fun mn -> Set (st, mn))
          | T.SList _ -> SList ot
          | t -> raise (Type_error (e0, set, t, "be an iterable")))
      | t ->
          raise (Type_error (e0, f, t, "be a function")))
  | E2 ((Min | Max), e, _) ->
      type_of l e
  | E2 (Member, _, _) -> T.bool
  | E0 (Identifier n) as e ->
      (try List.assoc e l
      with Not_found ->
        raise (Unbound_identifier (e0, false, n, l)))
  | E0 (ExtIdentifier n) as e ->
      (try List.assoc e l
      with Not_found ->
        raise (Unbound_identifier (e0, true, n, l)))
  | E0 (CopyField|SkipField|SetFieldNull) ->
      T.mask
  | E2 (Let n, e1, e2) ->
      type_of ((E0 (Identifier n), type_of l e1) :: l) e2
  | E1 (Function (fid, ts), e) ->
      let l =
        Array.fold_lefti (fun l i t ->
          (E0 (Param (fid, i)), t) :: l
        ) l ts in
      Function (ts, type_of l e)
  | E0 (Param p) as e ->
      (try List.assoc e l
      with Not_found ->
        raise (Unbound_parameter (e0, p, l)))
  | E3 (If, _, e, _) ->
      type_of l e
  | E4 (ReadWhile, _, _, e, _) ->
      T.pair (type_of l e) T.dataptr
  | E3 (LoopWhile, _, _, e)
  | E3 (LoopUntil, _, _, e)
  | E3 (Fold, e, _, _)
  | E4 (Repeat, _, _, _, e) ->
      type_of l e
  | E1 (MaskGet _, _) ->
      T.mask
  | E1 (LabelOf, _) ->
      T.u16
  | E1 (SlidingWindow mn, _) ->
      T.set Sliding mn
  | E1 (TumblingWindow mn, _) ->
      T.set Tumbling mn
  | E1 (Sampling mn, _) ->
      T.set Sampling mn
  | E1 (HashTable mn, _) ->
      T.set HashTable mn
  | E1 (Heap, cmp) ->
      let item_t = get_compared_type l cmp in
      T.set Heap item_t
  | E2 ((Insert | DelMin), _, _) ->
      T.void
  | E2 (SplitBy, _, _) ->
      T.list T.(required (Mac String))
  | E2 (SplitAt, _, _) ->
      T.tuple T.[| required (Mac String) ; required (Mac String) |]
  | E2 (Join, _, _) ->
      T.string
  | E2 (AllocLst, _, e2) ->
      let item_t =
        match type_of l e2 with
        | Value mn ->
            mn
        | t1 ->
            raise (Type_error (
              e0, e2, t1, "be a possibly nullable value type")) in
      T.list item_t
  | E2 (PartialSort, _, _) ->
      T.void
  | E2 ((ChopBegin | ChopEnd), lst, _) ->
      type_of l lst
  | E2 (ScaleWeights, _, _) ->
      T.void
  | E3 (Top mn, _, _, _) ->
      T.set Top mn
  | E3 (InsertWeighted, _, _, _) ->
      T.void

and get_item_type_err ?(vec=false) ?(lst=false) ?(set=false) l e =
  match type_of l e |> T.develop_user_types with
  | Value { vtyp = Vec (_, t) ; nullable = false } when vec -> Ok t
  | Value { vtyp = Lst t ; nullable = false } when lst -> Ok t
  | Value { vtyp = Set (_, t) ; nullable = false } when set -> Ok t
  | t -> Error t

(* Return the element type or fail: *)
and get_item_type ?(vec=false) ?(lst=false) ?(set=false) e0 l e =
  match get_item_type_err ~vec ~lst ~set l e with
  | Ok t -> t
  | Error t ->
      let acceptable = if vec then [ "vector" ] else [] in
      let acceptable = if lst then "list" :: acceptable else acceptable in
      let acceptable = if set then "set" :: acceptable else acceptable in
      raise (Type_error (e0, e, t, "be a "^ String.join " or " acceptable))

and get_compared_type l cmp =
  match type_of l cmp with
  | Function ([| T.Value item_t ; _ |], _) ->
      item_t
  | cmp_t ->
      let err = "should be a function of two values" in
      raise (Comparator_error (cmp, cmp_t, err))

(* depth last, pass the list of bound identifiers along the way: *)
let rec fold u l f e =
  let u = f u l e in
  match e with
  | E0 _ ->
      u
  | E0S (_, es) ->
      List.fold_left (fun u e1 -> fold u l f e1) u es
  | E1 (Function (id, ts), e1) ->
      let l =
        Array.fold_lefti (fun l i t ->
          (E0 (Param (id, i)), t) :: l
        ) l ts in
      fold u l f e1
  | E1 (_, e1) ->
      fold u l f e1
  | E1S (_, e1, es) ->
      let u = fold u l f e1 in
      List.fold_left (fun u e1 -> fold u l f e1) u es
  | E2 (Let s, e1, e2) ->
      let l' = (E0 (Identifier s), type_of l e1) :: l in
      fold (fold u l f e1) l' f e2
  | E2 (_, e1, e2) ->
      fold (fold u l f e1) l f e2
  | E3 (_, e1, e2, e3) ->
      fold (fold (fold u l f e1) l f e2) l f e3
  | E4 (_, e1, e2, e3, e4) ->
      fold (fold (fold (fold u l f e1) l f e2) l f e3) l f e4

let iter l f e =
  fold () l (fun () l e -> f l e) e

(* depth first expression transformation: *)
let rec map l f e =
  match e with
  | E0 _ ->
      f l e
  | E0S (op, es) ->
      let es = List.map (map l f) es in
      f l (E0S (op, es))
  | E1 (Function (id, ts), e1) ->
      let l =
        Array.fold_lefti (fun l i t ->
          (E0 (Param (id, i)), t) :: l
        ) l ts in
      let e1 = map l f e1 in
      f l (E1 (Function (id, ts), e1))
  | E1 (op, e1) ->
      let e1 = map l f e1 in
      f l (E1 (op, e1))
  | E1S (op, e1, es) ->
      let e1 = map l f e1
      and es = List.map (map l f) es in
      f l (E1S (op, e1, es))
  | E2 (Let s, e1, e2) ->
      let e1 = map l f e1 in
      let l = (E0 (Identifier s), type_of l e1) :: l in
      let e2 = map l f e2 in
      f l (E2 (Let s, e1, e2))
  | E2 (op, e1, e2) ->
      let e1 = map l f e1
      and e2 = map l f e2 in
      f l (E2 (op, e1, e2))
  | E3 (op, e1, e2, e3) ->
      let e1 = map l f e1
      and e2 = map l f e2
      and e3 = map l f e3 in
      f l (E3 (op, e1, e2, e3))
  | E4 (op, e1, e2, e3, e4) ->
      let e1 = map l f e1
      and e2 = map l f e2
      and e3 = map l f e3
      and e4 = map l f e4 in
      f l (E4 (op, e1, e2, e3, e4))

let has_side_effect ?(l=[]) e =
  try
    iter l (fun _l e0 ->
      match e0 with
      | E1 ((Dump | DataPtrPush | DataPtrPop | ReadByte | ReadWord _ |
             ReadDWord _ | ReadQWord _ |ReadOWord _ | Assert), _)
      | E2 ((ReadBytes | WriteByte | WriteBytes | WriteWord _ | WriteDWord _ |
             WriteQWord _ | WriteOWord _ | Insert | DelMin | PartialSort), _, _)
      | E3 ((SetVec | InsertWeighted), _, _, _)
      | E4 (ReadWhile, _, _, _, _)->
          raise Exit
      | _ -> ()
    ) e ;
    false
  with Exit ->
    true

(* [l] is the stack of expr * type *)
let rec type_check l e =
  iter l (fun l e0 ->
    let check_void l e =
      match type_of l e |> T.develop_user_types with
      | Void -> ()
      | t -> raise (Type_error (e0, e, t, "be Void")) in
    let check_nullable b l e =
      match type_of l e |> T.develop_user_types with
      | Value { nullable ; _ } when nullable = b -> ()
      | t -> raise (Type_error (e0, e, t, "be a "^ (if b then "" else "not ") ^
                                          "nullable value")) in
    let is_comparable = function
      | T.Size | Byte | Word | DWord | QWord | OWord | Mask
      | Value {
          vtyp = Mac (
            Float | String | Char |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
            I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128) ;
          nullable = false } ->
          true
      | _ ->
          false in
    let check_comparable l e =
      let t = type_of l e |> T.develop_user_types in
      if not (is_comparable t) then
        raise (Type_error (e0, e, t, "be comparable")) in
    let check_numeric ?(only_mac=false) l e =
      match type_of l e |> T.develop_user_types with
      | Size | Byte | Word | DWord | QWord | OWord when not only_mac ->
          ()
      | Value {
          vtyp = Mac (
            Float |
            U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
            I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128) ;
          nullable = false } -> ()
      | t -> raise (Type_error (e0, e, t, "be numeric")) in
    let check_integer l e =
      match type_of l e |> T.develop_user_types with
      | Size | Byte | Word | DWord | QWord | OWord
      | Value { vtyp = Mac (
          U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128 |
          I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128) ;
          nullable = false } -> ()
      | t -> raise (Type_error (e0, e, t, "be an integer")) in
    let is_unsigned = function
      | T.Size
      | Value { vtyp = Mac (
          U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128) ;
          nullable = false } ->
          true
      | _ ->
          false in
    let check_unsigned l e =
      let t = type_of l e |> T.develop_user_types in
      if not (is_unsigned t) then
        raise (Type_error (e0, e, t, "be an unsigned integer")) in
    let check_param fe n act exp =
      if not (T.eq act exp) then
        let expected = IO.to_string T.print act in
        raise (Type_error_param (e0, fe, n, act, "be a "^ expected)) in
    let check_eq l e exp =
      let act = type_of l e in
      if not (T.eq act exp) then
        let expected = IO.to_string T.print exp in
        raise (Type_error (e0, e, act, "be a "^ expected)) in
    let check_same_types l e1 e2 =
      let t1 = type_of l e1 in
      check_eq l e2 t1 in
    let check_all_same_types l e1 e2s =
      List.iter (check_same_types l e1) e2s in
    let check_maybe_nullable l e =
      match type_of l e |> T.develop_user_types with
      | Value _ -> ()
      | t -> raise (Type_error (e0, e, t,
               "be a possibly nullable value")) in
    let check_vector l e =
      ignore (get_item_type ~vec:true e0 l e) in
    let check_set l e =
      ignore (get_item_type ~set:true e0 l e) in
    let check_list l e =
      ignore (get_item_type ~lst:true e0 l e) in
    let check_list_or_vector l e =
      ignore (get_item_type ~lst:true ~vec:true e0 l e) in
    let check_list_or_vector_or_set l e =
      ignore (get_item_type ~lst:true ~vec:true ~set:true e0 l e) in
    let check_slist l e =
      match type_of l e |> T.develop_user_types with
      | SList _ -> ()
      | t -> raise (Type_error (e0, e, t, "be a slist")) in
    let check_slist_same_type e1 l e =
      match type_of l e |> T.develop_user_types with
      | SList t -> check_eq l e1 t
      | t -> raise (Type_error (e0, e, t, "be a slist")) in
    let check_pair l e =
      match type_of l e |> T.develop_user_types with
      | Pair _ -> ()
      | t -> raise (Type_error (e0, e, t, "be a pair")) in
    let bad_arity expected e t =
      let s = Printf.sprintf "be a function of %d parameter(s)" expected in
      raise (Type_error (e0, e, t, s)) in
    let check_params1 l e f =
      match type_of l e |> T.develop_user_types with
      | Function ([|t1|], t2) -> f t1 t2
      | Function _ as t -> bad_arity 1 e t
      | t -> raise (Type_error (e0, e, t, "be a function")) in
    let check_params2 l e f =
      match type_of l e |> T.develop_user_types with
      | Function ([|t1; t2|], t3) -> f t1 t2 t3
      | Function _ as t -> bad_arity 2 e t
      | t -> raise (Type_error (e0, e, t, "be a function")) in
    let check_slist_of_maybe_nullable l e =
      match type_of l e |> T.develop_user_types with
      | SList (Value _) -> ()
      | t -> raise (Type_error (e0, e, t,
               "be a slist of maybe nullable values")) in
    let check_sum l e =
      match type_of l e |> T.develop_user_types with
      | Value { vtyp = Sum _ ; nullable = false } -> ()
      | t -> raise (Type_error (e0, e, t, "be a union")) in
    (* Check that [f] signature correspond to the array of parameters *)
    let check_fun_sign l f ps =
      match type_of l f with
      | Function (ts, _) ->
          let lf = Array.length ts and lp = Array.length ps in
          if lf <> lp then (
            let err = string_of_int lp ^" parameter(s) but function expect "^
                      string_of_int lf in
            raise (Apply_error (e0, err))) ;
          for i = 0 to lf - 1 do
            let act = type_of l ps.(i) in
            if not (T.eq act ts.(i)) then
              let expected = IO.to_string T.print ts.(i) in
              raise (Type_error (e0, ps.(i), act, "be a "^ expected))
          done
      | t ->
          raise (Type_error (e0, f, t, "be a function")) in
    let rec check_ip ?(rec_=false) l t =
      (* Any 32 or 128 unsigned integer will do, or any sum of such thing,
       * but do not allow recursion in the sum type because code generator
       * won't deal with that. *)
      match t |> T.develop_user_types with
      | Value { vtyp = Mac (U32 | U128) ; nullable = false } ->
          ()
      | Value { vtyp = Sum mns ; nullable = false } when rec_ = false ->
          Array.iter (fun (_, mn) -> check_ip ~rec_:true l (T.Value mn)) mns
      | t -> raise (Type_error (e0, e, t, "be an ip")) in
    match e0 with
    | E0 (Null _ | EndOfList _ | EmptySet _ | Now
         | RandomFloat | RandomU8 | RandomU32 | RandomU64 | RandomU128
         | Unit | Float _ | String _ | Bool _ | Char _
         | U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _
         | I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _
         | Bit _ | Size _ | Byte _ | Word _ | DWord _ | QWord _ | OWord _
         | Bytes _ | DataPtrOfString _ | Identifier _ | ExtIdentifier _
         | Param _ | CopyField | SkipField | SetFieldNull)
    | E1 ((Comment _ | Dump | Identity | Ignore | Function _
          | Hash), _)
    | E2 ((Pair | Let _), _, _) ->
        (* Subexpressions will be type checked recursively already *)
        ()
    | E0S (Seq, es) ->
        let rec loop = function
          | [] | [_] -> ()
          | e::es -> check_void l e ; loop es in
        loop es
    | E0S (MakeVec, []) ->
        raise (Struct_error (e0, "vector dimension must be > 0"))
    | E0S (MakeVec, e1 :: e2s) ->
        check_maybe_nullable l e1 ;
        check_all_same_types l e1 e2s
    | E0S (MakeLst mn, e1s) ->
        List.iter (fun e1 -> check_eq l e1 (Value mn)) e1s
    | E0S (MakeTup, es) ->
        if List.compare_length_with es 2 < 0 then
          raise (Struct_error (e0, "tuple dimension must be ≥ 2")) ;
        List.iter (check_maybe_nullable l) es
    | E0S (MakeRec, es) ->
        let len = List.length es in
        if len mod 2 <> 0 then
          raise (Struct_error (e0,
            "record expressions must have an even number of values")) ;
        if len < 2 then
          raise (Struct_error (e0, "record dimension must be ≥ 1")) ;
        List.iteri (fun i e ->
          if i mod 2 = 0 then ignore (field_name_of_expr e)
          else check_maybe_nullable l e
        ) es
    | E1S (Apply, f, es) ->
        check_fun_sign l f (Array.of_list es)
    | E1 (IsNull, e) ->
        check_nullable true l e
    | E2 (Nth, e1, e2) ->
        check_list_or_vector l e2 ;
        check_integer l e1
    | E1 (NotNull, e) ->
        check_nullable false l e
    | E1 (Force _, e) ->
        check_nullable true l e
    | E2 ((Gt | Ge | Eq | Ne | Min | Max), e1, e2) ->
        check_comparable l e1 ;
        check_same_types l e1 e2
    | E2 ((Add | Sub | Mul), e1, e2) ->
        check_numeric l e1 ;
        check_same_types l e1 e2
    | E2 ((Div | Rem | Pow), e1, e2) ->
        check_numeric ~only_mac:true l e1 ;
        check_same_types l e1 e2
    | E2 (Member, e1, e2) ->
        (match type_of l e2 |> T.develop_user_types with
        | Value { vtyp = (Vec (_, t) | Lst t | Set (_, t)) ; nullable = false } ->
            check_eq l e1 (T.Value t)
        | t ->
            raise (Type_error (e0, e, t, "be a vector or list")))
    | E2 ((LogAnd | LogOr | LogXor), e1, e2) ->
        check_integer l e1 ;
        check_same_types l e1 e2
    | E2 ((LeftShift | RightShift), e1, e2) ->
        check_integer l e1 ;
        check_eq l e2 T.u8
    | E1 ((LogNot | StringOfInt), e) ->
        check_integer l e
    | E1 ((StringOfChar | U8OfChar), e) ->
        check_eq l e T.char
    | E1 (StringOfIp, e) ->
        check_ip l (type_of l e)
    | E1 ((FloatOfString | CharOfString | U8OfString | U16OfString
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
        check_eq l e T.dataptr
    | E1 ((FloatOfQWord | U64OfQWord), e) ->
        check_eq l e T.qword
    | E1 ((QWordOfFloat | StringOfFloat), e) ->
        check_eq l e T.float
    | E1 (U8OfByte, e) ->
        check_eq l e T.byte
    | E1 ((CharOfU8 | ByteOfU8 | BoolOfU8), e) ->
        check_eq l e T.u8
    | E1 ((ToU8 | ToI8 | ToI16 | ToU16 | ToI24 | ToU24 | ToI32 | ToU32
         | ToI40 | ToU40 | ToI48 | ToU48 | ToI56 | ToU56 | ToI64 | ToU64
         | ToI128 | ToU128 | ToFloat), e) ->
        check_numeric l e
    | E1 (U16OfWord, e) ->
        check_eq l e T.word
    | E1 (WordOfU16, e) ->
        check_eq l e T.u16
    | E1 (U32OfDWord, e) ->
        check_eq l e T.dword
    | E1 ((DWordOfU32 | SizeOfU32), e) ->
        check_eq l e T.u32
    | E1 (QWordOfU64, e) ->
        check_eq l e T.u64
    | E1 (OWordOfU128, e) ->
        check_eq l e T.u128
    | E1 (U128OfOWord, e) ->
        check_eq l e T.oword
    | E1 (U32OfSize, e) ->
        check_eq l e T.size
    | E1 ((BitOfBool | U8OfBool | Not | Assert), e) ->
        check_eq l e T.bool
    | E1 ((Abs | Neg), e) ->
        check_numeric l e
    | E1 ((Exp | Log | Log10 | Sqrt | Ceil | Floor | Round |
           Cos | Sin | Tan | ACos | ASin | ATan | CosH | SinH | TanH), e) ->
        check_eq l e T.float
    | E1 ((Lower | Upper), e) ->
        check_eq l e T.string
    | E1 (BoolOfBit, e) ->
        check_eq l e T.bit
    | E1 ((ListOfSList | ListOfSListRev | SetOfSList), e) ->
        check_slist_of_maybe_nullable l e
    | E1 (ListOfVec, e) ->
        check_vector l e
    | E1 (ListOfSet, e) ->
        check_set l e
    | E1 (DataPtrOfBuffer, e) ->
        check_eq l e T.size
    | E1 (GetEnv, e) ->
        check_eq l e T.string
    | E1 (GetMin, e) ->
        check_set l e
    | E2 (AppendByte, e1, e2) ->
        check_eq l e1 T.bytes ;
        check_eq l e2 T.byte
    | E2 (AppendBytes, e1, e2) ->
        check_eq l e1 T.bytes ;
        check_eq l e2 T.bytes
    | E2 ((AppendString | StartsWith | EndsWith | SplitBy), e1, e2) ->
        check_eq l e1 T.string ;
        check_eq l e2 T.string
    | E1 (StringOfBytes, e) ->
        check_eq l e T.bytes
    | E1 (Cardinality, e) ->
        check_list_or_vector_or_set l e
    | E2 (GetBit, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.size
    | E2 (GetVec, e1, e2) ->
        check_integer l e1 ;
        check_list_or_vector l e2
    | E2 (ScaleWeights, set, d) ->
        check_set l set ;
        check_numeric l d
    | E3 (SetBit, e1, e2, e3) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.size ;
        check_eq l e3 T.bit
    | E3 (SetVec, e1, e2, e3) ->
        check_integer l e1 ;
        (match type_of l e2 |> T.develop_user_types with
        | T.Value { vtyp = (T.Vec (_, mn) | T.Lst mn) ; nullable = false } ->
            check_eq l e3 (Value mn)
        | t ->
            raise (Type_error (e0, e1, t, "be a vector")))
    | E1 ((ReadByte | ReadWord _ | ReadDWord _ | ReadQWord _ | ReadOWord _), e) ->
        check_eq l e T.dataptr
    | E2 ((ReadBytes | PeekByte | PeekWord _ | PeekDWord _ | PeekQWord _
         | PeekOWord _), e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.size
    | E2 ((WriteByte | PokeByte), e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.byte
    | E2 (WriteWord _, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.word
    | E2 (WriteDWord _, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.dword
    | E2 (WriteQWord _, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.qword
    | E2 (WriteOWord _, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.oword
    | E2 (WriteBytes, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.bytes
    | E3 (BlitByte, e1, e2, e3) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.byte ;
        check_eq l e3 T.size
    | E2 (DataPtrAdd, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.size
    | E2 (DataPtrSub, e1, e2) ->
        check_eq l e1 T.dataptr ;
        check_eq l e2 T.dataptr
    | E1 ((DataPtrPush | DataPtrPop), e1) ->
        check_eq l e1 T.dataptr
    | E1 ((RemSize | DataPtrOffset), e) ->
        check_eq l e T.dataptr
    | E3 (DataPtrOfPtr, e1, e2, e3) ->
        check_eq l e1 T.dataptr ;
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
        ignore (type_of l e) (* everything checks already performed in [type_of] *)
    | E1 (Construct (mns, i), e) ->
          let max_lbl = Array.length mns - 1 in
          if i < 0 || i > max_lbl then (
            let msg =
              Printf.sprintf "Constructor (%d) must not be greater than %d"
                i max_lbl in
            raise (Struct_error (e0, msg))
          ) ;
          check_eq l e (Value (snd mns.(i)))
    | E1 (Fst, e) ->
        check_pair l e
    | E1 (Snd, e) ->
        check_pair l e
    | E1 (Head, e) ->
        check_slist l e
    | E1 (Tail, e) ->
        check_slist l e
    | E2 (Cons, e1, e2) ->
        check_slist_same_type e1 l e2
    | E2 (MapPair, e1, e2) ->
        (match type_of l e2 |> T.develop_user_types with
        | Function ([| i1 ; i2 |], _) ->
            (match type_of l e1 |> T.develop_user_types with
            | Pair (p1, p2) as t1 ->
                if not (T.eq p1 i1 && T.eq p2 i2) then
                  let err = "be a "^ T.to_string (Pair (i1, i2)) in
                  raise (Type_error (e0, e1, t1, err))
            | t1 ->
                raise (Type_error (e0, e1, t1, "be a pair")))
        | Function _ as t2 ->
            bad_arity 2 e2 t2
        | t2 ->
            raise (Type_error (e0, e2, t2, "be a function")))
    | E2 (Map, set, f) ->
        (match type_of l f |> T.develop_user_types with
        | Function ([| it |], ot) as f_t ->
            let check_fun_type_with t must_output_mn =
              if not (T.eq t it) then (
                let err = "be a function of "^ T.to_string t in
                raise (Type_error (e0, f, f_t, err))) ;
              if must_output_mn then
                match ot with T.Value _ -> ()
                | _ ->
                    let err = "be a function returning a value type" in
                    raise (Type_error (e0, f, f_t, err)) in
            (match type_of l set |> T.develop_user_types with
            | T.(Value { vtyp = Vec (_, mn) ; nullable = false }) ->
                check_fun_type_with (T.Value mn) true
            | T.(Value { vtyp = Lst mn ; nullable = false }) ->
                check_fun_type_with (T.Value mn) true
            | T.(Value { vtyp = Set (_, mn) ; nullable = false }) ->
                check_fun_type_with (T.Value mn) true
            | T.SList t ->
                check_fun_type_with t false
            | t ->
                raise (Type_error (e0, set, t, "be an iterable")))
        | Function _ as f_t ->
            raise (Type_error (e0, f, f_t, "be a function of one argument"))
        | t -> raise (Type_error (e0, f, t, "be a function")))
    | E3 (If, e1, e2, e3) ->
        check_eq l e1 T.bool ;
        check_same_types l e2 e3
    | E4 (ReadWhile, e1 (*byte->bool*), e2 (*'a->byte->'a*), e3 (*'a*), e4 (*ptr*)) ->
        check_params1 l e1 (fun t1 t2 ->
          check_param e1 0 t1 T.byte ;
          check_param e1 1 t2 T.bool) ;
        check_params2 l e2 (fun t1 t2 t3 ->
          check_eq l e3 t1 ;
          check_param e2 1 t2 T.byte ;
          check_eq l e3 t3) ;
        check_eq l e4 T.dataptr
    | E3 (LoopWhile, e1 (*'a->bool*), e2 (*'a->'a*), e3 (*'a*)) ->
        check_params1 l e1 (fun t1 t2 (* return type *)->
          check_eq l e3 t1 ;
          check_param e1 ~-1 t2 T.bool) ;
        check_params1 l e2 (fun t1 t2 (* return type *)->
          check_eq l e3 t1 ;
          check_eq l e3 t2)
    | E3 (LoopUntil, e1, e2, e3) ->
        check_params1 l e1 (fun t1 t2 (* return type *)->
          check_eq l e3 t1 ;
          check_eq l e3 t2) ;
        check_params1 l e2 (fun t1 t2 (* return type *)->
          check_eq l e3 t1 ;
          check_param e2 ~-1 t2 T.bool) ;
    | E3 (Fold, e1, e2, e3) ->
        (* Fold function first parameter is the result and second is the list
         * item *)
        let item_t =
          T.Value (get_item_type ~lst:true ~vec:true ~set:true e0 l e3) in
        check_params2 l e2 (fun p1 p2 ret ->
          check_eq l e1 p1 ;
          check_eq l e1 ret ;
          check_param e2 1 p2 item_t)
    | E4 (Repeat, e1 (*from*), e2 (*to*), e3 (*idx->'a->'a*), e4 (*'a*)) ->
        check_eq l e1 T.i32 ;
        check_eq l e2 T.i32 ;
        check_params2 l e3 (fun t1 t2 t3 ->
          check_param e3 0 t1 T.i32 ;
          check_eq l e4 t2 ;
          check_eq l e4 t3)
    | E1 (MaskGet _, e1) ->
        check_eq l e1 T.Mask
    | E1 (LabelOf, e1) ->
        check_sum l e1
    | E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _ | HashTable _), e1) ->
        check_unsigned l e1
    | E1 (Heap, cmp) ->
        let cmp_t = type_of l cmp in
        let err msg =
          raise (Comparator_error (cmp, cmp_t, msg)) in
        (* TODO: We could also accept a Null comparison function if the items
         * are readily comparable (as in [is_comparable]). *)
        (match cmp_t with
        | Function (ts, _) ->
            let ts_len = Array.length ts in
            if ts_len <> 2 then
              err "must have two parameters" ;
            if not (T.eq ts.(0) ts.(1)) then
              err "parameters must have the same type"
        | _ ->
            err "must be a function")
    | E2 (Insert, set, x) ->
        (match type_of l set |> T.develop_user_types with
        | T.Value { vtyp = T.Set (_, mn) ; nullable = false } ->
            check_eq l x (Value mn)
        | t ->
            raise (Type_error (e0, set, t, "be a set")))
    | E2 (DelMin, set, n) ->
        check_set l set ;
        check_unsigned l n
    | E2 (SplitAt, e1, e2) ->
        check_unsigned l e1 ;
        check_eq l e2 T.string
    | E2 (Join, e1, e2) ->
        check_eq l e1 T.string ;
        let item_t = get_item_type ~lst:true ~vec:true e0 l e2 in
        if item_t <> T.(required (Mac String)) then
          let msg = "be a list or vector of strings" in
          raise (Type_error (e0, e, T.Value item_t, msg))
    | E2 (AllocLst, e1, e2) ->
        check_unsigned l e1 ;
        check_maybe_nullable l e2
    | E2 (PartialSort, e1, e2) ->
        let item_t1 = T.Value (get_item_type ~lst:true ~vec:true e0 l e1) in
        if not (is_comparable item_t1) then
          raise (Type_error (e0, e1, item_t1,
                             "be a list or vector of comparable items")) ;
        let item_t2 = T.Value (get_item_type ~lst:true ~vec:true e0 l e2) in
        if not (is_unsigned item_t2) then
          raise (Type_error (e0, e2, item_t2,
                             "be a list or vector of unsigned integers"))
    | E2 ((ChopBegin | ChopEnd), lst, len) ->
        check_list l lst ;
        check_unsigned l len
    | E3 (Top _, size, max_size, sigmas) ->
        check_unsigned l size ;
        check_unsigned l max_size ;
        check_numeric l sigmas
    | E3 (InsertWeighted, set, w, x) ->
        (match type_of l set |> T.develop_user_types with
        | T.Value { vtyp = T.Set (_, mn) ; nullable = false } ->
            check_eq l w T.float ;
            check_eq l x (Value mn)
        | t ->
            raise (Type_error (e0, set, t, "be a set")))
  ) e

(*$inject
  let pass_type_check s =
    let e = Parser.expr s |> List.hd in
    try type_check [] e ; true
    with _ -> false *)

(*$T pass_type_check
   not (pass_type_check "(get-item 2 (read-qword big-endian (u64 17)))")
   not (pass_type_check "(get-alt \"pejh\" (random-float))")
*)

let size_of_expr l e =
  fold 0 l (fun n _l _e0 -> n + 1) e

let print_environment oc l =
  let p oc (e, t) =
    Printf.fprintf oc "%a:%a"
      (print ~max_depth:2) e
      T.print t in
  pretty_list_print p oc l

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
            T.print t)
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
            T.print t)
    | Type_error_path (e0, e, path, s) ->
        Some (
          Printf.sprintf2
            "Type Error: In expression\
             %s\
             path %a of expression\
             %s\
             should %s"
            (to_pretty_string ~max_depth e0)
            T.print_path path
            (to_pretty_string ~max_depth e)
            s)
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
    | Comparator_error (e0, t, s) ->
        Some (
          Printf.sprintf2
            "Invalid comparator function:\
             %s\
             %s but has type %a"
            (to_pretty_string ~max_depth e0)
            s
            T.print t)
    | Unbound_identifier (e0, ext, n, l) ->
        Some (
          let ext = if ext then "external " else "" in
          Printf.sprintf2
            "Unbound %sidentifier %S: In expression\
             %s\
             environment is %a"
            ext n
            (to_pretty_string ~max_depth e0)
            print_environment l)
    | Unbound_parameter (e0, p, l) ->
        Some (
          Printf.sprintf2
            "Unbound parameter %a: In expression\
             %s\
             environment is %a"
            param_print p
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

let let_ ?(l=[]) ?name e f =
  match e with
  (* If [e] is already an identifier (or a param) there is no need for a
   * new one: *)
  | E0 (Param _ | Identifier _)
  (* Also, if it's a constant then the optimizer will work better if it's
   * not hidden behind an identifier: *)
  | E0 (Null _ | EndOfList _ | EmptySet _ | Unit | Float _ | Bool _ | Char _
       | U8 _ | U16 _ | U24 _ | U32 _ | U40 _ | U48 _ | U56 _ | U64 _ | U128 _
       | I8 _ | I16 _ | I24 _ | I32 _ | I40 _ | I48 _ | I56 _ | I64 _ | I128 _
       | Bit _ | Size _ | Byte _ | Word _ | DWord _ | QWord _ | OWord _
       | CopyField | SkipField | SetFieldNull)  ->
      f l e
  | _ ->
      let n = match name with Some n -> gen_id n | None -> gen_id "gen" in
      (* Best effort, as sometime we cannot provide the environment but
       * do not need it in the [body]: *)
      let l =
        try (E0 (Identifier n), type_of l e) :: l
        with Unbound_identifier _ | Unbound_parameter _ -> l in
      (match f l (E0 (Identifier n)) with
      | E0 (Identifier n') when n' = n ->
          (* In that case the identifier is useless: *)
          e
      | body ->
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
            fold 0 l (fun c _l -> function
              | E0 (Identifier n') when n' = n -> c + 1
              | _ -> c
            ) body in
          if !optimize && use_count <= 1 then
            (* No need for the let: *)
            map l (fun _l -> function
              | E0 (Identifier n') when n' = n -> e
              | e -> e
            ) body
          else
            E2 (Let n, e, body))

(* Do not use a function (thus not MapPair) to avoid leaking function parameters *)
let with_sploded_pair ~l what e f =
  let pair_id = gen_id "pair" ^"_"^ what in
  let n1 = pair_id ^"_fst"
  and n2 = pair_id ^"_snd" in
  let_ ~l ~name:pair_id e (fun l pair_ ->
    let_ ~l ~name:n1 (E1 (Fst, pair_)) (fun l n1_ ->
      let_ ~l ~name:n2 (E1 (Snd, pair_)) (fun l n2_ ->
        f l n1_ n2_)))

(* Create a function expression: *)
let func =
  let next_id = ref 0 in
  fun ?(l=[]) typs f ->
    let fid = !next_id in
    incr next_id ;
    let l =
      List.rev_append (List.init (Array.length typs) (fun i ->
        E0 (Param (fid, i)), typs.(i))) l in
    E1 (Function (fid, typs), f l fid)

(* Specialized to a given arity: *)

let func0 ?l f =
  func ?l [||] (fun l _fid -> f l)

let func1 ?l t1 f =
  func ?l [| t1 |] (fun l fid ->
    let p1 = E0 (Param (fid, 0)) in
    f l p1)

let func2 ?l t1 t2 f =
  func ?l [| t1 ; t2 |] (fun l fid ->
    let p1 = E0 (Param (fid, 0))
    and p2 = E0 (Param (fid, 1)) in
    f l p1 p2)

let func3 ?l t1 t2 t3 f =
  func ?l [| t1 ; t2 ; t3 |] (fun l fid ->
    let p1 = E0 (Param (fid, 0))
    and p2 = E0 (Param (fid, 1))
    and p3 = E0 (Param (fid, 2)) in
    f l p1 p2 p3)

let func4 ?l t1 t2 t3 t4 f =
  func ?l [| t1 ; t2 ; t3 ; t4 |] (fun l fid ->
    let p1 = E0 (Param (fid, 0))
    and p2 = E0 (Param (fid, 1))
    and p3 = E0 (Param (fid, 2))
    and p4 = E0 (Param (fid, 3)) in
    f l p1 p2 p3 p4)

let func5 ?l t1 t2 t3 t4 t5 f =
  func ?l [| t1 ; t2 ; t3 ; t4 ; t5 |] (fun l fid ->
    let p1 = E0 (Param (fid, 0))
    and p2 = E0 (Param (fid, 1))
    and p3 = E0 (Param (fid, 2))
    and p4 = E0 (Param (fid, 3))
    and p5 = E0 (Param (fid, 4)) in
    f l p1 p2 p3 p4 p5)

let is_identity = function
  | E1 (Function (fid, [| _ |]), E0 (Param (fid', 0))) when fid = fid' ->
      true
  | _ ->
      false

(*$< DessserTypes *)
(*$= type_of & ~printer:(BatIO.to_string T.print)
  (Pair (u24, DataPtr)) (type_of [] Ops.(pair (to_u24 (i32 42l)) (data_ptr_of_string "")))
*)

(* Users can define additional expressions, defined in terms of the above
 * expressions with a specific name, type checker, pretty printer and
 * parser. *)

type user_expr =
  { name : string ;
    def : t ;
    type_check : (t * T.t) list -> unit ;
    type_of : (t * T.t) list -> T.t ;
    print : unit IO.output -> unit ;
    (* parse *) }

let user_expressions = Hashtbl.create 50

let register_user_expr name ?check ?type_ ?printer def =
  Hashtbl.modify_opt name (function
    | None ->
        let type_check = check |?
          fun l -> type_check l def
        and type_of =
          Option.default_delayed (fun () ->
            fun l -> type_of l def
          ) type_
        and printer = printer |?
          fun oc -> print oc def in
        Some { name ; def ; type_check ; type_of ; print = printer }
    | Some _ ->
        invalid_arg "register_user_expr"
  ) user_expressions

(*
 * Simplified notation:
 *)

module Ops =
struct
  let identity e1 = E1 (Identity, e1)

  let ignore_ e1 = E1 (Ignore, e1)

  let dump e1 = E1 (Dump, e1)

  let debug e1 =
    E1 ((if !dump_debug then Dump else Ignore), e1)

  let debugs es = E0S (Seq, List.map debug es)

  let bool n = E0 (Bool n)

  let false_ = bool false

  let true_ = bool true

  let bit n = E0 (Bit n)

  let i8 n = E0 (I8 n)

  let u8 n = E0 (U8 n)

  let i16 n = E0 (I16 n)

  let u16 n = E0 (U16 n)

  let i24 n = E0 (I24 n)

  let u24 n = E0 (U24 n)

  let i32 n = E0 (I32 n)

  let u32 n = E0 (U32 n)

  let i40 n = E0 (I40 n)

  let u40 n = E0 (U40 n)

  let i48 n = E0 (I48 n)

  let u48 n = E0 (U48 n)

  let i56 n = E0 (I56 n)

  let u56 n = E0 (U56 n)

  let i64 n = E0 (I64 n)

  let u64 n = E0 (U64 n)

  let i128 n = E0 (I128 n)

  let u128 n = E0 (U128 n)

  let char n = E0 (Char n)

  let unit = E0 Unit

  let float n = E0 (Float n)

  let string n = E0 (String n)

  let byte n = E0 (Byte n)

  let size n = E0 (Size n)

  let word n = E0 (Word n)

  let dword n = E0 (DWord n)

  let qword n = E0 (QWord n)

  let oword n = E0 (OWord n)

  let bytes s = E0 (Bytes s)

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

  let is_null = function
    | E0 (Null _) when !optimize -> true_
    | E1 (NotNull, _) when !optimize -> false_
    | e -> E1 (IsNull, e)

  let nth e1 e2 = E2 (Nth, e1, e2)

  let read_byte e1 = E1 (ReadByte, e1)

  let read_word en e1 = E1 (ReadWord en, e1)

  let read_dword en e1 = E1 (ReadDWord en, e1)

  let read_qword en e1 = E1 (ReadQWord en, e1)

  let read_oword en e1 = E1 (ReadOWord en, e1)

  let peek_word en e1 e2 = E2 (PeekWord en, e1, e2)

  let peek_dword en e1 e2 = E2 (PeekDWord en, e1, e2)

  let peek_qword en e1 e2 = E2 (PeekQWord en, e1, e2)

  let peek_oword en e1 e2 = E2 (PeekOWord en, e1, e2)

  let read_bytes e1 e2 = E2 (ReadBytes, e1, e2)

  let peek_byte e1 e2 = E2 (PeekByte, e1, e2)

  let write_bytes e1 e2 = E2 (WriteBytes, e1, e2)

  let write_byte e1 e2 = E2 (WriteByte, e1, e2)

  let write_word en e1 e2 = E2 (WriteWord en, e1, e2)

  let write_dword en e1 e2 = E2 (WriteDWord en, e1, e2)

  let write_qword en e1 e2 = E2 (WriteQWord en, e1, e2)

  let write_oword en e1 e2 = E2 (WriteOWord en, e1, e2)

  let insert set x = E2 (Insert, set, x)

  let insert_weighted set w x = E3 (InsertWeighted, set, w, x)

  let del_min set n = E2 (DelMin, set, n)

  let get_min set = E1 (GetMin, set)

  let scale_weights set d = E2 (ScaleWeights, set, d)

  let join e1 e2 =
    match e1, e2 with
    | E0 (String s1), E0S (MakeVec, ss) when !optimize ->
        (try
          List.map (function
            | E0 (String s) -> s
            | _ -> raise Exit
          ) ss |>
          String.join s1 |>
          string
        with Exit ->
          E2 (Join, e1, e2))
    | _ -> E2 (Join, e1, e2)

  let bytes_of_string e1 = E1 (BytesOfString, e1)

  let string_of_int_ = function
    | E0 (U8 n) when !optimize -> string (Uint8.to_string n)
    | E0 (U16 n) when !optimize -> string (Uint16.to_string n)
    | E0 (U24 n) when !optimize -> string (Uint24.to_string n)
    | E0 (U32 n) when !optimize -> string (Uint32.to_string n)
    | E0 (U40 n) when !optimize -> string (Uint40.to_string n)
    | E0 (U48 n) when !optimize -> string (Uint48.to_string n)
    | E0 (U56 n) when !optimize -> string (Uint56.to_string n)
    | E0 (U64 n) when !optimize -> string (Uint64.to_string n)
    | E0 (U128 n) when !optimize -> string (Uint128.to_string n)
    | E0 (I8 n) when !optimize -> string (Int8.to_string n)
    | E0 (I16 n) when !optimize -> string (Int16.to_string n)
    | E0 (I24 n) when !optimize -> string (Int24.to_string n)
    | E0 (I32 n) when !optimize -> string (Int32.to_string n)
    | E0 (I40 n) when !optimize -> string (Int40.to_string n)
    | E0 (I48 n) when !optimize -> string (Int48.to_string n)
    | E0 (I56 n) when !optimize -> string (Int56.to_string n)
    | E0 (I64 n) when !optimize -> string (Int64.to_string n)
    | E0 (I128 n) when !optimize -> string (Int128.to_string n)
    | e -> E1 (StringOfInt, e)

  let string_of_float_ = function
    | E0 (Float f) when !optimize -> string (hexstring_of_float f)
    | e -> E1 (StringOfFloat, e)

  let string_of_ip = function
    | E0 (U32 n) when !optimize -> string (DessserIpTools.V4.to_string n)
    | E0 (U128 n) when !optimize -> string (DessserIpTools.V6.to_string n)
    | e -> E1 (StringOfIp, e)

  let char_of_string = function
    | E0 (String s) when !optimize ->
        if String.length s = 1 then char s.[0]
        else invalid_arg "char_of_string"
    | e ->
        E1 (CharOfString, e)

  let string_of_char = function
    | E0 (Char c) when !optimize -> string (String.of_char c)
    | e -> E1 (StringOfChar, e)

  let null vt = E0 (Null vt)

  let not_null = function
    | E1 (Force _, e1) when !optimize -> e1
    | e1 -> E1 (NotNull, e1)

  let or_null_ vt op conv s =
    try not_null (op (conv s)) with Invalid_argument _ -> null vt

  let float_of_string_ = function
    | E0 (String s) when !optimize -> or_null_ (Mac Float) float float_of_string s
    | e -> E1 (FloatOfString, e)

  let u8_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U8) u8 Uint8.of_string s
    | e -> E1 (U8OfString, e)

  let u16_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U16) u16 Uint16.of_string s
    | e -> E1 (U16OfString, e)

  let u24_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U24) u24 Uint24.of_string s
    | e -> E1 (U24OfString, e)

  let u32_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U32) u32 Uint32.of_string s
    | e -> E1 (U32OfString, e)

  let u40_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U40) u40 Uint40.of_string s
    | e -> E1 (U40OfString, e)

  let u48_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U48) u48 Uint48.of_string s
    | e -> E1 (U48OfString, e)

  let u56_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U56) u56 Uint56.of_string s
    | e -> E1 (U56OfString, e)

  let u64_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U64) u64 Uint64.of_string s
    | e -> E1 (U64OfString, e)

  let u128_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac U128) u128 Uint128.of_string s
    | e -> E1 (U128OfString, e)

  let i8_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I8) i8 Int8.of_string s
    | e -> E1 (I8OfString, e)

  let i16_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I16) i16 Int16.of_string s
    | e -> E1 (I16OfString, e)

  let i24_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I24) i24 Int24.of_string s
    | e -> E1 (I24OfString, e)

  let i32_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I32) i32 Int32.of_string s
    | e -> E1 (I32OfString, e)

  let i40_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I40) i40 Int40.of_string s
    | e -> E1 (I40OfString, e)

  let i48_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I48) i48 Int48.of_string s
    | e -> E1 (I48OfString, e)

  let i56_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I56) i56 Int56.of_string s
    | e -> E1 (I56OfString, e)

  let i64_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I64) i64 Int64.of_string s
    | e -> E1 (I64OfString, e)

  let i128_of_string = function
    | E0 (String s) when !optimize -> or_null_ (Mac I128) i128 Int128.of_string s
    | e -> E1 (I128OfString, e)

  let float_of_ptr e1 = E1 (FloatOfPtr, e1)

  let char_of_ptr e1 = E1 (CharOfPtr, e1)

  let u8_of_ptr e1 = E1 (U8OfPtr, e1)

  let u16_of_ptr e1 = E1 (U16OfPtr, e1)

  let u24_of_ptr e1 = E1 (U24OfPtr, e1)

  let u32_of_ptr e1 = E1 (U32OfPtr, e1)

  let u40_of_ptr e1 = E1 (U40OfPtr, e1)

  let u48_of_ptr e1 = E1 (U48OfPtr, e1)

  let u56_of_ptr e1 = E1 (U56OfPtr, e1)

  let u64_of_ptr e1 = E1 (U64OfPtr, e1)

  let u128_of_ptr e1 = E1 (U128OfPtr, e1)

  let i8_of_ptr e1 = E1 (I8OfPtr, e1)

  let i16_of_ptr e1 = E1 (I16OfPtr, e1)

  let i24_of_ptr e1 = E1 (I24OfPtr, e1)

  let i32_of_ptr e1 = E1 (I32OfPtr, e1)

  let i40_of_ptr e1 = E1 (I40OfPtr, e1)

  let i48_of_ptr e1 = E1 (I48OfPtr, e1)

  let i56_of_ptr e1 = E1 (I56OfPtr, e1)

  let i64_of_ptr e1 = E1 (I64OfPtr, e1)

  let i128_of_ptr e1 = E1 (I128OfPtr, e1)

  let byte_of_u8 = function
    | E0 (U8 n) when !optimize -> byte n
    | e -> E1 (ByteOfU8, e)

  let bool_of_u8 = function
    | E0 (U8 n) when !optimize -> bool (Uint8.compare Uint8.zero n <> 0)
    | e -> E1 (BoolOfU8, e)

  let word_of_u16 = function
    | E0 (U16 n) when !optimize -> word n
    | e -> E1 (WordOfU16, e)

  let dword_of_u32 = function
    | E0 (U32 n) when !optimize -> dword n
    | e -> E1 (DWordOfU32, e)

  let qword_of_u64 = function
    | E0 (U64 n) when !optimize -> qword n
    | e -> E1 (QWordOfU64, e)

  let oword_of_u128 = function
    | E0 (U128 n) when !optimize -> oword n
    | e -> E1 (OWordOfU128, e)

  let u8_of_byte = function
    | E0 (Byte n) when !optimize -> u8 n
    | e -> E1 (U8OfByte, e)

  let u8_of_char = function
    | E0 (Char c) when !optimize -> u8 (Uint8.of_int (Char.code c))
    | e -> E1 (U8OfChar, e)

  let u8_of_bool = function
    | E0 (Bool false) when !optimize -> u8 (Uint8.of_int 0)
    | E0 (Bool true) when !optimize -> u8 (Uint8.of_int 1)
    | e -> E1 (U8OfBool, e)

  let bool_of_bit = function
    | E0 (Bit b) when !optimize -> bool b
    | e -> E1 (BoolOfBit, e)

  let bit_of_bool = function
    | E0 (Bool b) when !optimize -> bit b
    | e -> E1 (BitOfBool, e)

  let u8_of_bit = u8_of_bool % bool_of_bit

  let bit_of_u8 = bit_of_bool % bool_of_u8

  let char_of_u8 = function
    | E0 (U8 n) when !optimize -> char (Char.chr (Uint8.to_int n))
    | e -> E1 (CharOfU8, e)

  let u32_of_size = function
    | E0 (Size n) when !optimize -> u32 (Uint32.of_int n)
    | e -> E1 (U32OfSize, e)

  let size_of_u32 = function
    | E0 (U32 n) when !optimize -> size (Uint32.to_int n)
    | e -> E1 (SizeOfU32, e)

  let eol t = E0 (EndOfList t)

  let end_of_list = eol

  let sliding_window mn e1 = E1 (SlidingWindow mn, e1)

  let tumbling_window mn e1 = E1 (TumblingWindow mn, e1)

  let sampling mn e1 = E1 (Sampling mn, e1)

  let hash_table mn e1 = E1 (HashTable mn, e1)

  let heap cmp = E1 (Heap, cmp)

  let empty_set mn = E0 (EmptySet mn)

  let top mn size max_size sigmas = E3 (Top mn, size, max_size, sigmas)

  let now = E0 Now

  let random_float = E0 RandomFloat

  let random_u8 = E0 RandomU8

  let random_u32 = E0 RandomU32

  let random_u64 = E0 RandomU64

  let random_u128 = E0 RandomU128

  let pair e1 e2 = E2 (Pair, e1, e2)

  let first = function
    | E2 (Pair, e, _) when !optimize -> e
    | e -> E1 (Fst, e)

  let secnd = function
    | E2 (Pair, _, e) when !optimize -> e
    | e -> E1 (Snd, e)

  let cons e1 e2 = E2 (Cons, e1, e2)

  let head = function
    | E2 (Cons, e, _) when !optimize -> e
    | e -> E1 (Head, e)

  let rec tail = function
    | E0 (EndOfList _) when !optimize -> invalid_arg "tail" (* FIXME: return Null *)
    | E2 (Cons, e, E0 (EndOfList _)) when !optimize -> e
    | E2 (Cons, _, e) when !optimize -> tail e
    | e -> E1 (Tail, e)

  let byte_of_char = function
    | E0 (Char c) when !optimize -> byte (Uint8.of_int (Char.code c))
    | e -> byte_of_u8 (u8_of_char e)

  let byte_of_const_char c = byte_of_char (char c)

  let rec if_ ~cond ~then_ ~else_ =
    match cond with
    | E0 (Bool true) when !optimize -> then_
    | E0 (Bool false) when !optimize -> else_
    | E1 (Not, e) when !optimize -> if_ ~cond:e ~then_:else_ ~else_:then_
    | _->
        if eq then_ else_ && !optimize && not (has_side_effect cond) then
          then_
        else
          E3 (If, cond, then_, else_)

  let read_while ~cond ~reduce ~init ~pos =
    match cond with
    | E0 (Bool false) when !optimize -> pair init pos
    | _ -> E4 (ReadWhile, cond, reduce, init, pos)

  let float_of_qword = function
    | E0 (QWord n) when !optimize -> float (BatInt64.float_of_bits (Uint64.to_int64 n))
    | e -> E1 (FloatOfQWord, e)

  let qword_of_float = function
    | E0 (Float f) when !optimize -> qword (Uint64.of_int64 (BatInt64.bits_of_float f))
    | e -> E1 (QWordOfFloat, e)

  let comment n e1 = E1 (Comment n, e1)

  let ge e1 e2 = E2 (Ge, e1, e2)

  let gt e1 e2 = E2 (Gt, e1, e2)

  let le e1 e2 = ge e2 e1

  let lt e1 e2 = gt e2 e1

  let rec eq e1 e2 =
    match e1, e2 with
    | E0 Null _, E0 Null _
    | E0 (EndOfList _), E0 (EndOfList _)
    | E0 (EmptySet _), E0 (EmptySet _)
    | E0 Unit, E0 Unit
    | E0 CopyField, E0 CopyField
    | E0 SkipField, E0 SkipField
    | E0 SetFieldNull, E0 SetFieldNull when !optimize ->
        true_
    (* None other combination of those can be equal: *)
    | E0 (Null _ | EndOfList _ | EmptySet _ | Unit
         | CopyField | SkipField | SetFieldNull),
      E0 (Null _ | EndOfList _ | EmptySet _ | Unit
         | CopyField | SkipField | SetFieldNull) when !optimize ->
        false_
    (* Another easy case of practical importance: comparison of a null with
     * a NotNull: *)
    | E0 (Null _), E1 (NotNull, _)
    | E1 (NotNull, _), E0 (Null _) when !optimize ->
        false_
    (* Peel away some common wrappers: *)
    | E1 (NotNull, e1), E1 (NotNull, e2)
    | E1 (Force _, e1), E1 (Force _, e2) when !optimize ->
        eq e1 e2
    (* Compare numerical constant (only if of the same type (TODO)): *)
    | E0 (Float v1), E0 (Float v2) when !optimize -> bool (v1 = v2)
    | E0 (String v1), E0 (String v2) when !optimize -> bool (v1 = v2)
    | E0 (Bool v1), E0 (Bool v2) when !optimize -> bool (v1 = v2)
    | E0 (Char v1), E0 (Char v2) when !optimize -> bool (v1 = v2)
    | E0 (U8 v1), E0 (U8 v2) when !optimize -> bool (Uint8.compare v1 v2 = 0)
    | E0 (U16 v1), E0 (U16 v2) when !optimize -> bool (Uint16.compare v1 v2 = 0)
    | E0 (U24 v1), E0 (U24 v2) when !optimize -> bool (Uint24.compare v1 v2 = 0)
    | E0 (U32 v1), E0 (U32 v2) when !optimize -> bool (Uint32.compare v1 v2 = 0)
    | E0 (U40 v1), E0 (U40 v2) when !optimize -> bool (Uint40.compare v1 v2 = 0)
    | E0 (U48 v1), E0 (U48 v2) when !optimize -> bool (Uint48.compare v1 v2 = 0)
    | E0 (U56 v1), E0 (U56 v2) when !optimize -> bool (Uint56.compare v1 v2 = 0)
    | E0 (U64 v1), E0 (U64 v2) when !optimize -> bool (Uint64.compare v1 v2 = 0)
    | E0 (U128 v1), E0 (U128 v2) when !optimize -> bool (Uint128.compare v1 v2 = 0)
    | E0 (I8 v1), E0 (I8 v2) when !optimize -> bool (Int8.compare v1 v2 = 0)
    | E0 (I16 v1), E0 (I16 v2) when !optimize -> bool (Int16.compare v1 v2 = 0)
    | E0 (I24 v1), E0 (I24 v2) when !optimize -> bool (Int24.compare v1 v2 = 0)
    | E0 (I32 v1), E0 (I32 v2) when !optimize -> bool (Int32.compare v1 v2 = 0)
    | E0 (I40 v1), E0 (I40 v2) when !optimize -> bool (Int40.compare v1 v2 = 0)
    | E0 (I48 v1), E0 (I48 v2) when !optimize -> bool (Int48.compare v1 v2 = 0)
    | E0 (I56 v1), E0 (I56 v2) when !optimize -> bool (Int56.compare v1 v2 = 0)
    | E0 (I64 v1), E0 (I64 v2) when !optimize -> bool (Int64.compare v1 v2 = 0)
    | E0 (I128 v1), E0 (I128 v2) when !optimize -> bool (Int128.compare v1 v2 = 0)
    | E0 (Bit v1), E0 (Bit v2) when !optimize -> bool (v1 = v2)
    | E0 (Size v1), E0 (Size v2) when !optimize -> bool (v1 = v2)
    | E0 (Byte v1), E0 (Byte v2) when !optimize -> bool (Uint8.compare v1 v2 = 0)
    | E0 (Word v1), E0 (Word v2) when !optimize -> bool (Uint16.compare v1 v2 = 0)
    | E0 (DWord v1), E0 (DWord v2) when !optimize -> bool (Uint32.compare v1 v2 = 0)
    | E0 (QWord v1), E0 (QWord v2) when !optimize -> bool (Uint64.compare v1 v2 = 0)
    | E0 (OWord v1), E0 (OWord v2) when !optimize -> bool (Uint128.compare v1 v2 = 0)
    | E0 (Bytes v1), E0 (Bytes v2) when !optimize -> bool (v1 = v2)
    | E0 (DataPtrOfString v1), E0 (DataPtrOfString v2) when !optimize -> bool (v1 = v2)
    | _ -> E2 (Eq, e1, e2)

  let not_ = function
    | E0 (Bool b) when !optimize -> bool (not b)
    | E1 (Not, e) when !optimize -> e
    | e -> E1 (Not, e)

  let abs e1 = E1 (Abs, e1)

  let ne e1 e2 = not_ (eq e1 e2)

  let param fid n = E0 (Param (fid, n))

  (* TODO: optimize constants *)
  let add e1 e2 = E2 (Add, e1, e2)

  let sub e1 e2 = E2 (Sub, e1, e2)

  let mul e1 e2 = E2 (Mul, e1, e2)

  let div e1 e2 = E2 (Div, e1, e2)

  let rem e1 e2 = E2 (Rem, e1, e2)

  let pow e1 e2 = E2 (Pow, e1, e2)

  let left_shift e1 e2 = E2 (LeftShift, e1, e2)

  let right_shift e1 e2 = E2 (RightShift, e1, e2)

  let log_and e1 e2 = E2 (LogAnd, e1, e2)

  let log_or e1 e2 = E2 (LogOr, e1, e2)

  let log_xor e1 e2 = E2 (LogXor, e1, e2)

  let and_ e1 e2 =
    match e1, e2 with
    | E0 (Bool true), _ when !optimize -> e2
    | _, E0 (Bool true) when !optimize -> e1
    | E0 (Bool false), _ when !optimize -> e1  (* False, [e2] not evaluated *)
    (* Cannot ignore [e1] even if e2 is demonstrably false because of its
     * possible side effects! *)
    | _ -> E2 (And, e1, e2)

  let or_ e1 e2 =
    match e1, e2 with
    | E0 (Bool false), _ when !optimize -> e2
    | _, E0 (Bool false) when !optimize -> e1
    | E0 (Bool true), _ when !optimize -> e1  (* True, [e2] not evaluated *)
    (* Cannot ignore [e1] event if e2 is demonstrably true because if its
     * possible side effects! *)
    | _ -> E2 (Or, e1, e2)

  let let_ = let_

  let identifier n = E0 (Identifier n)

  let ext_identifier n = E0 (ExtIdentifier n)

  let to_i8 = function
    | E0 (I8 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI8, e)
        | i when !optimize -> E0 (I8 (Int8.of_int i))
        | _ -> E1 (ToI8, e))

  let to_u8 = function
    | E0 (U8 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU8, e)
        | i when !optimize -> E0 (U8 (Uint8.of_int i))
        | _ -> E1 (ToU8, e))

  let to_i16 = function
    | E0 (I16 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI16, e)
        | i when !optimize -> E0 (I16 (Int16.of_int i))
        | _ -> E1 (ToI16, e))

  let to_u16 = function
    | E0 (U16 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU16, e)
        | i when !optimize -> E0 (U16 (Uint16.of_int i))
        | _ -> E1 (ToU16, e))

  let to_i24 = function
    | E0 (I24 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI24, e)
        | i when !optimize -> E0 (I24 (Int24.of_int i))
        | _ -> E1 (ToI24, e))

  let to_u24 = function
    | E0 (U24 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU24, e)
        | i when !optimize -> E0 (U24 (Uint24.of_int i))
        | _ -> E1 (ToU24, e))

  let to_i32 = function
    | E0 (I32 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI32, e)
        | i when !optimize -> E0 (I32 (Int32.of_int i))
        | _ -> E1 (ToI32, e))

  let to_u32 = function
    | E0 (U32 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU32, e)
        | i when !optimize -> E0 (U32 (Uint32.of_int i))
        | _ -> E1 (ToU32, e))

  let to_i40 = function
    | E0 (I40 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI40, e)
        | i when !optimize -> E0 (I40 (Int40.of_int i))
        | _ -> E1 (ToI40, e))

  let to_u40 = function
    | E0 (U40 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU40, e)
        | i when !optimize -> E0 (U40 (Uint40.of_int i))
        | _ -> E1 (ToU40, e))

  let to_i48 = function
    | E0 (I48 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI48, e)
        | i when !optimize -> E0 (I48 (Int48.of_int i))
        | _ -> E1 (ToI48, e))

  let to_u48 = function
    | E0 (U48 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU48, e)
        | i when !optimize -> E0 (U48 (Uint48.of_int i))
        | _ -> E1 (ToU48, e))

  let to_i56 = function
    | E0 (I56 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI56, e)
        | i when !optimize -> E0 (I56 (Int56.of_int i))
        | _ -> E1 (ToI56, e))

  let to_u56 = function
    | E0 (U56 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU56, e)
        | i when !optimize -> E0 (U56 (Uint56.of_int i))
        | _ -> E1 (ToU56, e))

  let to_i64 = function
    | E0 (I64 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI64, e)
        | i when !optimize -> E0 (I64 (Int64.of_int i))
        | _ -> E1 (ToI64, e))

  let to_u64 = function
    | E0 (U64 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU64, e)
        | i when !optimize -> E0 (U64 (Uint64.of_int i))
        | _ -> E1 (ToU64, e))

  let to_i128 = function
    | E0 (I128 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToI128, e)
        | i when !optimize -> E0 (I128 (Int128.of_int i))
        | _ -> E1 (ToI128, e))

  let to_u128 = function
    | E0 (U128 _) as e when !optimize -> e
    | e ->
        (match to_cst_int e with
        | exception _ -> E1 (ToU128, e)
        | i when !optimize -> E0 (U128 (Uint128.of_int i))
        | _ -> E1 (ToU128, e))

  let to_float = function
    | E0 (Float _) as e when !optimize -> e
    | e -> E1 (ToFloat, e)

  (* Avoid useless sequences: *)
  let seq = function
    (* `seq []` is already synonymous with void/nop *)
    | [ e ] when !optimize -> e
    | es -> E0S (Seq, es)

  let nop = seq []

  let apply f es =
    (* If [f] is constant we cannot proceed directly with variable
     * substitutions unless each variable is used only once. We can
     * turn the apply into a sequence of lets that will further
     * substitute what can be substituted: *)
    match f with
    | E1 (Function (fid, _typs), body) when !optimize ->
        if es = [] then body else
        List.fold_lefti (fun body i e ->
          let_ e (fun l e ->
            map l (fun _ -> function
              | E0 (Param (fid', i')) when fid' = fid && i' = i -> e
              | e -> e
            ) body)
        ) body es
    | _ ->
        E1S (Apply, f, es)

  let repeat ~from ~to_ ~body ~init =
    match from, to_ with
    | E0 (I32 f), E0 (I32 t) when !optimize ->
        let c = Int32.compare f t in
        if c > 0 then nop
        else if c = 0 then apply body [ from ; init ]
        else E4 (Repeat, from, to_, body, init)
    | _ ->
        E4 (Repeat, from, to_, body, init)

  let loop_until ~body ~cond ~init =
    match cond with
    | E0 (Bool false) when !optimize ->
        apply body [ init ]
    | _ ->
        E3 (LoopUntil, body, cond, init)

  let loop_while ~cond ~body ~init =
    match cond with
    | E0 (Bool false) when !optimize -> init
    | _ -> E3 (LoopWhile, cond, body, init)

  let fold ~init ~body ~list =
    match list with
    | E0S ((MakeVec | MakeLst _), [ e ]) when !optimize ->
        apply body [ e ; init ]
    | _ ->
        E3 (Fold, init, body, list)

  let string_of_bytes e1 = E1 (StringOfBytes, e1)

  let rem_size e1 = E1 (RemSize, e1)

  let neg e1 = E1 (Neg, e1)

  let exp e1 = E1 (Exp, e1)

  let log_ e1 = E1 (Log, e1)

  let log10_ e1 = E1 (Log10, e1)

  let sqrt_ e1 = E1 (Sqrt, e1)

  let ceil_ e1 = E1 (Ceil, e1)

  let floor_ e1 = E1 (Floor, e1)

  let round e1 = E1 (Round, e1)

  let cos_ e1 = E1 (Cos, e1)

  let sin_ e1 = E1 (Sin, e1)

  let tan_ e1 = E1 (Tan, e1)

  let acos_ e1 = E1 (ACos, e1)

  let asin_ e1 = E1 (ASin, e1)

  let atan_ e1 = E1 (ATan, e1)

  let cosh_ e1 = E1 (CosH, e1)

  let sinh_ e1 = E1 (SinH, e1)

  let tanh_ e1 = E1 (TanH, e1)

  let lower e1 = E1 (Lower, e1)

  let upper e1 = E1 (Upper, e1)

  let hash e1 = E1 (Hash, e1)

  let u16_of_word e1 = E1 (U16OfWord, e1)

  let u32_of_dword e1 = E1 (U32OfDWord, e1)

  let u64_of_qword e1 = E1 (U64OfQWord, e1)

  let u128_of_oword e1 = E1 (U128OfOWord, e1)

  let data_ptr_add e1 e2 = E2 (DataPtrAdd, e1, e2)

  let data_ptr_sub e1 e2 = E2 (DataPtrSub, e1, e2)

  let data_ptr_push e1 = E1 (DataPtrPush, e1)

  let data_ptr_pop e1 = E1 (DataPtrPop, e1)

  let data_ptr_of_string s = E0 (DataPtrOfString s)

  let data_ptr_of_buffer e1 = E1 (DataPtrOfBuffer, e1)

  let data_ptr_of_ptr e1 e2 e3 = E3 (DataPtrOfPtr, e1, e2, e3)

  let data_ptr_offset e1 = E1 (DataPtrOffset, e1)

  let data_ptr_remsize e1 = E1 (RemSize, e1)

  let string_length e1 = E1 (StringLength, e1)

  let cardinality = function
    | E0S ((MakeVec | MakeLst _), es) when !optimize -> u32_of_int (List.length es)
    | e1 -> E1 (Cardinality, e1)

  let blit_byte e1 e2 e3 =
    (* Do nothing if blitint nothing: *)
    match e3 with
    | E0 (Size 0) when !optimize -> e1 (* return unmodified pointer *)
    | _ -> E3 (BlitByte, e1, e2, e3)

  let set_bit e1 e2 e3 = E3 (SetBit, e1, e2, e3)

  let get_bit e1 e2 = E2 (GetBit, e1, e2)

  let force ?(what="") = function
    | E1 (NotNull, e1) when !optimize ->
        e1
    | E0 (Null _) as e when !optimize ->
        (* TODO: A special "fail of type" instruction translated into
         * "assert false" *)
        E1 (Force what, e)
    | e ->
        E1 (Force what, e)

  let find_substring e1 e2 e3 =
    match e2, e3 with
    | E0 (String s1), E0 (String s2) when !optimize ->
        (* Let if_ optimize away that condition if the bool is known: *)
        let then_ =
          try not_null (u24 (Uint24.of_int (String.find s2 s1)))
          with Not_found -> null T.(Mac U24)
        and else_ =
          try not_null (u24 (Uint24.of_int (String.rfind s2 s1)))
          with Not_found -> null T.(Mac U24) in
        if_ ~cond:e1 ~then_ ~else_
    | _ -> E3 (FindSubstring, e1, e2, e3)

  let get_item n e1 = E1 (GetItem n, e1)

  let get_field s e1 = E1 (GetField s, e1)

  let get_alt s e1 = E1 (GetAlt s, e1)

  let construct mns i e1 = E1 (Construct (mns, i), e1)

  let map_pair e1 e2 = E2 (MapPair, e1, e2)

  let min_ e1 e2 = E2 (Min, e1, e2)

  let max_ e1 e2 = E2 (Max, e1, e2)

  let member e1 e2 =
    match e2 with
    | E1 ((SlidingWindow _ | TumblingWindow _ | Sampling _ | HashTable _ |
           Heap), _) ->
        (* Those are created empty: *)
        bool false
    | e2 ->
        E2 (Member, e1, e2)

  let make_vec es = E0S (MakeVec, es)

  let make_lst mn es = E0S (MakeLst mn, es)

  let alloc_lst ~l ~len:e1 ~init:e2 =
    match type_of l e2 with
    | T.Value mn ->
        (match e1 with
        | E0 (U8 n) when !optimize && Uint8.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U16 n) when !optimize && Uint16.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U24 n) when !optimize && Uint24.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U32 n) when !optimize && Uint32.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U40 n) when !optimize && Uint40.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U48 n) when !optimize && Uint48.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U56 n) when !optimize && Uint56.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U64 n) when !optimize && Uint64.(compare zero n) = 0 ->
            make_lst mn []
        | E0 (U128 n) when !optimize && Uint128.(compare zero n) = 0 ->
            make_lst mn []
        | _ ->
            E2 (AllocLst, e1, e2))
    | _ ->
        (* Let the type checker deal with this: *)
        E2 (AllocLst, e1, e2)

  let partial_sort vs ks =
    match vs, ks with
    | _, E0S (MakeVec, [])
    | _, E0S (MakeLst _, [])
    | E0S (MakeVec, []), _
    | E0S (MakeLst _, []), _ ->
        vs
    | _ ->
        E2 (PartialSort, vs, ks)

  let set_vec e1 e2 e3 =
    match to_cst_int e1 with
    | exception _ ->
        E3 (SetVec, e1, e2, e3)
    | i ->
        (match e2 with
        | E0S (MakeVec, es) when !optimize ->
            List.mapi (fun j e -> if i = j then e3 else e) es |>
            make_vec
        | _ ->
            E3 (SetVec, e1, e2, e3))

  let get_vec e1 e2 =
    match to_cst_int e1 with
    | exception _ ->
        E2 (GetVec, e1, e2)
    | i ->
        (match e2 with
        | E0S (MakeVec, es) when !optimize ->
            List.at es i
        | _ -> E2 (GetVec, e1, e2))

  let map_ lst f =
    match lst with
    | E0S (MakeVec, [ e ]) when !optimize ->
        (* Unearth the MakeVec might makes further optimisations possible: *)
        make_vec [ apply f [ e ] ]
    | _ ->
        if !optimize && is_identity f then lst
        else E2 (Map, lst, f)

  let list_of_slist e1 = E1 (ListOfSList, e1)

  let list_of_slist_rev e1 = E1 (ListOfSListRev, e1)

  let set_of_slist e1 = E1 (SetOfSList, e1)

  let list_of_vec e1 = E1 (ListOfVec, e1)

  let list_of_set e1 = E1 (ListOfSet, e1)

  let split_by e1 e2 =
    match e1, e2 with
    | E0 (String s1), E0 (String s2) when !optimize ->
        String.split_on_string s1 s2 |>
        List.map string |>
        make_lst T.(required (Mac String))
    | _ ->
        E2 (SplitBy, e1, e2)

  (* It might be easier for users to accept also 0 or 1 expressions and turn
   * them into what's expected: *)
  let make_tup = function
    | [] -> unit
    | [ x ] -> x
    | es -> E0S (MakeTup, es)

  let make_rec = function
    | [] -> unit
    | es ->
        (* Flatten the list to comply with E0S structure: *)
        let es =
          List.fold_left (fun lst (n, v) -> (string n) :: v :: lst) [] es in
        E0S (MakeRec, es)

  let split_at e1 e2 =
    match to_cst_int e1 with
    | exception _ ->
        E2 (SplitAt, e1, e2)
    | i ->
        let res s =
          make_tup [ string (String.sub s 0 i) ;
                     string (String.sub s i (String.length s - i)) ] in
        (match e2 with
        | E0 (String s) when !optimize -> res s
        | _ -> E2 (SplitAt, e1, e2))

  let append_byte e1 e2 = E2 (AppendByte, e1, e2)

  let append_bytes e1 e2 = E2 (AppendBytes, e1, e2)

  let append_string e1 e2 = E2 (AppendString, e1, e2)

  let starts_with e1 e2 = E2 (StartsWith, e1, e2)

  let ends_with e1 e2 = E2 (EndsWith, e1, e2)

  let size_of_dword = size_of_u32 % u32_of_dword

  let bool_of_byte = bool_of_u8 % u8_of_byte

  let byte_of_bool = byte_of_u8 % u8_of_bool

  let char_of_byte = char_of_u8 % u8_of_byte

  let byte_of_char = byte_of_u8 % u8_of_char

  let assert_ = function
    | E0 (Bool true) when !optimize -> nop
    | e -> E1 (Assert, e)

  let mask_get i m = E1 (MaskGet i, m)

  let label_of e = E1 (LabelOf, e)

  let copy_field = E0 CopyField

  let skip_field = E0 SkipField

  let set_field_null = E0 SetFieldNull

  let getenv e = E1 (GetEnv, e)

  let string_of_char_ = function
    | E0 (Char c) when !optimize -> string (String.of_char c)
    | e -> E1 (StringOfChar, e)

  (* Helpers for ref-cells (implemented with 1 dimensional vectors): *)
  let ref_ e = make_vec [ e ]

  let get_ref e = get_vec (u8 Uint8.zero) e

  let set_ref e x = set_vec (u8 Uint8.zero) e x

  let chop_begin lst n =
    let def = E2 (ChopBegin, lst, n) in
    match lst with
    | E0S (MakeLst _, []) ->
        lst (* Cannot be truncated further *)
    | E0S (MakeLst mn, items) ->
        (match to_cst_int n with
        | exception _ -> def
        | n -> E0S (MakeLst mn, List.drop n items))
    | _ ->
        (match to_cst_int n with
        | exception _ -> def
        | 0 -> lst
        | _ -> def)

  let chop_end lst n =
    let def = E2 (ChopEnd, lst, n) in
    match lst with
    | E0S (MakeLst _, []) ->
        lst (* Cannot be truncated further *)
    | E0S (MakeLst mn, items) ->
        (match to_cst_int n with
        | exception _ -> def
        | n ->
            let l = List.length items in
            if n >= l then
              E0S (MakeLst mn, [])
            else
              E0S (MakeLst mn, List.take (l - n) items))
    | _ ->
        (match to_cst_int n with
        | exception _ -> def
        | 0 -> lst
        | _ -> def)
end
