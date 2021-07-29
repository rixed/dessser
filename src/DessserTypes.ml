open Batteries
open Stdint

open DessserMiscTypes
open DessserTools

let pp = Printf.fprintf

(* "Value" types are all the types describing values that can be (de)serialized.
 * All of them can possibly be nullable. *)

type typ =
  | TUnknown
  (* To christen the following type. Notice that name is valid both during the
   * definition of that child type but also after. Once a name is met it sticks
   * with the type until the environment is "reset", ie at the end of the parse
   * usually. *)
  | TNamed of string * typ
  (* Refers to a type named during its definition. *)
  | TThis of string
  (* Base types: *)
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128
  (* Aliases with custom representations: *)
  | TUsr of user_type
  (* External types are known only by name, and are converted to some verbatim
   * text provided by the user when code is printed. *)
  | TExt of string
  (* Compound types: *)
  | TVec of int * mn
  | TArr of mn
  (* Special compound type amenable to incremental computation over sets.
   * There are different implementations with slightly different APIs,
   * depending on the use case (ie. the operator it is an operand of), for
   * instance to optimise FIFO updates, to keep it sorted, to skip nulls, etc.
   * But the backend will decide this on its own: *)
  | TSet of set_type * mn
  | TTup of mn array
  (* Exact same as a tuple, but with field names that can be used as
   * accessors (also used to name actual fields in generated code): *)
  | TRec of (string * mn) array
  (* Sum types, as a list of constructor and type. Constructor names uniqueness
   * will be checked at construction. *)
  | TSum of (string * mn) array
  (* The type for maps exist because there will be some operations using
   * that type indirectly (such as fetching from a DB by key, or describing
   * a key->value mapping in a type expression). But there is no value of
   * that type, ever. From a (de)serialized point of view, maps are
   * equivalent to association lists. *)
  | TMap of mn * mn
  (* The above types are used to hold data that can be serialized. The types
   * below are meant to help implement serializers themselves, and are not
   * serializable: *)
  (* Used for functions without return values: *)
  | TVoid
  (* Ptr are used to point into a stream of bytes to serialized into /
   * deserialized from. *)
  | TPtr
  (* A size in byte: *)
  | TSize
  (* An arbitrary address, used for DataPtrOfAddress: *)
  | TAddress
  | TBytes
  (* Types for the runtime representation of a field mask: *)
  | TMask  (* What to do with a tree of fields *)
  (* We'd like the DES/SERializer to be able to use complex types as their
   * "pointer", part of those types being actual pointers. Therefore, we cannot
   * use the value-types, as we cannot embed a pointer in there. So here are
   * defined two specific compound types: a pair and an homogeneous list
   * (sufficient in practice and better than untyped car/cdr!) *)
  (* A Data Arr is just a vector which length is unknown at compile time,
   * whereas an Lst can actually be constructed/destructed element by element.
   * "Lst" is for "singly-chained" list, and is written using "[[]]" instead
   * of "[]". *)
  | TLst of mn
  | TFunction of (* arguments: *) mn array * (* result: *) mn

(* User types are specialized types that can be build from basic or external
 * types and given their own name, pretty-printer and parser.
 * User expressions can restrict types to user types. Other than that,
 * every operation that applies to the implementation of a user type also
 * applies to the user type, and the other way around.
 * Unlike named types, user_types can be created only programmatically.
 * Unlike external types their implementation need not be supplied
 * externally though. *)

and user_type =
  { name : string ;
    def : typ }

(* "mn" for "maybe nullable": *)
and mn =
  { (* The underlying type: *)
    typ : typ ;
    (* Whether that value can be NULL: *)
    nullable : bool ;
    (* Default value: *)
    default : expr option }

and e0 =
  | Param of int (* parameter number *)
  (* Special identifier referencing the currently executing function.
   * Allows to encode recursive calls even though the name of the enclosing
   * function is unknown. The specified type is the output type (the input
   * type of the function can be retrieved from the environment). *)
  | Myself of mn
  (* Identifier are set with `Let` expressions, or obtained from the code
   * generators in exchange for an expression: *)
  | Identifier of string
  (* Contrary to identifiers which name can be arbitrary, an external identifier
   * name is used verbatim by the backend and must therefore correspond to a
   * valid object. *)
  | ExtIdentifier of ext_identifier
  | Now
  | RandomFloat
  | RandomU8
  | RandomU32
  | RandomU64
  | RandomU128
  (* Immediate values: *)
  | Null of typ
  | EndOfList of mn (* mn being the type of list items *)
  | EmptySet of mn (* just an unsophisticated set *)
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
  | Size of int
  | Address of Uint64.t
  | Bytes of Bytes.t
  (* Constant mask actions: *)
  | CopyField
  | SkipField
  | SetFieldNull

and e0s =
  | Seq
  (* Data constructors: *)
  | MakeTup
  (* For convenience, MakeRec is handled like an E0S but it is constrained to
   * have an even number of arguments, the field names being forced to be
   * constant strings *)
  | MakeRec
  (* Construct a value of some user type: *)
  | MakeUsr of string
  (* The Dessser equivalent of the `asm` directive.
   * The templates may use %1, %2 etc where the arguments should go. *)
  | Verbatim of ((backend_id * string) list * (* output type: *) mn)

and e0r =
  | MakeVec
  | MakeArr of mn

and e1 =
  | Function of (*args*) mn array
  | Comment of string
  | GetItem of int (* for tuples *)
  | GetField of string (* For records *)
  | GetAlt of string (* Destruct a sum type (See LabelOf) *)
  | Construct of (string * mn) array (* type of the resulting sum *)
               * int (* Which alternative is constructed *)
  | Dump
  | Identity  (* Useful as a default function *)
  | Ignore
  (* Tells if a nullable value is NULL. Returns false on non nullable
   * values. *)
  | IsNull
  (* Turn e into a nullable, if it's not already: *)
  | NotNull
  (* Turn e into a not-nullable if it's not already.
   * Fails on NULL values with the given message: *)
  | Force of string
  (* Convert from/to string for all base value types: *)
  | StringOfFloat
  (* Only for the hopeless: *)
  | DecimalStringOfFloat
  | StringOfChar
  | StringOfInt
  | StringOfIp
  | FloatOfString
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
  | BitNot
  | FloatOfU64
  | U64OfFloat
  | U8OfChar
  | CharOfU8
  | SizeOfU32
  | U32OfSize
  | AddressOfU64
  | U64OfAddress
  | ArrOfLst
  | ArrOfLstRev
  | SetOfLst
  | ArrOfVec
  | ArrOfSet
  (* à la C: *)
  | U8OfBool
  | BoolOfU8
  | StringLength
  | BytesLength
  | StringOfBytes
  | BytesOfString
  | Cardinality (* of lists, vectors or sets *)
  | ReadU8
  | RemSize
  | Offset
  | Not
  | Abs
  | Neg
  | Exp
  | Log
  | UnsafeLog
  | Log10
  | UnsafeLog10
  | Sqrt
  | UnsafeSqrt
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
  (* FIXME: make Head and Tail return nullables: *)
  | Head
  | Tail
  | ReadU16 of endianness
  | ReadU32 of endianness
  | ReadU64 of endianness
  | ReadU128 of endianness
  | Assert
  (* For tuples, int is the index of the item in the tuple.
   * For records, the index of the field in definition order: *)
  | MaskGet of int
  (* Given a value of a sum type, return the integer label associated with its
   * constructor, as an u16: *)
  | LabelOf
  (* Various set implementations, configured with their max size: *)
  | SlidingWindow of mn (* Sliding window of the last N added items *)
  | TumblingWindow of mn (* Tumbling window *)
  | Sampling of mn (* Reservoir sampling of N items *)
  (* A set with an O(1) implementation of Member, N is the initial size: *)
  | HashTable of mn
  (* A set that order items according to a given comparison function
   * (given as first and only argument, this function also provides the
   * set elements' type): *)
  | Heap
  | PtrOfString (* Use a string as a pointer *)
  | PtrOfBuffer (* Use an uninitialized buffer as a pointer *)
  | GetEnv
  (* Get the minimal value of a set (heap): *)
  | GetMin
  | AllocVec of int (* parameter is the initial value *)
  (* Convert the passed value into that type (using StdLib.conv_mn).
   * It is actually substituted by actual operations during type checking.
   * Users are thus exempt from knowing the actual type of the value
   * beforehand. *)
  | Convert of mn

and e1s =
  | Apply

and memo_mn = mn option ref

and e2 =
  (* Notes:
   * - It is forbidden to shadow a previously defined identifier, to make
   *   optimisation simpler (ie. it can be assumed that all instance of
   *   `(idetifier name)` in the let body refers to that definition.
   * - The type is cached here for performance reason *)
  | Let of string * memo_mn
  | LetPair of string * memo_mn * string * memo_mn
  (* Deconstructor for vectors/arrs/lists/sets/strings/bytes: *)
  | Nth
  | UnsafeNth
  (* Comparators: *)
  | Gt
  | Ge
  | Eq
  (* Arithmetic operators returning same type as their inputs, which must
   * be of the same type (namely, any numeric). *)
  | Add
  | Sub
  | Mul
  | Div (* Fails with Null *)
  | UnsafeDiv (* Not nullable but fails for real *)
  | Rem (* Fails with Null *)
  | UnsafeRem (* Not nullable but fails for real *)
  | Pow (* Fails with Null *)
  | UnsafePow (* Not nullable but fails for real *)
  | BitAnd
  | BitOr
  | BitXor
  | LeftShift
  | RightShift
  | AppendByte
  | AppendBytes
  | AppendString
  | StartsWith
  | EndsWith
  | GetBit
  | ReadBytes
  | PeekU8
  | WriteU8
  | WriteBytes
  | PokeU8
  | PtrAdd
  | PtrSub
  (* Unlike PtrSub that subtract two pointers, rewind subtract a size from a
   * pointer: *)
  | Rewind
  | And
  | Or
  | Cons
  | Min
  | Max
  (* Membership test for vectors, lists and sets; Not for CIDRs nor strings.
   * Args are: item, container *)
  | Member
  | PeekU16 of endianness
  | PeekU32 of endianness
  | PeekU64 of endianness
  | PeekU128 of endianness
  | WriteU16 of endianness
  | WriteU32 of endianness
  | WriteU64 of endianness
  | WriteU128 of endianness
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
  | AllocArr
  (* Sort (inplace) the 1st parameter (vector or array) until the indices given
   * in the 2sn parameter have reached their final location. Other part of the
   * array might not be sorted. *)
  | PartialSort
  (* Remove the first items from an array (args are the array and the length to
   * remove): *)
  | ChopBegin
  (* Truncate an array at the end (args are the array and the length to
   * remove): *)
  | ChopEnd
  (* Scale the weight of a weighted set (ie. top) *)
  | ScaleWeights
  (* Arguments are format string and time (in seconds from UNIX epoch): *)
  | Strftime
  | PtrOfAddress (* Points to a given address in memory *)
  | While (* Condition (bool) * body *)
  | ForEach of (string * memo_mn) (* list/vector/set/arr/string/bytes * body *)
  (* Apply e2 to e1, skipping nulls if e2 is nullable.
   * Like Convert, replaced at typing. *)
  | NullMap of (string * memo_mn) (* value * body *)
  | Index (* of a char in a string, or null *)

and e3 =
  | SetBit
  (* Similarly to Nth: first the index, then the vector or array, then
   * the value *)
  | SetVec  (* TODO: Rename? *)
  | BlitByte
  | If (* Condition * Consequent * Alternative *)
  | Map (* args are: init, (init -> item -> item'), item list/lst/vec *)
  (* Get a slice from a pointer, starting at given offset and shortened to
   * given length: *)
  | PtrOfPtr
  (* bool to indicate the search direction (true = from start), then the needle
   * and finally the haystack. *)
  | FindSubstring
  (* Parameters are: size, max_size and sigmas.
   * Tops can make use of the insert_weighted to specify the weight to be used
   * for each item, and the downscale operators to decay old entries (first
   * inflate the weight in time, and periodically downscale the whole set) *)
  | Top of mn
  (* Insert with an explicit weight. Args are: the set, the weight and the
   * value. *)
  | InsertWeighted
  (* Extract a substring. Arguments are the string, the start and stop positions
   * (counted from the end of the string if negative). Returns an empty string
   * if nothing the selected part is outside the string bounds. *)
  | SubString

and expr =
  | E0 of e0
  | E0S of e0s * expr list
  (* Those are mutable: *)
  | E0R of e0r * expr array
  | E1 of e1 * expr
  | E1S of e1s * expr * expr list
  | E2 of e2 * expr * expr
  | E3 of e3 * expr * expr * expr

(*
 * User-defined types
 *)

let user_types : (string, user_type) Hashtbl.t = Hashtbl.create 50

type gen_printer = { f : 'a. 'a IO.output -> unit }

let print_user_type oc ut =
  String.print oc ut.name

let get_user_type n =
  TUsr (Hashtbl.find user_types n)

(* See below for [register_user_type] and examples *)

(*
 * Printers
 *)

let rec string_of_e0 = function
  | Param n -> "param "^ string_of_int n
  | Myself mn -> "myself "^ String.quote (mn_to_string mn)
  | Null t -> "null "^ String.quote (to_string t)
  | EndOfList mn -> "end-of-list "^ String.quote (mn_to_string mn)
  | EmptySet mn -> "empty-set "^ String.quote (mn_to_string mn)
  | Now -> "now"
  | RandomFloat -> "random-float"
  | RandomU8 -> "random-u8"
  | RandomU32 -> "random-u32"
  | RandomU64 -> "random-u64"
  | RandomU128 -> "random-u128"

  | Float f -> "float "^ DessserFloatTools.hexstring_of_float f
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
  | Size n -> "size "^ string_of_int n
  | Address n -> "address "^ Uint64.to_string n
  | Bytes s -> "bytes "^ String.quote (Bytes.to_string s)
  | Identifier s -> "identifier "^ String.quote s
  | ExtIdentifier (Verbatim s) -> "ext-identifier "^ String.quote s
  | ExtIdentifier (Method { typ ; meth }) ->
      "ext-identifier "^ typ ^" "^ string_of_type_method meth
  | CopyField -> "copy-field"
  | SkipField -> "skip-field"
  | SetFieldNull -> "set-field-null"

and string_of_e0s = function
  | Seq -> "seq"
  | MakeTup -> "make-tup"
  | MakeRec -> "make-rec"
  | MakeUsr n -> "make-usr "^ String.quote n
  (* Of course the actual definition cannot be expressed in DIL: *)
  | Verbatim (temps, mn) ->
      Printf.sprintf2 "verbatim %a %S"
        (List.print ~first:"(" ~sep:" " ~last:")" (fun oc (id, temp) ->
          Printf.fprintf oc "(%s %S)" (string_of_backend id) temp)) temps
        (mn_to_string mn)

and string_of_e0r = function
  | MakeVec -> "make-vec"
  | MakeArr mn -> "make-arr "^ String.quote (mn_to_string mn)

and string_of_e1s = function
  | Apply -> "apply"

and string_of_e1 = function
  | Function typs ->
      "fun "^
      (if typs = [||] then "" else
       Printf.sprintf2 "%a" (Array.print ~first:"(" ~sep:" " ~last:")" (fun oc t ->
         Printf.fprintf oc "%S" (mn_to_string t))) typs)
  | Comment s -> "comment "^ String.quote s
  | GetItem n -> "get-item "^ string_of_int n
  | GetField s -> "get-field "^ String.quote s
  | GetAlt s -> "get-alt "^ String.quote s
  | Construct (mns, i) ->
      "construct "^ String.quote (to_string (TSum mns))
                  ^" "^ string_of_int i
  | Dump -> "dump"
  | Identity -> "identity"
  | Ignore -> "ignore"
  | IsNull -> "is-null"
  | NotNull -> "not-null"
  | Force "" -> "force"
  | Force what -> "force "^ String.quote what
  | StringOfFloat -> "string-of-float"
  | DecimalStringOfFloat -> "decimal-string-of-float"
  | StringOfChar -> "string-of-char"
  | StringOfInt -> "string-of-int"
  | StringOfIp -> "string-of-ip"
  | FloatOfString -> "float-of-string"
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
  | BitNot -> "bit-not"
  | FloatOfU64 -> "float-of-u64"
  | U64OfFloat -> "u64-of-float"
  | U8OfChar -> "u8-of-char"
  | CharOfU8 -> "char-of-u8"
  | SizeOfU32 -> "size-of-u32"
  | U32OfSize -> "u32-of-size"
  | AddressOfU64 -> "address-of-u64"
  | U64OfAddress -> "u64-of-address"
  | ArrOfLst -> "arr-of-lst"
  | ArrOfLstRev -> "arr-of-lst-rev"
  | SetOfLst -> "set-of-lst"
  | ArrOfVec -> "arr-of-vec"
  | ArrOfSet -> "arr-of-set"
  | U8OfBool -> "u8-of-bool"
  | BoolOfU8 -> "bool-of-u8"
  | StringLength -> "string-length"
  | BytesLength -> "bytes-length"
  | StringOfBytes -> "string-of-bytes"
  | BytesOfString -> "bytes-of-string"
  | Cardinality -> "cardinality"
  | ReadU8 -> "read-u8"
  | RemSize -> "rem-size"
  | Offset -> "offset"
  | Not -> "not"
  | Abs -> "abs"
  | Neg -> "neg"
  | Exp -> "exp"
  | Log -> "log"
  | UnsafeLog -> "unsafe-log"
  | Log10 -> "log10"
  | UnsafeLog10 -> "unsafe-log10"
  | Sqrt -> "sqrt"
  | UnsafeSqrt -> "unsafe-sqrt"
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
  | Head -> "head"
  | Tail -> "tail"
  | ReadU16 en -> "read-u16 "^ string_of_endianness en
  | ReadU32 en -> "read-u32 "^ string_of_endianness en
  | ReadU64 en -> "read-u64 "^ string_of_endianness en
  | ReadU128 en -> "read-u128 "^ string_of_endianness en
  | Assert -> "assert"
  | MaskGet d -> "mask-get "^ string_of_int d
  | LabelOf -> "label-of"
  | SlidingWindow mn ->
      "sliding-window "^ String.quote (mn_to_string mn)
  | TumblingWindow mn ->
      "tumbling-window "^ String.quote (mn_to_string mn)
  | Sampling mn ->
      "sampling "^ String.quote (mn_to_string mn)
  | HashTable mn ->
      "hash-table "^ String.quote (mn_to_string mn)
  | Heap ->
      "heap"
  | PtrOfString -> "ptr-of-string"
  | PtrOfBuffer -> "ptr-of-buffer"
  | GetEnv -> "getenv"
  | GetMin -> "get-min"
  | AllocVec d -> "alloc-vec "^ string_of_int d
  | Convert mn -> "convert "^ String.quote (mn_to_string mn)

and string_of_e2 = function
  | Let (n, _) ->
      "let "^ String.quote n
  | LetPair (n1, _, n2, _) ->
      "let-pair "^ String.quote n1 ^" "^ String.quote n2
  | Nth -> "nth"
  | UnsafeNth -> "unsafe-nth"
  | Gt -> "gt"
  | Ge -> "ge"
  | Eq -> "eq"
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | UnsafeDiv -> "unsafe-div"
  | Rem -> "rem"
  | UnsafeRem -> "unsafe-rem"
  | Pow -> "pow"
  | UnsafePow -> "unsafe-pow"
  | BitAnd -> "bit-and"
  | BitOr -> "bit-or"
  | BitXor -> "bit-xor"
  | LeftShift -> "left-shift"
  | RightShift -> "right-shift"
  | AppendByte -> "append-byte"
  | AppendBytes -> "append-bytes"
  | AppendString -> "append-string"
  | StartsWith -> "starts-with"
  | EndsWith -> "ends-with"
  | GetBit -> "get-bit"
  | ReadBytes -> "read-bytes"
  | PeekU8 -> "peek-u8"
  | WriteU8 -> "write-u8"
  | WriteBytes -> "write-bytes"
  | PokeU8 -> "poke-u8"
  | PtrAdd -> "ptr-add"
  | PtrSub -> "ptr-sub"
  | Rewind -> "rewind"
  | And -> "and"
  | Or -> "or"
  | Cons -> "cons"
  | Min -> "min"
  | Max -> "max"
  | Member -> "mem"
  | PeekU16 en -> "peek-u16 "^ string_of_endianness en
  | PeekU32 en -> "peek-u32 "^ string_of_endianness en
  | PeekU64 en -> "peek-u64 "^ string_of_endianness en
  | PeekU128 en -> "peek-u128 "^ string_of_endianness en
  | WriteU16 en -> "write-u16 "^ string_of_endianness en
  | WriteU32 en -> "write-u32 "^ string_of_endianness en
  | WriteU64 en -> "write-u64 "^ string_of_endianness en
  | WriteU128 en -> "write-u128 "^ string_of_endianness en
  | Insert -> "insert"
  | DelMin -> "del-min"
  | SplitBy -> "split-on"
  | SplitAt -> "split-at"
  | Join -> "join"
  | AllocArr -> "alloc-arr"
  | PartialSort -> "partial-sort"
  | ChopBegin -> "chop-begin"
  | ChopEnd -> "chop-end"
  | ScaleWeights -> "scale-weights"
  | Strftime -> "strftime"
  | PtrOfAddress -> "ptr-of-address"
  | While -> "while"
  | ForEach (n, _) -> "for-each "^ String.quote n
  | NullMap (n, _) -> "null-map "^ String.quote n
  | Index -> "index"

and string_of_e3 = function
  | SetBit -> "set-bit"
  | SetVec -> "set-vec"
  | BlitByte -> "blit-byte"
  | If -> "if"
  | Map -> "map"
  | PtrOfPtr -> "ptr-of-ptr"
  | FindSubstring -> "find-substring"
  | Top mn -> "top "^ String.quote (mn_to_string mn)
  | InsertWeighted -> "insert-weighted"
  | SubString -> "substring"

(* In many occasions we want the items of a record to be deterministically
 * ordered so they can be compared etc: *)
and sorted_rec fields =
  let cmp_nv (n1, _) (n2, _) =
    String.compare n1 n2 in
  let fields = Array.copy fields in
  Array.sort cmp_nv fields ;
  fields

and print ?(sorted=false) ?with_defaults oc =
  let print_mn = print_mn ~sorted ?with_defaults in
  let sp = String.print oc in
  function
  | TUnknown ->
      sp "UNKNOWN"
  | TNamed (n, t) ->
      pp oc "%s AS " n ;
      print ~sorted oc t
  | TThis "" ->
      sp "THIS"
  | TThis n ->
      sp n
  | TFloat ->
      sp "FLOAT"
  | TString ->
      sp "STRING"
  | TBool ->
      sp "BOOL"
  | TChar ->
      sp "CHAR"
  | TU8 ->
      sp "U8"
  | TU16 ->
      sp "U16"
  | TU24 ->
      sp "U24"
  | TU32 ->
      sp "U32"
  | TU40 ->
      sp "U40"
  | TU48 ->
      sp "U48"
  | TU56 ->
      sp "U56"
  | TU64 ->
      sp "U64"
  | TU128 ->
      sp "U128"
  | TI8 ->
      sp "I8"
  | TI16 ->
      sp "I16"
  | TI24 ->
      sp "I24"
  | TI32 ->
      sp "I32"
  | TI40 ->
      sp "I40"
  | TI48 ->
      sp "I48"
  | TI56 ->
      sp "I56"
  | TI64 ->
      sp "I64"
  | TI128 ->
      sp "I128"
  (* To having having to accept any valid identifiers as an external type when
   * parsing a type, we denote external types with a dollar sign, evicative of
   * some reference/placeholder. *)
  | TExt n ->
      pp oc "$%s" n
  | TUsr t ->
      print_user_type oc t
  | TVec (dim, mn) ->
      pp oc "%a[%d]" print_mn mn dim
  | TArr mn ->
      pp oc "%a[]" print_mn mn
  | TSet (st, mn) ->
      pp oc "%a{%s}" print_mn mn (string_of_set_type st)
  | TTup mns ->
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:"; "
          print_mn) mns
  | TRec mns ->
      (* When the string repr is used to identify the type (see BackEndCLike)
       * every equivalent record types must then be printed the same, thus the
       * optional sort: *)
      pp oc "%a"
        (Array.print ~first:"{" ~last:"}" ~sep:"; "
          (fun oc (n, mn) ->
            pp oc "%s: %a" n print_mn mn)
        ) (if sorted then sorted_rec mns else mns)
  | TSum cs ->
      (* Parenthesis are required to distinguish external from internal
       * nullable: *)
      pp oc "%a"
        (Array.print ~first:"[" ~last:"]" ~sep:" | "
          (fun oc (n, mn) ->
            pp oc "%s %a" n print_mn mn)
        ) (if sorted then sorted_rec cs else cs)
  | TMap (k, v) ->
      pp oc "%a[%a]"
        print_mn v
        print_mn k
  | TVoid ->
      sp "Void"
  | TPtr ->
      sp "Ptr"
  | TSize ->
      sp "Size"
  | TAddress ->
      sp "Address"
  | TBytes ->
      sp "Bytes"
  | TMask ->
      sp "Mask"
  | TLst mn ->
      pp oc "%a[[]]" print_mn mn
  | TFunction ([||], mn) ->
      pp oc "( -> %a)" print_mn mn
  | TFunction (mns, mn2) ->
      pp oc "(%a -> %a)"
        (Array.print ~first:"" ~last:"" ~sep:" -> " print_mn) mns
        print_mn mn2

and print_mn ?sorted ?(with_defaults=false) oc mn =
  pp oc "%a%s"
    (print ?sorted ~with_defaults) mn.typ
    (if mn.nullable then "?" else "") ;
  if with_defaults then
    match mn.default with
    | None -> ()
    | Some def ->
        Printf.fprintf oc " default %a"
          (print_expr ?max_depth:None) def

and print_expr ?max_depth oc e =
  if Option.map_default (fun m -> m <= 0) false max_depth then
    pp oc "…"
  else
    let max_depth = Option.map pred max_depth in
    let p = print_expr ?max_depth in
    match e with
    | E0 op ->
        pp oc "(%s)" (string_of_e0 op)
    | E0S (Seq, []) ->
        pp oc "nop"
    | E0S (op, es) ->
        pp oc "(%s%s%a)"
          (string_of_e0s op)
          (if es = [] then "" else " ")
          (List.print ~first:"" ~last:"" ~sep:" " p) es
    | E0R (op, es) ->
        pp oc "(%s%s%a)"
          (string_of_e0r op)
          (if es = [||] then "" else " ")
          (Array.print ~first:"" ~last:"" ~sep:" " p) es
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

and mn_to_string mn =
  Printf.sprintf2 "%a" (print_mn ~sorted:false ~with_defaults:false) mn

and to_string t =
  Printf.sprintf2 "%a" (print ~sorted:false ~with_defaults:false) t

let print_sorted oc = print ~sorted:true oc
let print oc = print ~sorted:false ~with_defaults:true oc
let print_mn_sorted oc = print_mn ~sorted:true ~with_defaults:false oc
let print_mn oc = print_mn ~sorted:false ~with_defaults:true oc

(*
 * Iterators
 *)

(* Need no environment since does not use This: *)
let rec develop = function
  | TUsr { def ; _ } ->
      develop def
  | TVec (d, mn) ->
      TVec (d, develop_mn mn)
  | TArr mn ->
      TArr (develop_mn mn)
  | TSet (st, mn) ->
      TSet (st, develop_mn mn)
  | TTup mns ->
      TTup (Array.map develop_mn mns)
  | TRec mns ->
      TRec (Array.map (fun (n, mn) -> n, develop_mn mn) mns)
  | TSum cs ->
      TSum (Array.map (fun (n, mn) -> n, develop_mn mn) cs)
  | TMap (mn1, mn2) ->
      TMap (develop_mn mn1, develop_mn mn2)
  | TLst mn ->
      TLst (develop_mn mn)
  | t -> t

and develop_mn mn =
  { mn with typ = develop mn.typ }

(* This develop user types at first level (ie. excluding sub-branches but
 * including when a user type is implemented with another): *)
let rec develop1 = function
  | { typ = TUsr { def ; _ } ; nullable ; default } ->
      develop1 ({ typ = def ; nullable ; default })
  | t ->
      t

(* Top-down folding of a type: *)
(* FIXME: either consider Usr types as opaque and stop the recursion, or as
 * transparent and do not call [f] on Usr: *)
let rec fold u f t =
  let u = f u t in
  match t with
  | TNamed (_, t) ->
      fold u f t
  | TUsr { def ; _ } ->
      fold u f def
  | TVec (_, mn) | TArr mn | TSet (_, mn) | TLst mn ->
      fold_mn u f mn
  | TTup mns ->
      Array.fold_left (fun u mn -> fold_mn u f mn) u mns
  | TRec mns | TSum mns ->
      Array.fold_left (fun u (_, mn) -> fold_mn u f mn) u mns
  | TMap (mn1, mn2) ->
      fold_mn (fold_mn u f mn1) f mn2
  | _ ->
      u

and fold_mn u f mn =
  fold u f mn.typ

let iter f t =
  fold () (fun () t -> f t) t

let iter_mn f mn =
  fold_mn () (fun () mn -> f mn) mn

(* While a type is being parsed, local type names can be assigned to
 * part of the constructed type. Those names will be favored by back-ends
 * when generating the code, and it is also possible to refer back to another
 * type by its name, possibly recursively.
 * We rely on a global index of type names to definitions (shrinked as much as
 * possible, possibly more than in the source definition).
 * The special empty name "" refers back to the global definition (when there
 * is only one). *)

exception Unbound_type of string
exception Redefined_type of string

let these = ref []

let find_this n =
  try
    List.assoc n !these
  with Not_found ->
    raise (Unbound_type n)

let () =
  Printexc.register_printer (function
    | Unbound_type n ->
        Some (
          Printf.sprintf2 "Unknown type %S. Only known types are: %a"
            n
            (pretty_list_print (fun oc (n, _) -> String.print oc n)) !these)
    | Redefined_type n ->
        Some (
          Printf.sprintf "Type %S can be defined only once" n)
    | _ ->
        None)

(* We need types featuring `this`, that can come with various degrees of
 * unfolding, to all look equal.
 * We cannot "expand" This, because that would just make more This appear.
 * But there exist a form where the type is folded as much as possible: *)
let rec shrink t =
  let find_def t =
    List.find (fun (_, def) -> eq t def) !these in
  let rec do_mn mn =
    { mn with typ = do_typ mn.typ }
  and do_typ t =
    let t' =
      match t with
      (* Leave Usr types as they are *)
      | TNamed (_, t) -> do_typ t
      | TVec (d, mn) -> TVec (d, do_mn mn)
      | TArr mn -> TArr (do_mn mn)
      | TSet (st, mn) -> TSet (st, do_mn mn)
      | TTup mns -> TTup (Array.map do_mn mns)
      | TRec mns -> TRec (Array.map (fun (n, mn) -> n, do_mn mn) mns)
      | TSum mns -> TSum (Array.map (fun (n, mn) -> n, do_mn mn) mns)
      | TMap (mn1, mn2) -> TMap (do_mn mn1, do_mn mn2)
      | t -> t in
    if t == t' then t else
    match find_def t' with
    | exception Not_found -> t'
    | n, _ -> TThis n
  in
  (* Avoid replacing the whole type with This: *)
  match find_def t with
  | exception Not_found -> do_typ t
  | _ -> t

and shrink_mn mn =
  { mn with typ = shrink mn.typ }

(* Will also add declared subtypes: *)
and add_type_as n t =
  (* As fold is top down, the result lst is bottom-up: *)
  let lst =
    fold [ n, t ] (fun lst -> function
      | TNamed (n, t) -> (n, t) :: lst
      | _ -> lst
    ) t in
  (* Shrink and add them, depth first: *)
  List.iter (fun (n, t) ->
    if List.mem_assoc n !these then raise (Redefined_type n) ;
    let t = shrink t in
    these := (n, t) :: !these
  ) lst

(*
 * Comparators
 *)

and eq ?(opaque_user_type=false) t1 t2 =
  let eq = eq ~opaque_user_type
  and eq_mn = eq_mn ~opaque_user_type in
  match t1, t2 with
  | TUnknown, _ | _, TUnknown ->
      invalid_arg "eq: Unknown type"
  | TNamed (_, t1), t2
  | t2, TNamed (_, t1) ->
      eq t1 t2
  | TThis r1, TThis r2 ->
      r1 = r2
  | TThis r, t
  | t, TThis r ->
      let t' = find_this r in
      eq t t'
  | TUsr ut1, TUsr ut2 when opaque_user_type ->
      ut1.name = ut2.name
  | TExt n1, TExt n2 ->
      n1 = n2
  | TVec (d1, mn1), TVec (d2, mn2) ->
      d1 = d2 && eq_mn mn1 mn2
  | TArr mn1, TArr mn2 ->
      eq_mn mn1 mn2
  | TSet (st1, mn1), TSet (st2, mn2) ->
      st1 = st2 &&
      eq_mn mn1 mn2
  | TTup mn1s, TTup mn2s ->
      Array.length mn1s = Array.length mn2s &&
      array_for_all2_no_exc eq_mn mn1s mn2s
  | (TRec mn1s, TRec mn2s)
  | (TSum mn1s, TSum mn2s) ->
      Array.length mn1s = Array.length mn2s &&
      array_for_all2_no_exc (fun (n1, mn1) (n2, mn2) ->
        n1 = n2 && eq_mn mn1 mn2
      ) (sorted_rec mn1s) (sorted_rec mn2s)
  | TMap (k1, v1), TMap (k2, v2) ->
      eq_mn k1 k2 &&
      eq_mn v1 v2
  (* User types are lost in des/ser so we have to accept this: *)
  | TUsr ut1, t when not opaque_user_type ->
      eq ut1.def t
  | t, TUsr ut2 when not opaque_user_type ->
      eq t ut2.def
  | TLst mn1, TLst mn2 ->
      eq_mn mn1 mn2
  | TFunction (pt1, rt1), TFunction (pt2, rt2) ->
      array_for_all2_no_exc eq_mn pt1 pt2 && eq_mn rt1 rt2
  | t1, t2 ->
      t1 = t2

(*$T eq
  eq TVoid TVoid
  eq (get_user_type "Eth") (get_user_type "Eth")
  eq (TFunction ([| required (get_user_type "Eth") ; size |], void)) \
     (TFunction ([| required (get_user_type "Eth") ; size |], void))
*)

and eq_mn ?opaque_user_type mn1 mn2 =
  mn1.nullable = mn2.nullable && eq ?opaque_user_type mn1.typ mn2.typ

(*$T eq_mn
  eq_mn (required (get_user_type "Eth")) \
        (required (get_user_type "Eth")) ;
*)

(*
 * Some functional constructors:
 *)

let usr x = TUsr x

let ext x = TExt x

let vec dim mn =
  if dim <= 0 then invalid_arg "vector" else TVec (dim, mn)

let arr mn = TArr mn

let lst mn = TLst mn

let set st mn = TSet (st, mn)

let tup mns = TTup mns

let record mns = TRec mns

let sum mns = TSum mns

let map k v = TMap (k, v)

let maybe_nullable ?default typ ~nullable = { typ ; nullable ; default }

let required = maybe_nullable ~nullable:false

let optional = maybe_nullable ~nullable:true

let named n t =
  add_type_as n t ;
  TNamed (n, t)

let this n = TThis n

(* Can come handy: *)
let tuple = function
  | [||] -> invalid_arg "tuple"
  | [| x |] -> x
  | mns -> required (TTup mns)

let pair mn1 mn2 = tuple [| mn1 ; mn2 |]
let address = required TAddress
let size = required TSize
let ptr = required TPtr
let bytes = required TBytes
let mask = required TMask

let func ins out = required (TFunction (ins, out))
let func1 i1 out = required (TFunction ([| i1 |], out))
let func2 i1 i2 out = required (TFunction ([| i1 ; i2 |], out))
let func3 i1 i2 i3 out = required (TFunction ([| i1 ; i2 ; i3 |], out))
let func4 i1 i2 i3 i4 out = required (TFunction ([| i1 ; i2 ; i3 ; i4 |], out))

let ref_ mn = required (TVec (1, mn))

(* Some short cuts for often used types: *)
let void = required TVoid
let bool = required TBool
let char = required TChar
let string = required TString
let float = required TFloat
let u8 = required TU8
let u16 = required TU16
let u24 = required TU24
let u32 = required TU32
let u40 = required TU40
let u48 = required TU48
let u56 = required TU56
let u64 = required TU64
let u128 = required TU128
let i8 = required TI8
let i16 = required TI16
let i24 = required TI24
let i32 = required TI32
let i40 = required TI40
let i48 = required TI48
let i56 = required TI56
let i64 = required TI64
let i128 = required TI128
(* nullable counterparts: *)
let nbool = optional TBool
let nchar = optional TChar
let nstring = optional TString
let nfloat = optional TFloat
let nu8 = optional TU8
let nu16 = optional TU16
let nu24 = optional TU24
let nu32 = optional TU32
let nu40 = optional TU40
let nu48 = optional TU48
let nu56 = optional TU56
let nu64 = optional TU64
let nu128 = optional TU128
let ni8 = optional TI8
let ni16 = optional TI16
let ni24 = optional TI24
let ni32 = optional TI32
let ni40 = optional TI40
let ni48 = optional TI48
let ni56 = optional TI56
let ni64 = optional TI64
let ni128 = optional TI128
let nptr = optional TPtr

let to_nullable mn =
  { mn with nullable = true }

let force mn =
  { mn with nullable = false }

(*
 * And destructors:
 *)

let pair_of_tpair = function
  | { typ = TTup [| mn1 ; mn2 |] ; nullable = false } -> mn1, mn2
  | _ -> invalid_arg "pair_of_tpair"

let is_defined t =
  try
    iter (function TUnknown -> raise Exit | _ -> ()) t ;
    true
  with Exit ->
    false

let rec is_num ~accept_float t =
  let is_num = is_num ~accept_float in
  match t with
  | TUnknown ->
      invalid_arg "is_num"
  | TNamed (_, t) ->
      is_num t
  | TThis n ->
      let t = find_this n in
      is_num t
  | TU8|TU16|TU24|TU32|TU40|TU48|TU56|TU64|TU128|
    TI8|TI16|TI24|TI32|TI40|TI48|TI56|TI64|TI128 ->
      true
  | TFloat ->
      accept_float
  | TUsr { def ; _ } ->
      is_num def
  | _ ->
      false

let is_integer t =
  is_num ~accept_float:false t

let is_numeric t =
  is_num ~accept_float:true t

(*
 * User-types registration
 *
 * Note that to actually create values of that type the constructor must
 * be registered also (see DessserExpressions.register_user_constructor).
 *)

let check t =
  iter (function
    (* TODO: also field names in a record *)
    | TSum mns ->
        Array.fold_left (fun s (n, _) ->
          if Set.String.mem n s then
            failwith "Constructor names not unique" ;
          Set.String.add n s
        ) Set.String.empty mns |>
        ignore
    | _ -> ()
  ) t

let register_user_type name def =
  if not (is_defined def) then invalid_arg "register_user_type" ;
  check def ;
  Hashtbl.modify_opt name (function
    | None -> Some { name ; def }
    | Some _ -> invalid_arg "register_user_type"
  ) user_types

let is_user_type_registered n =
  try ignore (get_user_type n) ; true
  with Not_found -> false

(*
 * Tools
 *)

(* Consider user types opaque by default, so that it matches DessserQCheck
 * generators. *)
let rec depth ?(opaque_user_type=true) t =
  let depth = depth ~opaque_user_type in
  match t with
  | TUnknown -> invalid_arg "depth"
  | TThis _ ->
      (* For this purpose assume This is not going to be recursed into,
       * and behave like a scalar: *)
      0
  | TUsr { def ; _ } ->
      if opaque_user_type then 0 else depth def
  | TVec (_, mn) | TArr mn | TSet (_, mn) | TLst mn ->
      1 + depth mn.typ
  | TTup mns ->
      1 + Array.fold_left (fun d mn ->
        max d (depth mn.typ)
      ) 0 mns
  | TRec mns | TSum mns ->
      1 + Array.fold_left (fun d (_, mn) ->
        max d (depth mn.typ)
      ) 0 mns
  | TMap (mn1, mn2) ->
      1 + max (depth mn1.typ) (depth mn2.typ)
  | _ -> 0

(*$= depth & ~printer:string_of_int
  0 (depth TU8)
  2 (depth (TTup [| required TU8 ; required (TArr (required TU8)) |]))
  7 (depth (\
    TArr (required (\
      TVec (4, (required (\
        TRec [| \
          "mgfhm", required (\
            TRec [| \
              "ceci", optional TChar ; \
              "zauxs", required (\
                TRec [| \
                  "gidf", required (\
                    TRec [| \
                      "qskv", optional TFloat ; \
                      "lefr", required TI16 ; \
                      "bdujmi", required (\
                        TVec (8, required (get_user_type "Cidr6"))) |]) ; \
                  "cdwcv", required TU64 ; \
                  "jcdivs", required (\
                    TRec [| \
                      "hgtixf", required (TArr (optional TI128)) ; \
                      "yuetd", required TChar ; \
                      "bsbff", required TU16 |]) |]) |]) ; \
          "pudia", required (get_user_type "Cidr4") ; \
          "qngl", required TBool ; \
          "iajv", optional TI128 |])))))))
*)

let uniq_id t =
  shrink t |>
  develop |>
  IO.to_string print_sorted |>
  Digest.string |>
  Digest.to_hex

let rec get_item_type ?(vec=false) ?(arr=false) ?(set=false) ?(lst=false)
                      ?(str=false) ?(bytes=false) = function
  | TUsr { def ; _ } -> get_item_type ~vec ~arr ~set ~lst ~str ~bytes def
  | TVec (_, mn) when vec -> mn
  | TArr mn when arr -> mn
  | TSet (_, mn) when set -> mn
  | TLst mn when lst -> mn
  | TString when str -> char
  | TBytes when bytes -> u8
  | t -> "get_item_type: "^ to_string t |> invalid_arg
