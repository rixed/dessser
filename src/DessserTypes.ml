open Batteries
open Stdint

open DessserMiscTypes
open DessserTools

module PConfig = ParsersPositions.LineCol (Parsers.SimpleConfig (Char))
module P = Parsers.Make (PConfig)

let pp = Printf.fprintf

(* Basic types that can be used to define more specialized user types *)

type base_type =
  | Bool | Char | Float | String
  | U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128
  | I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128

(* User types are specialized types that can be build from basic types and
 * given their own name, pretty-printer and parser.
 * User expressions can restrict types to user types. Other than that,
 * every operation that applies to the implementation of a user type also
 * applies to the user type, and the other way around. *)

and user_type =
  { name : string ;
    def : t }

(* "Value" types are all the types describing values that can be (de)serialized.
 * All of them can possibly be nullable. *)

and t =
  | Unknown
  (* Refers to the top-level type that's currently being defined. *)
  | This
  | Base of base_type
  (* Aliases with custom representations: *)
  | Usr of user_type
  (* External types are known only by name, and are converted to some verbatim
   * text provided by the user when code is printed. *)
  | Ext of string
  (* Compound types: *)
  | Vec of int * mn
  | Lst of mn
  (* Special compound type amenable to incremental computation over sets.
   * There are different implementations with slightly different APIs,
   * depending on the use case (ie. the operator it is an operand of), for
   * instance to optimise FIFO updates, to keep it sorted, to skip nulls, etc.
   * But the backend will decide this on its own: *)
  | Set of set_type * mn
  | Tup of mn array
  (* Exact same as a tuple, but with field names that can be used as
   * accessors (also used to name actual fields in generated code): *)
  | Rec of (string * mn) array
  (* Sum types, as a list of constructor and type. Constructor names uniqueness
   * will be checked at construction. *)
  | Sum of (string * mn) array
  (* The type for maps exist because there will be some operations using
   * that type indirectly (such as fetching from a DB by key, or describing
   * a key->value mapping in a type expression). But there is no value of
   * that type, ever. From a (de)serialized point of view, maps are
   * equivalent to association lists. *)
  | Map of mn * mn
  (* The above types are used to hold data that can be serialized. The types
   * below are meant to help implement serializers themselves, and are not
   * serializable: *)
  (* Used for functions without return values: *)
  | Void
  (* Ptr are used to point into a stream of bytes to serialized into /
   * deserialized from. *)
  | Ptr
  (* A size in byte: *)
  | Size
  (* An arbitrary address, used for DataPtrOfAddress: *)
  | Address
  (* FIXME: still worth it? *)
  | Bit
  | Byte
  | Word
  | DWord
  | QWord
  | OWord
  | Bytes
  (* Types for the runtime representation of a field mask: *)
  | Mask  (* What to do with a tree of fields *)
  (* We'd like the DES/SERializer to be able to use complex types as their
   * "pointer", part of those types being actual pointers. Therefore, we cannot
   * use the value-types, as we cannot embed a pointer in there. So here are
   * defined two specific compound types: a pair and an homogeneous list
   * (sufficient in practice and better than untyped car/cdr!) *)
  (* FIXME: still worth it? *)
  | Pair of mn * mn
  (* A Data Lst is just a vector which length is unknown at compile time,
   * whereas an SList can actually be constructed/destructed element by element.
   * "SList" is for "singly-chained" list, and is written using "[[]]" instead
   * of "[]". *)
  | SList of mn
  | Function of (* arguments: *) mn array * (* result: *) mn
  (* Makes mn mutable: *)
  | Ref of mn

and mn =
  { typ : t ; nullable : bool }


(* dessserc will set this to the global schema: *)
let this = ref Unknown

(*
 * Comparators
 *)

(* In many occasions we want the items of a record to be deterministically
 * ordered so they can be compared etc: *)
let sorted_rec fields =
  let cmp_nv (n1, _) (n2, _) =
    String.compare n1 n2 in
  let fields = Array.copy fields in
  Array.sort cmp_nv fields ;
  fields

let base_type_eq mt1 mt2 = mt1 = mt2

let rec eq ?(opaque_user_type=false) t1 t2 =
  match t1, t2 with
  | Unknown, _ | _, Unknown ->
      invalid_arg "eq: Unknown type"
  | This, This ->
      true
  | This, t
  | t, This ->
      eq t !this
  | Base b1, Base b2 ->
      base_type_eq b1 b2
  | Usr ut1, Usr ut2 when opaque_user_type ->
      ut1.name = ut2.name
  | Ext n1, Ext n2 ->
      n1 = n2
  | Vec (d1, mn1), Vec (d2, mn2) ->
      d1 = d2 && eq_mn mn1 mn2
  | Lst mn1, Lst mn2 ->
      eq_mn mn1 mn2
  | Set (st1, mn1), Set (st2, mn2) ->
      st1 = st2 &&
      eq_mn mn1 mn2
  | Tup mn1s, Tup mn2s ->
      Array.length mn1s = Array.length mn2s &&
      array_for_all2_no_exc eq_mn mn1s mn2s
  | (Rec mn1s, Rec mn2s)
  | (Sum mn1s, Sum mn2s) ->
      Array.length mn1s = Array.length mn2s &&
      array_for_all2_no_exc (fun (n1, mn1) (n2, mn2) ->
        n1 = n2 && eq_mn mn1 mn2
      ) (sorted_rec mn1s) (sorted_rec mn2s)
  | Map (k1, v1), Map (k2, v2) ->
      eq_mn k1 k2 &&
      eq_mn v1 v2
  (* User types are lost in des/ser so we have to accept this: *)
  | Usr ut1, t when not opaque_user_type ->
      eq ut1.def t
  | t, Usr ut2 when not opaque_user_type ->
      eq t ut2.def
  | Pair (mn11, mn12), Pair (mn21, mn22) ->
      eq_mn mn11 mn21 && eq_mn mn12 mn22
  | SList mn1, SList mn2 ->
      eq_mn mn1 mn2
  | Function (pt1, rt1), Function (pt2, rt2) ->
      array_for_all2_no_exc eq_mn pt1 pt2 && eq_mn rt1 rt2
  | Ref mn1, Ref mn2 ->
      eq_mn mn1 mn2
  | t1, t2 ->
      t1 = t2

(*$T eq
  eq Void Void
  eq (get_user_type "Eth") (get_user_type "Eth")
  eq (Function ([| required (get_user_type "Eth") ; size |], void)) \
     (Function ([| required (get_user_type "Eth") ; size |], void))
*)

and eq_mn mn1 mn2 =
  mn1.nullable = mn2.nullable && eq mn1.typ mn2.typ

(*$T eq_mn
  eq_mn (required (get_user_type "Eth")) \
        (required (get_user_type "Eth")) ;
*)

(*
 * Iterators
 *)

let rec develop = function
  | (Unknown | This | Base _ | Ext _) as t ->
      t
  | Usr { def ; _ } ->
      develop def
  | Vec (d, mn) ->
      Vec (d, develop_mn mn)
  | Lst mn ->
      Lst (develop_mn mn)
  | Set (st, mn) ->
      Set (st, develop_mn mn)
  | Tup mns ->
      Tup (Array.map develop_mn mns)
  | Rec mns ->
      Rec (Array.map (fun (n, mn) -> n, develop_mn mn) mns)
  | Sum cs ->
      Sum (Array.map (fun (n, mn) -> n, develop_mn mn) cs)
  | Map (mn1, mn2) ->
      Map (develop_mn mn1, develop_mn mn2)
  | Pair (mn1, mn2) ->
      Pair (develop_mn mn1, develop_mn mn2)
  | t -> t

and develop_mn mn =
  { mn with typ = develop mn.typ }

(* This develop user types at first level (ie. excluding sub-branches but
 * including when a user type is implemented with another): *)
let rec develop1 = function
  | { typ = Usr { def ; _ } ; nullable } ->
      develop1 ({ typ = def ; nullable })
  | t ->
      t

(* Top-down folding of a type: *)
(* FIXME: either consider Usr types as opaque and stop the recursion, or as
 * transparent and do not call [f] on Usr: *)
let rec fold u f t =
  let u = f u t in
  match t with
  | Usr { def ; _ } ->
      fold u f def
  | Vec (_, mn) | Lst mn | Set (_, mn) | SList mn | Ref mn ->
      fold_mn u f mn
  | Tup mns ->
      Array.fold_left (fun u mn -> fold_mn u f mn) u mns
  | Rec mns | Sum mns ->
      Array.fold_left (fun u (_, mn) -> fold_mn u f mn) u mns
  | Map (mn1, mn2) | Pair (mn1, mn2) ->
      fold_mn (fold_mn u f mn1) f mn2
  | _ ->
      u

and fold_mn u f mn =
  fold u f mn.typ

let iter f t =
  fold () (fun () t -> f t) t

let iter_mn f mn =
  fold_mn () (fun () mn -> f mn) mn

(*
 * User-defined types
 *)

type user_type_def =
  { usr_typ : user_type ;
    print : 'a. 'a IO.output -> unit ;
    mutable parse : t P.t }

let user_types : (string, user_type_def) Hashtbl.t = Hashtbl.create 50

type gen_printer = { f : 'a. 'a IO.output -> unit }

let default_user_type_printer ut oc =
  String.print oc ut.name

let get_user_type n =
  Usr (Hashtbl.find user_types n).usr_typ

(* See below after Parser definition for [register_user_type] and examples *)

(*
 * Printers
 *)

let string_of_set_type = function
  | Simple -> ""
  | Sliding -> "sliding"
  | Tumbling -> "tumbling"
  | Sampling -> "sampling"
  | HashTable -> "hashtable"
  | Heap -> "heap"
  | Top -> "top"

let print_base_type oc =
  let sp = String.print oc in
  function
  | Float -> sp "FLOAT"
  | String -> sp "STRING"
  | Bool -> sp "BOOL"
  | Char -> sp "CHAR"
  | U8 -> sp "U8"
  | U16 -> sp "U16"
  | U24 -> sp "U24"
  | U32 -> sp "U32"
  | U40 -> sp "U40"
  | U48 -> sp "U48"
  | U56 -> sp "U56"
  | U64 -> sp "U64"
  | U128 -> sp "U128"
  | I8 -> sp "I8"
  | I16 -> sp "I16"
  | I24 -> sp "I24"
  | I32 -> sp "I32"
  | I40 -> sp "I40"
  | I48 -> sp "I48"
  | I56 -> sp "I56"
  | I64 -> sp "I64"
  | I128 -> sp "I128"

let rec print ?(sorted=false) oc =
  let print_mn = print_mn ~sorted in
  let sp = String.print oc in
  function
  | Unknown ->
      sp "UNKNOWN"
  | This ->
      sp "THIS"
  | Base t ->
      print_base_type oc t
  (* To having having to accept any valid identifiers as an external type when
   * parsing a type, we denote external types with a dollar sign, evicative of
   * some reference/placeholder. *)
  | Ext n ->
      pp oc "$%s" n
  | Usr t ->
      (match Hashtbl.find user_types t.name with
      | exception Not_found ->
          default_user_type_printer t oc
      | def ->
          def.print oc)
  | Vec (dim, mn) ->
      pp oc "%a[%d]" print_mn mn dim
  | Lst mn ->
      pp oc "%a[]" print_mn mn
  | Set (st, mn) ->
      pp oc "%a{%s}" print_mn mn (string_of_set_type st)
  | Tup mns ->
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:"; "
          print_mn) mns
  | Rec mns ->
      (* When the string repr is used to identify the type (see BackEndCLike)
       * every equivalent record types must then be printed the same, thus the
       * optional sort: *)
      pp oc "%a"
        (Array.print ~first:"{" ~last:"}" ~sep:"; "
          (fun oc (n, mn) ->
            pp oc "%s: %a" n print_mn mn)
        ) (if sorted then sorted_rec mns else mns)
  | Sum cs ->
      (* Parenthesis are required to distinguish external from internal
       * nullable: *)
      pp oc "%a"
        (Array.print ~first:"[" ~last:"]" ~sep:" | "
          (fun oc (n, mn) ->
            pp oc "%s %a" n print_mn mn)
        ) (if sorted then sorted_rec cs else cs)
  | Map (k, v) ->
      pp oc "%a[%a]"
        print_mn v
        print_mn k
  | Void -> sp "Void"
  | Ptr -> sp "Ptr"
  | Size -> sp "Size"
  | Address -> sp "Address"
  | Bit -> sp "Bit"
  | Byte -> sp "Byte"
  | Word -> sp "Word"
  | DWord -> sp "DWord"
  | QWord -> sp "QWord"
  | OWord -> sp "OWord"
  | Bytes -> sp "Bytes"
  | Mask -> sp "Mask"
  | Pair (mn1, mn2) ->
      pp oc "(%a * %a)"
        print_mn mn1
        print_mn mn2
  | SList mn ->
      pp oc "%a[[]]" print_mn mn
  | Function ([||], mn) ->
      pp oc "( -> %a)" print_mn mn
  | Function (mns, mn2) ->
      pp oc "(%a -> %a)"
        (Array.print ~first:"" ~last:"" ~sep:" -> " print_mn) mns
        print_mn mn2
  | Ref mn ->
      pp oc "&%a" print_mn mn

and print_mn ?sorted oc mn =
  pp oc "%a%s"
    (print ?sorted) mn.typ
    (if mn.nullable then "?" else "")

let print_sorted oc = print ~sorted:true oc
let print oc = print ~sorted:false oc
let print_mn_sorted oc = print_mn ~sorted:true oc
let print_mn oc = print_mn ~sorted:false oc

let mn_to_string = IO.to_string print_mn
let to_string = IO.to_string print

module Parser =
struct
  module ParseUsual = ParsersUsual.Make (P)
  include P
  include ParseUsual

  let comment =
    ref (
      let all_but_newline =
        cond "anything until newline" (fun c -> c <> '\n' && c <> '\r') '_'
      in
      fun m ->
        let m = "comment" :: m in
        (
          char '/' -- char '/' --
          repeat_greedy ~sep:none ~what:"comment" all_but_newline
        ) m
    )

  let blanks =
    ref (
      let blank = ParseUsual.blank >>: ignore
      and newline = ParseUsual.newline >>: ignore in
      fun m ->
        let m = "blanks" :: m in
        (
        repeat_greedy ~min:1 ~sep:none ~what:"whitespaces"
          (blank ||| newline ||| !comment) >>: ignore
        ) m
    )

  let opt_blanks =
    optional_greedy ~def:() !blanks

  let allow_surrounding_blanks p =
    opt_blanks -+ p +- opt_blanks +- eof

  let opt_question_mark =
    optional ~def:false (char '?' >>: fun _ -> true)

  let pos_integer what m =
    let m = what :: m in
    (
      decimal_number >>: fun n ->
        let i = Num.to_int n in
        if i < 0 then raise (Reject "must be positive")
        else i
    ) m

  (* strinG will match the given string regardless of the case and
   * regardless of the surrounding (ie even if followed by other letters). *)
  let strinG s =
    dismiss_error_if (parsed_fewer_than (String.length s / 2))
      (ParseUsual.string ~case_sensitive:false s)

  let tup_sep =
    opt_blanks -- char ';' -- opt_blanks

  let user_type = ref fail

  let external_type : unit t ref = ref fail

  let identifier =
    let what = "identifier" in
    let first_char = letter ||| underscore ||| char '-' in
    let any_char = first_char ||| decimal_digit in
    first_char ++ repeat_greedy ~sep:none ~what any_char >>: fun (c, s) ->
      String.of_list (c :: s)

  type key_type =
    VecDim of int | ListDim | SetDim of set_type | MapKey of mn | SListDim

  let rec reduce_dims typ = function
    | [] -> typ
    | (nullable, VecDim d) :: rest ->
        reduce_dims (Vec (d, { nullable ; typ })) rest
    | (nullable, ListDim) :: rest ->
        reduce_dims (Lst { nullable ; typ }) rest
    | (nullable, SetDim st) :: rest ->
        reduce_dims (Set (st, { nullable ; typ })) rest
    | (nullable, MapKey k) :: rest ->
        reduce_dims (Map (k, { nullable ; typ })) rest
    | (nullable, SListDim) :: rest ->
        reduce_dims (SList { nullable ; typ }) rest

  let rec key_type m =
    let vec_dim m =
      let m = "vector dimension" :: m in
      (
        opt_question_mark +-
        char '[' +- opt_blanks ++
        pos_integer "vector dimensions" +-
        opt_blanks +- char ']' >>: fun (n, d) ->
          if d <= 0 then
            raise (Reject "Vector must have strictly positive dimension") ;
          n, VecDim d
      ) m in
    let list_dim m =
      let m = "list type" :: m in
      (
        opt_question_mark +-
        char '[' +- opt_blanks +- char ']' >>: fun n -> n, ListDim
      ) m in
    let slist_dim m =
      let m = "slist type" :: m in
      (
        opt_question_mark +-
        char '[' +- char '[' +- opt_blanks +- char ']' +- char ']' >>:
          fun n -> n, SListDim
      ) m in
    let set_type m =
      let m = "set type" :: m in
      (
        (strinG "simple" >>: fun () -> Simple) |<|
        (strinG "sliding" >>: fun () -> Sliding) |<|
        (strinG "tumbling" >>: fun () -> Tumbling) |<|
        (strinG "sampling" >>: fun () -> Sampling) |<|
        (strinG "hashtable" >>: fun () -> HashTable) |<|
        (strinG "heap" >>: fun () -> Heap) |<|
        (strinG "top" >>: fun () -> Top)
      ) m in
    let set_dim m =
      let m = "set type" :: m in
      (
        opt_question_mark +-
        char '{' +-
          opt_blanks ++ optional ~def:Simple set_type +- opt_blanks +-
        char '}' >>: fun (n, st) -> n, SetDim st
      ) m in
    let map_key m =
      let m = "map key" :: m in
      (
        opt_question_mark +-
        char '[' +- opt_blanks ++
          mn +- opt_blanks +- char ']' >>: fun (n, k) -> n, MapKey k
      ) m
    in
    (
      vec_dim |<| list_dim |<| set_dim |<| map_key |<| slist_dim
    ) m

  and mn m =
    let m = "maybe nullable" :: m in
    (
      typ ++ opt_question_mark >>:
        fun (typ, nullable) -> { typ ; nullable }
    ) m

  and typ m =
    let m = "type" :: m in
    (
      (
        scalar_typ |<|
        tuple_typ |<|
        record_typ |<|
        sum_typ |<|
        ext_typ |<|
        !user_type |<|
        (strinG "this" >>: fun () -> This) |<|
        (strinG "void" >>: fun () -> Void) |<|
        (strinG "ptr" >>: fun () -> Ptr) |<|
        (strinG "size" >>: fun () -> Size) |<|
        (strinG "address" >>: fun () -> Address) |<|
        (strinG "bit" >>: fun () -> Bit) |<|
        (strinG "byte" >>: fun () -> Byte) |<|
        (strinG "word" >>: fun () -> Word) |<|
        (strinG "dword" >>: fun () -> DWord) |<|
        (strinG "qword" >>: fun () -> QWord) |<|
        (strinG "oword" >>: fun () -> OWord) |<|
        (strinG "bytes" >>: fun () -> Bytes) |<|
        (strinG "mask" >>: fun () -> Mask) |<|
        (
          char '(' -- opt_blanks -+ mn +- opt_blanks +-
          char '*' +- opt_blanks ++ mn +- opt_blanks +- char ')' >>:
            fun (mn1, mn2) -> Pair (mn1, mn2)
        ) |<| (
          let sep = opt_blanks -- char '-' -- char '>' -- opt_blanks in
          char '(' -+
            repeat ~sep mn +- sep ++ mn +- opt_blanks +-
          char ')' >>: fun (ptyps, rtyp) ->
            Function (Array.of_list ptyps, rtyp)
        ) |<| (
          char '&' -- opt_blanks -+ mn >>: fun mn -> Ref mn
        )
      ) ++
      repeat ~sep:opt_blanks (key_type) >>: fun (t, dims) ->
        reduce_dims t dims
    ) m

  and scalar_typ m =
    let m = "scalar type" :: m in
    let st n mtyp =
      strinG n >>: fun () -> Base mtyp
    in
    (
      (st "float" Float) |<|
      (st "string" String) |<|
      (st "bool" Bool) |<|
      (st "boolean" Bool) |<|
      (st "char" Char) |<|
      (st "u8" U8) |<|
      (st "u16" U16) |<|
      (st "u24" U24) |<|
      (st "u32" U32) |<|
      (st "u40" U40) |<|
      (st "u48" U48) |<|
      (st "u56" U56) |<|
      (st "u64" U64) |<|
      (st "u128" U128) |<|
      (st "i8" I8) |<|
      (st "i16" I16) |<|
      (st "i24" I24) |<|
      (st "i32" I32) |<|
      (st "i40" I40) |<|
      (st "i48" I48) |<|
      (st "i56" I56) |<|
      (st "i64" I64) |<|
      (st "i128" I128)
    ) m

  and tuple_typ m =
    let m = "tuple type" :: m in
    (
      char '(' -- opt_blanks -+
        several ~sep:tup_sep mn
      +- opt_blanks +- char ')' >>: fun ts -> Tup (Array.of_list ts)
    ) m

  and record_typ m =
    let m = "record type" :: m in
    let field_typ =
      identifier +- opt_blanks +- char ':' +- opt_blanks ++ mn in
    (
      char '{' -- opt_blanks -+
        several ~sep:tup_sep field_typ +-
        opt_blanks +- optional ~def:() (char ';' -- opt_blanks) +-
      char '}' >>: fun ts ->
        (* TODO: check that all field names are distinct *)
        Rec (Array.of_list ts)
    ) m

  and sum_typ m =
    let m = "sum type" :: m in
    let constructor m =
      let m = "constructor" :: m in
      (
        identifier ++
        optional ~def:{ nullable = false ; typ = Void }
          (!blanks -+ mn)
      ) m
    and sep =
      opt_blanks -- char '|' -- opt_blanks in
    (
      char '[' -- opt_blanks -+
      several ~sep constructor +-
      opt_blanks +- char ']' >>: fun ts ->
          (* TODO: check that all constructors are case insensitively distinct *)
          Sum (Array.of_list ts)
    ) m

  and ext_typ m =
    let m = "external type" :: m in
    (
      char '$' -+ identifier >>: fun n -> Ext n
    ) m

  let string_parser ?what ~print p =
    let what =
      match what with None -> [] | Some w -> [w] in
    let p = allow_surrounding_blanks p in
    fun s ->
      let stream = stream_of_string s in
      let parse_with_err_budget e =
        let c = ParsersBoundedSet.make e in
        p what None c stream |> to_result in
      let err_out e =
        Printf.sprintf2 "Parse error: %a"
          (print_bad_result print) e |>
        failwith
      in
      let try_fix_typos = true in
      match parse_with_err_budget 0 with
      | Error e ->
          if try_fix_typos then
            (* Try again with some error correction activated, in order to
             * get a better error message: *)
            match parse_with_err_budget 1 with
            | Error e -> err_out e
            | _ -> assert false
          else
            err_out e
      | Ok (res, _) ->
          res

  (*$< Parser *)
  (*$inject
    open Batteries

    let test_printer res_printer = function
      | Ok (res, (_, [])) ->
        Printf.sprintf "%s" (IO.to_string res_printer res)
      | Ok (res, (len, rest)) ->
        Printf.sprintf "%S, parsed_len=%d, rest=%s"
          (IO.to_string res_printer res) len
          (IO.to_string (List.print Char.print) rest)
      | Error (Approximation _) ->
        "Approximation"
      | Error (NoSolution e) ->
        Printf.sprintf "No solution (%s)" (IO.to_string print_error e)
      | Error (Ambiguous lst) ->
        Printf.sprintf "%d solutions: %s"
          (List.length lst)
          (IO.to_string
            (List.print (fun oc (res, _corr, (_stream, pos)) ->
              Printf.fprintf oc "res=%a, pos=%d,%d"
                res_printer res
                pos.ParsersPositions.line pos.column)) lst)

    let strip_linecol = function
      | Ok (res, (x, _pos)) -> Ok (res, x)
      | Error _ as e -> e

    let test_p ?(postproc=identity) p s =
      (p +- eof) [] None Parsers.no_error_correction (PConfig.stream_of_string s) |>
      to_result |>
      strip_linecol |>
      Result.map (fun (r, rest) -> postproc r, rest)

    open DessserTypes
  *)

  (*$inject
    let pmn = Parser.mn *)
  (*$= pmn & ~printer:(test_printer print_mn)
    (Ok ((required (Base U8)), (2,[]))) \
       (test_p pmn "u8")
    (Ok ((optional (Base U8)), (3,[]))) \
       (test_p pmn "u8?")
    (Ok ((required (Vec (3, (required (Base U8))))), (5,[]))) \
       (test_p pmn "u8[3]")
    (Ok ((required (Vec (3, (optional (Base U8))))), (6,[]))) \
       (test_p pmn "u8?[3]")
    (Ok ((optional (Vec (3, (required (Base U8))))), (6,[]))) \
       (test_p pmn "u8[3]?")
    (Ok ((required (Vec (3, (optional (Lst (required (Base U8))))))), (8,[]))) \
       (test_p pmn "u8[]?[3]")
    (Ok ((optional (Lst (required (Vec (3, (optional (Base U8))))))), (9,[]))) \
       (test_p pmn "u8?[3][]?")
    (Ok ((optional (Map ((required (Base String)), (required (Base U8))))), (11,[]))) \
       (test_p pmn "u8[string]?")
    (Ok ((required (Map ((required (Map ((optional (Base U8)), (optional (Base String))))), (optional (Lst ((required (Tup [| (required (Base U8)) ; (required (Map ((required (Base String)), (required (Base Bool))))) |])))))))), (35,[]))) \
       (test_p pmn "(u8; bool[string])[]?[string?[u8?]]")
    (Ok ((required (Rec [| "f1", required (Base Bool) ; "f2", optional (Base U8) |])), (19,[]))) \
      (test_p pmn "{f1: Bool; f2: U8?}")
    (Ok ((required (Rec [| "f2", required (Base Bool) ; "f1", optional (Base U8) |])), (19,[]))) \
      (test_p pmn "{f2: Bool; f1: U8?}")
    (Ok ((required (Sum [| "c1", required (Base Bool) ; "c2", optional (Base U8) |])), (18,[]))) \
      (test_p pmn "[c1 Bool | c2 U8?]")
    (Ok ((required (Vec (1, required (Base Bool)))), (7,[]))) \
      (test_p pmn "Bool[1]")
  *)

  (* In addition to native dessser format for type specification, we
   * can also make sense of ClickHouse "NamesAndTypes" somewhat informal
   * specifications: *)
  let clickhouse_names_and_types m =
    let m = "ClickHouse NameAndTypes format" :: m in
    let backquoted_string_with_sql_style m =
      let m = "Backquoted field name" :: m in
      (
        char '`' -+
        repeat_greedy ~sep:none (
          cond "field name" ((<>) '`') 'x') +-
        char '`' >>: String.of_list
      ) m in
    let rec ptype m =
      let with_param np ap =
        np -- opt_blanks -- char '(' -+ ap +- char ')' in
      let with_2_params np p1 p2 =
        let ap = p1 -+ opt_blanks +- char ',' +- opt_blanks ++ p2 in
        with_param np ap in
      let unsigned =
        integer >>: fun n ->
          let i = Num.to_int n in
          if i < 0 then raise (Reject "Type parameter must be >0") ;
          i in
      let with_num_param s =
        with_param (strinG s) unsigned in
      let with_2_num_params s =
        with_2_params (strinG s) number number in
      let with_typ_param s =
        with_param (strinG s) ptype in
      let legit_identifier_chars =
        letter |<| underscore |<| decimal_digit in
      let iD s =
        ParseUsual.string ~case_sensitive:false s --
        nay legit_identifier_chars in
      let m = "Type name" :: m in
      (
        (* Look only for simple types, starting with numerics: *)
        (iD "UInt8" >>: fun () -> { nullable = false ; typ = Base U8 }) |<|
        (iD "UInt16" >>: fun () -> { nullable = false ; typ = Base U16 }) |<|
        (iD "UInt32" >>: fun () -> { nullable = false ; typ = Base U32 }) |<|
        (iD "UInt64" >>: fun () -> { nullable = false ; typ = Base U64 }) |<|
        ((iD "Int8" |<| iD "TINYINT") >>:
          fun () -> { nullable = false ; typ = Base I8 }) |<|
        ((iD "Int16" |<| iD "SMALLINT") >>:
          fun () -> { nullable = false ; typ = Base I16 }) |<|
        ((iD "Int32" |<| iD "INTEGER" |<| iD "INT") >>:
          fun () -> { nullable = false ; typ = Base I32 }) |<|
        ((iD "Int64" |<| iD "BIGINT") >>:
          fun () -> { nullable = false ; typ = Base I64 }) |<|
        ((iD "Float32" |<| iD "Float64" |<|
          iD "FLOAT" |<| iD "DOUBLE") >>:
          fun () -> { nullable = false ; typ = Base Float }) |<|
        (* Assuming UUIDs are just plain U128 with funny-printing: *)
        (iD "UUID" >>: fun () -> { nullable = false ; typ = Base U128 }) |<|
        (* Decimals: for now forget about the size of the decimal part,
         * just map into corresponding int type*)
        (with_num_param "Decimal32" >>:
          fun _p -> { nullable = false ; typ = Base I32 }) |<|
        (with_num_param "Decimal64" >>:
          fun _p -> { nullable = false ; typ = Base I64 }) |<|
        (with_num_param "Decimal128" >>:
          fun _p -> { nullable = false ; typ = Base I128 }) |<|
        (* TODO: actually do something with the size: *)
        ((with_2_num_params "Decimal" |<| with_2_num_params "DEC") >>:
          fun (_n, _m)  -> { nullable = false ; typ = Base I128 }) |<|
        ((iD "DateTime" |<| iD "TIMESTAMP") >>:
          fun () -> { nullable = false ; typ = Base U32 }) |<|
        (iD "Date" >>: fun () -> { nullable = false ; typ = Base U16 }) |<|
        ((iD "String" |<| iD "CHAR" |<| iD "VARCHAR" |<|
          iD "TEXT" |<| iD "TINYTEXT" |<| iD "MEDIUMTEXT" |<|
          iD "LONGTEXT" |<| iD "BLOB" |<| iD "TINYBLOB" |<|
          iD "MEDIUMBLOB" |<| iD "LONGBLOB") >>:
          fun () -> { nullable = false ; typ = Base String }) |<|
        ((with_num_param "FixedString" |<| with_num_param "BINARY") >>:
          fun d -> { nullable = false ;
                     typ = Vec (d, { nullable = false ;
                                       typ = Base Char }) }) |<|
        (with_typ_param "Nullable" >>:
          fun mn -> { mn with nullable = true }) |<|
        (with_typ_param "Array" >>:
          fun mn -> { nullable = false ; typ = Lst mn }) |<|
        (* Just ignore those ones (for now): *)
        (with_typ_param "LowCardinality")
        (* Etc... *)
      ) m
    in
    (
      optional ~def:() (
        string "columns format version: " -- number -- !blanks) --
      optional ~def:() (
        number -- !blanks -- string "columns:" -- !blanks) -+
      several ~sep:!blanks (
        backquoted_string_with_sql_style +- !blanks ++ ptype)
      >>: fun mns ->
        let mns = Array.of_list mns in
        Rec mns
    ) m

  (*$= clickhouse_names_and_types & ~printer:(test_printer print)
    (Ok (Rec [| "thing", required (Base U16) |], (14,[]))) \
       (test_p clickhouse_names_and_types "`thing` UInt16")

    (Ok (Rec [| "thing", required (Lst (required (Base U16))) |], (21,[]))) \
       (test_p clickhouse_names_and_types "`thing` Array(UInt16)")
  *)

  (*$>*)
end

let mn_of_string ?what =
  let print = print_mn in
  Parser.(string_parser ~print ?what mn)

(* If [any_format] then any known format to specify types will be tried.
 * If not then only dessser own format will be tried (faster, esp when
 * parsing DIL s-expressions) *)
let of_string ?(any_format=false) ?what =
  let open Parser in
  let p =
    if any_format then typ |<| clickhouse_names_and_types
    else typ in
  string_parser ~print ?what p

(*$= of_string & ~printer:Batteries.dump
  (Usr { name = "Ip4" ; def = Base U32 }) (of_string "Ip4")

  (SList { \
    typ = Sum [| "eugp", { typ = Usr { name = "Ip4" ; \
                                       def = Base U32 } ; \
                           nullable = false }; \
                 "jjbi", { typ = Base Bool ; nullable = false } ; \
                 "bejlu", { typ = Base I24 ; nullable = true } ; \
                 "bfid", { typ = Base Float ; nullable = false } |] ; \
    nullable = false }) \
  (of_string "[eugp Ip4 | jjbi BOOL | bejlu I24? | bfid FLOAT][[]]")
*)

(*
 * Tools
 *)

(* Consider user types opaque by default, so that it matches DessserQCheck
 * generators. *)
let rec depth ?(opaque_user_type=true) = function
  | Unknown -> invalid_arg "depth"
  | This ->
      (* For this purpose assume This is not going to be recursed into,
       * and behave like a scalar: *)
      0
  | Usr { def ; _ } ->
      if opaque_user_type then 0 else depth ~opaque_user_type def
  | Vec (_, mn) | Lst mn | Set (_, mn) | SList mn | Ref mn ->
      1 + depth ~opaque_user_type mn.typ
  | Tup mns ->
      1 + Array.fold_left (fun d mn ->
        max d (depth ~opaque_user_type mn.typ)
      ) 0 mns
  | Rec mns | Sum mns ->
      1 + Array.fold_left (fun d (_, mn) ->
        max d (depth ~opaque_user_type mn.typ)
      ) 0 mns
  | Map (mn1, mn2) | Pair (mn1, mn2) ->
      1 + max (depth ~opaque_user_type mn1.typ)
              (depth ~opaque_user_type mn2.typ)
  | _ -> 0

(*$= depth & ~printer:string_of_int
  0 (depth (Base U8))
  2 (depth (Tup [| required (Base U8) ; required (Lst (required (Base U8))) |]))
  7 (depth (\
    Lst (required (\
      Vec (4, (required (\
        Rec [| \
          "mgfhm", required (\
            Rec [| \
              "ceci", optional (Base Char) ; \
              "zauxs", required (\
                Rec [| \
                  "gidf", required (\
                    Rec [| \
                      "qskv", optional (Base Float) ; \
                      "lefr", required (Base I16) ; \
                      "bdujmi", required (\
                        Vec (8, required (get_user_type "Cidr6"))) |]) ; \
                  "cdwcv", required (Base U64) ; \
                  "jcdivs", required (\
                    Rec [| \
                      "hgtixf", required (Lst (optional (Base I128))) ; \
                      "yuetd", required (Base Char) ; \
                      "bsbff", required (Base U16) |]) |]) |]) ; \
          "pudia", required (get_user_type "Cidr4") ; \
          "qngl", required (Base Bool) ; \
          "iajv", optional (Base I128) |])))))))
*)

(* We need types featuring `this`, that can come with various degree of
 * unfolding, to all look equal.
 * We cannot "expand" This, because that would just make more This appear.
 * But there exist a form where the type is folded as much as possible: *)
let shrink t =
  if !this = Unknown || !this = This then t else
  let rec do_mn mn =
    { mn with typ = do_typ mn.typ }
  and do_typ t =
    if eq t !this then This else
    match t with
    | Usr { def ; _ } -> do_typ def
    | Vec (d, mn) -> Vec (d, do_mn mn)
    | Lst mn -> Lst (do_mn mn)
    | Set (st, mn) -> Set (st, do_mn mn)
    | Tup mns -> Tup (Array.map do_mn mns)
    | Rec mns -> Rec (Array.map (fun (n, mn) -> n, do_mn mn) mns)
    | Sum mns -> Sum (Array.map (fun (n, mn) -> n, do_mn mn) mns)
    | Map (mn1, mn2) -> Map (do_mn mn1, do_mn mn2)
    | Pair (mn1, mn2) -> Pair (do_mn mn1, do_mn mn2)
    | Ref mn -> Ref (do_mn mn)
    | t -> t
  in
  (* Avoid replacing the whole type with This: *)
  if eq t !this then t else
  do_typ t

let uniq_id t =
  shrink t |>
  develop |>
  IO.to_string print_sorted |>
  Digest.string |>
  Digest.to_hex

(*
 * Some functional constructors:
 *)

let base x = Base x

let usr x = Usr x

let ext x = Ext x

let vec dim mn =
  if dim <= 0 then invalid_arg "vector" else Vec (dim, mn)

let lst mn = Lst mn

let set st mn = Set (st, mn)

let tup mns = Tup mns

let record mns = Rec mns

let sum mns = Sum mns

let map k v = Map (k, v)

let maybe_nullable typ ~nullable = { typ ; nullable }

let required = maybe_nullable ~nullable:false

let optional = maybe_nullable ~nullable:true

(* Can come handy: *)
let tuple = function
  | [||] -> invalid_arg "tuple"
  | [| x |] -> x
  | mns -> required (Tup mns)

let bit = required Bit
let byte = required Byte
let word = required Word
let dword = required DWord
let qword = required QWord
let oword = required OWord
let pair mn1 mn2 = required (Pair (mn1, mn2))
let address = required Address
let size = required Size
let ptr = required Ptr
let bytes = required Bytes
let mask = required Mask

let slist mn = required (SList mn)

let func ins out = required (Function (ins, out))
let func1 i1 out = required (Function ([| i1 |], out))
let func2 i1 i2 out = required (Function ([| i1 ; i2 |], out))
let func3 i1 i2 i3 out = required (Function ([| i1 ; i2 ; i3 |], out))
let func4 i1 i2 i3 i4 out = required (Function ([| i1 ; i2 ; i3 ; i4 |], out))

let ref mn = required (Ref mn)

(* Some short cuts for often used types: *)
let void = required Void
let bool = required (Base Bool)
let char = required (Base Char)
let string = required (Base String)
let float = required (Base Float)
let u8 = required (Base U8)
let u16 = required (Base U16)
let u24 = required (Base U24)
let u32 = required (Base U32)
let u40 = required (Base U40)
let u48 = required (Base U48)
let u56 = required (Base U56)
let u64 = required (Base U64)
let u128 = required (Base U128)
let i8 = required (Base I8)
let i16 = required (Base I16)
let i24 = required (Base I24)
let i32 = required (Base I32)
let i40 = required (Base I40)
let i48 = required (Base I48)
let i56 = required (Base I56)
let i64 = required (Base I64)
let i128 = required (Base I128)
(* nullable counterparts: *)
let nbool = optional (Base Bool)
let nchar = optional (Base Char)
let nstring = optional (Base String)
let nfloat = optional (Base Float)
let nu8 = optional (Base U8)
let nu16 = optional (Base U16)
let nu24 = optional (Base U24)
let nu32 = optional (Base U32)
let nu40 = optional (Base U40)
let nu48 = optional (Base U48)
let nu56 = optional (Base U56)
let nu64 = optional (Base U64)
let nu128 = optional (Base U128)
let ni8 = optional (Base I8)
let ni16 = optional (Base I16)
let ni24 = optional (Base I24)
let ni32 = optional (Base I32)
let ni40 = optional (Base I40)
let ni48 = optional (Base I48)
let ni56 = optional (Base I56)
let ni64 = optional (Base I64)
let ni128 = optional (Base I128)

let to_nullable mn =
  { mn with nullable = true }

let force mn =
  { mn with nullable = false }

(*
 * And destructors:
 *)

let pair_of_tpair = function
  | { typ = Pair (mn1, mn2) ; nullable = false } -> mn1, mn2
  | _ -> invalid_arg "pair_of_tpair"

let is_defined t =
  try
    iter (function Unknown -> raise Exit | _ -> ()) t ;
    true
  with Exit ->
    false

let rec is_integer = function
  | Unknown -> invalid_arg "is_integer"
  | Base (U8|U16|U24|U32|U40|U48|U56|U64|U128|
          I8|I16|I24|I32|I40|I48|I56|I64|I128) -> true
  | Usr { def ; _ } ->
      is_integer def
  | _ -> false

let is_numeric t =
  is_integer t || t = Base Float

(*
 * Registering User Types.
 *
 * Note that to actually create values of that type the constructor must
 * be registered also (see DessserExpressions.register_user_constructor).
 *)

let check t =
  iter (function
    (* TODO: also field names in a record *)
    | Sum mns ->
        Array.fold_left (fun s (n, _) ->
          if Set.String.mem n s then
            failwith "Constructor names not unique" ;
          Set.String.add n s
        ) Set.String.empty mns |>
        ignore
    | _ -> ()
  ) t

let register_user_type
    name ?(print : gen_printer option) ?(parse : t P.t option) def =
  if not (is_defined def) then invalid_arg "register_user_type" ;
  check def ;
  Hashtbl.modify_opt name (function
    | None ->
        let ut = { name ; def } in
        let print = match print with
          | Some printer ->
              printer.f
          | None ->
              fun oc ->
                default_user_type_printer ut oc in
        let def = { usr_typ = ut ; print ; parse = P.fail } in
        let parse = match parse with
          | Some p -> p
          | None -> Parser.(strinG name >>: fun () -> Usr ut) in
        def.parse <- parse ;
        Parser.user_type := Parser.(oneof !user_type parse) ;
        Some def
    | Some _ ->
        invalid_arg "register_user_type"
  ) user_types

let is_user_type_registered n =
  try ignore (get_user_type n) ; true
  with Not_found -> false
