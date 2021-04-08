open Batteries
open Stdint
open DessserTools

module PConfig = ParsersPositions.LineCol (Parsers.SimpleConfig (Char))
module P = Parsers.Make (PConfig)

let pp = Printf.fprintf

(* Identifies the backend implementation: *)

type backend_id = ..

(* Basic scalar types that can be used to define more specialized user types *)

type mac_type =
  | Float | String | Bool | Char
  | U8 | U16 | U24 | U32 | U40 | U48 | U56 | U64 | U128
  | I8 | I16 | I24 | I32 | I40 | I48 | I56 | I64 | I128

(* Machine-types are the basic types from which more specialized types can be
 * build. Those constructed types are called user-types.
 * User types specialize machine types in several ways: they have their own
 * name, pretty-printer, parser, and of course an implementation as the
 * corresponding machine type.
 * Notice that user types cannot be parameterized (yet). So only scalar user
 * types are possible for now.
 * User expressions can restrict types to user types. *)

and user_type =
  { name : string ;
    def : value_type }

(* Those types describing values that can be (de)serialized.
 * All of them can possibly be nullable.
 * User types are build from those value types. *)

and value_type =
  | Unknown
  | Unit
  | Mac of mac_type
  (* Aliases with custom representations: *)
  | Usr of user_type
  (* External types: *)
  | Ext of string
  (* Compound types: *)
  | Vec of int * maybe_nullable
  | Lst of maybe_nullable
  (* Special compound type amenable to incremental computation over sets.
   * We might have different implementations with different APIs, depending on
   * the use case (ie. the operator it is an operand of), for instance to
   * optimise FIFO updates, to keep it sorted, to skip nulls, etc... But the
   * backend will decide this on its own: *)
  | Set of maybe_nullable
  | Tup of maybe_nullable array
  (* Exact same as a tuple, but with field names that can be used as
   * accessors (also used to name actual fields in generated code): *)
  | Rec of (string * maybe_nullable) array
  (* Sum types, as a list of constructor and type. Constructor names uniqueness
   * will be checked at construction. *)
  | Sum of (string * maybe_nullable) array
  (* The type for maps exist because there will be some operations using
   * that type indirectly (such as fetching from a DB by key, or describing
   * a key->value mapping in a type expression). But there is no value of
   * that type, ever. From a (de)serialized point of view, maps are
   * equivalent to association lists. *)
  | Map of maybe_nullable * maybe_nullable

and maybe_nullable =
  { vtyp : value_type ; nullable : bool }

let make ?(nullable=false) vtyp =
  { vtyp ; nullable }

let required = make ~nullable:false

let optional = make ~nullable:true

(* Consider user types opaque by default, so that it matches DessserQCheck
 * generators. *)
let rec depth ?(opaque_user_type=true) = function
  | Unknown -> invalid_arg "depth"
  | Unit | Mac _ | Ext _ -> 0
  | Usr { def ; _ } ->
      if opaque_user_type then 0 else depth ~opaque_user_type def
  | Vec (_, mn) | Lst mn | Set mn ->
      1 + depth ~opaque_user_type mn.vtyp
  | Tup mns ->
      1 + Array.fold_left (fun d mn ->
        max d (depth ~opaque_user_type mn.vtyp)
      ) 0 mns
  | Rec mns | Sum mns ->
      1 + Array.fold_left (fun d (_, mn) ->
        max d (depth ~opaque_user_type mn.vtyp)
      ) 0 mns
  | Map (k, v) ->
      1 + max (depth ~opaque_user_type k.vtyp) (depth ~opaque_user_type v.vtyp)

(*$= depth & ~printer:string_of_int
  0 (depth (Mac U8))
  2 (depth (Tup [| required (Mac U8) ; required (Lst (required (Mac U8))) |]))
  7 (depth (\
    Lst (required (\
      Vec (4, (required (\
        Rec [| \
          "mgfhm", required (\
            Rec [| \
              "ceci", optional (Mac Char) ; \
              "zauxs", required (\
                Rec [| \
                  "gidf", required (\
                    Rec [| \
                      "qskv", optional (Mac Float) ; \
                      "lefr", required (Mac I16) ; \
                      "bdujmi", required (\
                        Vec (8, required (get_user_type "Cidr6"))) |]) ; \
                  "cdwcv", required (Mac U64) ; \
                  "jcdivs", required (\
                    Rec [| \
                      "hgtixf", required (Lst (optional (Mac I128))) ; \
                      "yuetd", required (Mac Char) ; \
                      "bsbff", required (Mac U16) |]) |]) |]) ; \
          "pudia", required (get_user_type "Cidr4") ; \
          "qngl", required (Mac Bool) ; \
          "iajv", optional (Mac I128) |])))))))
*)

(* In many occasions we want the items of a record to be deterministically
 * ordered so they can be compared etc: *)
let cmp_nv (n1, _) (n2, _) =
  String.compare n1 n2

let sorted_rec fields =
  let fields = Array.copy fields in
  Array.sort cmp_nv fields ;
  fields

let mac_type_eq mt1 mt2 = mt1 = mt2

let rec value_type_eq ?(opaque_user_type=false) vt1 vt2 =
  match vt1, vt2 with
  | Unit, Unit ->
      true
  | Mac mt1, Mac mt2 ->
      mac_type_eq mt1 mt2
  | Usr ut1, Usr ut2 ->
      ut1.name = ut2.name
  | Ext n1, Ext n2 ->
      n1 = n2
  | Vec (d1, mn1), Vec (d2, mn2) ->
      d1 = d2 && maybe_nullable_eq mn1 mn2
  | Lst mn1, Lst mn2
  | Set mn1, Set mn2 ->
      maybe_nullable_eq mn1 mn2
  | Tup mn1s, Tup mn2s ->
      Array.length mn1s = Array.length mn2s &&
      array_for_all2_no_exc maybe_nullable_eq mn1s mn2s
  | (Rec mn1s, Rec mn2s)
  | (Sum mn1s, Sum mn2s) ->
      Array.length mn1s = Array.length mn2s &&
      array_for_all2_no_exc (fun (n1, mn1) (n2, mn2) ->
        n1 = n2 && maybe_nullable_eq mn1 mn2
      ) (sorted_rec mn1s) (sorted_rec mn2s)
  | Map (k1, v1), Map (k2, v2) ->
      maybe_nullable_eq k1 k2 && maybe_nullable_eq v1 v2
  (* User types are lost in des/ser so we have to accept this: *)
  | Usr ut1, vt2 when not opaque_user_type ->
      value_type_eq ut1.def vt2
  | vt1, Usr ut2 when not opaque_user_type ->
      value_type_eq vt1 ut2.def
  | _ ->
      false

(*$T value_type_eq
  value_type_eq (get_user_type "Eth") (get_user_type "Eth")
*)

and maybe_nullable_eq mn1 mn2 =
  mn1.nullable = mn2.nullable && value_type_eq mn1.vtyp mn2.vtyp

let not_null mn =
  { mn with nullable = true }

let force_maybe_nullable mn =
  { mn with nullable = false }

(* To all the above types we add a few low-level types that can not be used
 * in values but are useful to manipulate them. *)
type t =
  | Value of maybe_nullable
  | Void
  (* DataPtr are used to point into the stream of bytes that's being
   * serialized into / deserialized from. The type of the value that's
   * being (de)serialized is kept nonetheless. *)
  | DataPtr
  (* A size in byte. *)
  | Size
  (* Data access, may be just pointer to the actual serialized object: *)
  | Bit
  | Byte
  | Word
  | DWord
  | QWord
  | OWord
  | Bytes
  (* Types for the runtime representation of a field mask: *)
  | Mask  (* What to do with a tree of fields *)
  | Pair of t * t
  (* We'd like the DES/SERializer to be able to use complex types as their
   * "pointer", part of those types being actual pointers. Therefore, we cannot
   * use the value-types, as we cannot embed a pointer in there. So here we
   * define another LIST which is a simple persistent list (as opposed to the
   * Value Lst, which is just an array in disguise and serve another purpose
   * (namely, to compactly store a bunch of read only values).
   * Therefore this one is named SList, as in "single-chaned" list, and is
   * written using "{}" instead of "[]". *)
  | SList of t
  | Function of t array * (* result: *) t

(* Helper to get the two types constituting a Pair: *)
let pair_of_tpair = function
  | Pair (t1, t2) -> t1, t2
  | _ -> invalid_arg "pair_of_tpair"

(*
 * Iterators
 *)

let rec develop_value_type = function
  | (Unknown | Unit | Mac _ | Ext _) as vt ->
      vt
  | Usr { def ; _ } ->
      develop_value_type def
  | Vec (d, mn) ->
      Vec (d, develop_maybe_nullable mn)
  | Lst mn ->
      Lst (develop_maybe_nullable mn)
  | Set mn ->
      Set (develop_maybe_nullable mn)
  | Tup mns ->
      Tup (Array.map develop_maybe_nullable mns)
  | Rec mns ->
      Rec (Array.map (fun (n, mn) -> n, develop_maybe_nullable mn) mns)
  | Sum cs ->
      Sum (Array.map (fun (n, mn) -> n, develop_maybe_nullable mn) cs)
  | Map (mn1, mn2) ->
      Map (develop_maybe_nullable mn1, develop_maybe_nullable mn2)

and develop_maybe_nullable mn =
  { mn with vtyp = develop_value_type mn.vtyp }

and develop_user_types = function
  | Value mn -> Value (develop_maybe_nullable mn)
  | t -> t

(* Top-down folding of a value_type: *)
(* FIXME: either consider Usr types as opaque and stop the recursion, or as
 * transparent and do not call [f] on Usr: *)
let rec fold_value_type u f vt =
  let u = f u vt in
  match vt with
  | Unknown | Unit | Mac _ | Ext _ ->
      u
  | Usr { def ; _ } ->
      fold_value_type u f def
  | Vec (_, mn) | Lst mn | Set mn ->
      fold_maybe_nullable u f mn
  | Tup mns ->
      Array.fold_left (fun u mn -> fold_maybe_nullable u f mn) u mns
  | Rec mns | Sum mns ->
      Array.fold_left (fun u (_, mn) -> fold_maybe_nullable u f mn) u mns
  | Map (k, v) ->
      fold_maybe_nullable (fold_maybe_nullable u f k) f v

and fold_maybe_nullable u f mn =
  fold_value_type u f mn.vtyp

let iter_value_type f vt =
  fold_value_type () (fun () vt -> f vt) vt

let iter_maybe_nullable f mn =
  fold_maybe_nullable () (fun () mn -> f mn) mn

(*
 * Tools
 *)

let is_defined vt =
  try
    iter_value_type (function
      | Unknown -> raise Exit
      | _ -> ()
    ) vt ;
    true
  with Exit ->
    false

let check vt =
  iter_value_type (function
    | Sum mns ->
        Array.fold_left (fun s (n, _) ->
          if Set.String.mem n s then
            failwith "Constructor names not unique" ;
          Set.String.add n s
        ) Set.String.empty mns |>
        ignore
    | _ -> ()
  ) vt

let rec is_integer = function
  | Unknown -> invalid_arg "is_integer"
  | Mac (U8|U16|U24|U32|U40|U48|U56|U64|U128|
         I8|I16|I24|I32|I40|I48|I56|I64|I128) -> true
  | Usr { def ; _ } ->
      is_integer def
  | _ -> false

let is_numeric vt =
  is_integer vt || vt = Mac Float

let is_nullable = function
  | Value { nullable = true ; _ } -> true
  | _ -> false

let width_of_int = function
  | Mac (U8 | I8) -> 8
  | Mac (U16 | I16) -> 16
  | Mac (U24 | I24) -> 24
  | Mac (U32 | I32) -> 32
  | Mac (U40 | I40) -> 40
  | Mac (U48 | I48) -> 48
  | Mac (U56 | I56) -> 56
  | Mac (U64 | I64) -> 64
  | Mac (U128 | I128) -> 128
  | _ ->
      invalid_arg "width_of_int"


(*
 * User-defined types
 *)

type user_type_def =
  { typ : user_type ;
    print : 'a. 'a IO.output -> unit ;
    mutable parse : value_type P.t }

let user_types : (string, user_type_def) Hashtbl.t = Hashtbl.create 50

type gen_printer = { f : 'a. 'a IO.output -> unit }

let default_user_type_printer ut oc =
  String.print oc ut.name

let get_user_type n =
  Usr (Hashtbl.find user_types n).typ

(* See below after Parser definition for [register_user_type] and examples *)

(*
 * Printers
 *)

let print_mac_type oc =
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

let rec print_value_type ?(sorted=false) oc = function
  | Unknown ->
      pp oc "UNKNOWN"
  | Unit ->
      pp oc "unit"
  | Mac t ->
      print_mac_type oc t
  | Ext n ->
      pp oc "%s" n
  | Usr t ->
      (match Hashtbl.find user_types t.name with
      | exception Not_found ->
          default_user_type_printer t oc
      | def ->
          def.print oc)
  | Vec (dim, mn) ->
      pp oc "%a[%d]" (print_maybe_nullable ~sorted) mn dim
  | Lst mn ->
      pp oc "%a[]" (print_maybe_nullable ~sorted) mn
  | Set mn ->
      pp oc "%a{}" (print_maybe_nullable ~sorted) mn
  | Tup mns ->
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:"; "
          (print_maybe_nullable ~sorted)) mns
  | Rec mns ->
      (* When the string repr is used to identify the type (see BackEndCLike)
       * every equivalent record types must then be printed the same, thus the
       * optional sort: *)
      pp oc "%a"
        (Array.print ~first:"{" ~last:"}" ~sep:"; "
          (fun oc (n, mn) ->
            pp oc "%s: %a" n (print_maybe_nullable ~sorted) mn)
        ) (if sorted then sorted_rec mns else mns)
  | Sum cs ->
      (* Parenthesis are required to distinguish external from internal
       * nullable: *)
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:" | "
          (fun oc (n, mn) ->
            pp oc "%s %a" n (print_maybe_nullable ~sorted) mn)
        ) (if sorted then sorted_rec cs else cs)
  | Map (k, v) ->
      pp oc "%a[%a]"
        (print_maybe_nullable ~sorted) v
        (print_maybe_nullable ~sorted) k

and print_maybe_nullable ?sorted oc mn =
  pp oc "%a%s"
    (print_value_type ?sorted) mn.vtyp
    (if mn.nullable then "?" else "")

let to_maybe_nullable = function
  | Value mn -> mn
  | _ -> invalid_arg "to_maybe_nullable"

let rec eq t1 t2 =
  match t1, t2 with
  | Value mn1, Value mn2 ->
      maybe_nullable_eq mn1 mn2
  | Pair (t11, t12), Pair (t21, t22) ->
      eq t11 t21 && eq t12 t22
  | SList t1, SList t2 ->
      eq t1 t2
  | Function (pt1, rt1), Function (pt2, rt2) ->
      array_for_all2_no_exc eq pt1 pt2 && eq rt1 rt2
  | t1, t2 ->
      t1 = t2

(*$T eq
  eq unit unit
  eq (Value (required (get_user_type "Eth"))) \
     (Value (required (get_user_type "Eth"))) ;
  eq (Function ([| Value (required (get_user_type "Eth")) ; Size |], unit)) \
     (Function ([| Value (required (get_user_type "Eth")) ; Size |], unit))
*)

let rec print ?sorted oc =
  let sp = String.print oc in
  function
  | Value vt ->
      print_maybe_nullable oc ?sorted vt
  | Void -> sp "Void"
  | DataPtr -> sp "DataPtr"
  | Size -> sp "Size"
  | Bit -> sp "Bit"
  | Byte -> sp "Byte"
  | Word -> sp "Word"
  | DWord -> sp "DWord"
  | QWord -> sp "QWord"
  | OWord -> sp "OWord"
  | Bytes -> sp "Bytes"
  | Mask -> sp "Mask"
  | Pair (t1, t2) ->
      pp oc "(%a * %a)"
        (print ?sorted) t1
        (print ?sorted) t2
  | SList t1 ->
      pp oc "%a{}" (print ?sorted) t1
  | Function ([||], t1) ->
      pp oc "( -> %a)" (print ?sorted) t1
  | Function (ts, t2) ->
      pp oc "(%a -> %a)"
        (Array.print ~first:"" ~last:"" ~sep:" -> " (print ?sorted)) ts
        (print ?sorted) t2

let print_sorted oc = print ~sorted:true oc
let print oc = print ~sorted:false oc
let print_maybe_nullable_sorted oc = print_maybe_nullable ~sorted:true oc
let print_maybe_nullable oc = print_maybe_nullable ~sorted:false oc
let print_value_type_sorted oc = print_value_type ~sorted:true oc
let print_value_type oc = print_value_type ~sorted:false oc

let string_of_value_type = IO.to_string print_value_type
let string_of_maybe_nullable = IO.to_string print_maybe_nullable
let to_string = IO.to_string print

let uniq_id t =
  IO.to_string print_sorted (develop_user_types t) |>
  Digest.string |>
  Digest.to_hex

let to_nullable = function
  | Value { vtyp ; nullable = false } ->
      Value { vtyp ; nullable = true }
  | t ->
      Printf.eprintf "Cannot turn type %a into nullable\n%!"
        print t ;
      assert false

let force = function
  | Value { vtyp ; nullable = true } ->
      Value { vtyp ; nullable = false }
  | t ->
      Printf.eprintf "Cannot turn type %a into not-nullable\n%!"
        print t ;
      assert false

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
    VecDim of int | ListDim | SetDim | MapKey of maybe_nullable

  let rec reduce_dims t = function
    | [] -> t
    | (VecDim d, nullable) :: rest ->
        reduce_dims (make ~nullable (Vec (d, t))) rest
    | (ListDim, nullable) :: rest ->
        reduce_dims (make ~nullable (Lst t)) rest
    | (SetDim, nullable) :: rest ->
        reduce_dims (make ~nullable (Set t)) rest
    | (MapKey k, nullable) :: rest ->
        reduce_dims (make ~nullable (Map (k, t))) rest

  let rec key_type m =
    let vec_dim m =
      let m = "vector dimension" :: m in
      (
        char '[' -- opt_blanks -+
        pos_integer "vector dimensions" +-
        opt_blanks +- char ']' ++
        opt_question_mark >>: fun (d, n) ->
          if d <= 0 then
            raise (Reject "Vector must have strictly positive dimension") ;
          VecDim d, n
      ) m
    and list_dim m =
      let m = "list type" :: m in
      (
        char '[' -- opt_blanks -- char ']' -+
        opt_question_mark >>: fun n ->
          ListDim, n
      ) m
    and set_dim m =
      let m = "set type" :: m in
      (
        char '{' -- opt_blanks -- char '}' -+
        opt_question_mark >>: fun n ->
          SetDim, n
      ) m
    and map_key m =
      let m = "map key" :: m in
      (
        char '[' -- opt_blanks -+
          maybe_nullable +- opt_blanks +- char ']' ++
        opt_question_mark >>: fun (k, n) ->
          MapKey k, n
      ) m
    in
    (
      vec_dim ||| list_dim ||| set_dim ||| map_key
    ) m

  and maybe_nullable m =
    let m = "type" :: m in
    (
      (
        unit_typ ||| scalar_typ ||| tuple_typ ||| record_typ ||| sum_typ |||
        (!user_type ++ opt_question_mark >>: fun (vt, n) -> make ~nullable:n vt)
      ) ++
      repeat ~sep:opt_blanks (key_type) >>: fun (t, dims) ->
        reduce_dims t dims
    ) m

  and unit_typ m =
    let m = "unit type" :: m in
    (
      (strinG "unit" >>: fun () -> required Unit) |||
      (strinG "unit?" >>: fun () -> make ~nullable:true Unit)
    ) m

  and scalar_typ m =
    let m = "scalar type" :: m in
    let st n mtyp =
      let vtyp = Mac mtyp in
      (strinG (n ^"?") >>: fun () -> make ~nullable:true vtyp) |||
      (strinG n >>: fun () -> required vtyp)
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
        several ~sep:tup_sep maybe_nullable
      +- opt_blanks +- char ')' ++
      opt_question_mark >>: fun (ts, nullable) ->
        make ~nullable (Tup (Array.of_list ts))
    ) m

  and record_typ m =
    let m = "record type" :: m in
    let field_typ =
      identifier +- opt_blanks +- char ':' +- opt_blanks ++ maybe_nullable in
    (
      char '{' -- opt_blanks -+
        several ~sep:tup_sep field_typ +-
        opt_blanks +- optional ~def:() (char ';' -- opt_blanks) +-
      char '}' ++
      opt_question_mark >>: fun (ts, nullable) ->
        (* TODO: check that all field names are distinct *)
        make ~nullable (Rec (Array.of_list ts))
    ) m

  and sum_typ m =
    let m = "sum type" :: m in
    let constructor m =
      let m = "constructor" :: m in
      (
        identifier ++
        optional ~def:(required Unit) (!blanks -+ maybe_nullable)
      ) m
    and sep =
      opt_blanks -- char '|' -- opt_blanks in
    (
      char '(' -- opt_blanks -+
      several ~sep constructor +-
      opt_blanks +- char ')' ++ opt_question_mark >>:
        fun (ts, nullable) ->
          (* TODO: check that all constructors are case insensitively distinct *)
          make ~nullable (Sum (Array.of_list ts))
    ) m

  let value_type m =
    let m = "value type" :: m in
    (
      maybe_nullable >>: fun mn ->
        if mn.nullable then raise (Reject "must not be nullable") ;
        mn.vtyp
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

  (*$= maybe_nullable & ~printer:(test_printer print_maybe_nullable)
    (Ok ((required (Mac U8)), (2,[]))) \
       (test_p maybe_nullable "u8")
    (Ok ((optional (Mac U8)), (3,[]))) \
       (test_p maybe_nullable "u8?")
    (Ok ((required (Vec (3, (required (Mac U8))))), (5,[]))) \
       (test_p maybe_nullable "u8[3]")
    (Ok ((required (Vec (3, (optional (Mac U8))))), (6,[]))) \
       (test_p maybe_nullable "u8?[3]")
    (Ok ((optional (Vec (3, (required (Mac U8))))), (6,[]))) \
       (test_p maybe_nullable "u8[3]?")
    (Ok ((required (Vec (3, (optional (Lst (required (Mac U8))))))), (8,[]))) \
       (test_p maybe_nullable "u8[]?[3]")
    (Ok ((optional (Lst (required (Vec (3, (optional (Mac U8))))))), (9,[]))) \
       (test_p maybe_nullable "u8?[3][]?")
    (Ok ((optional (Map ((required (Mac String)), (required (Mac U8))))), (11,[]))) \
       (test_p maybe_nullable "u8[string]?")
    (Ok ((required (Map ((required (Map ((optional (Mac U8)), (optional (Mac String))))), (optional (Lst ((required (Tup [| (required (Mac U8)) ; (required (Map ((required (Mac String)), (required (Mac Bool))))) |])))))))), (35,[]))) \
       (test_p maybe_nullable "(u8; bool[string])[]?[string?[u8?]]")
    (Ok ((required (Rec [| "f1", required (Mac Bool) ; "f2", optional (Mac U8) |])), (19,[]))) \
      (test_p maybe_nullable "{f1: Bool; f2: U8?}")
    (Ok ((required (Rec [| "f2", required (Mac Bool) ; "f1", optional (Mac U8) |])), (19,[]))) \
      (test_p maybe_nullable "{f2: Bool; f1: U8?}")
    (Ok ((required (Sum [| "c1", required (Mac Bool) ; "c2", optional (Mac U8) |])), (18,[]))) \
      (test_p maybe_nullable "(c1 Bool | c2 U8?)")
    (Ok ((required (Vec (1, required (Mac Bool)))), (7,[]))) \
      (test_p maybe_nullable "Bool[1]")
  *)

  let rec typ m =
    let m = "type" :: m in
    (
      (maybe_nullable >>: fun mn -> Value mn) |<|
      (strinG "void" >>: fun () -> Void) |<|
      (strinG "dataptr" >>: fun () -> DataPtr) |<|
      (strinG "size" >>: fun () -> Size) |<|
      (strinG "bit" >>: fun () -> Bit) |<|
      (strinG "byte" >>: fun () -> Byte) |<|
      (strinG "word" >>: fun () -> Word) |<|
      (strinG "dword" >>: fun () -> DWord) |<|
      (strinG "qword" >>: fun () -> QWord) |<|
      (strinG "oword" >>: fun () -> OWord) |<|
      (strinG "bytes" >>: fun () -> Bytes) |<|
      (strinG "mask" >>: fun () -> Mask) |<|
      (
        char '(' -- opt_blanks -+ typ +- opt_blanks +-
        char '*' +- opt_blanks ++ typ +- opt_blanks +- char ')' >>:
          fun (t1, t2) -> Pair (t1, t2)
      ) |<|
      (
        char '{' -- opt_blanks -+ typ +- opt_blanks +- char '}' >>:
          fun t -> SList t
      ) |<|
      (
        let sep = opt_blanks -- char '-' -- char '>' -- opt_blanks in
        char '(' -+
          repeat ~sep typ +- sep ++ typ +- opt_blanks +-
        char ')' >>: fun (ptyps, rtyp) ->
          Function (Array.of_list ptyps, rtyp)
      )
    ) m

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
        (iD "UInt8" >>: fun () -> required (Mac U8)) |<|
        (iD "UInt16" >>: fun () -> required (Mac U16)) |<|
        (iD "UInt32" >>: fun () -> required (Mac U32)) |<|
        (iD "UInt64" >>: fun () -> required (Mac U64)) |<|
        ((iD "Int8" |<| iD "TINYINT") >>:
          fun () -> required (Mac I8)) |<|
        ((iD "Int16" |<| iD "SMALLINT") >>:
          fun () -> required (Mac I16)) |<|
        ((iD "Int32" |<| iD "INTEGER" |<| iD "INT") >>:
          fun () -> required (Mac I32)) |<|
        ((iD "Int64" |<| iD "BIGINT") >>:
          fun () -> required (Mac I64)) |<|
        ((iD "Float32" |<| iD "Float64" |<|
          iD "FLOAT" |<| iD "DOUBLE") >>:
          fun () -> required (Mac Float)) |<|
        (* Assuming UUIDs are just plain U128 with funny-printing: *)
        (iD "UUID" >>: fun () -> required (Mac U128)) |<|
        (* Decimals: for now forget about the size of the decimal part,
         * just map into corresponding int type*)
        (with_num_param "Decimal32" >>: fun _p -> required (Mac I32)) |<|
        (with_num_param "Decimal64" >>: fun _p -> required (Mac I64)) |<|
        (with_num_param "Decimal128" >>: fun _p -> required (Mac I128)) |<|
        (* TODO: actually do something with the size: *)
        ((with_2_num_params "Decimal" |<| with_2_num_params "DEC") >>:
          fun (_n, _m)  -> required (Mac I128)) |<|
        ((iD "DateTime" |<| iD "TIMESTAMP") >>:
          fun () -> required (Mac U32)) |<|
        (iD "Date" >>: fun () -> required (Mac U16)) |<|
        ((iD "String" |<| iD "CHAR" |<| iD "VARCHAR" |<|
          iD "TEXT" |<| iD "TINYTEXT" |<| iD "MEDIUMTEXT" |<|
          iD "LONGTEXT" |<| iD "BLOB" |<| iD "TINYBLOB" |<|
          iD "MEDIUMBLOB" |<| iD "LONGBLOB") >>:
          fun () -> required (Mac String)) |<|
        ((with_num_param "FixedString" |<| with_num_param "BINARY") >>:
          fun d -> required (Vec (d, required (Mac Char)))) |<|
        (with_typ_param "Nullable" >>: not_null) |<|
        (with_typ_param "Array" >>: fun mn -> required (Lst mn)) |<|
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
        Value { nullable = false ; vtyp = Rec mns }
    ) m

  (*$= clickhouse_names_and_types & ~printer:(test_printer print)
    (Ok (Value (required (Rec [| "thing", required (Mac U16) |])), (14,[]))) \
       (test_p clickhouse_names_and_types "`thing` UInt16")

    (Ok (Value (required (Rec [| "thing", required (Lst (required (Mac U16))) |])), (21,[]))) \
       (test_p clickhouse_names_and_types "`thing` Array(UInt16)")
  *)

  (* If [any_format] then any known format to specify types will be tried.
   * If not then only dessser own format will be tried (faster, esp when
   * parsing DIL s-expressions) *)
  let of_string ?(any_format=false) ?what =
    let p =
      if any_format then typ ||| clickhouse_names_and_types
      else typ in
    string_parser ~print ?what p

  (*$>*)
end

let maybe_nullable_of_string ?what =
  let print = print_maybe_nullable in
  Parser.(string_parser ~print ?what maybe_nullable)

let value_type_of_string ?what =
  let print = print_value_type in
  Parser.(string_parser ~print ?what value_type)

let register_user_type
    name ?(print : gen_printer option) ?(parse : value_type P.t option) def =
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
        let def = { typ = ut ; print ; parse = P.fail } in
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

(* Examples: *)
let () =
  register_user_type "Date" (Mac Float) ;
  register_user_type "Eth" (Mac U48) ;
  register_user_type "Ip4" (Mac U32) ;
  register_user_type "Ip6" (Mac U128) ;
  register_user_type "Ip"
    (* Note: for simplicity, make sure all constructor names are unique.
     * Also, start by a lowercase or a "v_" will be prepended needlessly: *)
    (Sum [| "v4", required (get_user_type "Ip4") ;
            "v6", required (get_user_type "Ip6") |]) ;
  register_user_type "Cidr4" (Rec [| "ip", required (get_user_type "Ip4") ;
                                     "mask", required (Mac U8) |]) ;
  register_user_type "Cidr6" (Rec [| "ip", required (get_user_type "Ip6") ;
                                     "mask", required (Mac U8) |]) ;
  register_user_type "Cidr"
    (Sum [| "v4", required (get_user_type "Cidr4") ;
            "v6", required (get_user_type "Cidr6") |])

(* Paths are used to locate subfield types within compound types.
 * Head of the list is the index of the considered type child, then
 * the index of the grandchild, and so on.
 * Lists and vectors are entered but need only one index: 0, as all
 * indices share the same subtype. *)
(* FIXME: a data structure that can be appended/prepended/matched from both ends *)
type path = int list

let path_append i path = path @ [i]

let print_path oc p =
  List.print ~first:"/" ~last:"" ~sep:"/" Int.print oc p

let path_of_string s =
  if s = "" || s.[0] <> '/' then
    Printf.sprintf "%S" s |> invalid_arg ;
  String.lchop s |>
  String.split_on_char '/' |>
  List.map int_of_string

let string_of_path p =
  "/" ^ (List.map string_of_int p |> String.join "/")

(* Return both the type and field name: *)
let type_and_name_of_path t path =
  let rec loop field_name t = function
    | [] -> t, field_name
    | i :: path ->
        let rec type_of = function
          | (Unknown | Unit | Mac _ | Ext _ | Map _) ->
              assert false
          | Usr t ->
              type_of t.def
          | Vec (dim, mn) ->
              assert (i < dim) ;
              loop (string_of_int i) mn path
          | Lst mn ->
              loop (string_of_int i) mn path
          | Set mn ->
              loop (string_of_int i) mn path
          | Tup mns ->
              assert (i < Array.length mns) ;
              loop ("field_"^ string_of_int i) mns.(i) path
          | Rec mns ->
              assert (i < Array.length mns) ;
              loop (fst mns.(i)) (snd mns.(i)) path
          | Sum cs ->
              assert (i < Array.length cs) ;
              loop (fst cs.(i)) (snd cs.(i)) path in
        type_of t.vtyp in
  loop "" t path

let type_of_path mn path =
  fst (type_and_name_of_path mn path)

let type_of_parent mn path =
  let path = list_drop_last path in
  type_of_path mn path

(*$inject
  let test_t = required (Tup [|
    required (Mac U8) ;
    optional (Mac String) ;
    optional (Vec (2, required (Mac Char))) |])
*)

(*$= type_of_path & ~printer:(BatIO.to_string print_maybe_nullable)
  test_t (type_of_path test_t [])
  (required (Mac U8)) (type_of_path test_t [0])
  (optional (Mac String)) (type_of_path test_t [1])
  (optional (Vec (2, required (Mac Char)))) (type_of_path test_t [2])
  (required (Mac Char)) (type_of_path test_t [2; 0])
*)

(* Some short cuts for often used types: *)

let bool = Value (required (Mac Bool))
let char = Value (required (Mac Char))
let nstring = Value (optional (Mac String))
let string = Value (required (Mac String))
let float = Value (required (Mac Float))
let u8 = Value (required (Mac U8))
let u16 = Value (required (Mac U16))
let u24 = Value (required (Mac U24))
let u32 = Value (required (Mac U32))
let u40 = Value (required (Mac U40))
let u48 = Value (required (Mac U48))
let u56 = Value (required (Mac U56))
let u64 = Value (required (Mac U64))
let u128 = Value (required (Mac U128))
let i8 = Value (required (Mac I8))
let i16 = Value (required (Mac I16))
let i24 = Value (required (Mac I24))
let i32 = Value (required (Mac I32))
let i40 = Value (required (Mac I40))
let i48 = Value (required (Mac I48))
let i56 = Value (required (Mac I56))
let i64 = Value (required (Mac I64))
let i128 = Value (required (Mac I128))
let void = Void
let unit = Value (required Unit)
let bit = Bit
let byte = Byte
let size = Size
let word = Word
let dword = DWord
let qword = QWord
let oword = OWord
let bytes = Bytes
let mask = Mask
let dataptr = DataPtr
let pair t1 t2 = Pair (t1, t2)
let slist t = SList t
let list mn = Value (required (Lst mn))
let tuple mns =
  match Array.length mns with
  | 0 -> invalid_arg "tuple"
  | 1 -> Value mns.(0)
  | _ -> Value (required (Tup mns))
let set mn = Value (required (Set mn))
let vector d mn = Value (required (Vec (d, mn)))
