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
  | TFloat | TString | TBool | TChar
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128

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
  | Mac of mac_type
  (* Aliases with custom representations: *)
  | Usr of user_type
  (* Compound types: *)
  | TVec of int * maybe_nullable
  | TList of maybe_nullable
  | TTup of maybe_nullable array
  (* Exact same as a tuple, but with field names that can be used as
   * accessors (also used to name actual fields in generated code): *)
  | TRec of (string * maybe_nullable) array
  (* Sum types, as a list of constructor and type. Constructor names uniqueness
   * will be checked at construction. *)
  | TSum of (string * maybe_nullable) array
  (* The type for maps exist because there will be some operations using
   * that type indirectly (such as fetching from a DB by key, or describing
   * a key->value mapping in a type expression). But there is no value of
   * that type, ever. From a (de)serialized point of view, maps are
   * equivalent to association lists. *)
  | TMap of maybe_nullable * maybe_nullable

and maybe_nullable =
  { vtyp : value_type ; nullable : bool }

let make ?(nullable=false) vtyp =
  { vtyp ; nullable }

let maken = make ~nullable:true

(* In many occasions we want the items of a record to be deterministically
 * ordered so they can be compared etc: *)
let cmp_nv (n1, _) (n2, _) =
  String.compare n1 n2

let sorted_rec fields =
  let fields = Array.copy fields in
  Array.sort cmp_nv fields ;
  fields

let rec value_type_eq ?(opaque_user_type=false) vt1 vt2 =
  match vt1, vt2 with
  | Mac mt1, Mac mt2 ->
      mt1 = mt2
  | Usr ut1, Usr ut2 ->
      ut1.name = ut2.name
  | TVec (d1, mn1), TVec (d2, mn2) ->
      d1 = d2 && maybe_nullable_eq mn1 mn2
  | TList mn1, TList mn2 ->
      maybe_nullable_eq mn1 mn2
  | TTup mn1s, TTup mn2s ->
      Array.length mn1s = Array.length mn2s &&
      Array.for_all2 maybe_nullable_eq mn1s mn2s
  | (TRec mn1s, TRec mn2s)
  | (TSum mn1s, TSum mn2s) ->
      Array.length mn1s = Array.length mn2s &&
      Array.for_all2 (fun (n1, mn1) (n2, mn2) ->
        n1 = n2 && maybe_nullable_eq mn1 mn2
      ) (sorted_rec mn1s) (sorted_rec mn2s)
  | TMap (k1, v1), TMap (k2, v2) ->
      maybe_nullable_eq k1 k2 && maybe_nullable_eq v1 v2
  (* User types are lost in des/ser so we have to accept this: *)
  | Usr ut1, vt2 when not opaque_user_type ->
      value_type_eq ut1.def vt2
  | vt1, Usr ut2 when not opaque_user_type ->
      value_type_eq vt1 ut2.def
  | _ ->
      false

and maybe_nullable_eq mn1 mn2 =
  mn1.nullable = mn2.nullable && value_type_eq mn1.vtyp mn2.vtyp

let maybe_nullable_to_nullable mn =
  { mn with nullable = true }

let maybe_nullable_to_not_nullable mn =
  { mn with nullable = false }

(* To all the above types we add a few low-level types that can not be used
 * in values but are useful to manipulate them. *)
type t =
  | TValue of maybe_nullable
  | TVoid
  (* DataPtr are used to point into the stream of bytes that's being
   * serialized into / deserialized from. The type of the value that's
   * being (de)serialized is kept nonetheless. *)
  | TDataPtr
  (* A size in byte. *)
  | TSize
  (* Data access, may be just pointer to the actual serialized object: *)
  | TBit
  | TByte
  | TWord
  | TDWord
  | TQWord
  | TOWord
  | TBytes
  (* Types for the runtime representation of a field mask: *)
  | TMask  (* What to do with a tree of fields *)
  | TMaskAction (* What to do with an individual field *)
  | TPair of t * t
  (* We'd like the DES/SERializer to be able to use complex types as their
   * "pointer", part of those types being actual pointers. Therefore, we cannot
   * use the value-types, as we cannot embed a pointer in there. So here we
   * define another TLIST which is a simple persistent list (as opposed to the
   * TValue TList, which is just an array in disguise and serve another purpose
   * (namely, to compactly store a bunch of read only values).
   * Therefore this one is named TSList, as in "single-chaned" list, and is
   * written using "{}" instead of "[]". *)
  | TSList of t
  | TFunction of t array * (* result: *) t

(*
 * Iterators
 *)

let rec develop_value_type = function
  | (Unknown | Mac _) as vt ->
      vt
  | Usr { def ; _ } ->
      develop_value_type def
  | TVec (d, mn) ->
      TVec (d, develop_maybe_nullable mn)
  | TList mn ->
      TList (develop_maybe_nullable mn)
  | TTup mns ->
      TTup (Array.map develop_maybe_nullable mns)
  | TRec mns ->
      TRec (Array.map (fun (n, mn) -> n, develop_maybe_nullable mn) mns)
  | TSum cs ->
      TSum (Array.map (fun (n, mn) -> n, develop_maybe_nullable mn) cs)
  | TMap (mn1, mn2) ->
      TMap (develop_maybe_nullable mn1, develop_maybe_nullable mn2)

and develop_maybe_nullable mn =
  { mn with vtyp = develop_value_type mn.vtyp }

and develop_user_types = function
  | TValue mn -> TValue (develop_maybe_nullable mn)
  | t -> t

(* Top-down folding of a value_type: *)
let rec fold_value_type u f vt =
  let u = f u vt in
  match vt with
  | Unknown | Mac _ ->
      u
  | Usr { def ; _ } ->
      fold_value_type u f def
  | TVec (_, mn) | TList mn ->
      fold_maybe_nullable u f mn
  | TTup mns ->
      Array.fold_left (fun u mn -> fold_maybe_nullable u f mn) u mns
  | TRec mns | TSum mns ->
      Array.fold_left (fun u (_, mn) -> fold_maybe_nullable u f mn) u mns
  | TMap (k, v) ->
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
    | TSum mns ->
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
  | Mac (TU8|TU16|TU24|TU32|TU40|TU48|TU56|TU64|TU128|
         TI8|TI16|TI24|TI32|TI40|TI48|TI56|TI64|TI128) -> true
  | Usr { def ; _ } ->
      is_integer def
  | _ -> false

let is_numeric vt =
  is_integer vt || vt = Mac TFloat

let width_of_int = function
  | Mac (TU8 | TI8) -> 8
  | Mac (TU16 | TI16) -> 16
  | Mac (TU24 | TI24) -> 24
  | Mac (TU32 | TI32) -> 32
  | Mac (TU40 | TI40) -> 40
  | Mac (TU48 | TI48) -> 48
  | Mac (TU56 | TI56) -> 56
  | Mac (TU64 | TI64) -> 64
  | Mac (TU128 | TI128) -> 128
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
  (Hashtbl.find user_types n).typ

(* See below after Parser definition for [register_user_type] and examples *)

(*
 * Printers
 *)

let print_mac_type oc =
  let sp = String.print oc in
  function
  | TFloat -> sp "FLOAT"
  | TString -> sp "STRING"
  | TBool -> sp "BOOL"
  | TChar -> sp "CHAR"
  | TU8 -> sp "U8"
  | TU16 -> sp "U16"
  | TU24 -> sp "U24"
  | TU32 -> sp "U32"
  | TU40 -> sp "U40"
  | TU48 -> sp "U48"
  | TU56 -> sp "U56"
  | TU64 -> sp "U64"
  | TU128 -> sp "U128"
  | TI8 -> sp "I8"
  | TI16 -> sp "I16"
  | TI24 -> sp "I24"
  | TI32 -> sp "I32"
  | TI40 -> sp "I40"
  | TI48 -> sp "I48"
  | TI56 -> sp "I56"
  | TI64 -> sp "I64"
  | TI128 -> sp "I128"

let rec print_value_type ?(sorted=false) oc = function
  | Unknown ->
      pp oc "UNKNOWN"
  | Mac t ->
      print_mac_type oc t
  | Usr t ->
      (match Hashtbl.find user_types t.name with
      | exception Not_found ->
          default_user_type_printer t oc
      | def ->
          def.print oc)
  | TVec (dim, vt) ->
      pp oc "%a[%d]" (print_maybe_nullable ~sorted) vt dim
  | TList vt ->
      pp oc "%a[]" (print_maybe_nullable ~sorted) vt
  | TTup vts ->
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:"; "
          (print_maybe_nullable ~sorted)) vts
  | TRec vts ->
      (* When the string repr is used to identify the type (see BackEndCLike)
       * every equivalent record types must then be printed the same, thus the
       * optional sort: *)
      pp oc "%a"
        (Array.print ~first:"{" ~last:"}" ~sep:"; "
          (fun oc (n, mn) ->
            pp oc "%s: %a" n (print_maybe_nullable ~sorted) mn)
        ) (if sorted then sorted_rec vts else vts)
  | TSum cs ->
      (* Parenthesis are required to distinguish external from internal
       * nullable: *)
      pp oc "%a"
        (Array.print ~first:"(" ~last:")" ~sep:" | "
          (fun oc (n, mn) ->
            pp oc "%s %a" n (print_maybe_nullable ~sorted) mn)
        ) (if sorted then sorted_rec cs else cs)
  | TMap (k, v) ->
      pp oc "%a[%a]"
        (print_maybe_nullable ~sorted) v
        (print_maybe_nullable ~sorted) k

and print_maybe_nullable ?sorted oc mn =
  pp oc "%a%s"
    (print_value_type ?sorted) mn.vtyp
    (if mn.nullable then "?" else "")

let to_maybe_nullable = function
  | TValue mn -> mn
  | _ -> invalid_arg "to_maybe_nullable"

let rec eq t1 t2 =
  match t1, t2 with
  | TValue mn1, TValue mn2 ->
      maybe_nullable_eq mn1 mn2
  | TPair (t11, t12), TPair (t21, t22) ->
      eq t11 t21 && eq t12 t22
  | TSList t1, TSList t2 ->
      eq t1 t2
  | TFunction (pt1, rt1), TFunction (pt2, rt2) ->
      Array.for_all2 eq pt1 pt2 && eq rt1 rt2
  | t1, t2 -> t1 = t2

let rec print ?sorted oc =
  let sp = String.print oc in
  function
  | TValue vt ->
      print_maybe_nullable oc ?sorted vt
  | TVoid -> sp "Void"
  | TDataPtr -> sp "DataPtr"
  | TSize -> sp "Size"
  | TBit -> sp "Bit"
  | TByte -> sp "Byte"
  | TWord -> sp "Word"
  | TDWord -> sp "DWord"
  | TQWord -> sp "QWord"
  | TOWord -> sp "OWord"
  | TBytes -> sp "Bytes"
  | TMask -> sp "Mask"
  | TMaskAction -> sp "MaskAction"
  | TPair (t1, t2) ->
      pp oc "(%a * %a)"
        (print ?sorted) t1
        (print ?sorted) t2
  | TSList t1 ->
      pp oc "%a{}" (print ?sorted) t1
  | TFunction ([||], t1) ->
      pp oc "( -> %a)" (print ?sorted) t1
  | TFunction (ts, t2) ->
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
  | TValue { vtyp ; nullable = false } ->
      TValue { vtyp ; nullable = true }
  | t ->
      Printf.eprintf "Cannot turn type %a into nullable\n%!"
        print t ;
      assert false

let to_not_nullable = function
  | TValue { vtyp ; nullable = true } ->
      TValue { vtyp ; nullable = false }
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
          char '-' -- char '-' --
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

  type key_type = VecDim of int | ListDim | MapKey of maybe_nullable

  let rec reduce_dims t = function
    | [] -> t
    | (VecDim d, nullable) :: rest ->
        reduce_dims (make ~nullable (TVec (d, t))) rest
    | (ListDim, nullable) :: rest ->
        reduce_dims (make ~nullable (TList t)) rest
    | (MapKey k, nullable) :: rest ->
        reduce_dims (make ~nullable (TMap (k, t))) rest

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
      vec_dim ||| list_dim ||| map_key
    ) m

  and maybe_nullable m =
    let m = "type" :: m in
    (
      (
        scalar_typ ||| tuple_typ ||| record_typ ||| sum_typ |||
        (!user_type ++ opt_question_mark >>: fun (vt, n) -> make ~nullable:n vt)
      ) ++
      repeat ~sep:opt_blanks (key_type) >>: fun (t, dims) ->
        reduce_dims t dims
    ) m

  and scalar_typ m =
    let m = "scalar type" :: m in
    let st n mtyp =
      let vtyp = Mac mtyp in
      (strinG (n ^"?") >>: fun () -> maken vtyp) |||
      (strinG n >>: fun () -> make vtyp)
    in
    (
      (st "float" TFloat) |||
      (st "string" TString) |||
      (st "bool" TBool) |||
      (st "boolean" TBool) |||
      (st "char" TChar) |||
      (st "u8" TU8) |||
      (st "u16" TU16) |||
      (st "u24" TU24) |||
      (st "u32" TU32) |||
      (st "u40" TU40) |||
      (st "u48" TU48) |||
      (st "u56" TU56) |||
      (st "u64" TU64) |||
      (st "u128" TU128) |||
      (st "i8" TI8) |||
      (st "i16" TI16) |||
      (st "i24" TI24) |||
      (st "i32" TI32) |||
      (st "i40" TI40) |||
      (st "i48" TI48) |||
      (st "i56" TI56) |||
      (st "i64" TI64) |||
      (st "i128" TI128)
    ) m

  and tuple_typ m =
    let m = "tuple type" :: m in
    (
      char '(' -- opt_blanks -+
        several ~sep:tup_sep maybe_nullable
      +- opt_blanks +- char ')' ++
      opt_question_mark >>: fun (ts, nullable) ->
        make ~nullable (TTup (Array.of_list ts))
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
        make ~nullable (TRec (Array.of_list ts))
    ) m

  and sum_typ m =
    let m = "sum type" :: m in
    let constructor m =
      let m = "constructor" :: m in
      (
        identifier +- !blanks ++ maybe_nullable
      ) m
    and sep =
      opt_blanks -- char '|' -- opt_blanks in
    (
      char '(' -- opt_blanks -+
      several ~sep constructor +-
      opt_blanks +- char ')' ++ opt_question_mark >>:
        fun (ts, nullable) ->
          (* TODO: check that all constructors are case insensitively distinct *)
          make ~nullable (TSum (Array.of_list ts))
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
      | Bad e ->
          if try_fix_typos then
            (* Try again with some error correction activated, in order to
             * get a better error message: *)
            match parse_with_err_budget 1 with
            | Bad e -> err_out e
            | _ -> assert false
          else
            err_out e
      | Ok (res, _) ->
          res

  let maybe_nullable_of_string ?what =
    let print = print_maybe_nullable in
    string_parser ~print ?what maybe_nullable

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
      | Bad (Approximation _) ->
        "Approximation"
      | Bad (NoSolution e) ->
        Printf.sprintf "No solution (%s)" (IO.to_string print_error e)
      | Bad (Ambiguous lst) ->
        Printf.sprintf "%d solutions: %s"
          (List.length lst)
          (IO.to_string
            (List.print (fun oc (res, _corr, (_stream, pos)) ->
              Printf.fprintf oc "res=%a, pos=%d,%d"
                res_printer res
                pos.ParsersPositions.line pos.column)) lst)

    let strip_linecol = function
      | Ok (res, (x, _pos)) -> Ok (res, x)
      | Bad x -> Bad x

    let test_p ?(postproc=identity) p s =
      (p +- eof) [] None Parsers.no_error_correction (PConfig.stream_of_string s) |>
      to_result |>
      strip_linecol |>
      Result.map (fun (r, rest) -> postproc r, rest)

    open DessserTypes
  *)

  (*$= maybe_nullable & ~printer:(test_printer print_maybe_nullable)
    (Ok ((make (Mac TU8)), (2,[]))) \
       (test_p maybe_nullable "u8")
    (Ok ((maken (Mac TU8)), (3,[]))) \
       (test_p maybe_nullable "u8?")
    (Ok ((make (TVec (3, (make (Mac TU8))))), (5,[]))) \
       (test_p maybe_nullable "u8[3]")
    (Ok ((make (TVec (3, (maken (Mac TU8))))), (6,[]))) \
       (test_p maybe_nullable "u8?[3]")
    (Ok ((maken (TVec (3, (make (Mac TU8))))), (6,[]))) \
       (test_p maybe_nullable "u8[3]?")
    (Ok ((make (TVec (3, (maken (TList (make (Mac TU8))))))), (8,[]))) \
       (test_p maybe_nullable "u8[]?[3]")
    (Ok ((maken (TList (make (TVec (3, (maken (Mac TU8))))))), (9,[]))) \
       (test_p maybe_nullable "u8?[3][]?")
    (Ok ((maken (TMap ((make (Mac TString)), (make (Mac TU8))))), (11,[]))) \
       (test_p maybe_nullable "u8[string]?")
    (Ok ((make (TMap ((make (TMap ((maken (Mac TU8)), (maken (Mac TString))))), (maken (TList ((make (TTup [| (make (Mac TU8)) ; (make (TMap ((make (Mac TString)), (make (Mac TBool))))) |])))))))), (35,[]))) \
       (test_p maybe_nullable "(u8; bool[string])[]?[string?[u8?]]")
    (Ok ((make (TRec [| "f1", make (Mac TBool) ; "f2", maken (Mac TU8) |])), (19,[]))) \
      (test_p maybe_nullable "{f1: Bool; f2: U8?}")
    (Ok ((make (TSum [| "c1", make (Mac TBool) ; "c2", maken (Mac TU8) |])), (18,[]))) \
      (test_p maybe_nullable "(c1 Bool | c2 U8?)")
    (Ok ((make (TVec (1, make (Mac TBool)))), (7,[]))) \
      (test_p maybe_nullable "Bool[1]")
  *)

  let rec typ m =
    let m = "type" :: m in
    (
      (maybe_nullable >>: fun mn -> TValue mn) |||
      (strinG "void" >>: fun () -> TVoid) |||
      (strinG "dataptr" >>: fun () -> TDataPtr) |||
      (strinG "size" >>: fun () -> TSize) |||
      (strinG "bit" >>: fun () -> TBit) |||
      (strinG "byte" >>: fun () -> TByte) |||
      (strinG "word" >>: fun () -> TWord) |||
      (strinG "dword" >>: fun () -> TDWord) |||
      (strinG "qword" >>: fun () -> TQWord) |||
      (strinG "oword" >>: fun () -> TOWord) |||
      (strinG "bytes" >>: fun () -> TBytes) |||
      (strinG "mask" >>: fun () -> TMask) |||
      (strinG "mask-action" >>: fun () -> TMaskAction) |||
      (
        char '(' -- opt_blanks -+ typ +- opt_blanks +-
        char '*' +- opt_blanks ++ typ +- opt_blanks +- char ')' >>:
          fun (t1, t2) -> TPair (t1, t2)
      ) |||
      (
        char '{' -- opt_blanks -+ typ +- opt_blanks +- char '}' >>:
          fun t -> TSList t
      ) |||
      (
        let sep = opt_blanks -- char '-' -- char '>' -- opt_blanks in
        char '(' -+
          repeat ~sep typ +- sep ++ typ +- opt_blanks +-
        char ')' >>: fun (ptyps, rtyp) ->
          TFunction (Array.of_list ptyps, rtyp)
      )
    ) m

  let of_string ?what =
    let print = print in
    string_parser ~print ?what typ

  (*$>*)
end

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

(* Examples: *)
let () =
  register_user_type "Date" (Mac TFloat) ;
  register_user_type "Eth" (Mac TU48) ;
  register_user_type "Ip4" (Mac TU32) ;
  register_user_type "Ip6" (Mac TU128) ;
  register_user_type "Ip"
    (* Note: for simplicity, make sure all constructor names are unique.
     * Also, start by a lowercase or a "v_" will be prepended needlessly: *)
    (TSum [| "v4", make (Usr (get_user_type "Ip4")) ;
             "v6", make (Usr (get_user_type "Ip6")) |]) ;
  register_user_type "Cidr4" (TRec [| "ip", make (Usr (get_user_type "Ip4")) ;
                                      "mask", make (Mac TU8) |]) ;
  register_user_type "Cidr6" (TRec [| "ip", make (Usr (get_user_type "Ip6")) ;
                                      "mask", make (Mac TU8) |]) ;
  register_user_type "Cidr"
    (TSum [| "v4", make (Usr (get_user_type "Cidr4")) ;
             "v6", make (Usr (get_user_type "Cidr6")) |])

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
  String.lchop s |>
  String.split_on_char '/' |>
  List.map int_of_string

(* Return both the type and field name: *)
let type_and_name_of_path t path =
  let rec loop field_name t = function
    | [] -> t, field_name
    | i :: path ->
        let rec type_of = function
          | (Unknown | Mac _ | TMap _) ->
              assert false
          | Usr t ->
              type_of t.def
          | TVec (dim, vt) ->
              assert (i < dim) ;
              loop (string_of_int i) vt path
          | TList vt ->
              loop (string_of_int i) vt path
          | TTup vts ->
              assert (i < Array.length vts) ;
              loop ("field_"^ string_of_int i) vts.(i) path
          | TRec vts ->
              assert (i < Array.length vts) ;
              loop (fst vts.(i)) (snd vts.(i)) path
          | TSum cs ->
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
  let test_t = make (TTup [|
    make (Mac TU8) ;
    maken (Mac TString) ;
    maken (TVec (2, make (Mac TChar))) |])
*)

(*$= type_of_path & ~printer:(BatIO.to_string print_maybe_nullable)
  test_t (type_of_path test_t [])
  (make (Mac TU8)) (type_of_path test_t [0])
  (maken (Mac TString)) (type_of_path test_t [1])
  (maken (TVec (2, make (Mac TChar)))) (type_of_path test_t [2])
  (make (Mac TChar)) (type_of_path test_t [2; 0])
*)

(* Some short cuts for often used types: *)

let bool = TValue (make (Mac TBool))
let char = TValue (make (Mac TChar))
let nstring = TValue (maken (Mac TString))
let string = TValue (make (Mac TString))
let float = TValue (make (Mac TFloat))
let u8 = TValue (make (Mac TU8))
let u16 = TValue (make (Mac TU16))
let u24 = TValue (make (Mac TU24))
let u32 = TValue (make (Mac TU32))
let u40 = TValue (make (Mac TU40))
let u48 = TValue (make (Mac TU48))
let u56 = TValue (make (Mac TU56))
let u64 = TValue (make (Mac TU64))
let u128 = TValue (make (Mac TU128))
let i8 = TValue (make (Mac TI8))
let i16 = TValue (make (Mac TI16))
let i24 = TValue (make (Mac TI24))
let i32 = TValue (make (Mac TI32))
let i40 = TValue (make (Mac TI40))
let i48 = TValue (make (Mac TI48))
let i56 = TValue (make (Mac TI56))
let i64 = TValue (make (Mac TI64))
let i128 = TValue (make (Mac TI128))
let void = TVoid
let bit = TBit
let byte = TByte
let size = TSize
let word = TWord
let dword = TDWord
let qword = TQWord
let oword = TOWord
let bytes = TBytes
let mask = TMask
let mask_action = TMaskAction
let dataptr = TDataPtr
let pair t1 t2 = TPair (t1, t2)
let slist t = TSList t
