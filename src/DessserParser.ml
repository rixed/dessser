(* Parsing text representations of types and expressions *)
open Batteries
open Stdint

open DessserTypes
open DessserMiscTypes

module FloatTools = DessserFloatTools
module PConfig = ParsersPositions.LineCol (Parsers.SimpleConfig (Char))
module P = Parsers.Make (PConfig)

module ParseUsual = ParsersUsual.Make (P)
include P
include ParseUsual

(* First a small s-expression parser: *)

(* String representation of expressions are mere s-expressions.
 * strings are represented as OCaml quoted strings. *)
type context = Blank | Enter | Leave | Symbol | String

type sexpr =
  | Sym of string
  | Str of string
  | Lst of sexpr list

exception Unknown_expression of sexpr
exception Extraneous_expressions of int
exception Garbage_after of int
exception Must_be_integer of sexpr * string
exception Must_be_quoted_type of sexpr

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

(*
 * Parsing types use combinator-based parser:
 *)

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

let user_type m =
  let m = "user type" :: m in
  (
    Hashtbl.fold (fun name ut p ->
      oneof p (strinG name >>: fun () -> TUsr ut)
    ) user_types fail
  ) m

let scalar_typ m =
  let m = "scalar type" :: m in
  let st n mtyp =
    strinG n >>: fun () -> mtyp
  in
  (
    (st "float" TFloat) |<|
    (st "string" TString) |<|
    (st "bool" TBool) |<|
    (st "boolean" TBool) |<|
    (st "char" TChar) |<|
    (st "u8" TU8) |<|
    (st "u16" TU16) |<|
    (st "u24" TU24) |<|
    (st "u32" TU32) |<|
    (st "u40" TU40) |<|
    (st "u48" TU48) |<|
    (st "u56" TU56) |<|
    (st "u64" TU64) |<|
    (st "u128" TU128) |<|
    (st "i8" TI8) |<|
    (st "i16" TI16) |<|
    (st "i24" TI24) |<|
    (st "i32" TI32) |<|
    (st "i40" TI40) |<|
    (st "i48" TI48) |<|
    (st "i56" TI56) |<|
    (st "i64" TI64) |<|
    (st "i128" TI128)
  ) m

let identifier =
  let what = "identifier" in
  let first_char = letter ||| underscore ||| char '-' in
  let any_char = first_char ||| decimal_digit in
  first_char ++ repeat_greedy ~sep:none ~what any_char >>: fun (c, s) ->
    (* TODO: exclude keywords *)
    String.of_list (c :: s)

let ext_typ m =
  let m = "external type" :: m in
  (
    char '$' -+ identifier >>: fun n -> TExt n
  ) m

let this m =
  let m = "this" :: m in
  (
    strinG "this" -+
    optional ~def:"t" (!blanks -+ identifier) >>: fun s -> TThis s
  ) m

type key_type =
  VecDim of int | ArrDim | SetDim of set_type | MapKey of mn | LstDim

(* Get rid of 1-uples, which are useful only to make parenthesis valid in type
 * expressions: *)
let rec simplify mn0 =
  match mn0.typ with
  | TTup [| mn |] ->
      { typ = mn.typ ;
        nullable = mn0.nullable || mn.nullable ;
        default = if mn0.default <> None then mn0.default else mn.default } |>
      simplify
  | TVec (dim, mn) ->
      { mn0 with typ = TVec (dim, simplify mn) }
  | TArr mn ->
      { mn0 with typ = TArr (simplify mn) }
  | TSet (st, mn) ->
      { mn0 with typ = TSet (st, simplify mn) }
  | TMap (k, v) ->
      { mn0 with typ = TMap (simplify k, simplify v) }
  | TLst mn ->
      { mn0 with typ = TLst (simplify mn) }
  | _ ->
      mn0

let rec reduce_dims typ =
  let default = None in (* TODO *)
  function
  | [] ->
      typ
  | (nullable, VecDim d) :: rest ->
      reduce_dims (TVec (d, { nullable ; typ ; default })) rest
  | (nullable, ArrDim) :: rest ->
      reduce_dims (TArr { nullable ; typ ; default }) rest
  | (nullable, SetDim st) :: rest ->
      reduce_dims (TSet (st, { nullable ; typ ; default })) rest
  | (nullable, MapKey k) :: rest ->
      reduce_dims (TMap (k, { nullable ; typ ; default })) rest
  | (nullable, LstDim) :: rest ->
      reduce_dims (TLst { nullable ; typ ; default }) rest

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
  let arr_dim m =
    let m = "arr type" :: m in
    (
      opt_question_mark +-
      char '[' +- opt_blanks +- char ']' >>: fun n -> n, ArrDim
    ) m in
  let lst_dim m =
    let m = "lst type" :: m in
    (
      opt_question_mark +-
      char '[' +- char '[' +- opt_blanks +- char ']' +- char ']' >>:
        fun n -> n, LstDim
    ) m in
  let set_type m =
    let m = "set type" :: m in
    (
      (strinG "simple" >>: fun () -> Simple) |<|
      (strinG "sliding" >>: fun () -> Sliding) |<|
      (strinG "tumbling" >>: fun () -> Tumbling) |<|
      (strinG "sampling" >>: fun () -> DessserMiscTypes.Sampling) |<|
      (strinG "hashtable" >>: fun () -> DessserMiscTypes.HashTable) |<|
      (strinG "heap" >>: fun () -> DessserMiscTypes.Heap) |<|
      (strinG "top" >>: fun () -> DessserMiscTypes.Top)
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
    vec_dim |<| arr_dim |<| set_dim |<| map_key |<| lst_dim
  ) m

and mn m =
  let m = "maybe nullable" :: m in
  (
    typ ++ opt_question_mark ++
    optional ~def:None (
      some (
        !blanks -- string "default" -- !blanks -+ (
          s_expr >>: fun s ->
            try e s
            with _ ->
              raise (Reject "not a valid default expression")))) >>:
      fun ((typ, nullable), default) ->
        simplify { typ ; nullable ; default }
  ) m

and typ m =
  let m = "type" :: m in
  let anonymous =
    (
      scalar_typ |<|
      tuple_typ |<|
      record_typ |<|
      sum_typ |<|
      ext_typ |<|
      user_type |<|
      this |<|
      (strinG "void" >>: fun () -> TVoid) |<|
      (strinG "ptr" >>: fun () -> TPtr) |<|
      (strinG "size" >>: fun () -> TSize) |<|
      (strinG "address" >>: fun () -> TAddress) |<|
      (strinG "bytes" >>: fun () -> TBytes) |<|
      (strinG "mask" >>: fun () -> TMask) |<|
      (
        let sep = opt_blanks -- char '-' -- char '>' -- opt_blanks in
        char '(' -+
          repeat ~sep mn +- sep ++ mn +- opt_blanks +-
        char ')' >>: fun (ptyps, rtyp) ->
          TFunction (Array.of_list ptyps, rtyp)
      ) |<| (
        char '&' -- opt_blanks -+ mn >>: fun mn -> TVec (1, mn)
      )
    ) ++ (
      repeat ~sep:opt_blanks key_type
    ) >>: fun (t, dims) -> reduce_dims t dims in
  (
    (identifier +- !blanks +- string "as" +- !blanks ++ anonymous >>:
      fun (n, t) -> TNamed (n, t)) |<|
    anonymous
  ) m

and tuple_typ m =
  let m = "tuple type" :: m in
  (
    char '(' -- opt_blanks -+
      several ~sep:tup_sep mn
    +- opt_blanks +- char ')' >>: fun ts ->
      TTup (Array.of_list ts)
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
      TRec (Array.of_list ts)
  ) m

and sum_typ m =
  let m = "sum type" :: m in
  let constructor m =
    let m = "constructor" :: m in
    (
      identifier ++
      optional ~def:{ nullable = false ; typ = TVoid ; default = None }
        (!blanks -+ mn)
    ) m
  and sep =
    opt_blanks -- char '|' -- opt_blanks in
  (
    char '[' -- opt_blanks -+
    several ~sep constructor +-
    opt_blanks +- char ']' >>: fun ts ->
        (* TODO: check that all constructors are case insensitively distinct *)
        TSum (Array.of_list ts)
  ) m

  and s_expr m =
    let m = "s-expression" :: m in
    let quoted_string m =
      let m = "s-expr-string" :: m in
      (
        char '"' -+
        repeat_greedy ~sep:none (
          (char '\\' -+ anything) |<|
          cond "not_quote" ((<>) '"') 'x') +-
        char '"' >>: String.of_list
      ) m
    and symbol m =
      let m = "s-expr-symbol" :: m in
      (
        several_greedy ~sep:none (cond "symb" (fun c ->
          c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n' &&
          c <> '"' && c <> '(' && c <> ')' && c <> ';') 'x') >>: String.of_list
      ) m in
    (* [n] is how many parentheses are yet to be closed: *)
    let rec at_depth n m =
      let m = ("s-expr-"^ string_of_int n) :: m in
      (
        opt_blanks -+
        several ~sep:!blanks (
          (quoted_string >>: fun s -> Str s) |<|
          (symbol >>: fun s -> Sym s) |<|
          (char '(' -+ at_depth (n + 1) +- char ')' >>: fun l -> Lst l))
      ) m in
    (
      at_depth 0 >>: function [ s ] -> s | l -> Lst l
    ) m

(*$inject
  open Batteries
  open Stdint
  module E = DessserExpressions

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

(*$= s_expr & ~printer:(test_printer print_sexpr)
  (Ok (Sym "[]", (2,[]))) \
    (test_p s_expr "[]")
  (Ok (Sym "3.14", (4,[]))) \
    (test_p s_expr "3.14")
  (Ok (Str "", (2,[]))) \
    (test_p s_expr "\"\"")
  (Ok (Sym "1", (1,[]))) \
    (test_p s_expr "1")
  (Ok (Lst [ Sym "foo" ; Sym "1" ; Sym "2" ], (9,[]))) \
    (test_p s_expr "(foo 1 2)")
  (Ok (Lst [ Sym "foo" ; Str "bar" ; Str "ba \"zz\"" ], (23,[]))) \
    (test_p s_expr "(foo \"bar\" \"ba \\\"zz\\\"\")")
  (Ok (Lst [ Sym "foo" ; Lst [ Str "bar" ; Str "ba \"zz\"" ] ], (25,[]))) \
    (test_p s_expr "(foo (\"bar\" \"ba \\\"zz\\\"\"))")
  (Ok (Lst [ Sym "null" ; Str "u8" ], (11,[]))) \
    (test_p s_expr "(null \"u8\")")
*)

(*$= mn & ~printer:(test_printer print_mn)
  (Ok ((required TU8), (2,[]))) \
     (test_p mn "u8")
  (Ok ((optional ~default:(E.Ops.null TU8) TU8), (23,[]))) \
     (test_p mn "u8? default (null \"u8\")")
  (Ok ((required ~default:(E.Ops.u8_of_int 42) TU8), (18,[]))) \
     (test_p mn "u8 default (u8 42)")
  (Ok ((optional TU8), (3,[]))) \
     (test_p mn "u8?")
  (Ok ((required (TVec (3, (required TU8)))), (5,[]))) \
     (test_p mn "u8[3]")
  (Ok ((required (TVec (3, (optional TU8)))), (6,[]))) \
     (test_p mn "u8?[3]")
  (Ok ((optional (TVec (3, (required TU8)))), (6,[]))) \
     (test_p mn "u8[3]?")
  (Ok ((required (TVec (3, (optional (TArr (required TU8)))))), (8,[]))) \
     (test_p mn "u8[]?[3]")
  (Ok ((optional (TArr (required (TVec (3, (optional TU8)))))), (9,[]))) \
     (test_p mn "u8?[3][]?")
  (Ok ((optional (TMap ((required TString), (required TU8)))), (11,[]))) \
     (test_p mn "u8[string]?")
  (Ok ((required (TMap ((required (TMap (nu8, nstring))), (optional (TArr ((required (TTup [| (required TU8) ; (required (TMap ((required TString), (required TBool)))) |])))))))), (35,[]))) \
     (test_p mn "(u8; bool[string])[]?[string?[u8?]]")
  (Ok ((required (TRec [| "f1", required TBool ; "f2", optional TU8 |])), (19,[]))) \
    (test_p mn "{f1: Bool; f2: U8?}")
  (Ok ((required (TRec [| "f2", required TBool ; "f1", optional TU8 |])), (19,[]))) \
    (test_p mn "{f2: Bool; f1: U8?}")
  (Ok ((required (TSum [| "c1", required TBool ; "c2", optional TU8 |])), (18,[]))) \
    (test_p mn "[c1 Bool | c2 U8?]")
  (Ok ((required (TVec (1, required TBool))), (7,[]))) \
    (test_p mn "Bool[1]")
  (Ok (required TU8, (4,[]))) \
    (test_p mn "(U8)")
  (Ok (required (named "foo" TU8), (11,[]))) \
    (test_p mn "(foo as U8)")
  (Ok (required (arr (required (named "bar" TU8))), (13,[]))) \
    (test_p mn "(bar as U8)[]")
*)

(* In addition to native dessser format for type specification, we
 * can also make sense of ClickHouse "NamesAndTypes" somewhat informal
 * specifications: *)
and clickhouse_names_and_types m =
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
    let m = "Type name" :: m
    and default = None in
    (
      (* Look only for simple types, starting with numerics: *)
      (iD "UInt8" >>:
        fun () -> { nullable = false ; typ = TU8 ; default }) |<|
      (iD "UInt16" >>:
        fun () -> { nullable = false ; typ = TU16 ; default }) |<|
      (iD "UInt32" >>:
        fun () -> { nullable = false ; typ = TU32 ; default }) |<|
      (iD "UInt64" >>:
        fun () -> { nullable = false ; typ = TU64 ; default }) |<|
      ((iD "Int8" |<| iD "TINYINT") >>:
        fun () -> { nullable = false ; typ = TI8 ; default }) |<|
      ((iD "Int16" |<| iD "SMALLINT") >>:
        fun () -> { nullable = false ; typ = TI16 ; default }) |<|
      ((iD "Int32" |<| iD "INTEGER" |<| iD "INT") >>:
        fun () -> { nullable = false ; typ = TI32 ; default }) |<|
      ((iD "Int64" |<| iD "BIGINT") >>:
        fun () -> { nullable = false ; typ = TI64 ; default }) |<|
      ((iD "Float32" |<| iD "Float64" |<|
        iD "FLOAT" |<| iD "DOUBLE") >>:
        fun () -> { nullable = false ; typ = TFloat ; default }) |<|
      (* Assuming UUIDs are just plain U128 with funny-printing: *)
      (iD "UUID" >>:
        fun () -> { nullable = false ; typ = TU128 ; default }) |<|
      (* Decimals: for now forget about the size of the decimal part,
       * just map into corresponding int type*)
      (with_num_param "Decimal32" >>:
        fun _p -> { nullable = false ; typ = TI32 ; default }) |<|
      (with_num_param "Decimal64" >>:
        fun _p -> { nullable = false ; typ = TI64 ; default }) |<|
      (with_num_param "Decimal128" >>:
        fun _p -> { nullable = false ; typ = TI128 ; default }) |<|
      (* TODO: actually do something with the size: *)
      ((with_2_num_params "Decimal" |<| with_2_num_params "DEC") >>:
        fun (_n, _m)  -> { nullable = false ; typ = TI128 ; default }) |<|
      ((iD "DateTime" |<| iD "TIMESTAMP") >>:
        fun () -> { nullable = false ; typ = TU32 ; default }) |<|
      (iD "Date" >>:
        fun () -> { nullable = false ; typ = TU16 ; default }) |<|
      ((iD "String" |<| iD "CHAR" |<| iD "VARCHAR" |<|
        iD "TEXT" |<| iD "TINYTEXT" |<| iD "MEDIUMTEXT" |<|
        iD "LONGTEXT" |<| iD "BLOB" |<| iD "TINYBLOB" |<|
        iD "MEDIUMBLOB" |<| iD "LONGBLOB") >>:
        fun () -> { nullable = false ; typ = TString ; default }) |<|
      ((with_num_param "FixedString" |<| with_num_param "BINARY") >>:
        fun d -> { nullable = false ; default ;
                   typ = TVec (d, { nullable = false ;
                                    typ = TChar ; default }) }) |<|
      (with_typ_param "Nullable" >>:
        fun mn -> { mn with nullable = true }) |<|
      (with_typ_param "Array" >>:
        fun mn -> { nullable = false ; typ = TArr mn ; default }) |<|
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
      TRec mns
  ) m

(*$= clickhouse_names_and_types & ~printer:(test_printer print)
  (Ok (TRec [| "thing", required TU16 |], (14,[]))) \
     (test_p clickhouse_names_and_types "`thing` UInt16")

  (Ok (TRec [| "thing", required (TArr (required TU16)) |], (21,[]))) \
     (test_p clickhouse_names_and_types "`thing` Array(UInt16)")
*)

and int_of_symbol s d =
  try int_of_string d
  with _ -> raise (Must_be_integer (s, d))

and mn_of_str s d =
  try mn_of_string d
  with _ -> raise (Must_be_quoted_type s)

and e =
  let is_int32 n =
    try ignore (Int32.of_string n) ; true with _ -> false
  and is_float f =
    try ignore (FloatTools.float_of_anystring f) ; true with _ -> false in
  function
  (* e0 *)
  | Lst [ Sym "param" ; Sym n ] as s ->
      E0 (Param (int_of_symbol s n))
  | Lst [ Sym "myself" ; Str mn ] as s ->
      E0 (Myself (mn_of_str s mn))
  | Lst [ Sym "null" ; Str t ] ->
      E0 (Null (typ_of_string t))
  | Lst [ Sym ("end-of-list" | "eol") ; Str mn ] as s ->
      E0 (EndOfList (mn_of_str s mn))
  | Sym "[[]]" ->
      (* Taking advantage of universal convertibility of empty lists: *)
      E0 (EndOfList void)
  | Lst [ Sym "empty-set" ; Str mn ] as s ->
      E0 (EmptySet (mn_of_str s mn))
  | Lst [ Sym "now" ] -> E0 Now
  | Lst [ Sym "random-float" ] -> E0 RandomFloat
  | Lst [ Sym "random-u8" ] -> E0 RandomU8
  | Lst [ Sym "random-u32" ] -> E0 RandomU32
  | Lst [ Sym "random-u64" ] -> E0 RandomU64
  | Lst [ Sym "random-u128" ] -> E0 RandomU128
  | Lst [ Sym "float" ; Sym f ] ->
      E0 (Float (FloatTools.float_of_anystring f))
  | Lst [ Sym "string" ; Str s ] -> E0 (String s)
  | Str s -> E0 (String s)
  | Lst [ Sym "bool" ; Sym b ] -> E0 (Bool (Bool.of_string b))
  | Sym "false" -> E0 (Bool false)
  | Sym "true" -> E0 (Bool true)
  | Lst [ Sym "char" ; Str c ] when String.length c = 1 -> E0 (Char c.[0])
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
  | Sym n when is_int32 n -> E0 (I32 (Int32.of_string n))
  | Sym f when is_float f -> E0 (Float (FloatTools.float_of_anystring f))
  | Lst [ Sym "i40" ; Sym n ] -> E0 (I40 (Int40.of_string n))
  | Lst [ Sym "i48" ; Sym n ] -> E0 (I48 (Int48.of_string n))
  | Lst [ Sym "i56" ; Sym n ] -> E0 (I56 (Int56.of_string n))
  | Lst [ Sym "i64" ; Sym n ] -> E0 (I64 (Int64.of_string n))
  | Lst [ Sym "i128" ; Sym n ] -> E0 (I128 (Int128.of_string n))
  | Lst [ Sym "size" ; Sym n ] as s -> E0 (Size (int_of_symbol s n))
  | Lst [ Sym "address" ; Sym n ] -> E0 (Address (Uint64.of_string n))
  | Lst [ Sym "bytes" ; Str s ] -> E0 (Bytes (Bytes.of_string s))
  | Lst [ Sym "identifier" ; Str s ] -> E0 (Identifier s)
  | Lst [ Sym "ext-identifier" ; Str s ] -> E0 (ExtIdentifier (Verbatim s))
  | Lst [ Sym "ext-identifier" ; Sym typ ;
          Sym ("ser"|"des"|"ssize" as meth) ] ->
      let meth = type_method_of_string meth in
      E0 (ExtIdentifier (Method { typ ; meth }))
  | Lst [ Sym "copy-field" ] -> E0 CopyField
  | Lst [ Sym "skip-field" ] -> E0 SkipField
  | Lst [ Sym "set-field-null" ] -> E0 SetFieldNull
  (* e0s *)
  | Lst (Sym "seq" :: xs) -> E0S (Seq, List.map e xs)
  | Lst [] -> E0S (Seq, [])
  | Sym "nop" -> E0S (Seq, [])
  | Lst (Sym "make-vec" :: xs) ->
      E0R (MakeVec,
           List.enum xs /@ e |> Array.of_enum)
  | Lst (Sym "make-arr" :: Str mn :: xs) as s ->
      E0R (MakeArr (mn_of_str s mn),
           List.enum xs /@ e |> Array.of_enum)
  | Sym "[]" ->
      (* Taking advantage of universal convertibility of empty arrays: *)
      E0R (MakeArr void, [||])
  | Sym "{}" ->
      (* Taking advantage of universal convertibility of empty sets: *)
      E0 (EmptySet void)
  | Sym "null" ->
      (* Also taking advantage of universal convertibility of NULLs: *)
      E0 (Null TVoid)
  | Lst (Sym "make-tup" :: xs) -> E0S (MakeTup, List.map e xs)
  | Lst (Sym "make-rec" :: xs) -> E0S (MakeRec, List.map e xs)
  | Lst (Sym "make-usr" :: Str n :: xs) -> E0S (MakeUsr n, List.map e xs)
  | Lst (Sym "verbatim" :: Lst temps :: Str mn :: xs) as s ->
      let temp_of_strings = function
        | Lst [ Sym id ; Str temp ] -> backend_of_string id, temp
        | x ->
            Printf.sprintf2 "Cannot parse verbatim template %a" print_sexpr x |>
            failwith in
      let temps = List.map temp_of_strings temps in
      E0S (Verbatim (temps, mn_of_str s mn), List.map e xs)
  (* e1 *)
  | Lst [ Sym ("function" | "fun") ; Lst typs ; body ] as s ->
      (* Syntax for functions is:
       *    (fun ("type arg 1" "type arg 2" ...) body)
       * where:
       *   - id is an integer used to identify this function when using param
       *   - body is an expression *)
      let typs =
        List.enum typs /@
        (function
          | Str d -> mn_of_str s d
          | x ->
              raise (Must_be_quoted_type x)) |>
        Array.of_enum in
      E1 (Function typs, e body)
  | Lst [ Sym "comment" ; Str s ; x ] ->
      E1 (Comment s, e x)
  | Lst [ Sym "get-item" ; Sym n ; x ] as s ->
      E1 (GetItem (int_of_symbol s n), e x)
  | Lst [ Sym "get-field" ; Str s ; x ] ->
      E1 (GetField s, e x)
  | Lst [ Sym "get-alt" ; Str s ; x ] ->
      E1 (GetAlt s, e x)
  | Lst [ Sym "construct" ; Str mn ; Sym i ; x ] as s ->
      let i = int_of_symbol s i in
      (match mn_of_str s mn with
      | { typ = TSum mns ; nullable = false } ->
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
  | Lst [ Sym "force" ; x ] -> E1 (Force "", e x)
  | Lst [ Sym "string-of-float" ; x ] -> E1 (StringOfFloat, e x)
  | Lst [ Sym "decimal-string-of-float" ; x ] -> E1 (DecimalStringOfFloat, e x)
  | Lst [ Sym "string-of-char" ; x ] -> E1 (StringOfChar, e x)
  | Lst [ Sym "string-of-int" ; x ] -> E1 (StringOfInt, e x)
  | Lst [ Sym "string-of-ip" ; x ] -> E1 (StringOfIp, e x)
  | Lst [ Sym "float-of-string" ; x ] -> E1 (FloatOfString, e x)
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
  | Lst [ Sym "bit-not" ; x ] -> E1 (BitNot, e x)
  | Lst [ Sym "float-of-u64" ; x ] -> E1 (FloatOfU64, e x)
  | Lst [ Sym "u64-of-float" ; x ] -> E1 (U64OfFloat, e x)
  | Lst [ Sym "u8-of-char" ; x ] -> E1 (U8OfChar, e x)
  | Lst [ Sym "char-of-u8" ; x ] -> E1 (CharOfU8, e x)
  | Lst [ Sym "size-of-u32" ; x ] -> E1 (SizeOfU32, e x)
  | Lst [ Sym "u32-of-size" ; x ] -> E1 (U32OfSize, e x)
  | Lst [ Sym "address-of-u64" ; x ] -> E1 (AddressOfU64, e x)
  | Lst [ Sym "u64-of-address" ; x ] -> E1 (U64OfAddress, e x)
  | Lst [ Sym "arr-of-lst" ; x ] -> E1 (ArrOfLst, e x)
  | Lst [ Sym "arr-of-lst-rev" ; x ] -> E1 (ArrOfLstRev, e x)
  | Lst [ Sym "set-of-lst" ; x ] -> E1 (SetOfLst, e x)
  | Lst [ Sym "arr-of-vec" ; x ] -> E1 (ArrOfVec, e x)
  | Lst [ Sym "arr-of-set" ; x ] -> E1 (ArrOfSet, e x)
  | Lst [ Sym "u8-of-bool" ; x ] -> E1 (U8OfBool, e x)
  | Lst [ Sym "bool-of-u8" ; x ] -> E1 (BoolOfU8, e x)
  | Lst [ Sym "string-length" ; x ] -> E1 (StringLength, e x)
  | Lst [ Sym "bytes-length" ; x ] -> E1 (BytesLength, e x)
  | Lst [ Sym "string-of-bytes" ; x ] -> E1 (StringOfBytes, e x)
  | Lst [ Sym "bytes-of-string" ; x ] -> E1 (BytesOfString, e x)
  | Lst [ Sym "cardinality" ; x ] -> E1 (Cardinality, e x)
  | Lst [ Sym "read-u8" ; x ] -> E1 (ReadU8, e x)
  | Lst [ Sym "rem-size" ; x ] -> E1 (RemSize, e x)
  | Lst [ Sym "offset" ; x ] -> E1 (Offset, e x)
  | Lst [ Sym "not" ; x ] -> E1 (Not, e x)
  | Lst [ Sym "abs" ; x ] -> E1 (Abs, e x)
  | Lst [ Sym "neg" ; x ] -> E1 (Neg, e x)
  | Lst [ Sym "exp" ; x ] -> E1 (Exp, e x)
  | Lst [ Sym "log" ; x ] -> E1 (Log, e x)
  | Lst [ Sym "unsafe-log" ; x ] -> E1 (UnsafeLog, e x)
  | Lst [ Sym "log10" ; x ] -> E1 (Log10, e x)
  | Lst [ Sym "unsafe-log10" ; x ] -> E1 (UnsafeLog10, e x)
  | Lst [ Sym "sqrt" ; x ] -> E1 (Sqrt, e x)
  | Lst [ Sym "unsafe-sqrt" ; x ] -> E1 (UnsafeSqrt, e x)
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
  | Lst [ Sym "fst" ; x ] -> E1 (GetItem 0, e x)
  | Lst [ Sym "snd" ; x ] -> E1 (GetItem 1, e x)
  | Lst [ Sym "head" ; x ] -> E1 (Head, e x)
  | Lst [ Sym "tail" ; x ] -> E1 (Tail, e x)
  | Lst [ Sym "read-u16" ; Sym en ; x ] ->
      E1 (ReadU16 (endianness_of_string en), e x)
  | Lst [ Sym "read-u32" ; Sym en ; x ] ->
      E1 (ReadU32 (endianness_of_string en), e x)
  | Lst [ Sym "read-u64" ; Sym en ; x ] ->
      E1 (ReadU64 (endianness_of_string en), e x)
  | Lst [ Sym "read-u128" ; Sym en ; x ] ->
      E1 (ReadU128 (endianness_of_string en), e x)
  | Lst [ Sym "assert" ; x1 ] -> E1 (Assert, e x1)
  | Lst [ Sym "mask-get" ; Sym d ; x1 ] as s ->
      E1 (MaskGet (int_of_symbol s d), e x1)
  | Lst [ Sym "label-of" ; x ] -> E1 (LabelOf, e x)
  | Lst [ Sym "sliding-window" ; Str mn ; x ] as s ->
      E1 (SlidingWindow (mn_of_str s mn), e x)
  | Lst [ Sym "tumbling-window" ; Str mn ; x ] as s ->
      E1 (TumblingWindow (mn_of_str s mn), e x)
  | Lst [ Sym "sampling" ; Str mn ; x ] as s ->
      E1 (Sampling (mn_of_str s mn), e x)
  | Lst [ Sym "hash-table" ; Str mn ; x ] as s ->
      E1 (HashTable (mn_of_str s mn), e x)
  | Lst [ Sym "heap" ; x ] ->
      E1 (Heap, e x)
  | Lst [ Sym "ptr-of-string" ; x ] ->
      E1 (PtrOfString, e x)
  | Lst [ Sym "ptr-of-buffer" ; x ] ->
      E1 (PtrOfBuffer, e x)
  | Lst [ Sym "getenv" ; x ] ->
      E1 (GetEnv, e x)
  | Lst [ Sym "get-min" ; x ] ->
      E1 (GetMin, e x)
  | Lst [ Sym "alloc-vec" ; Sym d ; x ] as s ->
      E1 (AllocVec (int_of_symbol s d), e x)
  | Lst [ Sym "convert" ; Str mn ; x ] as s ->
      E1 (Convert (mn_of_str s mn), e x)
  (* e1s *)
  | Lst (Sym "apply" :: x1 :: xs) -> E1S (Apply, e x1, List.map e xs)
  (* e2 *)
  | Lst [ Sym "let" ; Str n ; x1 ; x2 ] ->
      E2 (Let (n, ref None), e x1, e x2)
  | Lst [ Sym "let-pair" ; Str n1 ; Str n2 ; x1 ; x2 ] ->
      E2 (LetPair (n1, ref None, n2, ref None), e x1, e x2)
  | Lst [ Sym "nth" ; x1 ; x2 ] -> E2 (Nth, e x1, e x2)
  | Lst [ Sym "unsafe-nth" ; x1 ; x2 ] -> E2 (UnsafeNth, e x1, e x2)
  | Lst [ Sym "gt" ; x1 ; x2 ] -> E2 (Gt, e x1, e x2)
  | Lst [ Sym "lt" ; x1 ; x2 ] -> E2 (Gt, e x2, e x1)
  | Lst [ Sym "ge" ; x1 ; x2 ] -> E2 (Ge, e x1, e x2)
  | Lst [ Sym "le" ; x1 ; x2 ] -> E2 (Ge, e x2, e x1)
  | Lst [ Sym "eq" ; x1 ; x2 ] -> E2 (Eq, e x1, e x2)
  | Lst [ Sym "ne" ; x1 ; x2 ] -> E1 (Not, E2 (Eq, e x1, e x2))
  | Lst [ Sym "add" ; x1 ; x2 ] -> E2 (Add, e x1, e x2)
  | Lst [ Sym "sub" ; x1 ; x2 ] -> E2 (Sub, e x1, e x2)
  | Lst [ Sym "mul" ; x1 ; x2 ] -> E2 (Mul, e x1, e x2)
  | Lst [ Sym "div" ; x1 ; x2 ] -> E2 (Div, e x1, e x2)
  | Lst [ Sym "unsafe-div" ; x1 ; x2 ] -> E2 (UnsafeDiv, e x1, e x2)
  | Lst [ Sym "rem" ; x1 ; x2 ] -> E2 (Rem, e x1, e x2)
  | Lst [ Sym "unsafe-rem" ; x1 ; x2 ] -> E2 (UnsafeRem, e x1, e x2)
  | Lst [ Sym "pow" ; x1 ; x2 ] -> E2 (Pow, e x1, e x2)
  | Lst [ Sym "unsafe-pow" ; x1 ; x2 ] -> E2 (UnsafePow, e x1, e x2)
  | Lst [ Sym "bit-and" ; x1 ; x2 ] -> E2 (BitAnd, e x1, e x2)
  | Lst [ Sym "bit-or" ; x1 ; x2 ] -> E2 (BitOr, e x1, e x2)
  | Lst [ Sym "bit-xor" ; x1 ; x2 ] -> E2 (BitXor, e x1, e x2)
  | Lst [ Sym "left-shift" ; x1 ; x2 ] -> E2 (LeftShift, e x1, e x2)
  | Lst [ Sym "right-shift" ; x1 ; x2 ] -> E2 (RightShift, e x1, e x2)
  | Lst [ Sym "append-byte" ; x1 ; x2 ] -> E2 (AppendByte, e x1, e x2)
  | Lst [ Sym "append-bytes" ; x1 ; x2 ] -> E2 (AppendBytes, e x1, e x2)
  | Lst [ Sym "append-string" ; x1 ; x2 ] -> E2 (AppendString, e x1, e x2)
  | Lst [ Sym "starts-with" ; x1 ; x2 ] -> E2 (StartsWith, e x1, e x2)
  | Lst [ Sym "ends-with" ; x1 ; x2 ] -> E2 (EndsWith, e x1, e x2)
  | Lst [ Sym "get-bit" ; x1 ; x2 ] -> E2 (GetBit, e x1, e x2)
  | Lst [ Sym "read-bytes" ; x1 ; x2 ] -> E2 (ReadBytes, e x1, e x2)
  | Lst [ Sym "peek-u8" ; x1 ; x2 ] -> E2 (PeekU8, e x1, e x2)
  | Lst [ Sym "write-u8" ; x1 ; x2 ] -> E2 (WriteU8, e x1, e x2)
  | Lst [ Sym "write-bytes" ; x1 ; x2 ] -> E2 (WriteBytes, e x1, e x2)
  | Lst [ Sym "poke-u8" ; x1 ; x2 ] -> E2 (PokeU8, e x1, e x2)
  | Lst [ Sym "ptr-add" ; x1 ; x2 ] -> E2 (PtrAdd, e x1, e x2)
  | Lst [ Sym "ptr-sub" ; x1 ; x2 ] -> E2 (PtrSub, e x1, e x2)
  | Lst [ Sym "rewind" ; x1 ; x2 ] -> E2 (Rewind, e x1, e x2)
  | Lst [ Sym "and" ; x1 ; x2 ] -> E2 (And, e x1, e x2)
  | Lst [ Sym "or" ; x1 ; x2 ] -> E2 (Or, e x1, e x2)
  | Lst [ Sym "cons" ; x1 ; x2 ] -> E2 (Cons, e x1, e x2)
  | Lst [ Sym "min" ; x1 ; x2 ] -> E2 (Min, e x1, e x2)
  | Lst [ Sym "max" ; x1 ; x2 ] -> E2 (Max, e x1, e x2)
  | Lst [ Sym "mem" ; x1 ; x2 ] -> E2 (Member, e x1, e x2)
  | Lst [ Sym "peek-u16" ; Sym en ; x1 ; x2 ] ->
      E2 (PeekU16 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "peek-u32" ; Sym en ; x1 ; x2 ] ->
      E2 (PeekU32 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "peek-u64" ; Sym en ; x1 ; x2 ] ->
      E2 (PeekU64 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "peek-u128" ; Sym en ; x1 ; x2 ] ->
      E2 (PeekU128 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "write-u16" ; Sym en ; x1 ; x2 ] ->
      E2 (WriteU16 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "write-u32" ; Sym en ; x1 ; x2 ] ->
      E2 (WriteU32 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "write-u64" ; Sym en ; x1 ; x2 ] ->
      E2 (WriteU64 (endianness_of_string en), e x1, e x2)
  | Lst [ Sym "write-u128" ; Sym en ; x1 ; x2 ] ->
      E2 (WriteU128 (endianness_of_string en), e x1, e x2)
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
  | Lst [ Sym "alloc-arr" ; x1 ; x2 ] ->
      E2 (AllocArr, e x1, e x2)
  | Lst [ Sym "partial-sort" ; x1 ; x2 ] ->
      E2 (PartialSort, e x1, e x2)
  | Lst [ Sym "chop-begin" ; x1 ; x2 ] ->
      E2 (ChopBegin, e x1, e x2)
  | Lst [ Sym "chop-end" ; x1 ; x2 ] ->
      E2 (ChopEnd, e x1, e x2)
  | Lst [ Sym "scale-weights" ; set ; d ] ->
      E2 (ScaleWeights, e set, e d)
  | Lst [ Sym "strftime" ; fmt ; time ] ->
      E2 (Strftime, e fmt, e time)
  | Lst [ Sym "ptr-of-address" ; x1 ; x2 ] ->
      E2 (PtrOfAddress, e x1, e x2)
  | Lst [ Sym "while" ; x1 ; x2 ] ->
      E2 (While, e x1, e x2)
  | Lst [ Sym "for-each" ; Str n ; x1 ; x2 ] ->
      E2 (ForEach (n, ref None), e x1, e x2)
  | Lst [ Sym "null-map" ; Str n ; x1 ; x2 ] ->
      E2 (NullMap (n, ref None), e x1, e x2)
  | Lst [ Sym "index" ; x1 ; x2 ] ->
      E2 (Index, e x1, e x2)
  (* e3 *)
  | Lst [ Sym "set-bit" ; x1 ; x2 ; x3 ] -> E3 (SetBit, e x1, e x2, e x3)
  | Lst [ Sym "set-vec" ; x1 ; x2 ; x3 ] -> E3 (SetVec, e x1, e x2, e x3)
  | Lst [ Sym "blit-byte" ; x1 ; x2 ; x3 ] -> E3 (BlitByte, e x1, e x2, e x3)
  | Lst [ Sym "if" ; x1 ; x2 ; x3 ] -> E3 (If, e x1, e x2, e x3)
  | Lst [ Sym "map" ; x1 ; x2 ; x3 ] -> E3 (Map, e x1, e x2, e x3)
  | Lst [ Sym "ptr-of-ptr" ; x1 ; x2 ; x3 ] ->
      E3 (PtrOfPtr, e x1, e x2, e x3)
  | Lst [ Sym "find-substring" ; x1 ; x2 ; x3 ] ->
      E3 (FindSubstring, e x1, e x2, e x3)
  | Lst [ Sym "top" ; Str mn ; x1 ; x2 ; x3 ] as s ->
      E3 (Top (mn_of_str s mn), e x1, e x2, e x3)
  | Lst [ Sym "insert-weighted" ; x1 ; x2 ; x3 ] ->
      E3 (InsertWeighted, e x1, e x2, e x3)
  | Lst [ Sym "substring" ; x1 ; x2 ; x3 ] ->
      E3 (SubString, e x1, e x2, e x3)

  | x -> raise (Unknown_expression x)

and expr_of_toks toks str =
  List.map e (sexpr_of_toks toks str)

and expr str =
  List.map e (sexpr_of_string str)

(*$= expr & ~printer:(BatIO.to_string (BatList.print E.print))
  [ E.Ops.u8 (Uint8.of_int 42) ] (expr "(u8 42)")
  [ E.Ops.float 1. ] (expr "(float 1.0)")
  [ E.Ops.char '\019' ] (expr "(char \"\\019\")")
  [ E.Ops.null TString ] (expr "(null \"string\")")
  [ E.Ops.i56 (Int56.of_string "-36028797018963967") ] (expr "(i56 -36028797018963967)")
  [ E.Ops.i128 (Int128.of_string "-1213949874624120272") ] \
    (expr "(i128 -1213949874624120272)")
  [ E.Ops.false_ ] (expr "(bool false)")
  [ E.Ops.u64 (Uint64.of_int 8) ] (expr "(u64 8)")
  [ E.Ops.seq [ E.Ops.u16 (Uint16.of_int 45134) ; E.Ops.u64 (Uint64.of_int 6)] ] \
    (expr "(seq (u16 45134) (u64 6))")
  [ E.Ops.comment "foo" (E.Ops.u32 (Uint32.of_int 2)) ] \
    (expr "(comment \"foo\" (u32 2))")
  [ E.Ops.(make_vec [ u8 Uint8.one ; u8 (Uint8.of_int 2) ]) ] \
    (expr "(make-vec (u8 1) (u8 2))")
*)

and expr_of_string s =
  match expr s with
  | [ e ] -> e
  | _ ->
      Printf.sprintf2 "Cannot parse %S as a single expression" s |>
      failwith

and mn_of_string ?what =
  let print = print_mn in
  string_parser ~print ?what mn

(* If [any_format] then any known format to specify types will be tried.
 * If not then only dessser own format will be tried (faster, esp when
 * parsing DIL s-expressions) *)
and typ_of_string ?(any_format=false) ?what =
  let p =
    if any_format then typ |<| clickhouse_names_and_types
    else typ in
  string_parser ~print ?what p

(*$= typ_of_string & ~printer:Batteries.dump
  (TUsr { name = "Ip4" ; def = TU32 }) (typ_of_string "Ip4")

  (TLst { \
    typ = TSum [| "eugp", { typ = TUsr { name = "Ip4" ; \
                                         def = TU32 } ; \
                            nullable = false ; default = None }; \
                  "jjbi", { typ = TBool ; nullable = false ; default = None } ; \
                  "bejlu", { typ = TI24 ; nullable = true ; default = None } ; \
                  "bfid", { typ = TFloat ; nullable = false ; default = None } |] ; \
    nullable = false ; default = None }) \
  (typ_of_string "[eugp Ip4 | jjbi BOOL | bejlu I24? | bfid FLOAT][[]]")
*)

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
    | Must_be_quoted_type x ->
        Some (Printf.sprintf2 "Must be a quoted type: %a" print_sexpr x)
    | _ ->
        None)
