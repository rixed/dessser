(* Parsers for type specifications *)
open Batteries
open Stdint

module PConfig = ParsersPositions.LineCol (Parsers.SimpleConfig (Char))
module P = Parsers.Make (PConfig)
module ParseUsual = ParsersUsual.Make (P)
include P
include ParseUsual

module T = DessserTypes

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
    match parse_with_err_budget 0 with
    | Bad _ ->
        (* Try again with some error correction activated, in order to
         * get a better error message: *)
        (match parse_with_err_budget 1 with
        | Bad e -> err_out e
        | _ -> assert false)
    | Ok (res, _) -> res

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

let make_type nullable vtyp =
  if nullable then T.(Nullable vtyp) else T.(NotNullable vtyp)

let tup_sep =
  opt_blanks -- char ';' -- opt_blanks

type key_type = VecDim of int | ListDim | MapKey of T.maybe_nullable

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
      typ +- opt_blanks +- char ']' ++
      opt_question_mark >>: fun (k, n) ->
        MapKey k, n
    ) m
  in
  (
    vec_dim ||| list_dim ||| map_key
  ) m

and typ m =
  let rec reduce_dims base_type = function
    | [] -> base_type
    | (VecDim d, nullable) :: rest ->
        reduce_dims (make_type nullable T.(TVec (d, base_type))) rest
    | (ListDim, nullable) :: rest ->
        reduce_dims (make_type nullable T.(TList base_type)) rest
    | (MapKey k, nullable) :: rest ->
        reduce_dims (make_type nullable T.(TMap (k, base_type))) rest
  in
  let m = "type" :: m in
  (
    (scalar_typ ||| tuple_typ) ++
    repeat ~sep:opt_blanks (key_type) >>: fun (base_type, dims) ->
      reduce_dims base_type dims
  ) m

and scalar_typ m =
  let m = "scalar type" :: m in
  let st n mtyp =
    let vtyp = T.Mac mtyp in
    (strinG (n ^"?") >>: fun () -> make_type true vtyp) |||
    (strinG n >>: fun () -> make_type false vtyp)
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
    (st "i128" TI128) (* ||| any of the defined user type parsers *)
  ) m

and tuple_typ m =
  let m = "tuple type" :: m in
  (
    char '(' -- opt_blanks -+
      several ~sep:tup_sep typ
    +- opt_blanks +- char ')' ++
    opt_question_mark >>: fun (ts, nullable) ->
      make_type nullable T.(TTup (Array.of_list ts))
  ) m

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

(*$= typ & ~printer:(test_printer print_maybe_nullable)
  (Ok ((NotNullable (Mac TU8)), (2,[]))) \
     (test_p typ "u8")
  (Ok ((Nullable (Mac TU8)), (3,[]))) \
     (test_p typ "u8?")
  (Ok ((NotNullable (TVec (3, (NotNullable (Mac TU8))))), (5,[]))) \
     (test_p typ "u8[3]")
  (Ok ((NotNullable (TVec (3, (Nullable (Mac TU8))))), (6,[]))) \
     (test_p typ "u8?[3]")
  (Ok ((Nullable (TVec (3, (NotNullable (Mac TU8))))), (6,[]))) \
     (test_p typ "u8[3]?")
  (Ok ((NotNullable (TVec (3, (Nullable (TList (NotNullable (Mac TU8))))))), (8,[]))) \
     (test_p typ "u8[]?[3]")
  (Ok ((Nullable (TList (NotNullable (TVec (3, (Nullable (Mac TU8))))))), (9,[]))) \
     (test_p typ "u8?[3][]?")
  (Ok ((Nullable (TMap ((NotNullable (Mac TString)), (NotNullable (Mac TU8))))), (11,[]))) \
     (test_p typ "u8[string]?")
  (Ok ((NotNullable (TMap ((NotNullable (TMap ((Nullable (Mac TU8)), (Nullable (Mac TString))))), (Nullable (TList ((NotNullable (TTup [| (NotNullable (Mac TU8)) ; (NotNullable (TMap ((NotNullable (Mac TString)), (NotNullable (Mac TBool))))) |])))))))), (35,[]))) \
     (test_p typ "(u8; bool[string])[]?[string?[u8?]]")
*)
