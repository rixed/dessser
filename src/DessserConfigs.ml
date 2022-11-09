(* All module configurations are stored in this single module to work around
 * dependency loops. *)

module Csv =
struct
  type t =
    { separator : char ;
      (* Optional char to add (or skip) at the end of values: *)
      newline : char option ;
      null : string ;
      (* If None, strings are never quoted. Otherwise, look for quotes. *)
      quote : char option ;
      true_ : string ;
      false_ : string ;
      vectors_of_chars_as_string : bool ;
      (* Are values (esp. of compound types) encoded as described in
       * https://clickhouse.tech/docs/en/interfaces/formats#csv ? *)
      clickhouse_syntax : bool }

  let default =
    { separator = ',' ;
      newline = Some '\n' ;
      null = "\\N" ;  (* À la Postgresql *)
      quote = Some '"' ;
      true_ = "T" ;
      false_ = "F" ;
      vectors_of_chars_as_string = true ;
      clickhouse_syntax = false }

  let make separator newline null quote true_ false_ vectors_of_chars_as_string
           clickhouse_syntax =
    (* We want command line to be able to set no quote with an empty string: *)
    let quote =
      match quote with
      | None -> None
      | Some "" -> None
      | Some s ->
          assert (String.length s = 1) ;
          Some s.[0] in
    { separator ;
      newline ;
      null ;
      quote ;
      true_ ;
      false_ ;
      vectors_of_chars_as_string ;
      clickhouse_syntax }
end

module Json =
struct
  type t = { newline : char option }

  let default = { newline = None } ;
end

module SExpr =
struct
  type t =
    { list_prefix_length : bool ;
      (* Optional char added (or skipped) at end of values: *)
      newline : char option }

  let default =
    { list_prefix_length = true ;
      newline = None }

  let make list_prefix_length no_list_prefix_length newline =
    let list_prefix_length =
      match list_prefix_length, no_list_prefix_length with
      | false, false ->
          (* None set from the command line, use default: *)
          default.list_prefix_length
      | true, true ->
          invalid_arg "Cannot set and not set the list length prefix"
      | v, _ ->
          v in
    { list_prefix_length ; newline }
end

type all =
  { mutable csv : Csv.t ;
    null : unit ;
    mutable json : Json.t ;
    ringbuf : unit ;
    rowbinary : unit ;
    mutable sexpr : SExpr.t }

type all_ser = all
type all_des = all

let make_default () =
  { csv = Csv.default ;
    null = () ;
    json = Json.default ;
    ringbuf = () ;
    rowbinary = () ;
    sexpr = SExpr.default }
