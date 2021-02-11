open Batteries
open Stdint
open Dessser
module T = DessserTypes
module E = DessserExpressions
open E.Ops

(*$inject
  open Batteries
  module T = DessserTypes
*)

let debug = false

type csv_config =
  { separator : char ;
    newline : char ;
    null : string ;
    (* If None, strings are never quoted. Otherwise, look for quotes. *)
    quote : char option ;
    true_ : string ;
    false_ : string ;
    (* Are values (esp. of compound types) encoded as described in
     * https://clickhouse.tech/docs/en/interfaces/formats ? *)
    clickhouse_syntax : bool }

let default_config =
  { separator = ',' ;
    newline = '\n' ;
    null = "\\N" ;  (* À la Postgresql *)
    quote = Some '"' ;
    true_ = "T" ;
    false_ = "F" ;
    clickhouse_syntax = false }

(* Given how tuples/arrays/lists are flattened then any nullable compound type
 * must have no nullable types up to and including its first concrete item
 * (first actual serialized value). For simplicity, nullable compound types
 * are just declared invalid: *)
let rec is_serializable ?(to_first_concrete=false) mn =
  if mn.T.nullable && to_first_concrete then false else
  let to_first_concrete' = to_first_concrete || mn.nullable in
  let are_serializable mns =
    match Enum.get mns with
    | None -> false
    | Some fst ->
        is_serializable ~to_first_concrete:to_first_concrete' fst &&
        Enum.for_all (is_serializable ~to_first_concrete:false) mns in
  match mn.T.vtyp with
  | Unknown | Ext _ | Map _ | Tup [||] | Rec [||] | Sum [||] ->
      false
  | Unit | Mac _ ->
      true
  | Usr { def ; _ } ->
      is_serializable ~to_first_concrete T.{ mn with vtyp = def }
  | Vec (_, mn') | Lst mn' | Set mn' ->
      is_serializable ~to_first_concrete:to_first_concrete' mn'
  | Tup mns ->
      are_serializable (Array.enum mns)
  | Rec mns ->
      are_serializable (Array.enum mns |> Enum.map snd)
  | Sum mns ->
      (* Each alternative must be serializable independently: *)
      Array.for_all (fun (_, mn') ->
        is_serializable ~to_first_concrete:to_first_concrete' mn'
      ) mns

(*$T is_serializable
  is_serializable (T.maybe_nullable_of_string "Ip6?")
*)

(* Tells if the given type's first item is nullable *)
let rec nullable_at_first mn =
  mn.T.nullable ||
  match mn.vtyp with
  | Unknown | Ext _ | Map _ | Tup [||] | Rec [||] | Sum [||] ->
      invalid_arg "nullable_at_first"
  | Unit | Mac _ ->
      false
  | Usr { def ; _ } ->
      nullable_at_first T.{ mn with vtyp = def }
  | Vec (_, mn') | Lst mn' | Set mn' ->
      nullable_at_first mn'
  | Tup mns ->
      nullable_at_first mns.(0)
  | Rec mns ->
      nullable_at_first (snd mns.(0))
  | Sum mns ->
      Array.exists (fun (_, mn) ->
        nullable_at_first mn
      ) mns

(*$T nullable_at_first
  nullable_at_first (T.maybe_nullable_of_string "(a BOOL? | b BOOL)")
  nullable_at_first (T.maybe_nullable_of_string "(a BOOL | b BOOL?)")
  nullable_at_first (T.maybe_nullable_of_string "(a BOOL | b BOOL?)[]")
  nullable_at_first (T.maybe_nullable_of_string "(a BOOL | b BOOL?)[1]")
  not (nullable_at_first (T.maybe_nullable_of_string "(a BOOL | b BOOL)"))
*)

(* Take a maybe-nullable and make it serializable by making some compound
 * types non nullable: *)
let rec make_serializable mn =
  match mn.T.vtyp with
  | Unknown | Ext _ | Map _ | Tup [||] | Rec [||] | Sum [||] ->
      invalid_arg "make_serializable"
  | Unit | Mac _ ->
      mn
  | Usr { def ; _ } ->
      let mn' = T.{ mn with vtyp = def } in
      if is_serializable mn' then mn else make_serializable mn'
  | Vec (d, mn') ->
      let mn' = make_serializable mn' in
      { nullable = if nullable_at_first mn' then false else mn.nullable ;
        vtyp = Vec (d, mn') }
  | Lst mn' ->
      let mn' = make_serializable mn' in
      { nullable = if nullable_at_first mn' then false else mn.nullable ;
        vtyp = Lst mn' }
  | Set mn' ->
      let mn' = make_serializable mn' in
      { nullable = if nullable_at_first mn' then false else mn.nullable ;
        vtyp = Lst mn' }
  | Tup mns ->
      let mns = Array.map make_serializable mns in
      { nullable = if nullable_at_first mns.(0) then false else mn.nullable ;
        vtyp = Tup mns }
  | Rec mns ->
      let mns = Array.map (fun (n, mn) -> n, make_serializable mn) mns in
      { nullable =
          if nullable_at_first (snd mns.(0)) then false else mn.nullable ;
        vtyp = Rec mns }
  | Sum mns ->
      let mns = Array.map (fun (n, mn) -> n, make_serializable mn) mns in
      { nullable =
          if Array.exists (fun (_, mn) -> nullable_at_first mn) mns then false
          else mn.nullable ;
        vtyp = Sum mns }

let make_serializable =
  if debug then
    fun mn ->
      let mn = make_serializable mn in
      assert (is_serializable mn) ;
      mn
  else
    make_serializable

(*$inject
  let make_serializable_str =
    T.string_of_maybe_nullable %
    make_serializable %
    T.maybe_nullable_of_string
*)
(*$= make_serializable_str & ~printer:identity
  "BOOL?[4][5]" ("BOOL?[4][5]?" |> make_serializable_str)
  "(gnlj BOOL | jdlg BOOL?)[]" \
                ("(gnlj BOOL | jdlg BOOL?)[]?" |> make_serializable_str)
  "{b: {e: U32; f: (U64[]?; CHAR)[1]}[2]}" \
                ("{b: {e: U32; f: (U64[]?; CHAR)?[1]?}[2]}" |> make_serializable_str)
*)

module Ser : SER with type config = csv_config =
struct
  type config = csv_config

  type state = config

  let ptr mn =
    if not (is_serializable mn) then invalid_arg "not serializable" ;
    T.dataptr

  let start ?(config=default_config) _v p = config, p

  let stop conf p =
    write_byte p (byte_of_const_char conf.newline)

  type ser = state -> T.maybe_nullable -> T.path -> E.t -> E.t -> E.t

  let sfloat _conf _ _ v p =
    write_bytes p (bytes_of_string (string_of_float_ v))

  let sbytes_quoted conf v p =
    let quote_byte = byte_of_const_char (Option.get conf.quote) in
    (* Quote systematically: *)
    let p = write_byte p quote_byte in
    (* FIXME: escape double quotes: *)
    let p = write_bytes p v in
    write_byte p quote_byte

  let sbytes _conf v p =
    (* FIXME: escape separator/newline: *)
    write_bytes p v

  let sstring conf _ _ v p =
    (if conf.quote = None then sbytes else sbytes_quoted)
      conf (bytes_of_string v) p

  let schar conf _ _ v p =
    (if conf.quote = None then sbytes else sbytes_quoted)
      conf (bytes_of_string (string_of_char v)) p

  (* TODO: make true/false values optional *)
  let sbool conf _ _ v p =
    write_bytes p (if_ v (bytes (Bytes.of_string conf.true_))
                         (bytes (Bytes.of_string conf.false_)))

  let si _conf _ _ v p =
    write_bytes p (bytes_of_string (string_of_int_ v))

  let si8 = si
  let si16 = si
  let si24 = si
  let si32 = si
  let si40 = si
  let si48 = si
  let si56 = si
  let si64 = si
  let si128 = si
  let su8 = si
  let su16 = si
  let su24 = si
  let su32 = si
  let su40 = si
  let su48 = si
  let su56 = si
  let su64 = si
  let su128 = si

  let sep conf p =
    write_byte p (byte_of_const_char conf.separator)

  let tup_opn _conf _ _ _ p = p

  let tup_cls _conf _ _ p = p

  let tup_sep conf _ _ p = sep conf p

  let rec_opn _conf _ _ _ p = p

  let rec_cls _conf _ _ p = p

  let rec_sep conf _ _ p = sep conf p

  (* Sum label comes as a separate column: *)
  let sum_opn conf mn0 path _ lbl p =
    let p = su16 conf mn0 path lbl p in
    sep conf p

  let sum_cls _conf _ _ p = p

  let vec_opn conf _mn0 _path _dim _t p =
    if not conf.clickhouse_syntax then p else
    (* FIXME: we are supposed to switch to clickhouse's TSV from now on. *)
    (* Use mn0 and path to find out if opening that string is required *)
    let p = write_byte p (byte_of_const_char '"') in
    write_byte p (byte_of_const_char '[')

  let vec_cls conf _mn0 _path p =
    if not conf.clickhouse_syntax then p else
    (* Use mn0 and path to find out if opening that string is required *)
    let p = write_byte p (byte_of_const_char ']') in
    write_byte p (byte_of_const_char '"')

  let vec_sep conf _mn0 _path p =
    if not conf.clickhouse_syntax then sep conf p else
    write_byte p (byte_of_const_char '\t')

  (* Lists are prefixed with a column or their length: *)
  let list_opn conf mn0 path t n p =
    if not conf.clickhouse_syntax then
      match n with
      | Some n ->
          let p = su32 conf mn0 path n p in
          sep conf p
      | None ->
          failwith "Csv.Ser needs list length upfront"
    else
      vec_opn conf mn0 path 0 t p

  let list_cls conf mn0 path p =
    if not conf.clickhouse_syntax then p else
    vec_cls conf mn0 path p

  let list_sep conf mn0 path p =
    if not conf.clickhouse_syntax then sep conf p else
    vec_sep conf mn0 path p

  let nullable _conf _ _ p = p

  let snull _t conf _ _ p =
    write_bytes p (bytes (Bytes.of_string conf.null))

  let snotnull _t _conf _ _ p = p

  type ssizer = T.maybe_nullable -> T.path -> E.t -> ssize
  let todo_ssize _conf = failwith "TODO: ssize for CSV"
  let ssize_of_float _ _ _ = todo_ssize ()
  let ssize_of_string _ _ _ = todo_ssize ()
  let ssize_of_bool _ _ _ = todo_ssize ()
  let ssize_of_char _ _ _ = todo_ssize ()
  let ssize_of_i8 _ _ _ = todo_ssize ()
  let ssize_of_i16 _ _ _ = todo_ssize ()
  let ssize_of_i24 _ _ _ = todo_ssize ()
  let ssize_of_i32 _ _ _ = todo_ssize ()
  let ssize_of_i40 _ _ _ = todo_ssize ()
  let ssize_of_i48 _ _ _ = todo_ssize ()
  let ssize_of_i56 _ _ _ = todo_ssize ()
  let ssize_of_i64 _ _ _ = todo_ssize ()
  let ssize_of_i128 _ _ _ = todo_ssize ()
  let ssize_of_u8 _ _ _ = todo_ssize ()
  let ssize_of_u16 _ _ _ = todo_ssize ()
  let ssize_of_u24 _ _ _ = todo_ssize ()
  let ssize_of_u32 _ _ _ = todo_ssize ()
  let ssize_of_u40 _ _ _ = todo_ssize ()
  let ssize_of_u48 _ _ _ = todo_ssize ()
  let ssize_of_u56 _ _ _ = todo_ssize ()
  let ssize_of_u64 _ _ _ = todo_ssize ()
  let ssize_of_u128 _ _ _ = todo_ssize ()
  let ssize_of_tup _ _ _ = todo_ssize ()
  let ssize_of_rec _ _ _ = todo_ssize ()
  let ssize_of_sum _ _ _ = todo_ssize ()
  let ssize_of_vec _ _ _ = todo_ssize ()
  let ssize_of_list _ _ _ = todo_ssize ()
  let ssize_of_null _ _ = todo_ssize ()
  let ssize_start _ = todo_ssize ()
end

module Des : DES with type config = csv_config =
struct
  type config = csv_config

  type state = config

  let ptr mn =
    if not (is_serializable mn) then invalid_arg "not serializable" ;
    T.dataptr

  let start ?(config=default_config) _mn p = config, p

  type des = state -> T.maybe_nullable -> T.path -> E.t -> E.t

  let skip n p = data_ptr_add p (size n)

  let skip_byte b p =
    (* On debug, check that the expected character is present: *)
    if debug then
      seq [ assert_ (eq (peek_byte p (size 0)) b) ;
            skip 1 p ]
    else
      skip 1 p

  let skip_char c p =
    skip_byte (byte_of_const_char c) p

  let skip_sep conf p =
    skip_char conf.separator p

  let skip_nl conf p =
    skip_char conf.newline p

  (* Skip the final newline if present: *)
  let stop conf p =
    let ret =
      if_
        ~cond:(gt (rem_size p) (size 0))
        ~then_:(skip_nl conf p)
        ~else_:p in
    if debug then
      seq [ dump (string "rec stop at offset ") ;
            dump (data_ptr_offset p) ;
            dump (string "with rem size ") ;
            dump (data_ptr_remsize p) ;
            dump (string "\n") ;
            ret ]
    else
      ret

  let dfloat _conf _ _ p =
    float_of_ptr p

  let dbool conf _ _ p =
    (* TODO: Look for false_.[0] otherwise: *)
    (* TODO: find out where is the first distinct char of true and false (that
     * may not be in position 0) and test only that one *)
    assert (String.length conf.true_ > 0) ;
    if_
      ~cond:(eq (peek_byte p (size 0)) (byte_of_const_char (conf.true_.[0])))
      ~then_:(pair true_ (skip (String.length conf.true_) p))
      ~else_:(pair false_ (skip (String.length conf.false_) p))

  (* Read a string of bytes and process them through [conv]: *)
  let dbytes_quoted conf op p =
    (* Skip the double-quote: *)
    let quote_byte = byte_of_const_char (Option.get conf.quote)
    and sep_byte = byte_of_const_char conf.separator
    and nl_byte = byte_of_const_char conf.newline in
    let_ ~name:"had_quote"
      (and_ (ge (rem_size p) (size 2))
            (eq (peek_byte p (size 0)) quote_byte))
      (fun _l had_quote ->
        let pos = if_ ~cond:had_quote
                      ~then_:(skip_byte quote_byte p)
                      ~else_:p in
        (* Read up to next double-quote or separator/newline, depending on
         * had_quote: *)
        (* FIXME: handle escaping the separator/newline! *)
        let cond =
          E.func1 T.byte (fun _l b ->
            not_ (
              if_ ~cond:had_quote
                  ~then_:(eq b quote_byte)
                  ~else_:(or_ (eq b sep_byte)
                              (eq b nl_byte))))
        and init = size 0
        and reduce = E.func2 T.size T.byte (fun _l s _b -> add s (size 1)) in
        let sz_p = read_while ~cond ~reduce ~init ~pos in
        E.with_sploded_pair "dbytes_quoted1" sz_p (fun sz p' ->
          (* Skip the initial double-quote: *)
          let bytes_p = read_bytes pos sz in
          (* Skip the closing double-quote: *)
          let p' = if_ ~cond:had_quote
                       ~then_:(skip_byte quote_byte p')
                       ~else_:p' in
          pair (op (first bytes_p)) p'))

  let dbytes conf op p =
    let sep_byte = byte_of_const_char conf.separator
    and nl_byte = byte_of_const_char conf.newline in
    (* Read up to next separator/newline *)
    let cond =
      E.func1 T.byte (fun _l b ->
        not_ ((or_ (eq b sep_byte)
                   (eq b nl_byte))))
    and init = size 0
    and reduce = E.func2 T.size T.byte (fun _l s _b -> add s (size 1)) in
    let sz_p = read_while ~cond ~reduce ~init ~pos:p in
    E.with_sploded_pair "dbytes" sz_p (fun sz p' ->
      let bytes_p = read_bytes p sz in
      pair (op (first bytes_p)) p')

  let dstring conf _ _ p =
    (if conf.quote = None then dbytes else dbytes_quoted)
      conf string_of_bytes p

  (* Chars are encoded as single char strings *)
  let dchar conf _ _ p =
    (if conf.quote = None then dbytes else dbytes_quoted)
      conf (char_of_string % string_of_bytes) p

  let di8 _conf _ _ p = i8_of_ptr p
  let du8 _conf _ _ p = u8_of_ptr p
  let di16 _conf _ _ p = i16_of_ptr p
  let du16 _conf _ _ p = u16_of_ptr p
  let di24 _conf _ _ p = i24_of_ptr p
  let du24 _conf _ _ p = u24_of_ptr p
  let di32 _conf _ _ p = i32_of_ptr p
  let du32 _conf _ _ p = u32_of_ptr p
  let di40 _conf _ _ p = i40_of_ptr p
  let du40 _conf _ _ p = u40_of_ptr p
  let di48 _conf _ _ p = i48_of_ptr p
  let du48 _conf _ _ p = u48_of_ptr p
  let di56 _conf _ _ p = i56_of_ptr p
  let du56 _conf _ _ p = u56_of_ptr p
  let di64 _conf _ _ p = i64_of_ptr p
  let du64 _conf _ _ p = u64_of_ptr p
  let di128 _conf _ _ p = i128_of_ptr p
  let du128 _conf _ _ p = u128_of_ptr p

  let tup_opn _conf _ _ _ p = p

  let tup_cls _conf _ _ p = p

  let tup_sep conf _ _ p =
    skip_sep conf p

  let rec_opn _conf _ _ _ p = p

  let rec_cls _conf _ _ p = p

  let rec_sep conf _ _ p =
      skip_sep conf p

  let sum_opn conf mn0 path _ p =
    let c_p = du16 conf mn0 path p in
    E.with_sploded_pair "sum_opn" c_p (fun c p ->
      pair c (skip_sep conf p))

  let sum_cls _conf _ _ p = p

  let vec_opn conf _mn0 _path _dim _t p =
    if not conf.clickhouse_syntax then p else
    (* FIXME: we may switch back from clickhouse's TSV from now on. *)
    (* Use mn0 and path to find out if opening that string is required *)
    let p = skip_char '"' p in
    skip_char '[' p

  let vec_cls conf _mn0 _path p =
    if not conf.clickhouse_syntax then p else
    (* FIXME: we may switch back from clickhouse's TSV from now on. *)
    (* Use mn0 and path to find out if opening that string is required *)
    let p = skip_char ']' p in
    skip_char '"' p

  let vec_sep conf _ _ p =
    if not conf.clickhouse_syntax then skip_sep conf p else
    skip_char '\t' p

  let list_opn conf =
    if not conf.clickhouse_syntax then
      KnownSize (fun mn0 path _ p ->
        E.with_sploded_pair "list_opn" (du32 conf mn0 path p) (fun v p ->
          pair v (skip_sep conf p)))
    else
      UnknownSize (
        (fun _ _ _ p -> p),
        (fun _mn0 _path p ->
          (* Won't work for nested compound types: *)
          (eq (peek_byte p (size 0)) (byte_of_const_char ']'))))

  let list_cls conf mn0 path p =
    if not conf.clickhouse_syntax then p else
    vec_cls conf mn0 path p

  let list_sep conf mn0 path p =
    if not conf.clickhouse_syntax then skip_sep conf p else
    vec_sep conf mn0 path p

  let is_null conf _ _ p =
    let len = String.length conf.null in
    let rec loop i =
      if i >= len then
        (comment (Printf.sprintf "Test end of string %S" conf.null)
          (or_ (eq (rem_size p) (size len))
               (let_ ~name:"b" (peek_byte p (size len)) (fun _l b ->
                 or_ (eq b (byte_of_const_char conf.separator))
                     (eq b (byte_of_const_char conf.newline))))))
      else
        and_
          (comment (Printf.sprintf "Test char %d of %S" i conf.null)
            (let b = byte (Uint8.of_int (Char.code conf.null.[i])) in
             eq (peek_byte p (size i)) b))
          (loop (i + 1))
    in
    (* Note: avoids a warning when comparing size >= len if len is 0: *)
    if len > 0 then
      (and_ (ge (rem_size p) (size len))
            (loop 0))
    else
      (loop 0)

  let dnull _t conf _ _ p =
    skip (String.length conf.null) p

  let dnotnull _t _conf _ _ p = p
end
