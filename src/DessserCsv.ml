open Batteries
open Stdint

open Dessser
open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module Conf = DessserConfigs.Csv
module Path = DessserPath
open E.Ops

(*$inject
  open Batteries
  module P = DessserParser
  module T = DessserTypes
*)

let debug = false

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
  match mn.T.typ with
  | TThis _ ->
      (* If everything else is serializable then This is also serializable.
       * Or let any non-serializable field fails. *)
      true
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 ->
      true
  | TUsr { def ; _ } ->
      is_serializable ~to_first_concrete T.{ mn with typ = def }
  | TVec (_, mn') | TArr mn' | TSet (_, mn') ->
      is_serializable ~to_first_concrete:to_first_concrete' mn'
  | TTup mns ->
      are_serializable (Array.enum mns)
  | TRec mns ->
      are_serializable (Array.enum mns |> Enum.map snd)
  | TSum mns ->
      (* Each alternative must be serializable independently: *)
      Array.for_all (fun (_, mn') ->
        is_serializable ~to_first_concrete:to_first_concrete' mn'
      ) mns
  | _ ->
      false

(*$T is_serializable
  is_serializable (P.mn_of_string "Ip6?")
*)

(* Tells if the given type's first item is nullable *)
let rec nullable_at_first mn =
  mn.T.nullable ||
  match mn.typ with
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 ->
      false
  | TUsr { def ; _ } ->
      nullable_at_first T.{ mn with typ = def }
  | TVec (_, mn') | TArr mn' | TSet (_, mn') ->
      nullable_at_first mn'
  | TTup mns ->
      nullable_at_first mns.(0)
  | TRec mns ->
      nullable_at_first (snd mns.(0))
  | TSum mns ->
      Array.exists (fun (_, mn) ->
        nullable_at_first mn
      ) mns
  | _ ->
      invalid_arg "nullable_at_first"

(*$T nullable_at_first
  nullable_at_first (P.mn_of_string "[a BOOL? | b BOOL]")
  nullable_at_first (P.mn_of_string "[a BOOL | b BOOL?]")
  nullable_at_first (P.mn_of_string "[a BOOL | b BOOL?][]")
  nullable_at_first (P.mn_of_string "[a BOOL | b BOOL?][1]")
  not (nullable_at_first (P.mn_of_string "[a BOOL | b BOOL]"))
*)

(* Take a maybe-nullable and make it serializable by making some compound
 * types non nullable: *)
(* FIXME: try harder to keep user provided mn (with its default...) if it
 * is serialiable *)
let rec make_serializable mn =
  match mn.T.typ with
  | TThis _ ->
      todo "make_serializable for This"
  | TBool | TChar | TFloat | TString
  | TU8 | TU16 | TU24 | TU32 | TU40 | TU48 | TU56 | TU64 | TU128
  | TI8 | TI16 | TI24 | TI32 | TI40 | TI48 | TI56 | TI64 | TI128 ->
      mn
  | TUsr { def ; _ } ->
      let mn' = T.{ mn with typ = def } in
      if is_serializable mn' then mn else make_serializable mn'
  | TVec (d, mn') ->
      let mn' = make_serializable mn' in
      { nullable = if nullable_at_first mn' then false else mn.nullable ;
        typ = TVec (d, mn') ; default = None }
  | TArr mn' ->
      let mn' = make_serializable mn' in
      { nullable = if nullable_at_first mn' then false else mn.nullable ;
        typ = TArr mn' ; default = None }
  | TSet (_, mn') ->
      let mn' = make_serializable mn' in
      { nullable = if nullable_at_first mn' then false else mn.nullable ;
        typ = TArr mn' ; default = None }
  | TTup mns ->
      let mns = Array.map make_serializable mns in
      { nullable = if nullable_at_first mns.(0) then false else mn.nullable ;
        typ = TTup mns ; default = None }
  | TRec mns ->
      let mns = Array.map (fun (n, mn) -> n, make_serializable mn) mns in
      { nullable =
          if nullable_at_first (snd mns.(0)) then false else mn.nullable ;
        typ = TRec mns ; default = None }
  | TSum mns ->
      let mns = Array.map (fun (n, mn) -> n, make_serializable mn) mns in
      { nullable =
          if Array.exists (fun (_, mn) -> nullable_at_first mn) mns then false
          else mn.nullable ;
        typ = TSum mns ; default = None }
  | _ ->
      invalid_arg "make_serializable"

let make_serializable =
  if debug then
    fun mn ->
      let mn = make_serializable mn in
      assert (is_serializable mn) ;
      mn
  else
    make_serializable

(* As a special case, we want vectors of chars to be encoded as a string
 * (because ClickHouse FixedString are rightfully vectors of chars, but
 * then clickhouse expects to receive them in CSV as strings, so we do
 * the same) *)
let is_fixed_string mn0 path =
  match Path.(type_of_path mn0 path).typ |> T.develop with
  | TVec (_, { typ = TChar ; nullable = false } ) ->
      true
  | _ ->
      false

let is_in_fixed_string mn0 path =
  if path = [] then
    (* outermost value, so not within a vector *)
    false
  else
    let parent_path, _ = list_split_last path in
    is_fixed_string mn0 parent_path

let is_list mn0 path =
  match Path.(type_of_path mn0 path).typ |> T.develop with
  | TVec _ | TArr _ ->
      true
  | _ ->
      false

let is_in_list mn0 path =
  if path = [] then
    (* outermost value, so not within a vector *)
    false
  else
    let parent_path, _ = list_split_last path in
    is_list mn0 parent_path

(*$inject
  let make_serializable_str =
    T.mn_to_string %
    make_serializable %
    P.mn_of_string
*)
(*$= make_serializable_str & ~printer:identity
  "BOOL?[4][5]" ("BOOL?[4][5]?" |> make_serializable_str)
  "[gnlj BOOL | jdlg BOOL?][]" \
                ("[gnlj BOOL | jdlg BOOL?][]?" |> make_serializable_str)
  "{b: {e: U32; f: (U64[]?; CHAR)[1]}[2]}" \
                ("{b: {e: U32; f: (U64[]?; CHAR)?[1]?}[2]}" |> make_serializable_str)
*)

let quote_byte conf mn0 path =
  let quote_char =
    if conf.Conf.clickhouse_syntax && is_in_list mn0 path then '\''
    else Option.get conf.quote in
  u8_of_const_char quote_char

let sep_byte conf mn0 path =
  let sep_char =
    if conf.Conf.clickhouse_syntax && is_in_list mn0 path then
      ','
    else
      conf.separator in
  u8_of_const_char sep_char

let trimmed_string conf mn0 path =
  if conf.Conf.clickhouse_syntax && is_in_list mn0 path then
    " "
  else
    conf.trimmed

module Ser : SER with type config = Conf.t =
struct
  let id = CSV

  type config = Conf.t

  type state = config

  let select_config csv _sexpr = csv

  let make_state ?(config=Conf.default) mn0 =
    if not (is_serializable mn0) then invalid_arg "not serializable" ;
    config

  let start _conf p = p

  let stop conf p =
    match conf.Conf.newline with
    | None ->
        p
    | Some c ->
        write_u8 p (u8_of_const_char c)

  type ser = state -> T.mn -> Path.t -> E.t -> E.t -> E.t

  let sfloat _conf _ _ v p =
    write_bytes p (bytes_of_string (string_of_float_ v))

  let sbytes_quoted conf mn0 path v p =
    let quote_byte = quote_byte conf mn0 path in
    (* Quote systematically: *)
    let p = write_u8 p quote_byte in
    (* FIXME: escape double quotes: *)
    let p = write_bytes p v in
    write_u8 p quote_byte

  let sbytes_not_quoted v p =
    (* FIXME: escape separator/newline: *)
    write_bytes p v

  let sbytes conf mn0 path v p =
    if conf.Conf.quote <> None || (conf.clickhouse_syntax && is_in_list mn0 path) then
      sbytes_quoted conf mn0 path v p
    else
      sbytes_not_quoted v p

  let sstring conf mn0 path v p =
    sbytes conf mn0 path (bytes_of_string v) p

  (* Individual chars are represented as single char strings, but for
   * the special case of vectors of chars, in which case we want the
   * whole vector to be a string (see discussion about FixedStrings) *)
  let schar conf mn0 path v p =
    if conf.Conf.vectors_of_chars_as_string && is_in_fixed_string mn0 path then
      comment "char in a FixedString"
        (write_u8 p (u8_of_char v))
    else
      sbytes conf mn0 path (bytes_of_string (string_of_char v)) p

  (* TODO: make true/false values optional *)
  let sbool conf _ _ v p =
    write_bytes p (if_ v (bytes (Bytes.of_string conf.Conf.true_))
                         (bytes (Bytes.of_string conf.Conf.false_)))

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

  let sext f _conf _ _ v p =
    f v p

  let sep conf mn0 path p =
    let sep_byte = sep_byte conf mn0 path in
    write_u8 p sep_byte

  let tup_opn _conf _ _ _ p = p

  let tup_cls _conf _ _ p = p

  let tup_sep conf mn0 path p = sep conf mn0 path p

  let rec_opn _conf _ _ _ p = p

  let rec_cls _conf _ _ p = p

  let rec_sep conf mn0 path p = sep conf mn0 path p

  (* Sum label comes as a separate column: *)
  let sum_opn _ lbl conf mn0 path p =
    let p = su16 conf mn0 path lbl p in
    sep conf mn0 path p

  let sum_cls _lbl _conf _ _ p = p

  let write_quote conf p =
    match conf.Conf.quote with
    | None -> p
    | Some c -> write_u8 p (u8_of_const_char c)

  let vec_opn _dim _t conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_fixed_string mn0 path then
      match conf.quote with
      | None -> p
      | Some q -> write_u8 p (u8_of_const_char q)
    else if conf.clickhouse_syntax then
      (* FIXME: we are supposed to switch to clickhouse's TSV from now on. *)
      (* Use mn0 and path to find out if opening that string is required *)
      let p = write_quote conf p in
      write_u8 p (u8_of_const_char '[')
    else
      p

  let vec_cls conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_fixed_string mn0 path then
      match conf.quote with
      | None -> p
      | Some q -> write_u8 p (u8_of_const_char q)
    else if conf.clickhouse_syntax then
      (* Use mn0 and path to find out if opening that string is required *)
      let p = write_u8 p (u8_of_const_char ']') in
      write_quote conf p
    else
      p

  let vec_sep conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_in_fixed_string mn0 path then
      p
    else
      sep conf mn0 path p

  (* Lists are prefixed with a column or their length: *)
  let arr_opn t n conf mn0 path p =
    if conf.Conf.clickhouse_syntax then
      vec_opn 0 t conf mn0 path p
    else
      match n with
      | Some n ->
          let p = su32 conf mn0 path n p in
          sep conf mn0 path p
      | None ->
          failwith "Csv.Ser needs list length upfront"

  let arr_cls conf mn0 path p =
    if conf.Conf.clickhouse_syntax then
      vec_cls conf mn0 path p
    else
      p

  let arr_sep conf mn0 path p =
    sep conf mn0 path p

  let nullable _conf _ _ p = p

  let snull _t conf _ _ p =
    write_bytes p (bytes (Bytes.of_string conf.Conf.null))

  let snotnull _t _conf _ _ p = p

  type ssizer = T.mn -> Path.t -> E.t -> E.t
  let todo_ssize () = failwith "TODO: ssize for CSV"
  let ssize_of_float _ _ _ = todo_ssize ()
  let ssize_of_string _ _ _ = todo_ssize ()
  let ssize_of_bytes _ _ _ = todo_ssize ()
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
  let ssize_of_arr _ _ _ = todo_ssize ()
  let ssize_of_null _ _ = todo_ssize ()
  let ssize_of_notnull _ _ = todo_ssize ()
  let ssize_start ?(config=Conf.default) _ =
    ignore config ;
    todo_ssize ()
end

module Des : DES with type config = Conf.t =
struct
  let id = CSV

  type config = Conf.t

  type state = config

  let select_config csv _sexpr = csv

  let make_state ?(config=Conf.default) mn0 =
    if not (is_serializable mn0) then invalid_arg "not serializable" ;
    config

  let start _conf p = p

  type des = state -> T.mn -> Path.t -> E.t -> E.t

  let skip n p = ptr_add p (size n)

  let skip_byte b p =
    (* On debug, check that the expected character is present: *)
    if debug then
      let_ ~name:"skip_byte_p" p (fun p ->
        seq [
          StdLib.check_byte p b ;
          skip 1 p ])
    else
      skip 1 p

  let skip_char c p =
    skip_byte (u8_of_const_char c) p

  let skip_chars_ chars init_p =
    let_ ~name:"p_ref" (make_ref init_p) (fun p_ref ->
      let p = get_ref p_ref in
      let_ ~name:"sz_ref" (make_ref (size 0)) (fun sz_ref ->
        let sz = get_ref sz_ref in
        seq [
          while_
            (and_ (lt sz (size (String.length chars)))
                  (let b = peek_u8 p sz in
                   StdLib.is_in (char_of_u8 b) T.char (string chars) T.string))
            (set_ref sz_ref (add sz (size 1))) ;
          ptr_add init_p sz ]))

  let skip_chars conf mn0 path init_p =
    let chars = trimmed_string conf mn0 path in
    skip_chars_ chars init_p

  let skip_sep conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    skip_char conf.Conf.separator p

  let may_skip_quote conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    match conf.Conf.quote with
    | None -> p
    | Some c -> skip_char c p

  (* Skip the final newline if present: *)
  let stop conf p =
    (* We know we are outside of any special CH notation at stop, yet we still
     * want to trim the last value: *)
    let p = skip_chars_ conf.Conf.trimmed p in
    match conf.Conf.newline with
    | None ->
        p
    | Some c ->
        if_ (gt (rem_size p) (size 0))
          ~then_:(skip_char c p)
          ~else_:p

  let dfloat conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    float_of_ptr p

  let dbool conf mn0 path p =
    (* TODO: Look for false_.[0] otherwise: *)
    (* TODO: find out where is the first distinct char of true and false (that
     * may not be in position 0) and test only that one *)
    let p = skip_chars conf mn0 path p in
    assert (String.length conf.Conf.true_ > 0) ;
    if_ (eq (peek_u8 p (size 0)) (u8_of_const_char (conf.true_.[0])))
      ~then_:(make_pair true_ (skip (String.length conf.true_) p))
      ~else_:(make_pair false_ (skip (String.length conf.false_) p))

  let is_sep_or_newline conf mn0 path b =
    let sep_byte = sep_byte conf mn0 path in
    match conf.newline with
    | None ->
        or_ (eq b sep_byte)
            (StdLib.is_in (char_of_u8 b) T.char (string conf.Conf.trimmed) T.string)
    | Some c ->
        or_ (or_ (eq b sep_byte)
                 (eq b (u8_of_const_char c)))
            (StdLib.is_in (char_of_u8 b) T.char (string conf.Conf.trimmed) T.string)

  (* Read a string of bytes and process them through [conv]: *)
  let dbytes_quoted conf mn0 path p =
    (* Skip the double-quote: *)
    let quote_byte = quote_byte conf mn0 path in
    let_ ~name:"had_quote"
      (and_ (ge (rem_size p) (size 2))
            (eq (peek_u8 p (size 0)) quote_byte))
      (fun had_quote ->
        let init_p =
          (* Skip the initial quote: *)
          if_ had_quote
            ~then_:(skip_byte quote_byte p)
            ~else_:p in
        let_ ~name:"p_ref" (make_ref init_p) (fun p_ref ->
          let p = get_ref p_ref in
          let_ ~name:"sz_ref" (make_ref (size 0)) (fun sz_ref ->
            let sz = get_ref sz_ref in
            seq [
              while_
                (E.with_sploded_pair "dbytes_quoted" (read_u8 p) (fun b p' ->
                  (* Read up to next double-quote or separator/newline,
                   * depending on had_quote, without advancing the pointer
                   * after the delimiter: *)
                  (* FIXME: handle escaping the separator/newline! *)
                  let continue =
                    if_ had_quote
                      ~then_:(ne b quote_byte)
                      ~else_:(not_ (is_sep_or_newline conf mn0 path b)) in
                  let_ ~name:"continue" continue (fun continue ->
                    seq [
                      seq [];
                      if_ (or_ had_quote continue)
                        ~then_:(set_ref p_ref p')
                        ~else_:nop ;
                      continue ])))
                (set_ref sz_ref (add sz (size 1))) ;
              let bytes_p = read_bytes init_p sz in
              make_pair (first bytes_p) p ])))

  let dbytes_not_quoted conf mn0 path p =
    let init_p = p in
    let_ ~name:"p_ref" (make_ref init_p) (fun p_ref ->
      let p = get_ref p_ref in
      let_ ~name:"sz_ref" (make_ref (size 0)) (fun sz_ref ->
        let sz = get_ref sz_ref in
        seq [
          while_
            (E.with_sploded_pair "dbytes" (read_u8 p) (fun b p' ->
              (* Read up to next double-quote or separator/newline *)
              let continue = not_ (is_sep_or_newline conf mn0 path b) in
              let_ ~name:"continue" continue (fun continue ->
                seq [
                  if_ continue
                    ~then_:(set_ref p_ref p')
                    ~else_:nop ;
                  continue ])))
            (set_ref sz_ref (add sz (size 1))) ;
          read_bytes init_p sz ]))

  let dbytes conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    (
      if conf.Conf.quote <> None ||
         (conf.clickhouse_syntax && is_in_list mn0 path) then
        dbytes_quoted
      else
        dbytes_not_quoted
    ) conf mn0 path p

  let dstring conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    let_pair ~n1:"v" ~n2:"p" (dbytes conf mn0 path p) (fun v p ->
      make_pair (string_of_bytes v) p)

  (* Chars are encoded as single char strings (unless part of a FixedString) *)
  let dchar conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_in_fixed_string mn0 path then
      E.with_sploded_pair "dchar" (read_u8 p) (fun b p ->
        make_pair (char_of_u8 b) p)
    else
      let p = skip_chars conf mn0 path p in
      let_pair ~n1:"v" ~n2:"p" (dbytes conf mn0 path p) (fun v p ->
        make_pair (char_of_u8 (unsafe_nth (u8_of_int 0) v)) p)

  let trimmed x_of_ptr conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    x_of_ptr p

  let di8 = trimmed i8_of_ptr
  let du8 = trimmed u8_of_ptr
  let di16 = trimmed i16_of_ptr
  let du16 = trimmed u16_of_ptr
  let di24 = trimmed i24_of_ptr
  let du24 = trimmed u24_of_ptr
  let di32 = trimmed i32_of_ptr
  let du32 = trimmed u32_of_ptr
  let di40 = trimmed i40_of_ptr
  let du40 = trimmed u40_of_ptr
  let di48 = trimmed i48_of_ptr
  let du48 = trimmed u48_of_ptr
  let di56 = trimmed i56_of_ptr
  let du56 = trimmed u56_of_ptr
  let di64 = trimmed i64_of_ptr
  let du64 = trimmed u64_of_ptr
  let di128 = trimmed i128_of_ptr
  let du128 = trimmed u128_of_ptr

  let dext f conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    f p

  let tup_opn _conf _ _ _ p = p

  let tup_cls _conf _ _ p = p

  let tup_sep = skip_sep

  let rec_opn _conf _ _ _ p = p

  let rec_cls _conf _ _ p = p

  let rec_sep = skip_sep

  let sum_opn _ conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    let c_p = du16 conf mn0 path p in
    E.with_sploded_pair "sum_opn_csv" c_p (fun c p ->
      make_pair c (skip_sep conf mn0 path p))

  let sum_cls _lbl _conf _ _ p = p

  let vec_opn _dim _t conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_fixed_string mn0 path then
      match conf.quote with
      | None -> p
      | Some _ -> skip 1 p
    else if conf.clickhouse_syntax then
      (* FIXME: we may switch back from clickhouse's TSV from now on. *)
      (* Use mn0 and path to find out if opening that string is required *)
      (* TODO: support for optional quote around that notation? *)
      let p = may_skip_quote conf mn0 path p in
      skip_char '[' p
    else
      p

  let vec_cls conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_fixed_string mn0 path then
      match conf.quote with
      | None -> p
      | Some _ -> skip 1 p
    else if conf.clickhouse_syntax then
      (* FIXME: we may switch back from clickhouse's TSV from now on. *)
      (* Use mn0 and path to find out if opening that string is required *)
      (* TODO: support for optional quote around that notation? *)
      let p = skip_char ']' p in
      may_skip_quote conf mn0 path p
    else
      p

  let vec_sep conf mn0 path p =
    if conf.Conf.vectors_of_chars_as_string && is_in_fixed_string mn0 path then
      p
    else if conf.clickhouse_syntax then
      let p = skip_chars conf mn0 path p in
      skip_char ',' p
    else
      skip_sep conf mn0 path p

  let arr_opn conf =
    if conf.Conf.clickhouse_syntax then
      UnknownSize (
        (fun _ mn0 path p ->
          let p = may_skip_quote conf mn0 path p in
          skip_char '[' p),
        (fun mn0 path p ->
          (* Won't work for nested compound types: *)
          let p = skip_chars conf mn0 path p in
          eq (peek_u8 p (size 0)) (u8_of_const_char ']')))
    else
      KnownSize (fun _ mn0 path p ->
        let p = skip_chars conf mn0 path p in
        let sz = du32 conf mn0 path p in
        E.with_sploded_pair "arr_opn" sz (fun v p ->
          make_pair v (skip_sep conf mn0 path p)))

  let arr_cls conf mn0 path p =
    if conf.Conf.clickhouse_syntax then
      vec_cls conf mn0 path p
    else
      p

  let arr_sep conf mn0 path p =
    if conf.Conf.clickhouse_syntax then
      vec_sep conf mn0 path p
    else
      skip_sep conf mn0 path p

  let is_present _conf _mn0 _path _p = true_

  let is_null conf mn0 path p =
    let len = String.length conf.Conf.null in
    let rec loop i =
      if i >= len then
        comment (Printf.sprintf "Test end of string %S" conf.null)
          (or_ (eq (rem_size p) (size len))
               (let_ ~name:"b" (peek_u8 p (size len)) (fun b ->
                 is_sep_or_newline conf mn0 path b)))
      else
        and_
          (comment (Printf.sprintf "Test char %d of %S" i conf.null)
            (let b = u8_of_const_char conf.null.[i] in
             eq (peek_u8 p (size i)) b))
          (loop (i + 1))
    in
    (* Note: avoids a warning when comparing size >= len if len is 0: *)
    if len > 0 then
      let p = skip_chars conf mn0 path p in
      and_ (ge (rem_size p) (size len))
           (loop 0)
    else
      (loop 0)

  let dnull _t conf mn0 path p =
    let p = skip_chars conf mn0 path p in
    skip (String.length conf.Conf.null) p

  let dnotnull _t _conf _ _ p = p
end
