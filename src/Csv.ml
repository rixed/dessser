open Batteries
open Stdint
open Dessser
module T = DessserTypes
module E = DessserExpressions
open E.Ops

type csv_config =
  { separator : char ;
    newline : char ;
    null : string ;
    quote_strings : bool ;
    true_ : string ;
    false_ : string }

let default_config =
  { separator = ',' ;
    newline = '\n' ;
    null = "\\N" ;  (* À la Postgresql *)
    quote_strings = false ;
    true_ = "T" ;
    false_ = "F" }

(* In a CSV, all structures are flattened as CSV columns.
 * The problem there is that there is then no way to nullify the whole
 * compound value. *)
let rec no_nullable_compound_types mn =
  let rec no_nullable_compound_types_vt = function
    | T.Unknown | Mac _ as vt ->
        vt
    | Usr { def ; name } ->
        Usr { def = no_nullable_compound_types_vt def ; name }
    | TVec (d, mn) ->
        TVec (d, no_nullable_compound_types mn)
    | TList mn ->
        TList (no_nullable_compound_types mn)
    | TTup mns ->
        TTup (Array.map no_nullable_compound_types mns)
    | TRec mns ->
        TRec (Array.map (fun (n, mn) -> n, no_nullable_compound_types mn) mns)
    | TSum mns ->
        TSum (Array.map (fun (n, mn) -> n, no_nullable_compound_types mn) mns)
    | TMap (k, v) ->
        TMap (no_nullable_compound_types k, no_nullable_compound_types v)
  in
  T.{ vtyp = no_nullable_compound_types_vt mn.T.vtyp ;
      nullable = false }

module Ser : SER with type config = csv_config =
struct
  type config = csv_config

  type state = config

  let ptr _vtyp = T.dataptr

  let start ?(config=default_config) _v p = config, p

  let stop conf p =
    write_byte p (byte_of_const_char conf.newline)

  type ser = state -> T.maybe_nullable -> T.path -> E.t -> E.t -> E.t

  let sfloat _conf _ _ v p =
    write_bytes p (bytes_of_string (string_of_float v))

  let sbytes conf v p =
    let quo = byte_of_const_char '"' in
    let p = if conf.quote_strings then write_byte p quo else p in
    (* FIXME: escape double quotes/separator/newline: *)
    let p = write_bytes p v in
    if conf.quote_strings then write_byte p quo else p

  let sstring conf _ _ v p = sbytes conf (bytes_of_string v) p
  let schar conf _ _ v p = sbytes conf (bytes_of_string (string_of_char v)) p

  (* TODO: make true/false values optional *)
  let sbool conf _ _ v p =
    write_bytes p (choose v (bytes (Bytes.of_string conf.true_))
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

  let tup_sep _n conf _ _ p = sep conf p

  let rec_opn _conf _ _ _ p = p

  let rec_cls _conf _ _ p = p

  let rec_sep _n conf _ _ p = sep conf p

  (* Sum label comes as a separate column: *)
  let sum_opn conf mn0 path _ lbl p =
    let p = su16 conf mn0 path lbl p in
    sep conf p

  let sum_cls _conf _ _ p = p

  let vec_opn _conf _ _ _ _ p = p

  let vec_cls _conf _ _ p = p

  let vec_sep conf _ _ p = sep conf p

  (* Lists are prefixed with a column or their length: *)
  let list_opn conf vtyp0 path _ n p =
    match n with
    | Some n ->
        let p = su32 conf vtyp0 path n p in
        sep conf p
    | None ->
        failwith "Csv.Ser needs list length upfront"

  let list_cls _conf _ _ p = p

  let list_sep conf _ _ p = sep conf p

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
end

module Des : DES with type config = csv_config =
struct
  type config = csv_config

  type state = config

  let ptr _vtyp = T.dataptr

  let start ?(config=default_config) _mn p = config, p

  type des = state -> T.maybe_nullable -> T.path -> E.t -> E.t

  let skip n p = data_ptr_add p (size n)

  let skip1 = skip 1

  (* Skip the final newline if present: *)
  let stop conf p =
    choose
      ~cond:(and_ (gt (rem_size p) (size 0))
                  (eq (peek_byte p (size 0))
                      (byte_of_const_char conf.newline)))
      ~then_:(skip1 p)
      ~else_:p

  (* Accumulate bytes into a string that is then converted with [op]: *)
  let di op conf _ _ p =
    (* Accumulate everything up to the next field or line separator, and then
     * run [op] to convert from a string: *)
    let cond =
      E.func1 T.byte (fun _l b ->
        not_ (or_ (eq b (byte_of_const_char conf.separator))
                  (eq b (byte_of_const_char conf.newline))))
    and init = bytes_of_string (string "")
    and reduce = E.func2 T.bytes T.byte (fun _l -> append_byte) in
    let str_p = read_while ~cond ~reduce ~init ~pos:p in
    E.with_sploded_pair "di" str_p (fun str p ->
      pair (op (string_of_bytes str)) p)

  let tup_cls _conf _ _ p = p

  let tup_sep _n _conf _ _ p = skip1 p

  let dfloat = di float_of_string

  let dbool conf _ _ p =
    (* TODO: Look for false_.[0] otherwise: *)
    (* TODO: find out where is the first distinct char of true and false (that
     * may not be in position 0) and test only that one *)
    assert (String.length conf.true_ > 0) ;
    choose
      ~cond:(eq (peek_byte p (size 0)) (byte_of_const_char (conf.true_.[0])))
      ~then_:(pair (bool true) (skip (String.length conf.true_) p))
      ~else_:(pair (bool false) (skip (String.length conf.false_) p))

  (* Read a string of bytes and process them through [conv]: *)
  let dbytes conf conv p =
    (* Skip the double-quote: *)
    let p = if conf.quote_strings then skip1 p else p in
    (* Read up to next double-quote or separator depending on quote_strings: *)
    (* FIXME: handle escaping the separator/newline! *)
    let cond =
      E.func1 T.byte (fun _l b ->
        not_ (
          if conf.quote_strings then
            eq b (byte_of_const_char '"')
          else
            or_ (eq b (byte_of_const_char conf.separator))
                (eq b (byte_of_const_char conf.newline))))
    and init = bytes_of_string (string "")
    and reduce = E.func2 T.bytes T.byte (fun _l -> append_byte) in
    let str_p = read_while ~cond ~reduce ~init ~pos:p in
    E.with_sploded_pair "dbytes" str_p (fun str p ->
      (* Skip the closing double-quote: *)
      let p = if conf.quote_strings then skip1 p else p in
      pair (conv str) p)

  let dstring conf _ _ p = dbytes conf string_of_bytes p
  (* Chars are encoded as single char strings *)
  let dchar conf _ _ p = dbytes conf (char_of_string % string_of_bytes) p

  (* Accumulate digits into a value with the given reducer: *)
  let fold init reduce conf _ _ p =
    (* Accumulate everything up to the next separator, and then run [op] to
     * convert from a string: *)
    let cond = E.func1 T.byte (fun _l b ->
      and_ (ne b (byte_of_const_char conf.separator))
           (ne b (byte_of_const_char conf.newline))) in
    read_while ~cond ~reduce ~init ~pos:p

  let int_reducer int_type base of_byte =
    E.func2 int_type T.byte (fun _l n b ->
      add
        (mul n base)
        (of_byte (sub b (byte (Uint8.of_int (Char.code '0'))))))

  let unsigned int_type zero ten of_byte =
    fold zero (int_reducer int_type ten of_byte)

  let signed int_type zero one neg_one ten of_byte st mn0 path p =
    E.with_sploded_pair "maybe_sign1" (read_byte p) (fun b p' ->
      E.with_sploded_pair "maybe_sign2"
        (choose ~cond:(eq b (byte (Uint8.of_int (Char.code '-'))))
                ~then_:(pair p' neg_one)
                ~else_:(pair p one))
        (fun p sign ->
          let cont = unsigned int_type zero ten of_byte st mn0 path p in
          E.with_sploded_pair "maybe_sign3" cont (fun v p ->
            pair (mul sign v) p)))

  let di8 =
    signed T.i8 (i8 Int8.zero) (i8 Int8.one) (i8 (Int8.of_int ~-1)) (i8 (Int8.of_int 10)) (to_i8 % u8_of_byte)
  let du8 =
    unsigned T.u8 (u8 Uint8.zero) (u8 (Uint8.of_int 10)) u8_of_byte
  let di16 =
    signed T.i16 (i16 Int16.zero) (i16 Int16.one) (i16 (Int16.of_int ~-1)) (i16 (Int16.of_int 10)) (to_i16 % u8_of_byte)
  let du16 =
    unsigned T.u16 (u16 Uint16.zero) (u16 (Uint16.of_int 10)) (to_u16 % u8_of_byte)
  let di24 =
    signed T.i24 (i24 Int24.zero) (i24 Int24.one) (i24 (Int24.of_int ~-1)) (i24 (Int24.of_int 10)) (to_i24 % u8_of_byte)
  let du24 =
    unsigned T.u24 (u24 Uint24.zero) (u24 (Uint24.of_int 10)) (to_u24 % u8_of_byte)
  let di32 =
    signed T.i32 (i32 0l) (i32 1l) (i32 (-1l)) (i32 10l) (to_i32 % u8_of_byte)
  let du32 =
    unsigned T.u32 (u32 Uint32.zero) (u32 (Uint32.of_int 10)) (to_u32 % u8_of_byte)
  let di40 =
    signed T.i40 (i40 Int40.zero) (i40 Int40.one) (i40 (Int40.of_int ~-1)) (i40 (Int40.of_int 10)) (to_i40 % u8_of_byte)
  let du40 =
    unsigned T.u40 (u40 Uint40.zero) (u40 (Uint40.of_int 10)) (to_u40 % u8_of_byte)
  let di48 =
    signed T.i48 (i48 Int48.zero) (i48 Int48.one) (i48 (Int48.of_int ~-1)) (i48 (Int48.of_int 10)) (to_i48 % u8_of_byte)
  let du48 =
    unsigned T.u48 (u48 Uint48.zero) (u48 (Uint48.of_int 10)) (to_u48 % u8_of_byte)
  let di56 =
    signed T.i56 (i56 Int56.zero) (i56 Int56.one) (i56 (Int56.of_int ~-1)) (i56 (Int56.of_int 10)) (to_i56 % u8_of_byte)
  let du56 =
    unsigned T.u56 (u56 Uint56.zero) (u56 (Uint56.of_int 10)) (to_u56 % u8_of_byte)
  let di64 =
    signed T.i64 (i64 Int64.zero) (i64 Int64.one) (i64 (Int64.of_int ~-1)) (i64 (Int64.of_int 10)) (to_i64 % u8_of_byte)
  let du64 =
    unsigned T.u64 (u64 Uint64.zero) (u64 (Uint64.of_int 10)) (to_u64 % u8_of_byte)
  let di128 =
    signed T.i128 (i128 Int128.zero) (i128 Int128.one) (i128 (Int128.of_int ~-1)) (i128 (Int128.of_int 10)) (to_i128 % u8_of_byte)
  let du128 =
    unsigned T.u128 (u128 Uint128.zero) (u128 (Uint128.of_int 10)) (to_u128 % u8_of_byte)

  let tup_opn _conf _ _ _ p = p

  let tup_cls _conf _ _ p = p

  let tup_sep _n _conf _ _ p = skip1 p

  let rec_opn _conf _ _ _ p = p

  let rec_cls _conf _ _ p = p

  let rec_sep _n _conf _ _ p = skip1 p

  let sum_opn st mn0 path _ p =
    let c_p = du16 st mn0 path p in
    E.with_sploded_pair "sum_opn" c_p (fun c p ->
      pair c (skip1 p))

  let sum_cls _conf _ _ p = p

  let vec_opn _conf _ _ _ _ p = p

  let vec_cls _conf _ _ p = p

  let vec_sep _conf _ _ p = skip1 p

  let list_opn conf =
    KnownSize (fun vtyp0 path _ p ->
      E.with_sploded_pair "list_opn" (du32 conf vtyp0 path p) (fun v p ->
        pair v (skip1 p)))

  let list_cls _conf _ _ p = p

  let list_sep _conf _ _ p = skip1 p

  let is_null conf _ _ p =
    let len = String.length conf.null in
    let rec loop i =
      if i >= len then
        (comment (Printf.sprintf "Test end of string %S" conf.null)
          (or_ (eq (rem_size p) (size len))
               (eq (peek_byte
                     p (size len)) (byte_of_const_char conf.separator))))
      else
        and_
          (comment (Printf.sprintf "Test char %d of %S" i conf.null)
            (let b = byte (Uint8.of_int (Char.code conf.null.[i])) in
             eq (peek_byte p (size i)) b))
          (loop (i + 1))
    in
    (and_ (ge (rem_size p) (size len))
          (loop 0))

  let dnull _t conf _ _ p =
    skip (String.length conf.null) p

  let dnotnull _t _conf _ _ p = p
end
