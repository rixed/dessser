open Batteries
open Stdint

open Dessser
open DessserMiscTypes
module T = DessserTypes
module E = DessserExpressions
module Path = DessserPath
open E.Ops

let debug = false

type sexpr_config =
  { list_prefix_length : bool ;
    (* Optional char added (or skipped) at end of values: *)
    newline : char option }

let default_config =
  { list_prefix_length = true ;
    newline = None }

module Ser : SER with type config = sexpr_config =
struct
  let id = SExpr

  type config = sexpr_config

  type state = config

  let ptr _vtyp = T.ptr

  let start ?(config=default_config) _ p =
    config, p

  let stop conf p =
    match conf.newline with
    | None ->
        p
    | Some c ->
        write_u8 p (u8_of_const_char c)

  type ser = state -> T.mn -> Path.t -> E.t -> E.t -> E.t

  let sfloat _conf _ _ v p =
    write_bytes p (bytes_of_string (string_of_float_ v))

  let sbytes v p =
    let quo = u8_of_const_char '"' in
    let p = write_u8 p quo in
    (* FIXME: escape double quotes: *)
    let p = write_bytes p v in
    write_u8 p quo

  let sstring _conf _ _ v p = sbytes (bytes_of_string v) p
  let schar _conf _ _ v p = sbytes (bytes_of_string (string_of_char v)) p

  let sbool _conf _ _ v p =
    write_u8 p (if_ v (u8_of_const_char 'T') (u8_of_const_char 'F'))

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

  (* Could also write the field names with the value in a pair... *)
  let tup_opn _ _conf _ _ p =
    write_u8 p (u8_of_const_char '(')

  let tup_cls _conf _ _ p =
    write_u8 p (u8_of_const_char ')')

  let tup_sep _conf _ _ p =
    write_u8 p (u8_of_const_char ' ')

  let rec_opn _ _conf _ _ p =
    write_u8 p (u8_of_const_char '(')

  let rec_cls _conf _ _ p =
    write_u8 p (u8_of_const_char ')')

  let rec_sep _conf _ _ p =
    write_u8 p (u8_of_const_char ' ')

  let sum_opn mns lbl st mn0 path p =
    let p = tup_opn mns st mn0 path p in
    let p = su16 st mn0 path lbl p in
    tup_sep st mn0 path p

  let sum_cls _lbl st mn0 path p =
    tup_cls st mn0 path p

  let vec_opn _ _ _conf _ _ p =
    write_u8 p (u8_of_const_char '(')

  let vec_cls _conf _ _ p =
    write_u8 p (u8_of_const_char ')')

  let vec_sep _conf _ _ p =
    write_u8 p (u8_of_const_char ' ')

  let arr_opn _ n conf mn0 path p =
    let p =
      match n with
      | Some n ->
          let p = su32 conf mn0 path n p in
          write_u8 p (u8_of_const_char ' ')
      | None ->
          p in
    write_u8 p (u8_of_const_char '(')

  let arr_cls _conf _ _ p =
    write_u8 p (u8_of_const_char ')')

  let arr_sep _conf _ _ p =
    write_u8 p (u8_of_const_char ' ')

  let nullable _conf _ _ p = p

  let snull _t _conf _ _ p =
    write_u32 LittleEndian p (u32 (Uint32.of_int32 0x6c_6c_75_6el))

  let snotnull _t _conf _ _ p = p

  (* Overestimate some of them: *)
  type ssizer = T.mn -> Path.t -> E.t -> E.t

  let ssize_start ?(config=default_config) _mn =
    size (if config.newline = None then 0 else 1)

  let ssize_of_float _ _ _ = size 22

  let ssize_of_string _ _ v =
    size_of_u32 (add (string_length v) (u32_of_int 2))

  let ssize_of_bool _ _ _ = size 1

  let ssize_of_char _ _ _ = size 3

  let ssize_of_i8 _ _ _ = size 4

  let ssize_of_i16 _ _ _ = size 6

  let ssize_of_i24 _ _ _ = size 8

  let ssize_of_i32 _ _ _ = size 11

  let ssize_of_i40 _ _ _ = size 13

  let ssize_of_i48 _ _ _ = size 16

  let ssize_of_i56 _ _ _ = size 18

  let ssize_of_i64 _ _ _ = size 20

  let ssize_of_i128 _ _ _ = size 40

  let ssize_of_u8 _ _ _ = size 3

  let ssize_of_u16 _ _ _ = size 5

  let ssize_of_u24 _ _ _ = size 7

  let ssize_of_u32 _ _ _ = size 10

  let ssize_of_u40 _ _ _ = size 12

  let ssize_of_u48 _ _ _ = size 15

  let ssize_of_u56 _ _ _ = size 17

  let ssize_of_u64 _ _ _ = size 19

  let ssize_of_u128 _ _ _ = size 39

  let ssize_of_tup mn path _ =
    let num_items =
      match (Path.type_of_path mn path).typ |> T.develop with
      | TTup mns -> Array.length mns
      | TRec mns -> Array.length mns
      | _ -> assert false in
    size (2 + num_items - 1)

  let ssize_of_rec = ssize_of_tup

  let ssize_of_sum mn path _ =
    let max_label =
      match (Path.type_of_path mn path).typ |> T.develop with
      | TSum mns ->
          Array.fold_left (fun m (label, _) ->
            max m (String.length label)
          ) 0 mns
      | _ -> assert false in
    size (max_label + 3)

  let ssize_of_vec mn path _ =
    let dim =
      match (Path.type_of_path mn path).typ |> T.develop with
      | TVec (dim, _) -> dim
      | _ -> assert false in
    size (2 + dim - 1)

  let ssize_of_arr _ _ v =
    size_of_u32 (add (cardinality v) (u32_of_int 2))

  let ssize_of_null _mn _path = size 4
end

module Des : DES with type config = sexpr_config =
struct
  let id = SExpr

  type config = sexpr_config

  type state = config

  let ptr _vtyp = T.ptr

  let start ?(config=default_config) _mn p =
    config, p

  let skip n p = ptr_add p (size n)

  let skip1 = skip 1

  let skip_byte b p =
    (* On debug, check that the expected character is present: *)
    if debug then
      seq [ assert_ (eq (peek_u8 p (size 0)) b) ;
            skip1 p ]
    else
      skip1 p

  let skip_char c p =
    skip_byte (u8_of_const_char c) p

  let stop conf p =
    match conf.newline with
    | None ->
        p
    | Some c ->
        if_ (gt (rem_size p) (size 0))
          ~then_:(skip_char c p)
          ~else_:p

  type des = state -> T.mn -> Path.t -> E.t -> E.t

  let tup_cls _conf _ _ p = skip1 p

  let tup_sep _conf _ _ p = skip1 p

  let dfloat _conf _ _ p =
    float_of_ptr p

  let dbool _conf _ _ p =
    E.with_sploded_pair "dbool" (read_u8 p) (fun b p ->
      make_pair (eq b (u8_of_const_char 'T')) p)

  (* Read a string of bytes and process them through [conv]: *)
  let dbytes conv p =
    (* Skip the double-quote: *)
    let p = skip1 p in
    (* Read up to next double-quote: *)
    (* FIXME: handle escaping backslash! *)
    let empty_bytes = bytes_of_string (string "") in
    let_ ~name:"str_ref" (make_ref empty_bytes) (fun str_ref ->
      let str = get_ref str_ref in
      let_ ~name:"p_ref" (make_ref p) (fun p_ref ->
        let p = get_ref p_ref in
        let_ ~name:"b_ref" (make_ref (u8_of_int 0)) (fun b_ref ->
          let b = get_ref b_ref in
          seq [
            while_
              (E.with_sploded_pair "dbytes" (read_u8 p) (fun b' p' ->
                seq [
                  set_ref p_ref p' ;
                  set_ref b_ref b' ;
                  ne b' (u8_of_const_char '"') ]))
              (set_ref str_ref (append_byte str b)) ;
            make_pair (conv str) p ])))

  let dstring _conf _ _ p = dbytes string_of_bytes p
  (* Chars are encoded as single char strings *)
  let dchar _conf _ _ p =
    dbytes (fun e ->
      char_of_u8 (unsafe_nth (u8_of_int 0) e)
    ) p

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

  let dext f _conf _ _ p =
    f p

  let tup_opn _conf _ _ _ p = skip1 p

  let tup_cls _conf _ _ p = skip1 p

  let tup_sep _conf _ _ p = skip1 p

  let rec_opn _conf _ _ _ p = skip1 p

  let rec_cls _conf _ _ p = skip1 p

  let rec_sep _conf _ _ p = skip1 p

  (* Sums are encoded as a pair of numeric label and value: *)
  let sum_opn st mn0 path mos p =
    let p = tup_opn st mn0 path mos p in
    let c_p = du16 st mn0 path p in
    E.with_sploded_pair "sum_opn_sexp" c_p (fun c p ->
      let p = tup_sep st mn0 path p in
      make_pair c p)

  let sum_cls _lbl st mn0 path p =
    tup_cls st mn0 path p

  let vec_opn _conf _ _ _ _ p = skip1 p

  let vec_cls _conf _ _ p = skip1 p

  let vec_sep _conf _ _ p = skip1 p

  let arr_opn conf =
    if conf.list_prefix_length then
      KnownSize (fun mn0 path _ p ->
        E.with_sploded_pair "list_opn" (du32 conf mn0 path p) (fun v p ->
          make_pair v (skip 2 p))) (* skip separator and opening '(' *)
    else
      UnknownSize (
        (fun _ _ _ p -> skip1 p),
        (fun _ _ p ->
          eq (peek_u8 p (size 0)) (u8_of_const_char ')')))

  let arr_cls _conf _ _ p = skip1 p

  let arr_sep _conf _ _ p = skip1 p

  let is_null _conf _ _ p =
    (* null *)
    and_ (and_ (eq (peek_u8 p (size 0)) (u8_of_int 0x6e))
               (and_ (eq (peek_u8 p (size 1)) (u8_of_int 0x75))
                     (and_ (eq (peek_u8 p (size 2)) (u8_of_int 0x6c))
                           (eq (peek_u8 p (size 3)) (u8_of_int 0x6c)))))
         (or_ (eq (rem_size p) (size 4))
              (let_ ~name:"b" (peek_u8 p (size 4)) (fun b ->
                or_ (eq b (u8_of_const_char ' '))
                    (eq b (u8_of_const_char ')')))))

  let dnull _t _conf _ _ p = skip 4 p

  let dnotnull _t _conf _ _ p = p
end
