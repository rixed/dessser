open Batteries
open Stdint

open Dessser
open DessserTools
open DessserMiscTypes
module E = DessserExpressions
module Conf = DessserConfigs.Json
module StdLib = DessserStdLib
module T = DessserTypes
module U = DessserCompilationUnit
open E.Ops

let debug_flag = false

(* The name of the game is to be able to skip any JSON value quickly to locate
 * field values: *)

(* A JSON object start with '{', an array with '[' and a string with '"'.
 * A JSON scalar can be a number, "null", "false" or "true".
 * Any number must start with a digit (but 0) or a minus sign, and additional
 * digits may follow, or a dot, an 'e' or 'E', a plus or minus sign. There is
 * symbol for nan or infinity. *)

let skip_while p cond =
  let_ ~name:"p" p (fun p ->
    let_ ~name:"off_ref2" (make_ref (size 0)) (fun off_ref ->
      let off = get_ref off_ref in
      seq [
        while_ (and_ (gt (rem_size p) off)
                     (cond (peek_u8 p off)))
          ~do_:(set_ref off_ref (add off (size 1))) ;
        ptr_add p off ]))

let is_byte_in_string s b =
  StdLib.is_in (char_of_u8 b) T.char (string s) T.string

let skip_blanks p =
  skip_while p (is_byte_in_string " \t\r\n")

(* Here is is already known that [p] points to a number, just skip all
 * permitted characters for numbers: *)
let skip_number p =
  skip_while p (is_byte_in_string "0123456789+-eE.")

let skip_1_char p c =
  (* On debug, check that the expected character is present: *)
  if debug_flag then
    let_ ~name:"skip_char_p" p (fun p ->
      seq [
        StdLib.check_byte p (u8_of_const_char c) ;
        ptr_add p (size 1) ])
  else
    ptr_add p (size 1)

let skip_char p c =
  let p = skip_blanks p in
  skip_1_char p c

let skip_string p =
  let_ ~name:"p" p (fun p ->
    let off_ref =
      seq [
        StdLib.check_byte p (u8_of_const_char '"') ;
        make_ref (size 1) ] in
    let_ ~name:"off_ref3" off_ref (fun off_ref ->
      let off = get_ref off_ref in
      let incr n = set_ref off_ref (add off (size n)) in
      seq [
        while_ (
          let_ ~name:"c" (peek_u8 p off) (fun c ->
            if_ (eq c (u8_of_const_char '"'))
              ~then_:false_
              ~else_:(
                seq [
                  if_ (eq c (u8_of_const_char '\\'))
                    ~then_:(
                      let c2 = peek_u8 p (add off (size 1)) in
                      let_ ~name:"c2" c2 (fun c2 ->
                        if_ (is_byte_in_string "\"\\/bfnrt" c)
                          ~then_:(incr 2)
                          ~else_:(
                            seq [
                              assert_ (eq c2 (u8_of_const_char 'u')) ;
                              incr 6 ])))
                    ~else_:(incr 1) ;
                  true_ ])))
            ~do_:nop ;
          comment "Also skip the final double quote:"
            (ptr_add p (add off (size 1))) ]))

let skip_container ~item ~close p =
  (* The pointed byte is already known to be an opening char: *)
  let p_ref = make_ref (ptr_add p (size 1)) in
  let_ ~name:"p_ref" p_ref (fun p_ref ->
    let p = get_ref p_ref in
    seq [
      while_ (
        let_ ~name:"p2" (skip_blanks p) (fun p2 ->
          let_ ~name:"c" (peek_u8 p2 (size 0)) (fun c ->
            if_ (eq c (u8_of_const_char close))
              ~then_:(
                seq [
                  set_ref p_ref p2 ;
                  false_ ])
              ~else_:(
                seq [
                  item p2 |> skip_blanks |> set_ref p_ref ;
                  if_ (eq (peek_u8 p (size 0)) (u8_of_const_char ','))
                    ~then_:(set_ref p_ref (ptr_add p (size 1)))
                    ~else_:nop ;
                  true_ ]))))
        ~do_:nop ;
      comment "Skip the final closing char:"
        (ptr_add p (size 1)) ])

let skip_array p =
  let item p = apply (myself T.ptr) [ p ] in
  seq [
    assert_ (eq (peek_u8 p (size 0)) (u8_of_const_char '[')) ;
    skip_container ~item ~close:']' p ]

let skip_object p =
  let item p =
    let p = skip_string p |> skip_blanks in
    let p = (comment "skip the value-separator"
              (ptr_add p (size 1))) in
    apply (myself T.ptr) [ p ] in
  seq [
    assert_ (eq (peek_u8 p (size 0)) (u8_of_const_char '{')) ;
    skip_container ~item ~close:'}' p ]

(* This must be a function because of recursive calls: *)
let skip =  (* Known as "json_skip" *)
  func1 T.ptr (fun p ->
    let_ ~name:"p" (skip_blanks p) (fun p ->
      let_ ~name:"new_p" (
        let_ ~name:"c" (peek_u8 p (size 0)) (fun c ->
          if_ (or_ (eq c (u8_of_const_char 'n')) (eq c (u8_of_const_char 't')))
            ~then_:(comment "skip null/true" (ptr_add p (size 4)))
            ~else_:(
              if_ (eq c (u8_of_const_char 'f'))
                ~then_:(comment "skip false" (ptr_add p (size 5)))
                ~else_:(
                  if_ (eq c (u8_of_const_char '"'))
                    ~then_:(comment "skip string" (skip_string p))
                    ~else_:(
                      if_ (eq c (u8_of_const_char '{'))
                        ~then_:(comment "skip object" (skip_object p))
                        ~else_:(
                          if_ (eq c (u8_of_const_char '['))
                            ~then_:(comment "skip array" (skip_array p))
                            ~else_:(comment "skip number" (skip_number p))))))))
        (fun new_p ->
          seq [ assert_ (gt (offset new_p) (offset p)) ;
                new_p ])))

(* Call this function before using the Json module to initialize the
 * compilation unit with the required library functions (FIXME: should not
 * be required): *)
let init compunit =
  match U.add_identifier_of_expression compunit ~name:"json_skip" skip with
  | exception U.Already_defined _ -> compunit
  | compunit, _, _ -> compunit

(*$inject
  open Batteries
  open Dessser
  open DessserTools
  open DessserDSTools
  module E = DessserExpressions
  module T = DessserTypes
  module U = DessserCompilationUnit
  open E.Ops
  let dbg = false

  let do_exe e =
    if dbg then
      Format.eprintf "@[<v>Expression:@,%a@." (E.pretty_print ?max_depth:None) e ;
    let be = (module DessserBackEndOCaml : BACKEND) in
    let module BE = (val be : BACKEND) in
    let compunit = U.make "test_json_eo_exe" |> init in
    let compunit, _, entry_point =
      U.add_identifier_of_expression compunit e in
    let dst_fname = Filename.temp_file "dessser_skip_json_" "" in
    let outro =
      Printf.sprintf2 "let () = DessserGen.%s Sys.argv.(1)\n" entry_point in
    BE.compile ~dev_mode:true ~optim:3 ~link:Executable ~dst_fname ~outro compunit

  let skip_json =
    let e =
      func1 T.string (fun json_str ->
        let p = ptr_of_string json_str in
        let_ ~name:"p'" (apply skip [ p ]) (fun p' ->
          if_ (eq (rem_size p') (size 0))
            ~then_:nop
            ~else_:(
              seq [ dump (string "FAILURE: remaining bytes: ") ;
                    dump (rem_size p') ;
                    dump (string "\n") ]))) in
    let exe_fname = do_exe e in
    fun json ->
      run_converter exe_fname json
*)

(*$= skip_json & ~printer:String.quote
  "" (skip_json "true")
  "" (skip_json "false")
  "" (skip_json "null")
  "" (skip_json "0")
  "" (skip_json "42")
  "" (skip_json "-42")
  "" (skip_json "3.14")
  "" (skip_json "-1.09e+10")
  "" (skip_json "2E2")
  "" (skip_json "50.0E-10")
  "" (skip_json "[]")
  "" (skip_json "[1,2,3]")
  "" (skip_json "[ 50.0E-10 ,  [1, 2 ,3 ], \"lol\"]")
  "" (skip_json "\"lol solidus \\\\ \\/ \\\" \"")
  "" (skip_json " { \"foo\":\"bar\" }")
  "" (skip_json " {\"\":null,\"a\" :{}, \"b\": {\"c\" : 1 }}")
*)

(* Now that any json value, however deep, can be quickly skipped over, the time
 * is ripe to turn to the main piece: opening objects into records: *)

(* Return both the string and the advanced pointer: *)
let parse_bytes p =
  let_ ~name:"p" p (fun p ->
    let_ ~name:"p2" (skip_string p) (fun p' ->
      let len = sub (ptr_sub p' p) (size 2) in
      let s = first (read_bytes (ptr_add p (size 1)) len) in
      make_tup [ s ; p' ]))

(* Convert the passed json string (as bytes) into a proper string.
 * The string is computed into an array of bytes (which are mutable), that is
 * then converted into a string: *)
let parse_string bytes =
  (* Read 2 hex digits from offset [o] in bytes [bs] and return the u8,
   * assuming the bytes are indeed valid: *)
  let read_2_hexs o =
    let d1 = StdLib.u8_of_hex_digit (unsafe_nth o bytes) in
    let_ ~name:"read_2_hexs_d1" d1 (fun d1 ->
      let d2 = StdLib.u8_of_hex_digit (unsafe_nth (add o (size 1)) bytes) in
      let_ ~name:"read_2_hexs_d2" d2 (fun d2 ->
        add (left_shift d1 (u8_of_int 4)) d2)) in
  let inc r = set_ref r (add (get_ref r) (size 1)) in
  (* The resulting string will be at most as long as bytes: *)
  let res = alloc_arr (bytes_length bytes) (char_of_int 0) in
  let_ ~name:"parse_string_res" res (fun res ->
    let src_ref = make_ref (size 0) in
    let_ ~name:"src_ref" src_ref (fun src_ref ->
      let src = get_ref src_ref in
      let with_next_src f =
        let b = unsafe_nth src bytes in
        let_ ~name:"parse_string_b" b (fun b ->
          seq [
            inc src_ref ;
            f b ]) in
      let dst_ref = make_ref (size 0) in
      let_ ~name:"dst_ref" dst_ref (fun dst_ref ->
        let dst = get_ref dst_ref in
        let write_next_dst b =
          seq [
            set_vec dst res (char_of_u8 b) ;
            inc dst_ref ] in
        seq [
          while_ (and_ (lt src (bytes_length bytes))
                       (ne (unsafe_nth src bytes) (u8_of_const_char '"')))
            ~do_:(
              with_next_src (fun b1 ->
                if_ (eq b1 (u8_of_const_char '\\'))
                  ~then_:(
                    with_next_src (fun b2 ->
                      if_ (eq b2 (u8_of_const_char 'u'))
                        ~then_:(
                          (* 16 bits unicode. Let's write this as is.
                           * TODO: encode in UTF8. *)
                          let dd1 = read_2_hexs src in
                          let_ ~name:"dd1" dd1 (fun dd1 ->
                            let dd2 =  read_2_hexs (add src (size 2)) in
                            let_ ~name:"dd2" dd2 (fun dd2 ->
                              seq [
                                set_ref src_ref (add src (size 4)) ;
                                write_next_dst dd1 ;
                                write_next_dst dd2 ])))
                        ~else_:(
                          let b2' =
                            StdLib.cases [
                              eq b2 (u8_of_const_char 'b'), u8_of_int 8 ;
                              eq b2 (u8_of_const_char 't'), u8_of_int 9 ;
                              eq b2 (u8_of_const_char 'n'), u8_of_int 10 ;
                              eq b2 (u8_of_const_char 'f'), u8_of_int 12 ;
                              eq b2 (u8_of_const_char 'r'), u8_of_int 13
                            ] ~else_:b2 in
                          write_next_dst b2')))
                  ~else_:(
                    write_next_dst b1))) ;
          let n = sub (bytes_length bytes) dst in
          convert T.string (chop_end res n) ])))

(* Build a vector of pointers from an object value: *)
let parse_object mns p =
  let ptrs = alloc_arr (size (Array.length mns)) (null T.TPtr) in
  let_ ~name:"ptrs" ptrs (fun ptrs ->
    let p = comment "skip begin-object" (skip_char p '{') in
    let_ ~name:"p_ref" (make_ref p) (fun p_ref ->
      seq [
        while_ (
          let_ ~name:"p" (skip_blanks (get_ref p_ref)) (fun p ->
            if_ (eq (peek_u8 p (size 0)) (u8_of_const_char '}'))
              ~then_:(
                seq [
                  set_ref p_ref (ptr_add p (size 1)) ;
                  false_ ])
              ~else_:(
                let_pair ~n1:"s" ~n2:"p" (parse_bytes p) (fun s p ->
                  let p = comment "skip name-separator" (skip_char p ':') in
                  let_ ~name:"p" p (fun p ->
                    (* Which field is that? *)
                    let idx =
                      List.init (Array.length mns - 1) (fun i ->
                        let n, _ = mns.(i) in
                        eq (bytes_of_string (string n)) s,
                        u32_of_int i) |>
                      StdLib.cases ~else_:(u32_of_int (Array.length mns - 1)) in
                    seq [
                      set_vec idx ptrs (not_null p) ;
                      (let p = apply (identifier "json_skip") [ p ] |>
                               skip_blanks in
                      let_ ~name:"p" p (fun p ->
                        let p =
                          if_ (eq (peek_u8 p (size 0)) (u8_of_const_char ','))
                            ~then_:(ptr_add p (size 1))
                            ~else_:p in
                        set_ref p_ref p)) ;
                      true_ ])))))
          ~do_:nop ;
        make_pair ptrs (get_ref p_ref) ]))

(*$inject
  let locate_fields mns =
    let e =
      func1 T.string (fun json_str ->
        let p = ptr_of_string json_str in
        let_pair ~n1:"ptrs" ~n2:"p" (parse_object mns p)
          (fun ptrs p ->
            if_ (eq (rem_size p) (size 0))
              ~then_:(
                comment "Display pointers:" (
                  for_each ~name:"ptr" ptrs (fun ptr ->
                    seq [
                      if_null ptr
                        ~then_:(dump (string "NULL"))
                        ~else_:(dump (offset (force ~what:"locate_fields" ptr))) ;
                      dump (char ',') ])))
              ~else_:(
                seq [ dump (string "FAILURE: remaining bytes: ") ;
                      dump (rem_size p) ;
                      dump (string "\n") ]))) in
    let exe_fname = do_exe e in
    fun json ->
      run_converter exe_fname json

  let locate_1 =
    locate_fields T.[| "foo", u32 ; "bar", string |]
  let locate_2 =
    locate_fields T.[|
      "foo", u32 ; "bar", string ; "baz", optional (vec 3 float) ;
      "nested", optional (record [| "foo", u32 ; "bar", string |]) |]
*)
(*$= locate_1 & ~printer:String.quote
  "8,18," (locate_1 "{ \"foo\": 1, \"bar\": 2 }")
  "8,NULL," (locate_1 "{ \"foo\": 1 }")
  "NULL,8," (locate_1 "{ \"bar\": 2 }")
  "18,8," (locate_1 "{ \"bar\": 1, \"foo\": 2 }")
*)
(*$= locate_2 & ~printer:String.quote
  "8,18,NULL,NULL," (locate_2 "{ \"foo\": 1, \"bar\": 2 }")
  "8,NULL,NULL,NULL," (locate_2 "{ \"foo\": 1 }")
  "NULL,8,NULL,NULL," (locate_2 "{ \"bar\": 2 }")
  "NULL,NULL,42,13," \
    (locate_2 " { \"nested\" : { \"bar\":1,\"baz\":2 } , \"baz\":1  }")
*)

(* Now deserializing become easy: *)

(* Helper function to build the (constant) list of all constructors of a sum
 * type that have no associated value: *)
let no_value_cstrs mn0 path =
  match (Path.type_of_path mn0 path |> T.develop1).T.typ with
  | T.TSum mns ->
      Array.enum mns |>
      enum_filter_mapi (fun i (_n, mn) ->
        if T.eq_mn mn T.void then Some (u16_of_int i) else None) |>
      List.of_enum |>
      make_arr T.u16
  | _ ->
      assert false

(* For parsing JSON objects (as records), fields need to be reordered.
 * Therefore, when "opening" a record, all fields are located within the
 * incoming bytes and recorded in an array of pointers, in order of
 * definition, as well as a pointer to the end of the record.
 * All objects we are currently in form a stack of such objects. *)
let frame_t = T.(tuple [| required (arr nptr) ; ptr |])

module Des : DES with type config = Conf.t =
struct
  let id = JSON

  type config = Conf.t

  type state = config

  let frame_push ptrs end_p stk =
    let frame = make_tup [ ptrs ; end_p ] in
    cons frame stk

  let frame_top stk =
    force ~what:"Json.frame_top" (head stk)

  (* Pop the last frame from the stack, returning both the pointer at the end
   * of the object and the previous stack: *)
  let frame_pop stk =
    let_ ~name:"stk" stk (fun stk ->
      let end_p = secnd (frame_top stk) in
      make_pair end_p (force ~what:"Json.frame_pop" (tail stk)))

  let locate_p mn0 path p stk =
    match Path.type_of_parent mn0 path with
    | exception Invalid_argument _ ->
        (* No parent, therefore cannot be in a record: *)
        not_null p
    | T.{ typ = TRec mns ; _ } ->
        let frame = frame_top stk in
        let ptrs = get_item 0 frame in
        (* Our index in the parent: *)
        (match List.last path with
        | Path.CompTime i ->
            assert (i >= 0 && i < Array.length mns) ;
            let_ ~name:"p_opt" (nth (u32_of_int i) ptrs) (fun p_opt ->
              seq [
                debug (string ("Reading field #"^ string_of_int i ^"@")) ;
                debug (if_null p_opt ~then_:(string "NULL") ~else_:(string_of_int_ (offset (force p_opt)))) ;
                debug (char '\n') ;
                p_opt ])
        | Path.RunTime i ->
            nth i ptrs)
    | _ ->
        not_null p

  let select_config _csv _sexpr =
    (* TODO *)
    DessserConfigs.Json.default

  let make_state ?(config=Conf.default) _mn = config

  (* Pass the compunit to this function, so it can register its own stuff,
   * such as the skip function? *)
  let start _mn0 _conf p =
    make_pair p (end_of_list frame_t)

  let stop _mn0 _conf p_stk =
    skip_blanks (first p_stk)

  type des = state -> T.mn -> Path.t -> E.t -> E.t

  let with_p_stk f _conf mn0 path p_stk =
    let_pair ~n1:"p" ~n2:"stk" p_stk (fun p stk ->
      (* If we are currently decoding an object, the given [p] is not
       * where the value is. Instead, it is one of the already located
       * object value, to be found in the top frame: *)
      let p_opt = locate_p mn0 path p stk in
      let what = "locate_p("^ Path.field_name mn0 path ^")" in
      let p = force ~what p_opt in
      let p = skip_blanks p in
      let_ ~name:"p" p (fun p -> f p stk))

  let dstring : des =
    with_p_stk (fun p stk ->
      let_pair ~n1:"b" ~n2:"p" (parse_bytes p) (fun bytes p ->
        make_pair (parse_string bytes) (make_pair p stk)))

  let dbytes conf mn0 path p_stk =
    let_pair ~n1:"v" ~n2:"p" (dstring conf mn0 path p_stk) (fun v p ->
      make_pair (bytes_of_string v) p)

  let dbool : des =
    with_p_stk (fun p stk ->
      let_ ~name:"c" (peek_u8 p (size 0)) (fun c ->
        if_ (eq c (u8_of_const_char 'f'))
          ~then_:(
            make_pair false_ (make_pair (ptr_add p (size 5)) stk))
          ~else_:(
            seq [
              assert_ (eq c (u8_of_const_char 't')) ;
              make_pair true_ (make_pair (ptr_add p (size 4)) stk) ])))

  (* Chars are represented as 1 char strings (with usual escaping): *)
  let dchar : des =
    with_p_stk (fun p stk ->
      let_pair ~n1:"bytes" ~n2:"p" (parse_bytes p) (fun bytes p ->
        let s = parse_string bytes in
        make_pair (unsafe_nth (size 0) s)
                  (make_pair p stk)))

  let dnum of_ptr : des =
    with_p_stk (fun p stk ->
      seq [
        debug (string "dnum@") ; debug (offset p) ; debug (char '\n') ;
        let_pair ~n1:"n" ~n2:"p" (of_ptr p) (fun n p ->
          make_pair n (make_pair p stk)) ])

  let di8 = dnum i8_of_ptr
  let du8 = dnum u8_of_ptr
  let di16 = dnum i16_of_ptr
  let du16 = dnum u16_of_ptr
  let di24 = dnum i24_of_ptr
  let du24 = dnum u24_of_ptr
  let di32 = dnum i32_of_ptr
  let du32 = dnum u32_of_ptr
  let di40 = dnum i40_of_ptr
  let du40 = dnum u40_of_ptr
  let di48 = dnum i48_of_ptr
  let du48 = dnum u48_of_ptr
  let di56 = dnum i56_of_ptr
  let du56 = dnum u56_of_ptr
  let di64 = dnum i64_of_ptr
  let du64 = dnum u64_of_ptr
  let di128 = dnum i128_of_ptr
  let du128 = dnum u128_of_ptr
  let dfloat = dnum float_of_ptr

  let dext f _conf mn0 path p_stk =
    (* Adapted from with_p_stk: *)
    let_pair ~n1:"p" ~n2:"stk" p_stk (fun p stk ->
      let p_opt = locate_p mn0 path p stk in
      let what = "dext:locate_p("^ Path.field_name mn0 path ^")" in
      let p = force ~what p_opt in
      let p = skip_blanks p in
      let_ ~name:"p_stk" (make_pair p stk) f)

  (* If in a record, locate the pointer at the beginning of the field first: *)
  let skip_1_reloc c : des =
    with_p_stk (fun p stk ->
      let p = skip_char p c in
      make_pair p stk)

  (* Skip the character regardless of where the pointer is: *)
  let skip_1 c _conf _mn0 _path p_stk =
    let_pair ~n1:"p" ~n2:"stk" p_stk (fun p stk ->
      let p = skip_char p c in
      make_pair p stk)

  (* Tuples are represented by arrays
   * "There is no requirement that the values in an array be of the same
   *  type." Of course there isn't! *)

  let tup_opn _mns = skip_1_reloc '['

  let tup_cls = skip_1 ']'

  let tup_sep  = skip_1 ','

  let vec_opn _dim _mn = skip_1_reloc '['

  let vec_cls _ = skip_1 ']'

  let vec_sep = skip_1 ','

  (* Records: locate all fields, push that frame and return the position of the
   * and of the object. But every reader will read from the pointers stored in
   * the frame. *)
  let rec_opn mns : des =
    with_p_stk (fun p stk ->
      let ptrs_p = parse_object mns p in
      let_pair ~n1:"p" ~n2:"stop" ptrs_p (fun ptrs end_p ->
        make_pair end_p (frame_push ptrs end_p stk)))

  let rec_sep _conf _mn0 _path p_stk =
    p_stk

  let rec_cls _conf _mn0 _path p_stk =
    let stk = secnd p_stk in
    frame_pop stk

  (* Sum types are represented as single field object when the constructor
   * has a value or a mere string otherwise: *)
  let sum_opn mns =
    with_p_stk (fun p stk ->
      let_ ~name:"p" (skip_blanks p) (fun p ->
        (* A pair with both the string (as byte) of the label and the pointer: *)
        let cstr_p =
          if_ (eq (peek_u8 p (size 0)) (u8_of_const_char '"'))
            (* A string: that's our constructor *)
            ~then_:(parse_bytes p)
            ~else_:(
              let p = comment "skip begin-object for sum" (skip_1_char p '{') in
              let p = skip_blanks p in
              let_pair ~n1:"s" ~n2:"p" (parse_bytes p) (fun lbl p ->
                let p = skip_char p ':' in
                make_pair lbl p)) in
        let_pair ~n1:"s" ~n2:"p" cstr_p (fun cstr p ->
          let i =
            Array.enum mns |>
            Enum.mapi (fun i (n, _) ->
              eq cstr (bytes_of_string (string n)),
              u16_of_int i) |>
            List.of_enum |>
            StdLib.cases ~else_:(seq [ assert_ false_ ; u16_of_int 0 ]) in
          let_ ~name:"cstr" i (fun i ->
              let p_stk = make_pair p stk in
              make_pair i p_stk))))

  let sum_cls lbl conf mn0 path p_stk =
    let_ ~name:"p_stk" p_stk (fun p_stk ->
      let no_value_cstrs = no_value_cstrs mn0 path in
      if_ (StdLib.is_in lbl T.u16 no_value_cstrs T.(required (arr u16)))
        ~then_:p_stk
        ~else_:(skip_1 '}' conf mn0 path p_stk))

  let arr_opn conf =
    UnknownSize (
      (fun _mn -> skip_1_reloc '[' conf),
      (fun _mn0 _path p_stk ->
        let p = first p_stk in
        let p = skip_blanks p in
        eq (peek_u8 p (size 0)) (u8_of_const_char ']')))

  let arr_cls _ = skip_1 ']'

  let arr_sep = skip_1 ','

  let is_present _conf mn0 path p_stk =
    let_pair ~n1:"p" ~n2:"stk" p_stk (fun p stk ->
      let_ ~name:"p_opt" (locate_p mn0 path p stk) (fun p_opt ->
        make_pair (not_ (is_null p_opt)) p_stk))

  (* Note that even without a default value the value could be entirely
   * missing and be NULLs *)
  let is_null _conf mn0 path p_stk =
    let_pair ~n1:"p" ~n2:"stk" p_stk (fun p stk ->
      let_ ~name:"p_opt" (locate_p mn0 path p stk) (fun p_opt ->
        let res =
          or_ (is_null p_opt)
              (let_ ~name:"p" (force ~what:"is_null" p_opt) (fun p ->
                let_ ~name:"p" (skip_blanks p) (fun p ->
                  and_ (ge (rem_size p) (size 4))
                       (eq (peek_u32 BigEndian p (size 0))
                           (u32_of_int 0x6e756c6c)) (* "null" *)))) in
        let_ ~name:"is_null" res (fun res ->
          seq [
            debug (string "is_null: ") ; debug res ; debug (char '\n') ;
            res ])))

  let dnull _t _conf mn0 path p_stk =
    let_pair ~n1:"p" ~n2:"stk" p_stk (fun p stk ->
      let_ ~name:"p_opt" (locate_p mn0 path p stk) (fun p_opt ->
        (* If the field is not even present, just do nothing: *)
        if_null p_opt
          ~then_:p_stk
          ~else_:(
            (* If there is an explicit "null", read it *)
            let_ ~name:"p" (force ~what:"dnull" p_opt) (fun p ->
              seq [
                debug (string "dnull@") ; debug (offset p) ; debug (char '\n') ;
                let p = skip_blanks p in
                let p = ptr_add p (size 4) in
                make_pair p stk ]))))

  let dnotnull _t _conf _mn0 _path p_stk =
    p_stk
end

(* Compared to desserialization, serialization offers little sophistication,
 * as we are free to dump values in the order specified by the schema. *)

module Ser : SER with type config = Conf.t =
struct
  let id = JSON

  type config = Conf.t

  type state = config

  let select_config _csv _sexpr =
    (* TODO *)
    DessserConfigs.Json.default

  let make_state ?(config=Conf.default) _mn = config

  let start _mn0 _conf p = p

  let stop _mn0 conf p =
    match conf.Conf.newline with
    | None ->
        p
    | Some c ->
        write_u8 p (u8_of_const_char c)

  type ser = state -> T.mn -> Path.t -> (*v*) E.t -> (*ptr*) E.t -> (*ptr*) E.t

  let write_str p v =
    let escape p = write_u8 p (u8_of_const_char '\\') in
    let p = write_u8 p (u8_of_const_char '"') in
    let_ ~name:"p_ref" (make_ref p) (fun p_ref ->
      let p = get_ref p_ref in
      seq [
        for_each ~name:"write_str_c" v (fun c ->
          let escape_char c =
            let p = escape p in
            write_u8 p (u8_of_char c) in
          let p =
            StdLib.cases [
              eq c (char '\b'), escape_char (char 'b') ;
              eq c (char '\012'), escape_char (char 'f') ;
              eq c (char '\n'), escape_char (char 'n') ;
              eq c (char '\r'), escape_char (char 'r') ;
              eq c (char '\t'), escape_char (char 't') ;
              or_ (lt c (char ' ')) (gt c (char '~')), (
                (* Here any non ascii is going to be encoded as a 8 bits
                 * UTF2. TODO: decode multichars as UTF8. *)
                let p = escape p in
                let p = write_u8 p (u8_of_const_char 'u') in
                let p = write_u8 p (u8_of_const_char '0') in
                let p = write_u8 p (u8_of_const_char '0') in
                let p = write_u8 p (StdLib.hex_digit_of_u8
                             (right_shift (u8_of_char c) (u8_of_int 4))) in
                let p = write_u8 p
                          (StdLib.hex_digit_of_u8
                            (bit_and (u8_of_char c) (u8_of_int 15))) in
                p) ]
            ~else_:(
              if_ (or_ (eq c (char '"'))
                       (or_ (eq c (char '\\'))
                            (* This one sacrificed to gods it seems: *)
                            (eq c (char '/'))))
                ~then_:(escape_char c)
                ~else_:(write_u8 p (u8_of_char c))) in
          set_ref p_ref p) ;
        write_u8 p (u8_of_const_char '"') ])

  let json_string s =
    String.fold_left (fun s c ->
      let c' =
        match c with
        | '"' -> "\""
        | '\\' -> "\\\\"
        | '/' -> "\\/"
        | '\b' -> "\\b"
        | '\012' -> "\\f"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | c when c < ' ' || c > '~' -> Printf.sprintf "\\u%04x" (Char.code c)
        | c -> String.of_char c in
      s ^ c'
    ) "" s

  let write_field mn0 path p =
    match Path.type_of_parent mn0 path with
    | exception Invalid_argument _ ->
        (* No parent, therefore cannot be in a record: *)
        p
    | T.{ typ = TRec mns ; _ } ->
        let p = write_u8 p (u8_of_const_char '"') in
        (* Our index in the parent: *)
        let p =
          match List.last path with
          | Path.CompTime i ->
              assert (i >= 0 && i < Array.length mns) ;
              write_bytes p
                (bytes_of_string (string (json_string (fst mns.(i)))))
          | Path.RunTime i ->
              StdLib.cases (
                Array.enum mns |>
                Enum.mapi (fun j (n, _) ->
                  eq (to_u32 i) (u32_of_int j),
                  write_str p (string (json_string n))) |>
                List.of_enum
              ) ~else_:(seq [ assert_ false_ ; p ]) in
        let p = write_u8 p (u8_of_const_char '"') in
        write_u8 p (u8_of_const_char ':')
    | _ ->
        (* No need for a field name: *)
        p

  let write_bytes_of to_bytes _conf mn0 path v p =
    let p = write_field mn0 path p in
    write_bytes p (to_bytes v)

  let sfloat =
    write_bytes_of (bytes_of_string % decimal_string_of_float)

  let sstring _conf mn0 path v p =
    let p = write_field mn0 path p in
    write_str p v

  let sbytes conf mn0 path v p =
    sstring conf mn0 path (string_of_bytes v) p

  let sbool =
    write_bytes_of
      (fun v ->
        bytes_of_string (
          if_ v ~then_:(string "true") ~else_:(string "false")))

  let schar conf mn0 path v p =
    sstring conf mn0 path (string_of_char v) p

  let num =
    write_bytes_of (bytes_of_string % string_of_int_)

  let si8 = num
  let si16 = num
  let si24 = num
  let si32 = num
  let si40 = num
  let si48 = num
  let si56 = num
  let si64 = num
  let si128 = num
  let su8 = num
  let su16 = num
  let su24 = num
  let su32 = num
  let su40 = num
  let su48 = num
  let su56 = num
  let su64 = num
  let su128 = num

  let sext f _conf mn0 path v p =
    let p = write_field mn0 path p in
    f v p

  let write_opn_chr c _conf mn0 path p =
    let p = write_field mn0 path p in
    write_u8 p (u8_of_const_char c)

  let write_chr c _conf _mn0 _path p =
    write_u8 p (u8_of_const_char c)

  let tup_opn _mns = write_opn_chr '['

  let tup_cls = write_chr ']'

  let tup_sep = write_chr ','

  let rec_opn _mns = write_opn_chr '{'

  let rec_cls = write_chr '}'

  let rec_sep = write_chr ','

  let sum_opn mns lbl _conf mn0 path p =
    let p = write_field mn0 path p in
    let s =
      StdLib.cases (
        Array.enum mns |>
        Enum.mapi (fun i (n, _) ->
          eq lbl (u16_of_int i),
          string n) |>
        List.of_enum
      ) ~else_:(seq [ assert_ false_ ; string "" ]) in
    let no_value_cstrs = no_value_cstrs mn0 path in
    if_ (StdLib.is_in lbl T.u16 no_value_cstrs T.(required (arr u16)))
      ~then_:(write_str p s)
      ~else_:(
        let p = write_u8 p (u8_of_const_char '{') in
        let p = write_str p s in
        write_u8 p (u8_of_const_char ':'))

  let sum_cls lbl _conf mn0 path p =
    let no_value_cstrs = no_value_cstrs mn0 path in
    if_ (StdLib.is_in lbl T.u16 no_value_cstrs T.(required (arr u16)))
      ~then_:p
      ~else_:(write_u8 p (u8_of_const_char '}'))

  let vec_opn _dim _mns = write_opn_chr '['

  let vec_cls _ = write_chr ']'

  let vec_sep = write_chr ','

  let arr_opn _mns _card = write_opn_chr '['

  let arr_cls _ = write_chr ']'

  let arr_sep = write_chr ','

  let nullable _conf _mn0 _path p = p

  let snull _t _conf mn0 path p =
    (* TODO: when we have a field, then skip the 'null' altogether! *)
    let p = write_field mn0 path p in
    write_u32 BigEndian p (u32 (Uint32.of_int32 0x6e_75_6c_6cl))

  let snotnull _t _conf _mn0 _path p = p

  type ssizer = T.mn -> Path.t -> E.t -> E.t

  let ssize_start ?(config=Conf.default) _mn0 =
    ignore config ;
    size 0

  (* Those are maximums: *)
  let ssize_of_float _mn0 _path _v =
    size 20

  let const_string_size s =
    size (String.length (json_string s))

  let ssize_of_str len =
    (* Assuming only non ascii chars (and no utf8 support): *)
    size_of_u32 (add (mul (u32_of_int 5) len) (u32_of_int 2))

  let ssize_of_string _mn0 _path v =
    ssize_of_str (string_length v)

  let ssize_of_bytes _mn0 _path v =
    ssize_of_str (u32_of_size (bytes_length v))

  let ssize_of_bool _mn0 _path v =
    if_ v ~then_:(size 4) ~else_:(size 5)

  let ssize_of_char _mn0 _path _v =
    ssize_of_str (u32_of_int 1)

  let ssize_of_i8 _mn0 _path _v = size 4

  let ssize_of_i16 _mn0 _path _v = size 6

  let ssize_of_i24 _mn0 _path _v = size 8

  let ssize_of_i32 _mn0 _path _v = size 11

  let ssize_of_i40 _mn0 _path _v = size 13

  let ssize_of_i48 _mn0 _path _v = size 16

  let ssize_of_i56 _mn0 _path _v = size 18

  let ssize_of_i64 _mn0 _path _v = size 20

  let ssize_of_i128 _mn0 _path _v = size 40

  let ssize_of_u8 _mn0 _path _v = size 3

  let ssize_of_u16 _mn0 _path _v = size 5

  let ssize_of_u24 _mn0 _path _v = size 7

  let ssize_of_u32 _mn0 _path _v = size 10

  let ssize_of_u40 _mn0 _path _v = size 12

  let ssize_of_u48 _mn0 _path _v = size 15

  let ssize_of_u56 _mn0 _path _v = size 17

  let ssize_of_u64 _mn0 _path _v = size 19

  let ssize_of_u128 _mn0 _path _v = size 39

  let ssize_of_array mn0 path v =
    let num_items =
      match (Path.type_of_path mn0 path |> T.develop1).typ with
      | TTup mns -> u32_of_int (Array.length mns)
      | (TArr _ | TSet _ | TLst _) -> cardinality v
      | TVec (d, _) -> u32_of_int d
      | _ -> assert false in
    size_of_u32 (add (u32_of_int 2) (sub num_items (u32_of_int 1)))

  let ssize_of_tup = ssize_of_array
  let ssize_of_vec = ssize_of_array
  let ssize_of_arr = ssize_of_array

  let ssize_of_sum mn0 path v =
    StdLib.cases (
      match (Path.type_of_path mn0 path |> T.develop1).typ with
      | TSum mns ->
          Array.enum mns |>
          Enum.mapi (fun i (n, _) ->
            eq (u16_of_int i) (label_of v), size (String.length n + 2)) |>
          List.of_enum
      | _ ->
          assert false
    ) ~else_:(seq [assert_ false_ ; size 0 ])

  let ssize_of_rec mn0 path _v =
    match (Path.type_of_path mn0 path |> T.develop1).typ with
    | TRec mns ->
        Array.fold_left (fun e (n, _) ->
          add e (add (size 1 (* sep *)) (const_string_size n))
        ) (size 2 (* braces *)) mns
    | _ ->
        assert false

  let ssize_of_null _mn0 _path =
    size 4

  let ssize_of_notnull _mn0 _path =
    size 0
end

let () =
  let dptr = T.(pair ptr (required (lst frame_t)))
  and sptr = T.ptr in
  T.register_ptr_type JSON sptr dptr
