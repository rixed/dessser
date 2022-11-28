(* Some common expressions that might be useful in various programs *)
open Stdint

open DessserTools
module C = DessserConversions
module E = DessserExpressions
module T = DessserTypes
open BatOption.Infix
open E.Ops

(* [coalesce es] build an expression that selects the first non null
 * expression in [es].
 * All items of [es] are pairs of E.t and nullability flag, and all of these
 * expressions must have the same value-type.
 * If the last alternative is nullable then the result of the coalesce is still
 * nullable.
 * The first non-nullable alternative will be the last checked item obviously. *)
let coalesce es =
  if es = [] then
    invalid_arg "coalesce with no alternatives" ;
  let _, ret_nullable = BatList.last es in
  let rec loop i = function
    | [] ->
        assert false (* because of the pre-condition *)
    | [ e, _ ] ->
        e
    | (e, nullable) :: es ->
        if nullable then
          let name = "coalesced_"^ string_of_int i in
          let_ ~name e (fun e ->
            if_null e
              ~then_:(loop (i + 1) es)
              ~else_:(
                if ret_nullable then e
                else force ~what:"coalesce" e))
        else
          (* If [e] is not a nullable thing there is no point looking
           * further: *)
          if ret_nullable then not_null e else e in
  loop 0 es

(* Implement a simple for-loop: *)
let repeat ~from ~to_ body =
  let_ ~name:"repeat_n" (make_ref (to_i32 from)) (fun n_ref ->
    let n = get_ref n_ref in
    while_ (lt n (to_i32 to_))
      (seq [
        body n ;
        set_ref n_ref (add n (i32_of_int 1)) ]))

let random_i32 =
  to_i32 random_u32

(* Returns a random value of type [mn]. Behavior on TThis is configurable.
 * Not meant to be used directly. See [random] and [func_random]. *)
let random_gen handle_this mn0 =
  let rec random_lst mn =
    let max_list_length = i32_of_int 8 in
    let from = i32_of_int 0
    and to_ =
      force ~what:"random_lst rem"
        (rem (add (i32_of_int 1) random_i32) max_list_length) in
    let_ ~name:"lst" (make_ref (eol mn)) (fun lst_ref ->
      let lst = get_ref lst_ref in
      seq [
        repeat ~from ~to_ (fun _n ->
          set_ref lst_ref (cons (random mn) lst)) ;
        lst ])
  and random mn =
    if mn.T.nullable then
      if_ (random T.bool)
        ~then_:(null mn.typ)
        ~else_:(not_null (random { mn with nullable = false }))
    else match mn.typ with
    | T.TUnknown ->
        invalid_arg "random for unknown type"
    | TNamed (_, t) ->
        random T.(required t)
    | TThis n ->
        handle_this n
    | TVoid ->
        void
    | TFloat ->
        random_float
    | TBool ->
        eq (u32_of_int 0) (bit_and random_u32 (u32_of_int 128))
    | TString ->
        (* Just a few random letters for now: *)
        let_ ~name:"s_ref" (make_ref (string "")) (fun s_ref ->
          let s = get_ref s_ref in
          let len = to_i32 (bit_and random_u32 (u32_of_int 15)) in
          let_ ~name:"len" len (fun len ->
            seq [
              repeat ~from:(i32 0l) ~to_:len (fun _i ->
                  let c = random T.char in
                  let s' = append_string s (string_of_char_ c) in
                  (set_ref s_ref s')) ;
              s ]))
    | TChar ->
        (* Just a random lowercase letter for now *)
        (char_of_u8
          (add (u8_of_char (char 'a'))
               (force ~what:"random rem"
                      (rem (random T.u8)
                           (u8_of_int 26)))))
    | TU8 ->
        random_u8
    | TU16 ->
        to_u16 random_u32
    | TU24 ->
        to_u24 random_u32
    | TU32 ->
        random_u32
    | TU40 ->
        to_u40 random_u64
    | TU48 ->
        to_u48 random_u64
    | TU56 ->
        to_u56 random_u64
    | TU64 ->
        random_u64
    | TU128 ->
        random_u128
    | TI8 ->
        to_i8 random_u8
    | TI16 ->
        to_i16 random_u32
    | TI24 ->
        to_i24 random_u32
    | TI32 ->
        to_i32 random_u32
    | TI40 ->
        to_i40 random_u64
    | TI48 ->
        to_i48 random_u64
    | TI56 ->
        to_i56 random_u64
    | TI64 ->
        to_i64 random_u64
    | TI128 ->
        to_i128 random_u128
    | TUsr ut ->
        random T.(required ut.def)
    | TExt n ->
        invalid_arg ("random for Ext type "^ n)
    | TVec (dim, mn) ->
        List.init dim (fun _ -> random mn) |>
        make_vec
    | TArr mn ->
        random_lst mn |>
        arr_of_lst
    | TSet (Simple, mn) ->
        random_lst mn |>
        set_of_lst
    | TSet _ ->
        todo "random for non simple sets"
    | TTup mns ->
        Array.map random mns |>
        Array.to_list |>
        make_tup
    | TRec mns ->
        Array.map (fun (name, mn) -> name, random mn) mns |>
        Array.to_list |>
        make_rec
    | TSum mns ->
        let num_options = Array.length mns in
        assert (num_options > 0) ;
        let index = rem (random T.u16) (u16_of_int num_options) in
        let index = force ~what:"num_options>0" index in
        let_ ~name:"index" index (fun index ->
          let rec loop i =
            let _label, mn = mns.(i) in
            if i >= num_options - 1 then
              seq [
                assert_ (eq index (u16_of_int i)) ;
                construct mns i (random mn) ]
            else
              if_ (eq index (u16_of_int i))
                  ~then_:(construct mns i (random mn))
                  ~else_:(loop (i + 1)) in
          loop 0)
    | TMap _ ->
        invalid_arg "random for Map type"
    | TSize | TPtr | TAddress | TBytes | TMask | TLst _ ->
        todo "random"
    | TFunction _ ->
        todo "randomfunctions"
  in
  random mn0

(* [random mn] returns an expression with a (runtime) random value of
 * maybe-nullable type [mn]. No support for recursive types (but see
 * [func_random]) *)
let random mn =
  let rec handle_this n =
    let mn = T.(required (find_this n)) in
    (* Will blow the stack on recursive types: *)
    random_gen handle_this mn in
  random_gen handle_this mn

(* [func_random mn] generates a function taking no parameters and returning
 * a random value of type [mn]. Support recursive types. *)
let func_random mn0 =
  (* "this" is not allowed as the tope level or this would just loop around
   * without doing any work: *)
  let mn0 = T.develop_this_mn mn0 in
  let rec handle_this n =
    let mn = T.(required (find_this n)) in
    if T.eq_mn mn mn0 then
      (* Call myself recursively: *)
      apply (myself mn0) []
    else
      (* Will blow the stack on mutually recursive types: *)
      random_gen handle_this mn in
  func0 (fun () ->
    random_gen handle_this mn0)

(*
 * Kahan sums for better accuracy when summing large number of floats:
 *)

module Kahan =
struct
  (* The state of the sum consists of the sum itself and some carry: *)
  let state_t =
    T.(tuple [| float ; float |])

  (* Return the initial value for the state: *)
  let init =
    let open E.Ops in
    let zero = float 0. in
    make_tup [ zero ; zero ]

  (* Add [x] (a float) to [sum] ([c] is carried along and must be added too
   * eventually): *)
  let add sum_c x =
    let open E.Ops in
    let_ ~name:"kahan_state" sum_c (fun sum_c ->
      let_ ~name:"kahan_x" (to_float x) (fun x ->
        let sum = get_item 0 sum_c
        and carry = get_item 1 sum_c in
        let_ ~name:"kahan_sum" (add sum x) (fun s ->
          let carry' =
            if_ (ge (abs sum) (abs x))
              ~then_:(add (sub sum s) x)
              ~else_:(add (sub x s) sum) in
          make_tup [ s ; add carry carry' ])))

  (* In some rare cases we might want to scale the counter: *)
  let mul sum_c x =
    let open E.Ops in
    let_ ~name:"kahan_state" sum_c (fun sum_c ->
      let_ ~name:"kahan_x2" (to_float x) (fun x ->
        let sum = get_item 0 sum_c
        and carry = get_item 1 sum_c in
        make_tup [ mul sum x ; mul carry x ]))

  let finalize sum_c =
    let open E.Ops in
    let_ ~name:"kahan_state" sum_c (fun sum_c ->
      let sum = get_item 0 sum_c
      and carry = get_item 1 sum_c in
      add sum carry)
end

(*
 * Compute the percentiles [ps] of a set of values [vs], which actual types
 * are passed as [vs_t] and [ps_t].
 * The result is a vector of same dimension than [ps].
 *
 * TODO: rewrite without map, using only imperative operations
 *)

let percentiles vs vs_t ps ps_t =
  let open E.Ops in
  null_map ~name:"vs" vs (fun vs ->
    let vs_t = T.force vs_t in
    comment "Compute the indices of those percentiles" (
      let ks =
        let card_vs = to_float (sub (cardinality vs) (u32_of_int 1)) in
        let_ ~name:"card_vs" card_vs (fun card_vs ->
          let p_t =
            T.get_item_type ~vec:true ~arr:true ~set:true ~lst:true ps_t in
          map_ card_vs (func2 T.float p_t (fun card_vs p ->
            seq [
              assert_ (and_ (ge (to_float p) (float 0.))
                            (le (to_float p) (float 100.))) ;
              mul (mul (to_float p) (float 0.01)) card_vs |>
              round |>
              to_u32 ])
          ) ps) in
      let_ ~name:"perc_ks" ks (fun ks ->
        seq [
          (* Sort vs: *)
          partial_sort vs ks ;
          map_ vs (func2 vs_t T.u32 (fun vs k ->
            unsafe_nth k vs)
          ) ks ])))

(* Tells if a string/vector/list/set/lst/ptr is empty, dealing with nullable: *)
let is_empty e e_t =
  let open E.Ops in
  let prop_null nullable e f =
    if nullable then
      if_null e
        ~then_:(null T.TBool)
        ~else_:(not_null (f (force e)))
    else
      f e in
  match e_t with
  | T.{ typ = TString ; nullable ; _ } ->
      prop_null nullable e (fun e ->
        eq (u32_of_int 0) (string_length e))
  | { typ = (TVec _ | TArr _ | TSet _) ; nullable } ->
      prop_null nullable e (fun e ->
        eq (u32_of_int 0) (cardinality e))
  | { typ = TUsr { name = "Cidr4" ; _ } ; nullable } ->
      prop_null nullable e (fun e ->
        lt (get_field "mask" e) (u8_of_int 32))
  | { typ = TUsr { name = "Cidr6" ; _ } ; nullable } ->
      prop_null nullable e (fun e ->
        lt (get_field "mask" e) (u8_of_int 128))
  | { typ = TUsr { name = "Cidr" ; _ } ; nullable } ->
      prop_null nullable e (fun e ->
        if_ (eq (label_of e) (u16_of_int 0))
          ~then_:(lt (get_field "mask" (get_alt "v4" e)) (u8_of_int 32))
          ~else_:(lt (get_field "mask" (get_alt "v6" e)) (u8_of_int 128)))
  | { typ = TPtr ; nullable } ->
      prop_null nullable e (fun e -> eq (size 0) (rem_size e))
  | { typ = TLst mn ; nullable } ->
      prop_null nullable e (fun e -> eq e (eol mn))
  | mn ->
      BatPrintf.sprintf2 "is_empty for %a" T.print_mn mn |>
      invalid_arg

(* Tells if any item [i] from the container [lst] matches [f i]. *)
let exists lst f =
  let open E.Ops in
  (* FIXME: a way to exit the loop that iterates through a container *)
  let_ ~name:"res" (make_ref false_) (fun res_ref ->
    let res = get_ref res_ref in
    seq [
      for_each ~name:"item" lst (fun item ->
        set_ref res_ref (or_ res (f item))) ;
      res ])

let first_ip_of_cidr width all_ones cidr =
  let open E.Ops in
  let ip = get_field "ip" cidr in
  let mask = get_field "mask" cidr in
  let shf = sub (u8_of_int width) mask in
  let nm = left_shift all_ones shf in
  bit_and nm ip

let first_ip_of_cidr4 cidr =
  let open E.Ops in
  first_ip_of_cidr 32 (u32 Uint32.(sub zero one)) cidr

let first_ip_of_cidr6 cidr =
  let open E.Ops in
  first_ip_of_cidr 128 (u128 Uint128.(sub zero one)) cidr

let last_ip_of_cidr width all_ones one cidr =
  let open E.Ops in
  let ip = get_field "ip" cidr in
  let mask = get_field "mask" cidr in
  let shf = sub (u8_of_int width) mask in
  let nm = left_shift one shf in
  (* Left-shifts of more than the int width are undefined: *)
  if_ (eq (u8_of_int 0) mask)
    ~then_:all_ones
    ~else_:(sub (bit_or nm ip) one)

let last_ip_of_cidr4 cidr =
  let open E.Ops in
  last_ip_of_cidr 32 (u32 Uint32.(sub zero one)) (u32_of_int 1) cidr

let last_ip_of_cidr6 cidr =
  let open E.Ops in
  last_ip_of_cidr 128 (u128 Uint128.(sub zero one)) (u128_of_int 1) cidr

(* Tells whether [e] is in the iterable/cidr [lst].
 * Also works when [lst] is a set of sets (of sets...).
 * Also handles the case where [lst] and/or [e] is null (note than null is known
 * not to be in a (non-null) empty set).
 * Return value is nullable whenever [item] or [lst] is. *)
let rec is_in item item_t lst lst_t =
  let open E.Ops in
  let_ ~name:"lst" lst (fun lst ->
    let_ ~name:"item" item (fun item ->
      if lst_t.T.nullable then
        if_null lst
          ~then_:(null T.TBool)
          ~else_:(
            let lst = force ~what:"is_in(0)" lst
            and lst_t = { lst_t with nullable = false } in
            not_null (is_in item item_t lst lst_t))
      else
        if_ (comment "is_in: Is List empty?"
              ((* It makes that code simpler to allow `scalar is_in scalar` and
                  treat it as meaning `scalar = scalar`, so we must accept non
                  containers as never empty: *)
                try is_empty lst lst_t
                with Invalid_argument _ -> bool false))
          ~then_:(
            let ret = false_ in
            if item_t.T.nullable then not_null ret else ret)
          ~else_:(
            if item_t.T.nullable then
              if_null item
                ~then_:(null T.TBool)
                ~else_:(
                  let item = force ~what:"is_in(1)" item
                  and item_t = { item_t with nullable = false } in
                  not_null (is_in item item_t lst lst_t))
            else (
              let err () =
                BatPrintf.sprintf2 "is_in: invalid types (%a in %a)"
                  T.print_mn item_t
                  T.print_mn lst_t |>
                invalid_arg in
              (* Now that neither lst nor item are nullable, let's deal with
               * the actual question: *)
              match item_t.T.typ, lst_t.T.typ with
              (* Substring search: *)
              | TString, TString ->
                  not_ (is_null (find_substring (bool true) item lst))
              (* In all other cases where [item] and [lst] are of the same type
               * then [in_in item lst] is just a comparison: *)
              | _ when T.eq_mn item_t lst_t ->
                  eq item lst
              (* Otherwise [lst] must be some kind of set: *)
              | TChar, TString ->
                  not_ (is_null (index item lst))
              | TUsr { name = "Ip4" ; _ }, TUsr { name = "Cidr4" ; _ } ->
                  and_ (ge item (first_ip_of_cidr4 lst))
                       (le item (last_ip_of_cidr4 lst))
              | TUsr { name = "Ip6" ; _ }, TUsr { name = "Cidr6" ; _ } ->
                  and_ (ge item (first_ip_of_cidr6 lst))
                       (le item (last_ip_of_cidr6 lst))
              | TUsr { name = "Ip" ; _ }, TUsr { name = "Cidr" ; _ } ->
                  if_ (eq (label_of item) (label_of lst))
                    ~then_:(
                      if_ (eq (label_of item) (u16_of_int 0))
                        ~then_:(
                          let_ ~name:"ip" (get_alt "v4" item) (fun ip ->
                            let_ ~name:"cidr" (get_alt "v4" lst) (fun cidr ->
                              let ip_t = T.(required (get_user_type "Ip4"))
                              and cidr_t = T.(required (get_user_type "Cidr4")) in
                              is_in ip ip_t cidr cidr_t)))
                        ~else_:(
                          let_ ~name:"ip" (get_alt "v6" item) (fun ip ->
                            let_ ~name:"cidr" (get_alt "v6" lst) (fun cidr ->
                              let ip_t = T.(required (get_user_type "Ip6"))
                              and cidr_t = T.(required (get_user_type "Cidr6")) in
                              is_in ip ip_t cidr cidr_t))))
                    ~else_:(bool false)
              | item_typ, (TVec (_, { typ = lst_typ ; nullable }) |
                           TArr { typ = lst_typ ; nullable }) ->
                  (* If the set item type is the same as item type then perform
                   * direct comparisons with [eq], otherwise call [is_in]
                   * recursively.
                   * [is_in] cannot be merely called recursively because of the
                   * semantic with strings: "ba" is in "foobar" whereas it is
                   * not in [ "foobar" ]! *)
                  let op =
                    if T.eq item_typ lst_typ then eq
                    else (fun a b ->
                      is_in a item_t
                            b T.(required lst_typ)) in
                  exists lst (fun i ->
                    if nullable then
                      let_ ~name:"i" i (fun i ->
                        if_null i
                          ~then_:(bool false)
                          ~else_:(op item (force ~what:"is_in(2)" i)))
                    else
                      op item i)
              | _ ->
                  (* If we can convert item into lst (which at this point is
                   * known not to be a list) then we can try equality: *)
                  (match C.conv_mn ~from:item_t ~to_:lst_t item with
                  | exception _ ->
                      err ()
                  | item' ->
                      eq item' lst)))))

let rec cases ~else_ = function
  | [] -> else_
  | [ c, t ] -> if_ c ~then_:t ~else_
  | (c, t) :: rest -> if_ c ~then_:t ~else_:(cases ~else_ rest)

(* Given a byte that's a hex digit (0-9, a-f or A-F), returns the value of
 * that digit as a u8: *)
let u8_of_hex_digit b =
  let_ ~name:"u8_of_hex_b" b (fun b ->
    cases [
      le b (u8_of_const_char '9'), sub b (u8_of_const_char '0') ;
      le b (u8_of_const_char 'F'), sub b (u8_of_const_char 'A')
    ] ~else_:(sub b (u8_of_const_char 'a')))

(* Assuming [c] is a number between 0 and 15, returns the digit (as an u8): *)
let hex_digit_of_u8 c =
  (* TODO: have a conversion table once those constants are moved into global *)
  add c (
    if_ (ge c (u8_of_int 10))
      ~then_:(u8_of_int 87)
      ~else_:(u8_of_const_char '0'))

let cap_size n e =
  let_ ~name:"cap_size_e" e (fun e ->
    if_ (ge e (size n)) ~then_:(size n) ~else_:e)

let string_around ?(width=6) p =
  let_ ~name:"string_around_p" p (fun p ->
    let rwd = cap_size width (offset p) in
    let fwd = cap_size width (rem_size p) in
    let_ ~name:"string_around_fwd" fwd (fun fwd ->
      let_ ~name:"string_around_rwd_p" (rewind p rwd) (fun p ->
        let bytes_p = read_bytes p (add rwd fwd) in
        string_of_bytes (first bytes_p))))

let dump_u8_as_char b =
  seq [
    dump (char '\'') ;
    dump (char_of_u8 b) ;
    dump (char '\'') ;
    dump (string " (") ;
    dump b ;
    dump (char ')') ]

let check_byte p c =
  let_ ~name:"check_byte_c" c (fun c ->
    let_ ~name:"check_byte_b" (peek_u8 p (size 0)) (fun b ->
      if_ (ne b c)
        ~then_:(
          seq [ dump (string "Bad char at ") ; dump (offset p) ;
                dump (string ": ") ; dump_u8_as_char b ;
                dump (string " should be: ") ; dump_u8_as_char c ;
                dump (string " (") ; dump (string_around p) ; dump (string ")") ;
                dump (char '\n') ;
                assert_ false_ ])
        ~else_:nop))

(* [copy_rec ~with_:["field1", val1 ; ... ] r] will make a shallow copy of r1,
 * replacing named fields's value by other values (not necessarily of the same
 * types. Meant to compensate somewhat for the immutability of records. *)
let copy_rec l ?(with_=[]) r =
  match E.type_of l r |> T.develop1 with
  | { typ = T.TRec mns ; nullable = false } ->
      (* Check invalid fields first: *)
      List.iter (fun (n, _) ->
        if not (Array.exists (fun (n', _) -> n = n') mns) then
          let e0 = copy_rec ~with_ r in
          raise (E.Struct_error (e0, "Unknown field '"^ n ^"'"))
      ) with_ ;
      let_ ~name:"rec" r (fun r ->
        make_rec (
          BatArray.enum mns |>
          BatEnum.map (fun (name, _mn) ->
            name,
            try List.assoc name with_
            with Not_found -> get_field name r
          ) |>
          BatList.of_enum))
  | _ ->
      invalid_arg "copy_rec"

(* Same as [copy_rec] with for tuples, with [with_] referring to item indices
 * with integers instead of field names: *)
let copy_tup l ?(with_=[]) r =
  match E.type_of l r |> T.develop1 with
  | { typ = T.TTup mns ; nullable = false } ->
      (* Check invalid indices first: *)
      List.iter (fun (i, _) ->
        if i < 0 || i >= Array.length mns then
          let e0 = copy_tup ~with_ r in
          raise (E.Struct_error (e0, "Invalid indice '"^ string_of_int i ^"'"))
      ) with_ ;
      let_ ~name:"tup" r (fun r ->
        make_tup (
          List.init (Array.length mns) (fun i ->
            try List.assoc i with_
            with Not_found -> get_item i r)))
  | _ ->
      invalid_arg "copy_tup"
