(* This is a (de)serializer for an external format where all values are
 * encoded as human readable s-expressions, and are internally represented
 * as regular OCaml values. *)
open Dessert

module Common (IntRepr : INTREPR) =
struct
  module IntRepr = IntRepr
  module SerData = IntRepr.SerData
  type pointer = SerData.pointer

  let size_1 = SerData.size_of_const 1
  let size_4 = SerData.size_of_const 4
  let dword_null = SerData.dword_of_const 0x6c_6c_75_6el
  let i8v_ascii_T = IntRepr.i8v_of_const (Char.code 'T')
  let i8v_ascii_F = IntRepr.i8v_of_const (Char.code 'F')
  let i8v_0 = IntRepr.i8v_of_const 0
  let i8v_10 = IntRepr.i8v_of_const 10
  let i8v_100 = IntRepr.i8v_of_const 100
  let i16v_0 = IntRepr.i16v_of_const 0
  let i16v_10 = IntRepr.i16v_of_const 10
  let i16v_10000 = IntRepr.i16v_of_const 10000
  let i8v_ascii_0 = IntRepr.i8v_of_const (Char.code '0')
  let i8v_ascii_9 = IntRepr.i8v_of_const (Char.code '9')
  let i8v_ascii_e = IntRepr.i8v_of_const (Char.code 'e')
  let i8v_ascii_dot = IntRepr.i8v_of_const (Char.code '.')
  let i8v_ascii_plus = IntRepr.i8v_of_const (Char.code '+')
  let i8v_ascii_minus = IntRepr.i8v_of_const (Char.code '-')
  let i8v_opn = IntRepr.i8v_of_const (Char.code '(')
  let i8v_cls = IntRepr.i8v_of_const (Char.code ')')
  let i8v_spc = IntRepr.i8v_of_const (Char.code ' ')
  let i8v_quo = IntRepr.i8v_of_const (Char.code '"')
end

module MakeDes (IntRepr : INTREPR) : DES with module IntRepr = IntRepr =
struct
  include Common (IntRepr)
  type 'a des = (pointer -> 'a * pointer) code

  let read_digit : pointer code -> (IntRepr.i8v * pointer) code = fun pc ->
    SerData.read_byte pc |>
    IntRepr.map_fst (fun vc ->
      IntRepr.(i8v_sub (IntRepr.i8v_of_byte vc) .<i8v_ascii_0>.))

  let is_digit =
    .<
      fun byte ->
        let c = .~(IntRepr.i8v_of_byte .<byte>.) in
        .~(IntRepr.(boolv_and (i8v_ge .<c>. .<i8v_ascii_0>.)
                              (i8v_ge .<i8v_ascii_9>. .<c>.)))
    >.

  let is_float_char =
    .<
      fun byte ->
        let c = .~(IntRepr.i8v_of_byte .<byte>.) in
        .~(IntRepr.(boolv_or
            (boolv_and (i8v_ge .<c>. .<i8v_ascii_0>.)
                       (i8v_ge .<i8v_ascii_9>. .<c>.))
            (boolv_or (boolv_or (i8v_eq .<c>. .<i8v_ascii_plus>.)
                                (i8v_eq .<c>. .<i8v_ascii_minus>.))
                      (boolv_or (i8v_eq .<c>. .<i8v_ascii_dot>.)
                                (i8v_eq .<c>. .<i8v_ascii_e>.)))))
    >.

  let skip_blanks pc =
    let cond =
      .<
        fun byte ->
          .~(IntRepr.(i8v_eq (i8v_of_byte .<byte>.) .<i8v_spc>.))
      >.
    and reduce =
      .< fun () _byte -> () >.
    in
    IntRepr.read_while ~cond ~reduce .<((), .~pc)>. |>
    IntRepr.snd

  let skip_expected_char c pc =
    let i8v_exp = IntRepr.i8v_of_const (Char.code c) in
    .<
      let byte_opn, p = .~(SerData.read_byte (skip_blanks pc)) in
      let msg =
        Printf.sprintf "Expected '%c' not '%c'"
          c
          (Char.chr .~(SerData.int_of_byte .<byte_opn>.)) in
      fail_if ~cond:(.~(IntRepr.i8v_of_byte .<byte_opn>.) = i8v_exp) ~msg ;
      p
    >.

  let float pc =
    let cond = is_float_char in
    let reduce = (* Simple version: append the bytes into a string *)
      .<
        fun bytes byte ->
          .~(SerData.bytes_append .<bytes>. .<byte>.)
      >. in
    IntRepr.read_while ~cond ~reduce .< .~SerData.make_bytes, .~pc >. |>
    IntRepr.map_fst IntRepr.floatv_of_bytes

  let dfloat = .< fun p -> .~(float (skip_blanks .<p>.)) >.

  let string pc =
    let pc = skip_expected_char '"' pc in
    let cond =
      .<
        fun byte -> .~(IntRepr.(i8v_ne (i8v_of_byte .<byte >.) .<i8v_quo>.))
      >. in
    let reduce =
      .<
        fun old_bytes new_byte ->
          .~(SerData.bytes_append .<old_bytes>. .<new_byte>.)
      >. in
    .<
      let v, p =
        .~(IntRepr.read_while ~cond ~reduce .< .~SerData.make_bytes, .~pc >.) in
      .~(IntRepr.stringv_of_bytes .<v>.),
      .~(SerData.add .<p>. .<SerData.size_of_const 1>.)
    >.

  let dstring = .< fun p -> .~(string (skip_blanks .<p>.)) >.

  let bool pc =
    SerData.read_byte pc |>
    IntRepr.map_pair
      ~fst:(fun vc ->
        let c = IntRepr.(i8v_of_byte vc) in
        IntRepr.i8v_eq c .<i8v_ascii_T>.)
      ~snd:(fun x -> x)

  let dbool = .< fun p -> .~(bool (skip_blanks .<p>.)) >.

  let integer of_i8v add mul zero pc =
    let cond = is_digit
    and reduce =
      .<
        fun v byte ->
          let c = .~(IntRepr.i8v_of_byte .<byte>.) in
          .~(add (mul .<v>. (of_i8v .<i8v_10>.))
                 (of_i8v (IntRepr.i8v_sub .<c>. .<i8v_ascii_0>.)))
      >.
    in
    IntRepr.read_while ~cond ~reduce .< .~zero, .~pc >.

  let i8 = integer BatPervasives.identity IntRepr.i8v_add IntRepr.i8v_mul .<i8v_0>.
  let di8 = .< fun p -> .~(i8 (skip_blanks .<p>.)) >.

  let i16 = integer IntRepr.i16v_of_i8v IntRepr.i16v_add IntRepr.i16v_mul .<i16v_0>.
  let di16 = .< fun p -> .~(i16 (skip_blanks .<p>.)) >.

  let tup_opn _typs pc =
    skip_expected_char '(' pc

  let tup_cls _typs pc =
    skip_expected_char ')' pc

  let tup_sep _typs _n pc =
    skip_blanks pc

  let vec_opn _dim _typ pc =
    skip_expected_char '(' pc

  let vec_cls _dim _typ pc =
    skip_expected_char ')' pc

  let vec_sep _dim _typ _n pc =
    skip_blanks pc

  (* Regardless of the type structure, nulls are simply encoded as `null` *)
  let is_null _typ pc =
    .<
      let p = .~pc in
      (* Notice that boolv_and must translate into code that terminate
       * evaluation early (even if it's not the case in stage0) *)
      .~(IntRepr.(boolv_and
           (size_ge (SerData.rem .<p>.) .<SerData.size_of_const 4>.)
           (dword_eq (SerData.peek_dword .<p>.) .<SerData.dword_of_const 0x6c_6c_75_6el>.)))
    >.

  let dnull pc = SerData.add pc .<SerData.size_of_const 4>.
end

module MakeSer (IntRepr : INTREPR) : SER with module IntRepr = IntRepr =
struct
  include Common (IntRepr)
  type 'a ser = ('a * pointer -> pointer) code

  let float v_p =
    .<
      let v, p = .~v_p in
      let bytes = .~(IntRepr.bytes_of_floatv .<v>.) in
      .~(SerData.write_bytes .<p>. .<bytes>.)
    >.

  let sfloat = .< fun v_p -> .~(float .<v_p>.) >.

  let string v_p =
    .<
      let v, p = .~v_p in
      let p = .~(SerData.write_byte .<p>. (IntRepr.byte_of_i8v .<i8v_quo>.)) in
      let v = .~(IntRepr.bytes_of_stringv .<v>.) in
      let p = .~(SerData.write_bytes .<p>. .<v>.) in
      .~(SerData.write_byte .<p>. (IntRepr.byte_of_i8v .<i8v_quo>.))
    >.

  let sstring = .< fun v_p -> .~(string .<v_p>.) >.

  let bool v_p =
    .<
      let v, p = .~v_p in
      let byte =
        .~(IntRepr.choose .<v>. .<i8v_ascii_T>. .<i8v_ascii_F>. |>
           IntRepr.byte_of_i8v) in
      .~(SerData.write_byte .<p>. .<byte>.)
    >.

  let sbool = .< fun v_p -> .~(bool .<v_p>.) >.

  let integer to_i8v zero max_scale eq gt div mod_ ten v_p =
    .<
      let v, p = .~v_p in
      .~(IntRepr.choose (eq .<v>. zero)
           (SerData.write_byte .<p>. (IntRepr.byte_of_i8v .<i8v_ascii_0>.))
           (let cond =
              .< fun _ (_v, scale) -> .~(gt .<scale>. zero) >.
            and loop =
              .<
                fun p (v, scale) ->
                  let digit = .~(to_i8v (mod_ (div .<v>. .<scale>.) ten)) in
                  let chr = .~(IntRepr.i8v_add .<i8v_ascii_0>. .<digit>.) in
                  let byte = .~(IntRepr.byte_of_i8v .<chr>.) in
                  let p = .~(SerData.write_byte .<p>. .<byte>.) in
                  p, (v, .~(div .<scale>. ten))
              >. in
            IntRepr.do_while ~cond ~loop .<(v, .~max_scale)>. .<p>.))
    >.

  let i8 =
    integer BatPervasives.identity .<i8v_0>. .<i8v_100>. IntRepr.i8v_eq IntRepr.i8v_gt IntRepr.i8v_div IntRepr.i8v_mod .<i8v_10>.
  let si8 = .< fun v_p -> .~(i8 .<v_p>.) >.

  let i16 =
    integer IntRepr.i8v_of_i16v .<i16v_0>. .<i16v_10000>. IntRepr.i16v_eq IntRepr.i16v_gt IntRepr.i16v_div IntRepr.i16v_mod .<i16v_10>.
  let si16 = .< fun v_p -> .~(i16 .<v_p>.) >.

  let tup_opn _typs pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v .<i8v_opn>.)

  let tup_cls _typs pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v .<i8v_cls>.)

  let tup_sep _typs _n pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v .<i8v_spc>.)

  let vec_opn _dim _typ pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v .<i8v_opn>.)

  let vec_cls _dim _typ pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v .<i8v_cls>.)

  let vec_sep _dim _typ _n pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v .<i8v_spc>.)

  (* Regardless of the type structure, nulls are simply encoded as `null` *)
  let snull pc = SerData.write_dword pc .<SerData.dword_of_const 0x6c_6c_75_6el>.
end
