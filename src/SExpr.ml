(* This is a (de)serializer for an external format where all values are
 * encoded as human readable s-expressions, and are internally represented
 * as regular OCaml values. *)
open Dessert

module Common (IntRepr : INTREPR) =
struct
  module IntRepr = IntRepr
  module SerData = IntRepr.SerData
  type pointer = SerData.pointer

  let i8v_ascii_T = IntRepr.i8v_of_const (Char.code 'T')
  let i8v_ascii_F = IntRepr.i8v_of_const (Char.code 'F')
  let i8v_0 = IntRepr.i8v_of_const 0
  let i8v_10 = IntRepr.i8v_of_const 10
  let i8v_100 = IntRepr.i8v_of_const 100
  let i8v_ascii_0 = IntRepr.i8v_of_const (Char.code '0')
  let i8v_ascii_9 = IntRepr.i8v_of_const (Char.code '9')
  let i8v_opn = IntRepr.i8v_of_const (Char.code '(')
  let i8v_cls = IntRepr.i8v_of_const (Char.code ')')
  let i8v_spc = IntRepr.i8v_of_const (Char.code ' ')
end

module MakeDes (IntRepr : INTREPR) : DES with module IntRepr = IntRepr =
struct
  include Common (IntRepr)

  let bool pc =
    SerData.read_byte pc |>
    IntRepr.map_pair
      ~fst:(fun vc ->
        let c = IntRepr.(i8v_of_byte vc) in
        IntRepr.i8v_eq c i8v_ascii_T)
      ~snd:(fun x -> x)

  let i8 pc =
    (* Assume that we can read at least one byte and that it's a valid digit *)
    let v_p =
      SerData.read_byte pc in
    let v_p =
      IntRepr.map_fst .<
        fun v ->
          let c = .~(IntRepr.i8v_of_byte .<v>.) in
          .~(IntRepr.i8v_sub .<c>. i8v_ascii_0)
      >. v_p in
    let cond =
      .<
        fun byte ->
          let c = .~(IntRepr.i8v_of_byte .<byte>.) in
          .~(IntRepr.boolv_and (IntRepr.i8v_ge .<c>. i8v_ascii_0)
                               (IntRepr.i8v_ge i8v_ascii_9 .<c>.))
      >.
    and reduce =
      .<
        fun v byte ->
          let c = .~(IntRepr.i8v_of_byte .<byte>.) in
          .~(IntRepr.i8v_add (IntRepr.i8v_mul .<v>. i8v_10)
                             (IntRepr.i8v_sub .<c>. i8v_ascii_0))
      >.
    in
    IntRepr.read_while ~cond ~reduce v_p

  let skip_blanks pc =
    let cond =
      .<
        fun byte ->
          .~(IntRepr.i8v_eq (IntRepr.i8v_of_byte .<byte>.) i8v_spc)
      >.
    and reduce =
      .< fun () _byte -> () >.
    in
    IntRepr.read_while ~cond ~reduce .<((), .~pc)>. |>
    IntRepr.snd

  let dbool = .< fun p -> .~(bool (skip_blanks .<p>.)) >.
  let di8 = .< fun p -> .~(i8 (skip_blanks .<p>.)) >.

  let skip_expected_char c p =
    let i8v_exp = IntRepr.i8v_of_const (Char.code c) in
    .<
      let byte_opn, p = .~(SerData.read_byte (skip_blanks p)) in
      let msg =
        Printf.sprintf "Expected '%c' not '%c'"
          c
          (Char.chr .~(SerData.int_of_byte .<byte_opn>.)) in
      fail ~cond:(.~(IntRepr.i8v_of_byte .<byte_opn>.) = .~i8v_exp) ~msg ;
      p
    >.

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
end

module MakeSer (IntRepr : INTREPR) : SER with module IntRepr = IntRepr =
struct
  include Common (IntRepr)

  let bool v_p =
    .<
      let v, p = .~v_p in
      let byte =
        .~(IntRepr.choose .<v>. i8v_ascii_T i8v_ascii_F |>
           IntRepr.byte_of_i8v) in
      .~(SerData.write_byte .<p>. .<byte>.)
    >.

  let i8 v_p =
    .<
      let v, p = .~v_p in
      .~(IntRepr.choose (IntRepr.i8v_eq .<v>. i8v_0)
           (SerData.write_byte .<p>. (IntRepr.byte_of_i8v i8v_ascii_0))
           (let cond =
              .< fun _ (_v, scale) -> .~(IntRepr.i8v_gt .<scale>. i8v_0) >.
            and loop =
              .<
                fun p (v, scale) ->
                  let digit =
                    .~(IntRepr.(i8v_mod (i8v_div .<v>. .<scale>.) i8v_10)) in
                  let chr = .~(IntRepr.i8v_add i8v_ascii_0 .<digit>.) in
                  let byte = .~(IntRepr.byte_of_i8v .<chr>.) in
                  let p = .~(SerData.write_byte .<p>. .<byte>.) in
                  p, (v, .~(IntRepr.i8v_div .<scale>. i8v_10))
              >. in
            IntRepr.do_while ~cond ~loop .<(v, .~i8v_100)>. .<p>.))
    >.

  let i16 _v_p = assert false

  let sbool = .< fun v_p -> .~(bool .<v_p>.) >.

  let si8 = .< fun v_p -> .~(i8 .<v_p>.) >.

  let tup_opn _typs pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v i8v_opn)

  let tup_cls _typs pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v i8v_cls)

  let tup_sep _typs _n pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v i8v_spc)

  let vec_opn _dim _typ pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v i8v_opn)

  let vec_cls _dim _typ pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v i8v_cls)

  let vec_sep _dim _typ _n pc =
    SerData.write_byte pc (IntRepr.byte_of_i8v i8v_spc)
end
