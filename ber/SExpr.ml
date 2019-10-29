(* This is a (de)serializer for an external format where all values are
 * encoded as human readable s-expressions, and are internally represented
 * as regular OCaml values. *)
open Batteries
open Stdint
open Dessser
open Helpers
open MyLifts

module Common =
struct
  module SerData = IntRepr.SerData
  type pointer = SerData.pointer

  let size_1 = SerData.size_of_const 1
  let size_4 = SerData.size_of_const 4
  let dword_null = SerData.dword_of_const (Uint32.of_int32 0x6c_6c_75_6el)
  let ascii_T = IntRepr.U8.of_const (Char.code 'T')
  let ascii_F = IntRepr.U8.of_const (Char.code 'F')
  let ascii_0 = IntRepr.U8.of_const (Char.code '0')
  let ascii_9 = IntRepr.U8.of_const (Char.code '9')
  let ascii_e = IntRepr.U8.of_const (Char.code 'e')
  let ascii_dot = IntRepr.U8.of_const (Char.code '.')
  let ascii_plus = IntRepr.U8.of_const (Char.code '+')
  let ascii_minus = IntRepr.U8.of_const (Char.code '-')
  let opn = IntRepr.U8.of_const (Char.code '(')
  let cls = IntRepr.U8.of_const (Char.code ')')
  let spc = IntRepr.U8.of_const (Char.code ' ')
  let quo = IntRepr.U8.of_const (Char.code '"')
  let i8v_10 = IntRepr.I8.of_const 10
  let i8v_100 = IntRepr.I8.of_const 100
  let u8v_10 = IntRepr.U8.of_const 10
  let u8v_100 = IntRepr.U8.of_const 100
  let i16v_10 = IntRepr.I16.of_const 10
  let i16v_10e4 = IntRepr.I16.of_const 10_000
  let u16v_10 = IntRepr.U16.of_const 10
  let u16v_10e4 = IntRepr.U16.of_const 10_000
  let i32v_10 = IntRepr.I32.of_const 10l
  let i32v_10e9 = IntRepr.I32.of_const 1_000_000_000l
  let u32v_10 = IntRepr.U32.of_const (Uint32.of_int 10)
  let u32v_10e9 = IntRepr.U32.of_const (Uint32.of_int32 1_000_000_000l)
  let i64v_10 = IntRepr.I64.of_const 10L
  let i64v_10e18 = IntRepr.I64.of_const 1_000_000_000_000_000_000L
  let u64v_10 = IntRepr.U64.of_const (Uint64.of_int 10)
  let u64v_10e19 = IntRepr.U64.of_const (Uint64.of_int64 (-8446744073709551616L) (* aka 10_000_000_000_000_000_000 *))
  let i128v_10 = IntRepr.I128.of_const (Int128.of_int 10)
  let i128v_10e38 = IntRepr.I128.of_const (Int128.of_string "100000000000000000000000000000000000000")
  let u128v_10 = IntRepr.U128.of_const (Uint128.of_int 10)
  let u128v_10e38 = IntRepr.U128.of_const (Uint128.of_string "100000000000000000000000000000000000000")
end

module Des : DES =
struct
  include Common
  type 'a des = (pointer -> 'a * pointer) code

  let read_digit pc =
    SerData.read_byte pc |>
    IntRepr.map_fst (fun vc ->
      IntRepr.(U8.sub (IntRepr.U8.of_byte vc) (lift_uint8 ascii_0)))

  let is_digit =
    .<
      fun byte ->
        let c = .~(IntRepr.U8.of_byte .<byte>.) in
        .~(IntRepr.(boolv_and (U8.ge .<c>. (lift_uint8 ascii_0))
                              (U8.ge (lift_uint8 ascii_9) .<c>.)))
    >.

  let is_float_char =
    .<
      fun byte ->
        let c = .~(IntRepr.U8.of_byte .<byte>.) in
        .~(IntRepr.(boolv_or
            (boolv_and (U8.ge .<c>. (lift_uint8 ascii_0))
                       (U8.ge (lift_uint8 ascii_9) .<c>.))
            (boolv_or (boolv_or (U8.eq .<c>. (lift_uint8 ascii_plus))
                                (U8.eq .<c>. (lift_uint8 ascii_minus)))
                      (boolv_or (U8.eq .<c>. (lift_uint8 ascii_dot))
                                (U8.eq .<c>. (lift_uint8 ascii_e))))))
    >.

  let skip_blanks pc =
    let cond =
      .<
        fun byte ->
          .~(IntRepr.(U8.eq (U8.of_byte .<byte>.) (lift_uint8 spc)))
      >.
    and reduce =
      .< fun () _byte -> () >.
    in
    IntRepr.read_while ~cond ~reduce .<((), .~pc)>. |>
    IntRepr.snd

  let skip_expected_char c pc =
    let i8v_exp = IntRepr.U8.of_const (Char.code c) in
    .<
      let byte_opn, p = .~(SerData.read_byte (skip_blanks pc)) in
      let msg =
        Printf.sprintf "Expected '%c' not '%c'"
          c
          (Char.chr .~(SerData.int_of_byte .<byte_opn>.)) in
      fail_if ~cond:(.~(IntRepr.U8.of_byte .<byte_opn>.) = i8v_exp) ~msg ;
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
    IntRepr.map_fst IntRepr.Float.of_bytes

  let dfloat = .< fun p -> .~(float (skip_blanks .<p>.)) >.

  let string pc =
    let pc = skip_expected_char '"' pc in
    let cond =
      .<
        fun byte -> .~(IntRepr.(U8.ne (U8.of_byte .<byte >.) (lift_uint8 quo)))
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
      .~(SerData.add .<p>. .<size_1>.)
    >.

  let dstring = .< fun p -> .~(string (skip_blanks .<p>.)) >.

  let bool pc =
    SerData.read_byte pc |>
    IntRepr.map_pair
      ~fst:(fun vc ->
        let c = IntRepr.(U8.of_byte vc) in
        IntRepr.U8.eq c (lift_uint8 ascii_T))
      ~snd:(fun x -> x)

  let dbool = .< fun p -> .~(bool (skip_blanks .<p>.)) >.

  let integer of_u8v add mul zero pc =
    let cond = is_digit
    and reduce =
      .<
        fun v byte ->
          let c = .~(IntRepr.U8.of_byte .<byte>.) in
          .~(add (mul .<v>. (of_u8v (lift_uint8 u8v_10)))
                 (of_u8v (IntRepr.U8.sub .<c>. (lift_uint8 ascii_0))))
      >.
    in
    IntRepr.read_while ~cond ~reduce .< .~zero, .~pc >.

  let i8 = integer IntRepr.I8.of_u8v IntRepr.I8.add IntRepr.I8.mul IntRepr.I8.zero
  let di8 = .< fun p -> .~(i8 (skip_blanks .<p>.)) >.

  let u8 = integer identity IntRepr.U8.add IntRepr.U8.mul IntRepr.U8.zero
  let du8 = .< fun p -> .~(u8 (skip_blanks .<p>.)) >.

  let i16 = integer IntRepr.I16.of_u8v IntRepr.I16.add IntRepr.I16.mul IntRepr.I16.zero
  let di16 = .< fun p -> .~(i16 (skip_blanks .<p>.)) >.

  let u16 = integer IntRepr.U16.of_u8v IntRepr.U16.add IntRepr.U16.mul IntRepr.U16.zero
  let du16 = .< fun p -> .~(u16 (skip_blanks .<p>.)) >.

  let i32 = integer IntRepr.I32.of_u8v IntRepr.I32.add IntRepr.I32.mul IntRepr.I32.zero
  let di32 = .< fun p -> .~(i32 (skip_blanks .<p>.)) >.

  let u32 = integer IntRepr.U32.of_u8v IntRepr.U32.add IntRepr.U32.mul IntRepr.U32.zero
  let du32 = .< fun p -> .~(u32 (skip_blanks .<p>.)) >.

  let i64 = integer IntRepr.I64.of_u8v IntRepr.I64.add IntRepr.I64.mul IntRepr.I64.zero
  let di64 = .< fun p -> .~(i64 (skip_blanks .<p>.)) >.

  let u64 = integer IntRepr.U64.of_u8v IntRepr.U64.add IntRepr.U64.mul IntRepr.U64.zero
  let du64 = .< fun p -> .~(u64 (skip_blanks .<p>.)) >.

  let i128 = integer IntRepr.I128.of_u8v IntRepr.I128.add IntRepr.I128.mul IntRepr.I128.zero
  let di128 = .< fun p -> .~(i128 (skip_blanks .<p>.)) >.

  let u128 = integer IntRepr.U128.of_u8v IntRepr.U128.add IntRepr.U128.mul IntRepr.U128.zero
  let du128 = .< fun p -> .~(u128 (skip_blanks .<p>.)) >.

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

  (* Regardless of the type structure, nulls are simply encoded as "null" *)
  let is_null _typ pc =
    .<
      let p = .~pc in
      let knull = .~(lift_uint32 dword_null) in
      (* Notice that boolv_and must translate into code that terminate
       * evaluation early (even if it's not the case in stage0) *)
      .~(IntRepr.(boolv_and
           (size_ge (SerData.rem .<p>.) .<size_4>.)
           (dword_eq (SerData.peek_dword .<p>.) .<knull>.)))
    >.

  let dnull pc = SerData.add pc .<size_4>.
  let dnotnull pc = pc
end

module Ser : SER =
struct
  include Common
  type 'a ser = ('a * pointer -> pointer) code

  let float v_p =
    .<
      let v, p = .~v_p in
      let bytes = .~(IntRepr.Float.to_bytes .<v>.) in
      .~(SerData.write_bytes .<p>. .<bytes>.)
    >.

  let sfloat = .< fun v_p -> .~(float .<v_p>.) >.

  let string v_p =
    .<
      let v, p = .~v_p in
      let p = .~(SerData.write_byte .<p>. (IntRepr.U8.to_byte (lift_uint8 quo))) in
      let v = .~(IntRepr.bytes_of_stringv .<v>.) in
      let p = .~(SerData.write_bytes .<p>. .<v>.) in
      .~(SerData.write_byte .<p>. (IntRepr.U8.to_byte (lift_uint8 quo)))
    >.

  let sstring = .< fun v_p -> .~(string .<v_p>.) >.

  let bool v_p =
    .<
      let v, p = .~v_p in
      let byte =
        .~(IntRepr.choose .<v>. (lift_uint8 ascii_T) (lift_uint8 ascii_F) |>
           IntRepr.U8.to_byte) in
      .~(SerData.write_byte .<p>. .<byte>.)
    >.

  let sbool = .< fun v_p -> .~(bool .<v_p>.) >.

  let integer to_u8v zero max_scale eq gt div rem ten v_p =
    .<
      let v, p = .~v_p in
      .~(IntRepr.choose (eq .<v>. zero)
           (SerData.write_byte .<p>. (IntRepr.U8.to_byte (lift_uint8 ascii_0)))
           (let cond =
              .< fun _ (_v, scale) -> .~(gt .<scale>. zero) >.
            and loop =
              .<
                fun p (v, scale) ->
                  let digit = .~(to_u8v (rem (div .<v>. .<scale>.) ten)) in
                  let chr = .~(IntRepr.U8.add (lift_uint8 ascii_0) .<digit>.) in
                  let byte = .~(IntRepr.U8.to_byte .<chr>.) in
                  let p = .~(SerData.write_byte .<p>. .<byte>.) in
                  p, (v, .~(div .<scale>. ten))
              >. in
            IntRepr.do_while ~cond ~loop .<(v, .~max_scale)>. .<p>.))
    >.

  let i8 =
    integer IntRepr.I8.to_u8v IntRepr.I8.zero (lift_int8 i8v_100) IntRepr.I8.eq IntRepr.I8.gt IntRepr.I8.div IntRepr.I8.rem (lift_int8 i8v_10)
  let si8 = .< fun v_p -> .~(i8 .<v_p>.) >.

  let u8 =
    integer identity IntRepr.U8.zero (lift_uint8 u8v_100) IntRepr.U8.eq IntRepr.U8.gt IntRepr.U8.div IntRepr.U8.rem (lift_uint8 u8v_10)
  let su8 = .< fun v_p -> .~(u8 .<v_p>.) >.

  let i16 =
    integer IntRepr.I16.to_u8v IntRepr.I16.zero (lift_int16 i16v_10e4) IntRepr.I16.eq IntRepr.I16.gt IntRepr.I16.div IntRepr.I16.rem (lift_int16 i16v_10)
  let si16 = .< fun v_p -> .~(i16 .<v_p>.) >.

  let u16 =
    integer IntRepr.U16.to_u8v IntRepr.U16.zero (lift_uint16 u16v_10e4) IntRepr.U16.eq IntRepr.U16.gt IntRepr.U16.div IntRepr.U16.rem (lift_uint16 u16v_10)
  let su16 = .< fun v_p -> .~(u16 .<v_p>.) >.

  let i32 =
    integer IntRepr.I32.to_u8v IntRepr.I32.zero (lift_int32 i32v_10e9) IntRepr.I32.eq IntRepr.I32.gt IntRepr.I32.div IntRepr.I32.rem (lift_int32 i32v_10)
  let si32 = .< fun v_p -> .~(i32 .<v_p>.) >.

  let u32 =
    integer IntRepr.U32.to_u8v IntRepr.U32.zero (lift_uint32 u32v_10e9) IntRepr.U32.eq IntRepr.U32.gt IntRepr.U32.div IntRepr.U32.rem (lift_uint32 u32v_10)
  let su32 = .< fun v_p -> .~(u32 .<v_p>.) >.

  let i64 =
    integer IntRepr.I64.to_u8v IntRepr.I64.zero (lift_int64 i64v_10e18) IntRepr.I64.eq IntRepr.I64.gt IntRepr.I64.div IntRepr.I64.rem (lift_int64 i64v_10)
  let si64 = .< fun v_p -> .~(i64 .<v_p>.) >.

  let u64 =
    integer IntRepr.U64.to_u8v IntRepr.U64.zero (lift_uint64 u64v_10e19) IntRepr.U64.eq IntRepr.U64.gt IntRepr.U64.div IntRepr.U64.rem (lift_uint64 u64v_10)
  let su64 = .< fun v_p -> .~(u64 .<v_p>.) >.

  let i128 =
    integer IntRepr.I128.to_u8v IntRepr.I128.zero (lift_int128 i128v_10e38) IntRepr.I128.eq IntRepr.I128.gt IntRepr.I128.div IntRepr.I128.rem (lift_int128 i128v_10)
  let si128 = .< fun v_p -> .~(i128 .<v_p>.) >.

  let u128 =
    integer IntRepr.U128.to_u8v IntRepr.U128.zero (lift_uint128 u128v_10e38) IntRepr.U128.eq IntRepr.U128.gt IntRepr.U128.div IntRepr.U128.rem (lift_uint128 u128v_10)
  let su128 = .< fun v_p -> .~(u128 .<v_p>.) >.

  let tup_opn _typs pc =
    SerData.write_byte pc (IntRepr.U8.to_byte (lift_uint8 opn))

  let tup_cls _typs pc =
    SerData.write_byte pc (IntRepr.U8.to_byte (lift_uint8 cls))

  let tup_sep _typs _n pc =
    SerData.write_byte pc (IntRepr.U8.to_byte (lift_uint8 spc))

  let vec_opn _dim _typ pc =
    SerData.write_byte pc (IntRepr.U8.to_byte (lift_uint8 opn))

  let vec_cls _dim _typ pc =
    SerData.write_byte pc (IntRepr.U8.to_byte (lift_uint8 cls))

  let vec_sep _dim _typ _n pc =
    SerData.write_byte pc (IntRepr.U8.to_byte (lift_uint8 spc))

  (* Regardless of the type structure, nulls are simply encoded as "null" *)
  let snull pc =
    .< let knull = .~(lift_uint32 dword_null) in
       .~(SerData.write_dword pc .<knull>.) >.
  let snotnull pc = pc
end
