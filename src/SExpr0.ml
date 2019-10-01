(* This is a (de)serializer for an external format where all values are
 * encoded as human readable s-expressions, and are internally represented
 * as regular OCaml values. *)
open BerSerdes

module Raw (IntRepr : INTREPR) =
struct
  module IntRepr = IntRepr
  module SerData = IntRepr.SerData

  module Ser =
  struct
    type 'a ser = SerData.pointer -> 'a -> SerData.pointer

    let bool p v =
      (* Note: this returns a nop, not unit. Shouldn't we have a new sequencing
       * operation to compose two nops into one? *)
      let i8v_ascii_T = IntRepr.i8v_of_const (Char.code 'T')
      and i8v_ascii_F = IntRepr.i8v_of_const (Char.code 'F') in
      let i8v_chr = IntRepr.choose v i8v_ascii_T i8v_ascii_F in
      let byte = IntRepr.byte_of_i8v i8v_chr in
      SerData.write_byte p byte

    let i8 p v =
      let i8v_10 = IntRepr.i8v_of_const 10
      and i8v_ascii0 = IntRepr.i8v_of_const (Char.code '0') in
      let rec write p scale =
        if scale = 0 then p else
          let digit =
            IntRepr.(i8v_mod (i8v_div v (i8v_of_const scale)) i8v_10) in
          let digit_char = IntRepr.(i8v_add i8v_ascii0) digit in
          let byte = IntRepr.byte_of_i8v digit_char in
          write (SerData.write_byte p byte) (scale / 10)
      in
      write p 100

    let i16 _p _v = assert false

    let string len p v =
      let i8v_quote = IntRepr.i8v_of_const (Char.code '"') in
      let p = SerData.write_byte p (IntRepr.byte_of_i8v i8v_quote) in
      let p =
        IntRepr.loop len p (fun i p ->
          let byte = IntRepr.(byte_of_i8v (string_get v i)) in
          SerData.write_byte p byte) in
      SerData.write_byte p (IntRepr.byte_of_i8v i8v_quote)

    let array _len _p _v = assert false
  end
end

module Make (IntRepr : INTREPR) : SERDES with module IntRepr = IntRepr =
  Raw (IntRepr)
