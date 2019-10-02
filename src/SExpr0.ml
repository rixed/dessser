(* This is a (de)serializer for an external format where all values are
 * encoded as human readable s-expressions, and are internally represented
 * as regular OCaml values. *)
open BerSerdes

module Raw (IntRepr : INTREPR) =
struct
  module IntRepr = IntRepr
  module SerData = IntRepr.SerData

  let bool p v =
    (* Note: this returns a nop, not unit. Shouldn't we have a new sequencing
     * operation to compose two nops into one? *)
    let i8v_ascii_T = IntRepr.(VI8 (i8v_of_const (Char.code 'T')))
    and i8v_ascii_F = IntRepr.(VI8 (i8v_of_const (Char.code 'F'))) in
    let i8v_chr = IntRepr.choose v i8v_ascii_T i8v_ascii_F |>
                  IntRepr.i8v_of_value in
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

  let rec ser p = function
    | IntRepr.VBool v -> bool p v
    | IntRepr.VI8 v -> i8 p v
    | IntRepr.VI16 v -> i16 p v
    | IntRepr.VVec (dim, vs) -> vec p dim vs
    | IntRepr.VTuple (dim, vs) -> tuple p dim vs
    (* Those are not meant to be serialized: *)
    | IntRepr.VPointer _
    | IntRepr.VSize _
    | IntRepr.VLength _ ->
        assert false

  and tuple p dim vs =
    let i8v_opn = IntRepr.i8v_of_const (Char.code '(')
    and i8v_cls = IntRepr.i8v_of_const (Char.code ')')
    and i8v_spc = IntRepr.i8v_of_const (Char.code ' ')
    and i16v_zero = IntRepr.i16v_of_const 0 in
    let p = SerData.write_byte p (IntRepr.byte_of_i8v i8v_opn) in
    let pv =
      IntRepr.loop dim (IntRepr.value_of_pointer p) (fun i pv ->
        let not_first = IntRepr.i16v_gt i i16v_zero in
        let pv =
          IntRepr.choose not_first
            (IntRepr.value_of_pointer (SerData.write_byte (IntRepr.pointer_of_value pv) (IntRepr.byte_of_i8v i8v_spc)))
            pv in
        let v = IntRepr.tuple_get vs i in
        IntRepr.value_of_pointer (ser (IntRepr.pointer_of_value pv) v)) in
    SerData.write_byte (IntRepr.pointer_of_value pv) (IntRepr.byte_of_i8v i8v_cls)

  and vec p dim vs =
    let i8v_opn = IntRepr.i8v_of_const (Char.code '(')
    and i8v_cls = IntRepr.i8v_of_const (Char.code ')')
    and i8v_spc = IntRepr.i8v_of_const (Char.code ' ')
    and i16v_zero = IntRepr.i16v_of_const 0 in
    let p = SerData.write_byte p (IntRepr.byte_of_i8v i8v_opn) in
    let pv =
      IntRepr.loop dim (IntRepr.value_of_pointer p) (fun i pv ->
        let not_first = IntRepr.i16v_gt i i16v_zero in
        let pv =
          IntRepr.choose not_first
            (IntRepr.value_of_pointer (SerData.write_byte (IntRepr.pointer_of_value pv) (IntRepr.byte_of_i8v i8v_spc)))
            pv in
        let v = IntRepr.vec_get vs i in
        IntRepr.value_of_pointer (ser (IntRepr.pointer_of_value pv) v)) in
    SerData.write_byte (IntRepr.pointer_of_value pv) (IntRepr.byte_of_i8v i8v_cls)

end

module Make (IntRepr : INTREPR) : SERDES with module IntRepr = IntRepr =
  Raw (IntRepr)
