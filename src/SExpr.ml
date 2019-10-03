(* This is a (de)serializer for an external format where all values are
 * encoded as human readable s-expressions, and are internally represented
 * as regular OCaml values. *)
open BerSerdes

module Raw (IntRepr : INTREPR) =
struct
  module IntRepr = IntRepr
  module SerData = IntRepr.SerData

  let i8v_ascii_T = IntRepr.i8v_of_const (Char.code 'T')
  let i8v_ascii_F = IntRepr.i8v_of_const (Char.code 'F')
  let i8v_10 = IntRepr.i8v_of_const 10
  let i8v_ascii_0 = IntRepr.i8v_of_const (Char.code '0')
  let i8v_ascii_9 = IntRepr.i8v_of_const (Char.code '9')
  let i8v_opn = IntRepr.i8v_of_const (Char.code '(')
  let i8v_cls = IntRepr.i8v_of_const (Char.code ')')
  let i8v_spc = IntRepr.i8v_of_const (Char.code ' ')
  let sizev_1 = SerData.size_of_const 1

  module Ser =
  struct
    let bool p v =
      (* Note: this returns a nop, not unit. Shouldn't we have a new sequencing
       * operation to compose two nops into one? *)
      let i8v_chr = IntRepr.choose v (IntRepr.VI8 i8v_ascii_T)
                                     (IntRepr.VI8 i8v_ascii_F) |>
                    IntRepr.i8v_of_value in
      let byte = IntRepr.byte_of_i8v i8v_chr in
      SerData.write_byte p byte

    let i8 p v =
      let rec loop seq scale =
        if scale > 0 then
          let digit =
            IntRepr.(i8v_mod (i8v_div v (i8v_of_const scale)) i8v_10) in
          let digit_char = IntRepr.(i8v_add i8v_ascii_0) digit in
          let byte = IntRepr.byte_of_i8v digit_char in
          let seq = SerData.(and_then seq (SerData.write_byte p byte)) in
          loop seq (scale / 10)
        else
          seq
      in
      loop SerData.nop 100

    let i16 _p _v = assert false

    let rec ser p = function
      | IntRepr.VBool v -> bool p v
      | IntRepr.VI8 v -> i8 p v
      | IntRepr.VI16 v -> i16 p v
      | IntRepr.VVec vs -> vec p vs
      | IntRepr.VTuple vs -> tup p vs
      (* Those are not meant to be serialized: *)
      | IntRepr.VPointer _
      | IntRepr.VSize _ ->
          assert false

    and tup p vs =
      let len = IntRepr.tup_length vs in
      let seq = SerData.write_byte p (IntRepr.byte_of_i8v i8v_opn) in
      let rec loop seq i =
        if i < len then
          let seq =
            if i > 0 then
              SerData.(and_then seq
                (write_byte p (IntRepr.byte_of_i8v i8v_spc)))
            else
              seq in
          let v = IntRepr.tup_get vs i in
          let seq = SerData.and_then seq (ser p v) in
          loop seq (i + 1)
        else
          seq
      in
      let seq = loop seq 0 in
      SerData.and_then seq
        (SerData.write_byte p (IntRepr.byte_of_i8v i8v_cls))

    and vec p vs =
      let len = IntRepr.vec_length vs in
      let seq = SerData.write_byte p (IntRepr.byte_of_i8v i8v_opn) in
      let rec loop seq i =
        if i < len then
          let seq =
            if i > 0 then
              SerData.(and_then seq
                (write_byte p (IntRepr.byte_of_i8v i8v_spc)))
            else
              seq in
          let v = IntRepr.vec_get vs i in
          let seq = SerData.(and_then seq (ser p v)) in
          loop seq (i + 1)
        else
          seq
      in
      let seq = loop seq 0 in
      SerData.(and_then seq
        (write_byte p (IntRepr.byte_of_i8v i8v_cls)))
  end

  module Des =
  struct
    let bool p =
      let c = IntRepr.i8v_of_byte (SerData.read_byte p) in
      let is_true = IntRepr.i8v_eq c i8v_ascii_T in
      IntRepr.VBool is_true

    let i8 p =
      (* Assume we can read at least one byte and its a valid digit *)
      let c = IntRepr.i8v_of_byte (SerData.read_byte p) in
      let v0 = IntRepr.i8v_sub c i8v_ascii_0 in

      let cond byte =
        let c = IntRepr.i8v_of_byte byte in
        IntRepr.boolv_and (IntRepr.i8v_ge c i8v_ascii_0)
                          (IntRepr.i8v_ge i8v_ascii_9 c)
      and reduce v byte =
        let c = IntRepr.i8v_of_byte byte in
        let v = IntRepr.i8v_of_value v in
        IntRepr.i8v_add (IntRepr.i8v_mul v i8v_10)
                        (IntRepr.i8v_sub c i8v_ascii_0) |>
        IntRepr.value_of_i8v in
      IntRepr.read_while p cond reduce (IntRepr.value_of_i8v v0)

    let rec des typ p =
      match typ with
      | TBool -> bool p
      | TI8   -> i8 p
      | TI16  -> assert false
      | TVec (dim, typ) ->
          vec dim typ p
      | TTuple typs ->
          tup typs p
      (* Those are not supposed to be serialized: *)
      | TPointer
      | TSize ->
          assert false

    and vec dim typ p =
      let rec loop vs i =
        if i >= dim then
          IntRepr.(value_of_vecv (vecv_of_const (List.rev vs)))
        else
          let v = des typ p in
          loop (v :: vs) (i + 1)
      in
      loop [] 0

    and tup typs p =
      let rec loop vs i =
        if i >= Array.length typs then
          IntRepr.(value_of_tupv (tupv_of_const (List.rev vs)))
        else
          let v = des typs.(i) p in
          loop (v :: vs) (i + 1)
      in
      loop [] 0

  end

  let ser p v =
    let seq = Ser.ser p v in
    SerData.(and_then seq (print p))

  let des = Des.des
end

module Make (IntRepr : INTREPR) : SERDES with module IntRepr = IntRepr =
  Raw (IntRepr)
