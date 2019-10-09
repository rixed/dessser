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
      let rec loop p scale =
        if scale > 0 then
          let digit =
            IntRepr.(i8v_mod (i8v_div v (i8v_of_const scale)) i8v_10) in
          let digit_char = IntRepr.(i8v_add i8v_ascii_0) digit in
          let byte = IntRepr.byte_of_i8v digit_char in
          let p = SerData.write_byte p byte in
          loop p (scale / 10)
        else
          p
      in
      loop p 100

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
      let len = Array.length vs in
      let p = SerData.write_byte p (IntRepr.byte_of_i8v i8v_opn) in
      let rec loop p i =
        if i < len then
          let p =
            if i > 0 then
              SerData.write_byte p (IntRepr.byte_of_i8v i8v_spc)
            else
              p in
          let v = vs.(i) in
          let p = ser p v in
          loop p (i + 1)
        else
          p
      in
      let p = loop p 0 in
      SerData.write_byte p (IntRepr.byte_of_i8v i8v_cls)

    and vec p vs =
      let len = Array.length vs in
      let p = SerData.write_byte p (IntRepr.byte_of_i8v i8v_opn) in
      let rec loop p i =
        if i < len then
          let p =
            if i > 0 then
              SerData.write_byte p (IntRepr.byte_of_i8v i8v_spc)
            else
              p in
          let v = vs.(i) in
          let p = ser p v in
          loop p (i + 1)
        else
          p
      in
      let p = loop p 0 in
      SerData.write_byte p (IntRepr.byte_of_i8v i8v_cls)
  end

  module Des =
  struct
    let bool p =
      let v_p = SerData.read_byte p in
      let c = IntRepr.(i8v_of_byte (fst v_p)) in
      let is_true = IntRepr.i8v_eq c i8v_ascii_T in
      IntRepr.VBool is_true, p

    let i8 p =
      (* Assume we can read at least one byte and its a valid digit *)
      let v, p = SerData.read_byte p in
      let c = IntRepr.i8v_of_byte v in
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
      IntRepr.read_while ~cond ~reduce p (IntRepr.value_of_i8v v0)

    let rec des typ p =
      (* Swallow any blank before the actual value: *)
(*      let p = IntRepr.read_while p cond 
      let rec skip_blanks p =
        let v = SerData.peek p in
        if is_blank p then
          skip_blanks (SerData.add p 1)
        else
          p
      in
      let p = skip_blanks p in*)
      match typ with
      | TBool -> bool p
      | TI8 -> i8 p
      | TI16 -> assert false
      | TVec (dim, typ) ->
          vec dim typ p
      | TTuple typs ->
          tup typs p
      (* Those are not supposed to be serialized: *)
      | TPointer
      | TSize ->
          assert false

    and vec dim typ p =
      let rec loop vs p i =
        if i >= dim then
          IntRepr.(value_of_vecv (vecv_of_const (List.rev vs))),
          p
        else
          let v, p = des typ p in
          loop (v :: vs) p (i + 1)
      in
      loop [] p 0

    and tup typs p =
      let rec loop vs p i =
        if i >= Array.length typs then
          IntRepr.(value_of_tupv (tupv_of_const (List.rev vs))),
          p
        else
          let v, p = des typs.(i) p in
          loop (v :: vs) p (i + 1)
      in
      loop [] p 0

  end

  let ser = Ser.ser

  let des = Des.des
end

module Make (IntRepr : INTREPR) : SERDES with module IntRepr = IntRepr =
  Raw (IntRepr)
