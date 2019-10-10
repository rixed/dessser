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
  let i8v_0 = IntRepr.i8v_of_const 0
  let i8v_10 = IntRepr.i8v_of_const 10
  let i8v_100 = IntRepr.i8v_of_const 100
  let i8v_ascii_0 = IntRepr.i8v_of_const (Char.code '0')
  let i8v_ascii_9 = IntRepr.i8v_of_const (Char.code '9')
  let i8v_opn = IntRepr.i8v_of_const (Char.code '(')
  let i8v_cls = IntRepr.i8v_of_const (Char.code ')')
  let i8v_spc = IntRepr.i8v_of_const (Char.code ' ')
  let sizev_1 = SerData.size_of_const 1

  module Ser =
  struct
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

    let rec ser = function
      | IntRepr.VPBool v_p -> bool v_p
      | IntRepr.VPI8 v_p -> i8 v_p
      | _ -> assert false
(*      | IntRepr.VPI16 v -> i16 p v
      | IntRepr.VPVec vs -> vec p vs
      | IntRepr.VPTup vs -> tup p vs*)
(*
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
*)
  end

  module Des =
  struct
    let map_pair ~fst ~snd a_b =
      .<
        let a, b = .~a_b in
        .~(fst .<a>.), .~(snd .<b>.)
      >.

    let bool pc =
      SerData.read_byte pc |>
      map_pair
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

    let rec des typ pc =
      (* Swallow any blank before the actual value: *)
      let pc = skip_blanks pc in
      match typ with
      | TBool -> IntRepr.VPBool (bool pc)
      | TI8 -> IntRepr.VPI8 (i8 pc)
      | TI16 -> assert false
      | TVec (dim, typ) -> vec dim typ pc
      | TTup typs -> IntRepr.VPTup (tup typs pc)
      (* Those are not supposed to be serialized: *)
      | TPointer
      | TSize ->
          assert false

    and vec _dim _typ _p =
      assert false (*
      let rec loop vs p i =
        if i >= dim then
          IntRepr.(value_of_vecv (vecv_of_const (List.rev vs))),
          p
        else
          let v, p = des typ p in
          loop (v :: vs) p (i + 1)
      in
      loop [] p 0*)

    (* Expected returned value:
     * [| VPBool .< code to deser the first bool and the end pointer >. ;
     *    VPI8   .< code to deser the following i8 and the end pointer >. ;
     *    ... |].
     * As expected there is no way to get the end pointer without executing
     * all that pieces of code one by one.
     * The first deser function also has to read and skip the opening
     * parenthesis and the first one has to parse the closing one. *)
    and tup typs pc =
      let desers =
        Array.map (fun typ ->

      let pc = until_char '(' in
      let vs_p =
        Array.fold_left (fun vs_p typ ->
          .<
            let vs, p = .~vs_p in
            let v, p = .~(des typ .<p>.) in
            v :: vs, p
          >.
        ) .< [], .~pc >. typs in
      .<
        let vs, p = .~vs_p in
        IntRepr.tupv_of_list Array.of_list (List.rev vs), p
      >.,


  end

  let ser = Ser.ser

  let des = Des.des
end

module Make (IntRepr : INTREPR) : SERDES with module IntRepr = IntRepr =
  Raw (IntRepr)
