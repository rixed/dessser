(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open BerSerdes

module MakeTypes (SerData : SerDataBytes0.SERDATA_OCAML0) =
struct
  module SerData = SerData

  type value =
    | VPointer of SerData.pointer
    | VSize of SerData.size
    | VLength of length
    | VBool of boolv
    | VI8 of i8v
    | VI16 of i16v
    | VVec of length * vecv
    | VTuple of length * tuplev

  (* The values we serialize and read back are normal OCaml values: *)

  and length = int
  and boolv = bool
  and i8v = int
  and i16v = int
  and vecv = value array
  and tuplev = value array
end

module Raw (SerData : SerDataBytes0.SERDATA_OCAML0) =
struct
  module T = MakeTypes (SerData)
  include T
  include MakeCasts (T)

  let byte_of_i8v n = n
  let i8v_of_byte n = n
  let word_of_i16v n = n
  let i16v_of_word n = n

  let choose b c1 c2 =
    if b then c1 else c2

  let loop len u f =
    let rec do_loop i u =
      if i >= len then u else do_loop (i + 1) (f i u)
    in
    do_loop 0 u

  let vec_get = Array.get
  let tuple_get = Array.get

  let lengthv_of_const n = n
  let vecv_of_const = Array.of_list
  let tuplev_of_const = Array.of_list
  let boolv_of_const n = n
  let i8v_of_const n = n
  let i8v_add = (+)
  let i8v_sub = (-)
  let i8v_mul = ( * )
  let i8v_mod = (mod)
  let i8v_div = (/)
  let i16v_of_const n = n
  let i16v_gt (n : int)  (m : int) = n > m
end

module Make (SerData : SerDataBytes0.SERDATA_OCAML0) :
  INTREPR with module SerData = SerData = Raw (SerData)
