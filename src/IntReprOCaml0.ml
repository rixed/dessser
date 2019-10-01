(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open Batteries
open Stdint
open BerSerdes

module Raw (SerData : SerDataBytes0.SERDATA_OCAML0) =
struct
  module SerData = SerData

  (* The values we serialize and read back are normal OCaml values: *)

  type boolv = bool
  type i8v = int
  type i16v = int
  type stringv = string
  type 'a arrayv = 'a array
  type length = int

  let byte_of_i8v n = n
  let i8v_of_byte n = n
  let word_of_i16v n = n
  let i16v_of_word n = n
  let bytes_of_string = Bytes.of_string
  let string_of_bytes = Bytes.to_string

  let choose b c1 c2 =
    if b then c1 else c2

  let loop len u f =
    let rec do_loop i u =
      if i >= len then u else do_loop (i + 1) (f i u)
    in
    do_loop 0 u

  let string_get s i =
    Char.code (String.get s i)

  let boolv_of_const n = n
  let i8v_of_const n = n
  let i8v_add = (+)
  let i8v_sub = (-)
  let i8v_mul = ( * )
  let i8v_mod = (mod)
  let i8v_div = (/)

end

module Make (SerData : SerDataBytes0.SERDATA_OCAML0) :
  INTREPR with module SerData = SerData =
  Raw (SerData)
