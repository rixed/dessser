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
  type length = int
  type 'a arr = 'a array

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

  let arr_get = Array.get
  let arr_len = Array.length

  let arr_of_const = Array.of_list
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
  INTREPR with module SerData = SerData =
  MakeIntRepr (Raw (SerData))
