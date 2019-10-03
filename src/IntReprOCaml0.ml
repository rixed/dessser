(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open BerSerdes


module Types =
struct

  module SerData = SerDataBytes0

  type value =
    | VPointer of SerData.pointer
    | VSize of SerData.size
    | VBool of boolv
    | VI8 of i8v
    | VI16 of i16v
    | VVec of vecv
    | VTuple of tupv

  (* The values we serialize and read back are normal OCaml values: *)

  and boolv = bool
  and i8v = int
  and i16v = int
  and vecv = value array
  and tupv = value array
end

include Types
include MakeCasts (Types)

let byte_of_i8v n = n
let i8v_of_byte n = n
let word_of_i16v n = n
let i16v_of_word n = n

let choose b c1 c2 =
  if b then c1 else c2

let vec_length arr = Array.length arr
let tup_length arr = Array.length arr
let vec_get arr i = Array.get arr i
let tup_get arr i = Array.get arr i

let vecv_of_const = Array.of_list
let tupv_of_const = Array.of_list
let boolv_of_const n = n
let boolv_and n m = n && m
let i8v_of_const n = n
let i8v_eq = (=)
let i8v_ge = (>=)
let i8v_add = (+)
let i8v_sub = (-)
let i8v_mul = ( * )
let i8v_mod = (mod)
let i8v_div = (/)
let i16v_of_const n = n
let i16v_gt = (>)

let read_while p cond reducer v0 =
  let rec loop v =
    let b = SerData.peek_byte p in
    if cond b then (
      SerData.skip p 1 ;
      loop (reducer v b)
    ) else v
  in
  loop v0
