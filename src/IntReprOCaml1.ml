(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open BerSerdes

module Types =
struct
  module SerData = SerDataBytes1

  type value =
    | VPointer of SerData.pointer
    | VSize of SerData.size
    | VBool of boolv
    | VI8 of i8v
    | VI16 of i16v
    | VVec of vecv
    | VTuple of tupv

  and boolv = bool code
  and i8v = int code
  and i16v = int code
  and vecv = value array
  and tupv = value array

  type value_pointer =
    | VBoolP of boolvp
    | VI8P of i8vp
  and boolvp = IntReprOCaml0.boolvp code
  and i8vp = IntReprOCaml0.i8vp code
end

include Types

include MakeCasts (Types)

let byte_of_i8v n = n
let i8v_of_byte n = n
let word_of_i16v n = n
let i16v_of_word n = n

(* Could GADT help with choose prototype while preserving tuples? *)
let choose b c1 c2 =
  let aux c1 c2 =
    .< if .~b then .~c1 else .~c2 >. in
  match c1, c2 with
  | VPointer c1, VPointer c2 -> VPointer (aux c1 c2)
  | VSize c1, VSize c2 -> VSize (aux c1 c2)
  | VI8 c1, VI8 c2 -> VI8 (aux c1 c2)
  | _ -> assert false

let vec_length = Array.length
let tup_length = Array.length

let vec_get = Array.get

let tup_get = Array.get

let vecv_of_const = Array.of_list

let tupv_of_const = Array.of_list

let boolv_of_const n = .< n >.
let boolv_and n m = .< .~n && .~m >.
let i8v_of_const i = .< i >.
let i8v_eq n m = .< .~n = .~m >.
let i8v_ge n m = .< .~n >= .~m >.
let i8v_add n m = .< .~n + .~m >.
let i8v_sub n m = .< .~n - .~m >.
let i8v_mul n m = .< .~n * .~m >.
let i8v_mod n m = .< .~n mod .~m >.
let i8v_div n m = .< .~n / .~m >.
let i16v_of_const i = .< i >.
let i16v_gt n m = .< .~n > .~m >.

let make_value_pointer v p =
  match v with
  | VBool v -> VBoolP .< .~v, .~p >.
  | VI8 v -> VI8P .< .~v, .~p >.
  | _ -> assert false


let read_while cond reducer v0 =
  let rec aux of_value to_value c0_p =
    .<
      let rec loop c p =
        let b = .~(SerDataBytes1.peek_byte .<p>.) in
        if .~(cond .<b>.) then (
          let p = .~(SerDataBytes1.add .<p>. .<1>.) in
          loop .~(of_value (reducer (to_value .<c>.) .<b>.)) p
        ) else c, p
      in
      let c0, p = .~c0_p in
      loop c0 p
    >. in
  match v0 with
  | VBoolP c_p -> VBoolP (aux boolv_of_value value_of_boolv c_p)
  | VI8P c_p -> VI8P (aux i8v_of_value value_of_i8v c_p)
  (*| _ -> assert false*)
