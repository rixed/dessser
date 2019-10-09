(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open BerSerdes

module Types =
struct
  module SerData = SerDataBytes

  type value =
    | VPointer of SerData.pointer code
    | VSize of SerData.size code
    | VBool of boolv code
    | VI8 of i8v code
    | VI16 of i16v code
    | VVec of value array
    | VTuple of value array

  and boolv = bool
  and i8v = int
  and i16v = int
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

let read_while ~cond ~reducer p v0 =
  .<
    let rec loop p v =
      let b = .~(SerDataBytes.peek_byte .<p>.) in
      if .~(cond .<b>.) then (
        let p = .~(SerDataBytes.add .<p>. .<1>.) in
        loop p (.~reducer v b)
      ) else v, p
    in
    loop .~p .~v0
  >.
