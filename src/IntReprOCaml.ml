(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open Dessert

module SerData = SerDataBytes

type boolv = bool and i8v = int and i16v = int

let byte_of_i8v n = n
let i8v_of_byte n = n
let word_of_i16v n = n
let i16v_of_word n = n

(* Could GADT help with choose prototype while preserving tuples? *)
let choose b c1 c2 =
  .< if .~b then .~c1 else .~c2 >.

let fst a_b =
  .< let a, _ = .~a_b in a >.

let snd a_b =
  .< let _, b = .~a_b in b >.

let map_fst f a_b =
  .< let a, b = .~a_b in .~f a, b >.

let map_pair ~fst ~snd a_b =
  .<
    let a, b = .~a_b in
    .~(fst .<a>.), .~(snd .<b>.)
  >.

let boolv_of_const (n : bool) = .< n >.
let boolv_and n m = .< .~n && .~m >.
let i8v_of_const (i : int) = .< i >.
let i8v_eq n m = .< .~n = .~m >.
let i8v_ne n m = .< .~n <> .~m >.
let i8v_ge n m = .< .~n >= .~m >.
let i8v_gt n m = .< .~n > .~m >.
let i8v_add n m = .< .~n + .~m >.
let i8v_sub n m = .< .~n - .~m >.
let i8v_mul n m = .< .~n * .~m >.
let i8v_mod n m = .< .~n mod .~m >.
let i8v_div n m = .< .~n / .~m >.
let i16v_of_const (i : int) = .< i >.
let i16v_gt n m = .< .~n > .~m >.

let read_while ~cond ~reduce v_p =
  .<
    let rec loop v p =
      let b = .~(SerDataBytes.peek_byte .<p>.) in
      if .~cond b then (
        let p = .~(SerDataBytes.add .<p>. .<1>.) in
        loop (.~reduce v b) p
      ) else v, p
    in
    let v, p = .~v_p in
    loop v p
  >.

let do_while ~cond ~loop ic vc =
  .<
    let rec loop_ v i =
      if .~cond v i then (
        let v, i = .~loop v i in
        loop_ v i
      ) else
        v
    in
    loop_ .~vc .~ic
  >.
