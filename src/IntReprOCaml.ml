(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open Dessert

module SerData = SerDataBytes

type floatv = float and stringv = string and boolv = bool
and i8v = int and i16v = int and i32v = int32

let length_of_stringv n = .< String.length .~n >.
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
  .< let a, b = .~a_b in .~(f .<a>.), b >.

let map_pair ~fst ~snd a_b =
  .<
    let a, b = .~a_b in
    .~(fst .<a>.), .~(snd .<b>.)
  >.

let dword_eq (n : SerData.dword code) (m : SerData.dword code) = .< .~n = .~m >.
let size_ge (n : SerData.size code) (m : SerData.size code) = .< .~n >= .~m >.
let boolv_of_const n = n
let boolv_of_i8v n = .< .~n <> 0 >.
let i8v_of_boolv n = .< if .~n then 1 else 0 >.
let boolv_and n m = .< .~n && .~m >.
let boolv_or n m = .< .~n || .~m >.
let i8v_of_const i = i
let i8v_eq n m = .< .~n = .~m >.
let i8v_ne n m = .< .~n <> .~m >.
let i8v_ge n m = .< .~n >= .~m >.
let i8v_gt n m = .< .~n > .~m >.
let i8v_add n m = .< .~n + .~m >.
let i8v_sub n m = .< .~n - .~m >.
let i8v_mul n m = .< .~n * .~m >.
let i8v_mod n m = .< .~n mod .~m >.
let i8v_div n m = .< .~n / .~m >.
let i8v_lsl n m = .< .~n lsl .~m >.
let i8v_lsr n m = .< .~n lsr .~m >.
let i16v_of_const i = i
let i16v_eq n m = .< .~n = .~m >.
let i16v_ne n m = .< .~n <> .~m >.
let i16v_ge n m = .< .~n >= .~m >.
let i16v_gt n m = .< .~n > .~m >.
let i16v_add n m = .< .~n + .~m >.
let i16v_sub n m = .< .~n - .~m >.
let i16v_mul n m = .< .~n * .~m >.
let i16v_mod n m = .< .~n mod .~m >.
let i16v_div n m = .< .~n / .~m >.
let i16v_of_i8v n = n
let i16v_lsl n m = .< .~n lsl .~m >.
let i16v_lsr n m = .< .~n lsr .~m >.
let i8v_of_i16v n = n

let byte_of_i32v n = .< Int32.to_int .~n >.
let i32v_of_byte n = .< Int32.of_int .~n >.
let i8v_of_i32v n = .< Int32.to_int .~n >.
let i32v_of_i8v n = .< Int32.of_int .~n >.
let size_of_i32v n = .< Int32.to_int .~n >.
let i32v_of_size n = .< Int32.of_int .~n >.
let i32v_of_const n = Int32.of_int n
let i32v_eq n m = .< .~n = .~m >.
let i32v_ne n m = .< .~n <> .~m >.
let i32v_ge n m = .< .~n >= .~m >.
let i32v_gt n m = .< .~n > .~m >.
let i32v_add n m = .< Int32.add .~n .~m >.
let i32v_sub n m = .< Int32.sub .~n .~m >.
let i32v_mul n m = .< Int32.mul .~n .~m >.
let i32v_div n m = .< Int32.div .~n .~m >.
let i32v_mod n m = .< Int32.rem .~n .~m >.
let i32v_lsl n m = .< Int32.shift_left .~n .~m >.
let i32v_lsr n m = .< Int32.shift_right_logical .~n .~m >.

let qword_of_floatv : floatv code -> SerData.qword code = fun _n -> .< todo "qword_of_float" >.
let floatv_of_qword : SerData.qword code -> floatv code = fun _n -> .< todo "float_of_qword" >.

let stringv_of_bytes n = .< Bytes.to_string .~n >.
let bytes_of_stringv n = .< Bytes.of_string .~n >.
(* Shortcuts: *)
let floatv_of_bytes n =
  .<
    let s = Bytes.to_string .~n in
    try float_of_string s
    with e ->
      Format.eprintf "Cannot convert %S into a float@." s ;
      raise e
  >.

let bytes_of_floatv n = .< Bytes.of_string (string_of_float.~n) >.

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
