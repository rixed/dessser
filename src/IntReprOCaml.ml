(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open Dessert

module SerData = SerDataBytes

type floatv = float and stringv = string and boolv = bool
and i8v = int and i16v = int and i32v = int32 and i64v = int64

let length_of_stringv n = .< String.length .~n >.

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
let boolv_and n m = .< .~n && .~m >.
let boolv_or n m = .< .~n || .~m >.

type i8v_ = i8v
module I8 =
struct
  type t = i8v and boolv = bool and i8v = i8v_
  let of_const i = i
  let eq n m = .< .~n = .~m >.
  let ne n m = .< .~n <> .~m >.
  let ge n m = .< .~n >= .~m >.
  let gt n m = .< .~n > .~m >.
  let add n m = .< .~n + .~m >.
  let sub n m = .< .~n - .~m >.
  let mul n m = .< .~n * .~m >.
  let modulo n m = .< .~n mod .~m >.
  let div n m = .< .~n / .~m >.
  let shift_left n m = .< .~n lsl .~m >.
  let shift_right n m = .< .~n asr .~m >.
  let to_byte n = n
  let of_byte n = n
  let to_boolv n = .< .~n <> 0 >.
  let of_boolv n = .< if .~n then 1 else 0 >.
end

module I16 =
struct
  type t = i16v and boolv = bool and i8v = i8v_
  let of_const i = i
  let eq n m = .< .~n = .~m >.
  let ne n m = .< .~n <> .~m >.
  let ge n m = .< .~n >= .~m >.
  let gt n m = .< .~n > .~m >.
  let add n m = .< .~n + .~m >.
  let sub n m = .< .~n - .~m >.
  let mul n m = .< .~n * .~m >.
  let modulo n m = .< .~n mod .~m >.
  let div n m = .< .~n / .~m >.
  let of_i8v n = n
  let shift_left n m = .< .~n lsl .~m >.
  let shift_right n m = .< .~n asr .~m >.
  let to_word n = n
  let of_word n = n
  let to_i8v n = .< .~n land 0xff >.
  let of_i8v n = n
end

module I32 =
struct
  type t = i32v and boolv = bool and i8v = i8v_
  let of_const n = n
  let eq n m = .< .~n = .~m >.
  let ne n m = .< .~n <> .~m >.
  let ge n m = .< .~n >= .~m >.
  let gt n m = .< .~n > .~m >.
  let add n m = .< Int32.add .~n .~m >.
  let sub n m = .< Int32.sub .~n .~m >.
  let mul n m = .< Int32.mul .~n .~m >.
  let div n m = .< Int32.div .~n .~m >.
  let modulo n m = .< Int32.(abs (rem .~n .~m)) >.
  let shift_left n m = .< Int32.shift_left .~n .~m >.
  let shift_right n m = .< Int32.shift_right .~n .~m >.
  let to_i8v n = .< (Int32.to_int .~n) land 0xff >.
  let of_i8v n = .< Int32.of_int .~n >.
  let to_byte n = .< (Int32.to_int .~n) land 0xff >.
  let of_byte n = .< Int32.of_int .~n >.
  let to_dword n = n
  let of_dword n = n
  let to_size n = .< Int32.to_int .~n >.
  let of_size n = .< Int32.of_int .~n >.
end

module I64 =
struct
  type t = i64v and boolv = bool and i8v = i8v_
  let of_const n = n
  let eq n m = .< .~n = .~m >.
  let ne n m = .< .~n <> .~m >.
  let ge n m = .< .~n >= .~m >.
  let gt n m = .< .~n > .~m >.
  let add n m = .< Int64.add .~n .~m >.
  let sub n m = .< Int64.sub .~n .~m >.
  let mul n m = .< Int64.mul .~n .~m >.
  let div n m = .< Int64.div .~n .~m >.
  let modulo n m = .< Int64.(abs (rem .~n .~m)) >.
  let to_i8v n = .< (Int64.to_int .~n) land 0xff >.
  let of_i8v n = .< Int64.of_int .~n >.
  let shift_left n m = .< Int64.shift_left .~n .~m >.
  let shift_right n m = .< Int64.shift_right .~n .~m >.
  let to_qword n = n
  let of_qword n = n
end

let stringv_of_bytes n = .< Bytes.to_string .~n >.
let bytes_of_stringv n = .< Bytes.of_string .~n >.

module Float =
struct
  (* Shortcuts: *)
  let of_bytes n =
    .<
      let s = Bytes.to_string .~n in
      try float_of_string s
      with e ->
        Format.eprintf "Cannot convert %S into a float@." s ;
        raise e
    >.

  let to_bytes n = .< Bytes.of_string (string_of_float.~n) >.

  let to_qword : floatv code -> SerData.qword code = fun _n -> .< todo "qword_of_float" >.
  let of_qword : SerData.qword code -> floatv code = fun _n -> .< todo "float_of_qword" >.
end

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
