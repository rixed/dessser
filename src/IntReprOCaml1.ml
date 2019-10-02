(* Immediate representation of possible values, using a straightforward
 * algebraic data type. *)
open BerSerdes

module MakeTypes (SerData : SERDATA) =
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

  and boolv = bool code
  and i8v = int code
  and i16v = int code
  and length = int code
  and vecv = value array code
  and tuplev = value array code
end

module Raw (SerData0 : SerDataBytes0.SERDATA_OCAML0) =
struct
  module T = MakeTypes (SerDataStaged (SerData0))
  include T
  include MakeCasts (T)

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
    | VLength c1, VLength c2 -> VLength (aux c1 c2)
    | _ -> assert false

  (* Again... *)
  let loop len u f =
    let aux : type a. (a code -> value) -> (value -> a code) -> a code -> a code =
      fun to_value of_value c ->
        .<
          let rec do_loop i u =
            if i >= .~len then
              u
            else
              let u = .~(of_value (f .<i>. (to_value .<u>.))) in
              do_loop (i + 1) u
          in
          do_loop 0 .~c
        >. in
    match u with
    | VPointer c -> VPointer (aux value_of_pointer pointer_of_value c)
    | VSize c -> VSize (aux value_of_size size_of_value c)
    | VLength c -> VLength (aux value_of_length length_of_value c)
    | _ -> assert false

  let vec_get _a _i = assert false

  let tuple_get a i = assert false

  let vecv_of_const lst =
    .< Array.of_list lst >.

  let tuplev_of_const lst =
    .< Array.of_list lst >.

  let lengthv_of_const n = .< n >.
  let boolv_of_const n = .< n >.
  let i8v_of_const i = .< i >.
  let i8v_add n m = .< .~n + .~m >.
  let i8v_sub n m = .< .~n - .~m >.
  let i8v_mul n m = .< .~n * .~m >.
  let i8v_mod n m = .< .~n mod .~m >.
  let i8v_div n m = .< .~n / .~m >.
  let i16v_of_const i = .< i >.
  let i16v_gt n m = .< .~n > .~m >.
end

module Make (SerData : SerDataBytes0.SERDATA_OCAML0) :
  INTREPR with module SerData = SerDataStaged (SerData) = Raw (SerData)
