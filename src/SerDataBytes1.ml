(* Staged version of SerDataBytes0 *)

type pointer = SerDataBytes0.pointer code
type size = SerDataBytes0.size code
type nop = SerDataBytes0.nop code
type input = SerDataBytes0.input code
let nop = .< SerDataBytes0.nop >.
let and_then c1 c2 =
  .< .~c1 ; .~c2 >.

let skip pc sc =
  .< SerDataBytes0.skip .~pc .~sc >.

let add pc sc =
  .< SerDataBytes0.add .~pc .~sc >.

let sub p1c p2c =
  .< SerDataBytes0.sub .~p1c .~p2c >.

type bit = SerDataBytes0.bit code
type byte = SerDataBytes0.byte code
type word = SerDataBytes0.word code
type dword = SerDataBytes0.dword code
type qword = SerDataBytes0.qword code
type bytes = SerDataBytes0.bytes code

let test_bit pc o =
  .< let p = .~pc in SerDataBytes0.test_bit p o >.

let peek_byte pc =
  .< let p = .~pc in SerDataBytes0.peek_byte p >.

let read_byte pc =
  (* Hopefully the compiler will be smart enough to remove the useless
   * pointer increase: *)
  .< let p = .~pc in SerDataBytes0.read_byte p |> fst >.,
  (* Beware not to read once more! (ie. not "SerDataBytes0.read_byte |> snd"!) *)
  .< let p = .~pc in SerDataBytes0.add p 1 >.

let read_word ?be pc =
  .< let p = .~pc in SerDataBytes0.read_word ?be p |> fst >.,
  .< let p = .~pc in SerDataBytes0.add p 2 >.

let read_dword ?be pc =
  .< let p = .~pc in SerDataBytes0.read_dword ?be p |> fst >.,
  .< let p = .~pc in SerDataBytes0.add p 4 >.

let read_qword ?be pc =
  .< let p = .~pc in SerDataBytes0.read_qword ?be p |> fst >.,
  .< let p = .~pc in SerDataBytes0.add p 8 >.

let read_bytes pc sc =
  .< let p, s = .~pc, .~sc in SerDataBytes0.read_bytes p s |> fst >.,
  .< let p, s = .~pc, .~sc in SerDataBytes0.add p s >.

let set_bit pc o bc =
  .< let p, b = .~pc, .~bc in SerDataBytes0.set_bit p o b >.

let poke_byte pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.poke_byte p v >.

let write_byte pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_byte p v >.

let write_word ?be pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_word ?be p v >.

let write_dword ?be pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_dword ?be p v >.

let write_qword ?be pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_qword ?be p v >.

let write_bytes pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_bytes p v >.

let make_buffer sc =
  .< let s = .~sc in SerDataBytes0.make_buffer s >.

let of_input ic =
  .< let i = .~ic in SerDataBytes0.of_input i >.

let size_of_const n =
  .< SerDataBytes0.size_of_const n >.
