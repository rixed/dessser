(* Staged version of SerDataBytes0 *)

type pointer = SerDataBytes0.pointer
type size = SerDataBytes0.size

let add pc sc =
  .< SerDataBytes0.add .~pc .~sc >.

let sub p1c p2c =
  .< SerDataBytes0.sub .~p1c .~p2c >.

type bit = SerDataBytes0.bit
type byte = SerDataBytes0.byte
type word = SerDataBytes0.word
type dword = SerDataBytes0.dword
type qword = SerDataBytes0.qword
type bytes = SerDataBytes0.bytes

let test_bit pc oc =
  .< let p, o = .~pc, .~oc in SerDataBytes0.test_bit p o >.

let peek_byte pc =
  .< let p = .~pc in SerDataBytes0.peek_byte p >.

let read_byte pc =
  .< let p = .~pc in SerDataBytes0.read_byte p >.

let read_word ?be pc =
  .< let p = .~pc in SerDataBytes0.read_word ?be p >.

let read_dword ?be pc =
  .< let p = .~pc in SerDataBytes0.read_dword ?be p >.

let read_qword ?be pc =
  .< let p = .~pc in SerDataBytes0.read_qword ?be p >.

let read_bytes pc sc =
  .< let p, s = .~pc, .~sc in SerDataBytes0.read_bytes p s >.

let set_bit pc oc bc =
  .< let p, o, b = .~pc, .~oc, .~bc in SerDataBytes0.set_bit p o b >.

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

let of_string ic =
  .< let i = .~ic in SerDataBytes0.of_string i >.

let size_of_const n =
  .< SerDataBytes0.size_of_const n >.
