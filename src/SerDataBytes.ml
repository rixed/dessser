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

let int_of_byte = SerDataBytes0.int_of_byte
let byte_of_int = SerDataBytes0.byte_of_int

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

(* Very efficient as you can see: *)

let make_bytes = .< Bytes.empty >.

let bytes_append bs b =
  .<
    let old_bs = .~bs in
    let len = Bytes.length old_bs in
    let new_bs = Bytes.create (len + 1) in
    Bytes.blit old_bs 0 new_bs 0 len ;
    Bytes.set new_bs len (Char.chr .~b) ;
(*    Format.printf "old bytes was %S, new bytes is %S@."
      (Bytes.to_string old_bs) (Bytes.to_string new_bs) ;*)
    new_bs
  >.
