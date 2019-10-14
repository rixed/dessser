(* Staged version of SerDataBytes0 *)
open Stdint
open MyLifts

type pointer = SerDataBytes0.pointer
type size = SerDataBytes0.size

let print_pointer = .< SerDataBytes0.print_pointer >.
let print_data = .< SerDataBytes0.print_data >.

let add pc sc =
  .< SerDataBytes0.add .~pc .~sc >.

let sub p1c p2c =
  .< SerDataBytes0.sub .~p1c .~p2c >.

let rem pc =
  .< SerDataBytes0.rem .~pc >.

type bit = SerDataBytes0.bit
type byte = SerDataBytes0.byte
type word = SerDataBytes0.word
type dword = SerDataBytes0.dword
type qword = SerDataBytes0.qword
type oword = SerDataBytes0.oword
type bytes = SerDataBytes0.bytes

let int_of_byte = SerDataBytes0.int_of_byte
let byte_of_int = SerDataBytes0.byte_of_int

let test_bit pc oc =
  .< let p, o = .~pc, .~oc in SerDataBytes0.test_bit p o >.

let peek_byte ?at pc =
  .< let p, at = .~pc, .~(lift_option Lifts.Lift_int.lift at) in
     SerDataBytes0.peek_byte ?at p >.

let peek_word ?be ?at pc =
  .< let p, be, at = .~pc, .~(lift_option lift_bool be), .~(lift_option Lifts.Lift_int.lift at) in
     SerDataBytes0.peek_word ?be ?at p >.

let peek_dword ?be ?at pc =
  .< let p, be, at = .~pc, .~(lift_option lift_bool be), .~(lift_option Lifts.Lift_int.lift at) in
 SerDataBytes0.peek_dword ?be ?at p >.

let peek_qword ?be ?at pc =
  .< let p, be, at = .~pc, .~(lift_option lift_bool be), .~(lift_option Lifts.Lift_int.lift at) in
 SerDataBytes0.peek_qword ?be ?at p >.

let peek_oword ?be ?at pc =
  .< let p, be, at = .~pc, .~(lift_option lift_bool be), .~(lift_option Lifts.Lift_int.lift at) in
 SerDataBytes0.peek_oword ?be ?at p >.

let read_byte pc =
  .< let p = .~pc in SerDataBytes0.read_byte p >.

let read_word ?be pc =
  .< let p, be = .~pc, .~(lift_option lift_bool be) in
 SerDataBytes0.read_word ?be p >.

let read_dword ?be pc =
  .< let p, be = .~pc, .~(lift_option lift_bool be) in
 SerDataBytes0.read_dword ?be p >.

let read_qword ?be pc =
  .< let p, be = .~pc, .~(lift_option lift_bool be) in
 SerDataBytes0.read_qword ?be p >.

let read_oword ?be pc =
  .< let p, be = .~pc, .~(lift_option lift_bool be) in
 SerDataBytes0.read_oword ?be p >.

let read_bytes v_p =
  .< let v_p = .~v_p in SerDataBytes0.read_bytes v_p >.

let set_bit pc oc bc =
  .< let p, o, b = .~pc, .~oc, .~bc in SerDataBytes0.set_bit p o b >.

let poke_byte pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.poke_byte p v >.

let write_byte pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_byte p v >.

let write_word ?be pc vc =
  .< let p, v, be = .~pc, .~vc, .~(lift_option lift_bool be) in
 SerDataBytes0.write_word ?be p v >.

let write_dword ?be pc vc =
  .< let p, v, be = .~pc, .~vc, .~(lift_option lift_bool be) in
 SerDataBytes0.write_dword ?be p v >.

let write_qword ?be pc vc =
  .< let p, v, be = .~pc, .~vc, .~(lift_option lift_bool be) in
 SerDataBytes0.write_qword ?be p v >.

let write_oword ?be pc vc =
  .< let p, v, be = .~pc, .~vc, .~(lift_option lift_bool be) in
 SerDataBytes0.write_oword ?be p v >.

let write_bytes pc vc =
  .< let p, v = .~pc, .~vc in SerDataBytes0.write_bytes p v >.

let make_buffer sc =
  .< let s = .~sc in SerDataBytes0.make_buffer s >.

let of_string ic =
  .< let i = .~ic in SerDataBytes0.of_string i >.

let size_of_const = SerDataBytes0.size_of_const
let byte_of_const = SerDataBytes0.byte_of_const
let word_of_const = SerDataBytes0.word_of_const
let dword_of_const = SerDataBytes0.dword_of_const
let qword_of_const = SerDataBytes0.qword_of_const
let oword_of_const = SerDataBytes0.oword_of_const

(* Very efficient as you can see: *)

let make_bytes = .< Bytes.empty >.

let bytes_append bs b =
  .<
    let old_bs = .~bs in
    let len = Bytes.length old_bs in
    let new_bs = Bytes.create (len + 1) in
    Bytes.blit old_bs 0 new_bs 0 len ;
    Bytes.set new_bs len (Char.chr (Uint8.to_int .~b)) ;
(*    Format.printf "old bytes was %S, new bytes is %S@."
      (Bytes.to_string old_bs) (Bytes.to_string new_bs) ;*)
    new_bs
  >.
