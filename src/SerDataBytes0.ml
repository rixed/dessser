(* Implementation of SERDATA reading/writing directly from/to an OCaml string
 * of bytes, and reading into direct OCaml values. *)
open Batteries
open Stdint

type pointer =
  { data : bytes ; mutable offset : int }

let make_buffer sz =
  { data = Bytes.create sz ; offset = 0 }

type size = int

let of_string s =
  { data = Bytes.of_string s ; offset = 0 }

let add p s =
  { p with offset = p.offset + s }

let sub p1 p2 =
  assert (p1.data == p2.data) ;
  p1.offset - p2.offset

let rem p =
  Bytes.length p.data - p.offset

let print_pointer oc p =
  Int.print oc p.offset

let print_data oc p s =
  let s = Bytes.sub_string p.data p.offset (min s (rem p)) in
  String.print_quoted oc s

type bit = bool
(* We do not do any computation with bytes, words, etc. Those are purely
 * data storage. So we could for simplicity keep signed OCaml integers as
 * long as they save us some conversions. *)
type byte = Uint8.t
type word = Uint16.t
type dword = Uint32.t
type qword = Uint64.t
type oword = Uint128.t
type bytes = Bytes.t

let size_of_const n = n
let byte_of_const = Uint8.of_int
let word_of_const = Uint16.of_int
let dword_of_const n = n
let qword_of_const n = n
let oword_of_const n = n

let int_of_byte : byte code -> int code = fun n -> .< Uint8.to_int .~n >.
let byte_of_int : int code -> byte code = fun n -> .< Uint8.of_int .~n >.

let peek_byte ?(at=0) p =
  let o = p.offset + at in
  assert (o < Bytes.length p.data) ;
  Bytes.get p.data o |> Char.code |> Uint8.of_int

let peek_word ?(be=false) ?(at=0) p =
  let cat b0 b1 =
    Uint16.(logor (shift_left (of_uint8 b0) 8) (of_uint8 b1)) in
  let b0 = peek_byte ~at p in
  let b1 = peek_byte ~at:(at+1) p in
  if be then cat b0 b1 else cat b1 b0

let peek_dword ?(be=false) ?(at=0) p =
  let cat w0 w1 =
    Uint32.(logor (shift_left (of_uint16 w0) 16) (of_uint16 w1)) in
  let w0 = peek_word ~be ~at p in
  let w1 = peek_word ~be ~at:(at+2) p in
  if be then cat w0 w1 else cat w1 w0

let peek_qword ?(be=false) ?(at=0) p =
  let cat d0 d1 =
    Uint64.(logor (shift_left (of_uint32 d0) 32) (of_uint32 d1)) in
  let d0 = peek_dword ~be ~at p in
  let d1 = peek_dword ~be ~at:(at+4) p in
  if be then cat d0 d1 else cat d1 d0

let peek_oword ?(be=false) ?(at=0) p =
  let cat d0 d1 =
    Uint128.(logor (shift_left (of_uint64 d0) 64) (of_uint64 d1)) in
  let d0 = peek_qword ~be ~at p in
  let d1 = peek_qword ~be ~at:(at+8) p in
  if be then cat d0 d1 else cat d1 d0

let test_bit p o =
  let b = peek_byte p in
  Uint8.(compare one (logand (shift_right_logical b o) one) = 0)

let read_byte p =
  peek_byte p,
  add p 1

let read_word ?be p =
  peek_word ?be p,
  add p 2

let read_dword ?be p =
  peek_dword ?be p,
  add p 4

let read_qword ?be p =
  peek_qword ?be p,
  add p 8

let read_oword ?be p =
  peek_oword ?be p,
  add p 16

let read_bytes (l, p) =
  assert (Bytes.length p.data - p.offset >= l) ;
  Bytes.sub p.data p.offset l,
  add p l

let poke_byte p v =
  assert (p.offset < Bytes.length p.data) ;
  Bytes.set p.data p.offset (Uint8.to_int v |> Char.chr)

let set_bit p o v =
  let bit = Uint8.(shift_left one o) in
  let b = peek_byte p in
  let b =
    if v then Uint8.(logor b bit)
         else Uint8.(logand b (lognot bit)) in
  poke_byte p b

let write_byte p v =
  poke_byte p v ;
  add p 1

let write_word ?(be=false) p v =
  let fst, snd =
    if be then Uint16.shift_right_logical v 8, v
          else v, Uint16.shift_right_logical v 8
  in
  let p = write_byte p (Uint16.to_uint8 fst) in
  write_byte p (Uint16.to_uint8 snd)

let write_dword ?(be=false) p v =
  let fst, snd =
    if be then Uint32.shift_right_logical v 16, v
          else v, Uint32.shift_right_logical v 16
  in
  let p = write_word ~be p (Uint32.to_uint16 fst) in
  write_word ~be p (Uint32.to_uint16 snd)

let write_qword ?(be=false) p v =
  let fst, snd =
    if be then Uint64.shift_right_logical v 32, v
          else v, Uint64.shift_right_logical v 32
  in
  let p = write_dword p (Uint64.to_uint32 fst) in
  write_dword p (Uint64.to_uint32 snd)

let write_oword ?(be=false) p v =
  let fst, snd =
    if be then Uint128.shift_right_logical v 64, v
          else v, Uint128.shift_right_logical v 64
  in
  let p = write_qword p (Uint128.to_uint64 fst) in
  write_qword p (Uint128.to_uint64 snd)

let write_bytes p v =
  let len = Bytes.length v in
  Bytes.blit v 0 p.data p.offset len ;
  add p len

(* For debugging: *)
let print p =
  Format.printf "Destination = %S@."
    (Bytes.sub_string p.data 0 p.offset)
