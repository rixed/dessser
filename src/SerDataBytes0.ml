(* Implementation of SERDATA reading/writing directly from/to an OCaml string
 * of bytes, and reading into direct OCaml values. *)

type pointer =
  { data : bytes ; mutable offset : int }

let make_buffer sz =
  { data = Bytes.create sz ; offset = 0 }

type size = int

type nop = unit

type input = string

let of_input input =
  { data = Bytes.of_string input ;
    offset = 0 }

let nop = ()

let and_then () () = ()

let size_of_const n = n

let skip p s =
  p.offset <- p.offset + s

let add p s =
  { p with offset = p.offset + s }

let sub p1 p2 =
  assert (p1.data == p2.data) ;
  p1.offset - p2.offset

type bit = bool
type byte = int
let byte_of_const n = n
type word = int
type dword = int32
type qword = int64
type bytes = Bytes.t

let peek_byte p =
  (* It's OK to peek past the end of the puffer, and then one reads 0: *)
  if p.offset = Bytes.length p.data then
    0
  else (
    assert (p.offset < Bytes.length p.data) ;
    Bytes.get p.data p.offset |> Char.code
  )

let test_bit p o =
  let b = peek_byte p in
  (b lsr o) mod 1 = 1

let and_skip p s v =
  skip p s ; v

let read_byte p =
  peek_byte p,
  add p 1

let read_word ?(be=false) p =
  let cat b0 b1 = (b0 lsl 8) lor b1 in
  let b0, p = read_byte p in
  let b1, p = read_byte p in
  (if be then cat b0 b1 else cat b1 b0),
  add p 2

let read_dword ?(be=false) p =
  let cat w0 w1 =
    Int32.(logor (shift_left (of_int w0) 16) (of_int w1)) in
  let w0, p = read_word ~be p in
  let w1, p = read_word ~be p in
  (if be then cat w0 w1 else cat w1 w0),
  add p 4

let read_qword ?(be=false) p =
  let cat d0 d1 =
    Int64.(logor (shift_left (of_int32 d0) 32) (of_int32 d1)) in
  let d0, p = read_dword ~be p in
  let d1, p = read_dword ~be p in
  (if be then cat d0 d1 else cat d1 d0),
  add p 8

let read_bytes p l =
  assert (Bytes.length p.data - p.offset >= l) ;
  Bytes.sub p.data p.offset l,
  add p l

let poke_byte p v =
  assert (p.offset < Bytes.length p.data) ;
  Bytes.set p.data p.offset (Char.chr v) ;
  skip p 1

let set_bit p o v =
  let bit = 1 lsl o in
  let b = peek_byte p in
  let b =
    if v then b lor bit
         else b land (lnot bit) in
  poke_byte p b

let write_byte p v =
  poke_byte p v ;
  add p 1

let write_word ?(be=false) _p _v = ignore be ; assert false
let write_dword ?(be=false) _p _v = ignore be ; assert false
let write_qword ?(be=false) _p _v = ignore be ; assert false
let write_bytes _p _v = assert false
