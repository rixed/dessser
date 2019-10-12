(* Implementation of SERDATA reading/writing directly from/to an OCaml string
 * of bytes, and reading into direct OCaml values. *)

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

type bit = bool
type byte = int
type word = int
type dword = int32
type qword = int64
type bytes = Bytes.t

let size_of_const n = n
let byte_of_const n = n land 0xff
let word_of_const n = n
let dword_of_const n = n
let qword_of_const n = n

let int_of_byte : byte code -> int code = fun n -> .< .~n land 0xff >.
let byte_of_int : int code -> byte code = fun n -> .< .~n land 0xff >.

let peek_byte ?(at=0) p =
  let o = p.offset + at in
  assert (o < Bytes.length p.data) ;
  Bytes.get p.data o |> Char.code

let peek_word ?(be=false) ?(at=0) p =
  let cat b0 b1 = (b0 lsl 8) lor b1 in
  let b0 = peek_byte ~at p in
  let b1 = peek_byte ~at:(at+1) p in
  if be then cat b0 b1 else cat b1 b0

let peek_dword ?(be=false) ?(at=0) p =
  let cat w0 w1 =
    Int32.(logor (shift_left (of_int w0) 16) (of_int w1)) in
  let w0 = peek_word ~be ~at p in
  let w1 = peek_word ~be ~at:(at+2) p in
  if be then cat w0 w1 else cat w1 w0

let peek_qword ?(be=false) ?(at=0) p =
  let cat d0 d1 =
    Int64.(logor (shift_left (of_int32 d0) 32) (of_int32 d1)) in
  let d0 = peek_dword ~be ~at p in
  let d1 = peek_dword ~be ~at:(at+4) p in
  if be then cat d0 d1 else cat d1 d0

let test_bit p o =
  let b = peek_byte p in
  (b lsr o) mod 1 = 1

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

let read_bytes (l, p) =
  assert (Bytes.length p.data - p.offset >= l) ;
  Bytes.sub p.data p.offset l,
  add p l

let poke_byte p v =
  assert (p.offset < Bytes.length p.data) ;
  Bytes.set p.data p.offset (Char.chr v)

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

let write_word ?(be=false) p v =
  let fst, snd =
    if be then v lsr 8, v
          else v, v lsr 8
  in
  let p = write_byte p (fst land 0xff) in
  write_byte p (snd land 0xff)

let write_dword ?(be=false) p v =
  let fst, snd =
    if be then Int32.shift_right_logical v 16, v
          else v, Int32.shift_right_logical v 16
  in
  let p = write_word ~be p (Int32.to_int fst) in
  write_word ~be p (Int32.to_int snd)

let write_qword ?(be=false) p v =
  let fst, snd =
    if be then Int64.shift_right_logical v 32, v
          else v, Int64.shift_right_logical v 32
  in
  let p = write_dword p (Int64.to_int32 fst) in
  write_dword p (Int64.to_int32 snd)

let write_bytes p v =
  let len = Bytes.length v in
  Bytes.blit v 0 p.data p.offset len ;
  add p len

(* For debugging: *)
let print p =
  Format.printf "Destination = %S@."
    (Bytes.sub_string p.data 0 p.offset)
