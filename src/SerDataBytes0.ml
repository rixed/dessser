(* Implementation of SERDATA reading/writing directly from/to an OCaml string
 * of bytes, and reading into direct OCaml values. *)
open Batteries

module Raw =
struct
  type pointer =
    { data : bytes ; offset : int }

  let make_buffer sz =
    { data = Bytes.create sz ; offset = 0 }

  type size = int

  let size_of_const n = n

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

  let read_byte p =
    assert (p.offset < Bytes.length p.data) ;
    Bytes.get p.data p.offset |> Char.code

  let test_bit p o =
    let b = read_byte p in
    (b lsr o) mod 1 = 1

  let read_word ?(be=false) p =
    let cat b0 b1 = (b0 lsl 8) lor b1 in
    let b0 = read_byte p
    and b1 = read_byte (add p 1) in
    if be then cat b0 b1 else cat b1 b0

  let read_dword ?(be=false) p =
    let cat w0 w1 =
      Int32.(logor (shift_left (of_int w0) 16) (of_int w1)) in
    let w0 = read_word ~be p
    and w1 = read_word ~be (add p 2) in
    if be then cat w0 w1 else cat w1 w0

  let read_qword ?(be=false) p =
    let cat d0 d1 =
      Int64.(logor (shift_left (of_int32 d0) 32) (of_int32 d1)) in
    let d0 = read_dword ~be p
    and d1 = read_dword ~be (add p 4) in
    if be then cat d0 d1 else cat d1 d0

  let read_bytes p l =
    assert (Bytes.length p.data - p.offset >= l) ;
    Bytes.sub p.data p.offset l

  type nop = unit

  let write_byte p v =
    assert (p.offset < Bytes.length p.data) ;
    Bytes.set p.data p.offset (Char.chr v) ;
    add p 1

  let set_bit p o v =
    let bit = 1 lsl o in
    let b = read_byte p in
    let b =
      if v then b lor bit
           else b land (lnot bit) in
    write_byte p b |> ignore

  let write_word ?(be=false) _p _v = ignore be ; assert false
  let write_dword ?(be=false) _p _v = ignore be ; assert false
  let write_qword ?(be=false) _p _v = ignore be ; assert false
  let write_bytes _p _v = assert false

  let rec print_data p len oc =
    if p.offset < Bytes.length p.data then
      if len = 0 then
        String.print oc "..."
      else
        Printf.fprintf oc "%s%02x%t"
          (if p.offset mod 4 = 0 then " " else "")
          (read_byte p)
          (print_data (add p 1) (len - 1))

  let print_pointer oc p =
    Printf.fprintf oc "%d/%d (%t)"
      p.offset
      (Bytes.length p.data)
      (print_data p 10)
end

module type SERDATA_OCAML0 =
  BerSerdes.SERDATA with
    type bit = bool and
    type byte = int and
    type word = int and
    type dword = int32 and
    type qword = int64 and
    type bytes = Bytes.t

module Impl : SERDATA_OCAML0 = Raw
