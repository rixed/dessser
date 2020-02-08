open Stdint
include DessserFloatTools

let debug = false

let option_get = function
  | Some x -> x
  | None -> invalid_arg "option_get"

exception NotImplemented of string
(* Parameter is the minimum length of the missing part: *)
exception NotEnoughInput of { offset : int ; missing : int }

let () =
  Printexc.register_printer (function
    | NotEnoughInput b ->
        Some (
          Printf.sprintf "NotEnoughInput: %d byte%s missing at offset %d"
            b.missing (if b.missing > 1 then "s" else "")
            b.offset)
    | _ ->
        None)

module Size =
struct
  type t = int

  let of_int v = v
  let to_int v = v
  let to_string = string_of_int

  let add = (+)
  let sub = (-)
  let mul = ( * )
  let div = (/)
  let rem a b = a mod b

  let logand v1 v2 = v1 land v2
  let logor v1 v2 = v1 lor v2
  let logxor v1 v2 = v1 lxor v2
  let lognot b = lnot b
  let shift_left v1 s = v1 lsl s
  let shift_right_logical v1 s = v1 lsr s
end

module Slice =
struct
  type t = { bytes : Bytes.t ; offset : int ; length : int }

  let make bytes offset length =
    { bytes ; offset ; length }

  let append s1 s2 =
    if s1.bytes == s2.bytes &&
       s1.offset + s1.length = s2.offset
    then
      { s1 with length = s1.length + s2.length }
    else if s1.length = 0 then s2
    else if s2.length = 0 then s1
    else
      let length = s1.length + s2.length in
      let bytes = Bytes.create length in
      Bytes.blit s1.bytes s1.offset bytes 0 s1.length ;
      Bytes.blit s2.bytes s2.offset bytes s1.length s2.length ;
      { bytes ; offset = 0 ; length }

  let add s1 b =
    let b = Char.chr (Uint8.to_int b) in
    if s1.offset + s1.length < Bytes.length s1.bytes &&
       Bytes.get s1.bytes (s1.offset + s1.length) = b
    then
      { s1 with length = s1.length + 1 }
    else
      let length = s1.length + 1 in
      let bytes = Bytes.create length in
      Bytes.blit s1.bytes s1.offset bytes 0 s1.length ;
      Bytes.set bytes s1.length b ;
      { bytes ; offset = 0 ; length }

  let to_string s =
    Bytes.sub_string s.bytes s.offset s.length

  let of_string s =
    make (Bytes.of_string s) 0 (String.length s)
end

module Pointer =
struct
  type t =
    { bytes : Bytes.t ;
      offset : int ;
      length : int ;
      (* Saved positions: *)
      stack : int list }

  let make sz =
    { bytes = Bytes.create sz ;
      offset = 0 ;
      length = sz ;
      stack = [] }

  let of_bytes bytes offset length =
    { bytes ; offset ; length ; stack = [] }

  let of_string s =
    { bytes = Bytes.of_string s ;
      offset = 0 ;
      length = String.length s ;
      stack = [] }

  let of_buffer n =
    { bytes = Bytes.create n ;
      offset = 0 ;
      length = n ;
      stack = [] }

  (* Check that the given offset is not past the end; But end position is OK *)
  let check_input_length o l =
    if o > l then raise (NotEnoughInput { missing = o - l ; offset = o })

  let skip p n =
    if debug then
      Printf.eprintf "Advance from %d to %d\n%!" p.offset (p.offset + n) ;
    check_input_length (p.offset + n) p.length ;
    { p with offset = p.offset + n }

  let sub p1 p2 =
    assert (p1.bytes == p2.bytes) ;
    assert (p1.offset >= p2.offset) ;
    Size.of_int (p1.offset - p2.offset)

  let remSize p =
    Size.of_int (p.length - p.offset)

  let offset p =
    Size.of_int p.offset

  let peekByte p at =
    check_input_length (p.offset + at + 1) p.length ;
    let c = Bytes.get p.bytes (p.offset + at) in
    if debug then
      Printf.eprintf "PeekByte 0x%02x at %d\n%!" (Char.code c) (p.offset+at) ;
    Uint8.of_int (Char.code c)

  let peekWord ?(big_endian=false) p at =
    let cat b0 b1 =
      Uint16.(logor (shift_left (of_uint8 b0) 8) (of_uint8 b1)) in
    let b0 = peekByte p at in
    let b1 = peekByte p (at+1) in
    if big_endian then cat b0 b1 else cat b1 b0

  let peekDWord ?(big_endian=false) p at =
    let cat b0 b1 =
      Uint32.(logor (shift_left (of_uint16 b0) 16) (of_uint16 b1)) in
    let b0 = peekWord ~big_endian p at in
    let b1 = peekWord ~big_endian p (at+2) in
    if big_endian then cat b0 b1 else cat b1 b0

  let peekQWord ?(big_endian=false) p at =
    let cat b0 b1 =
      Uint64.(logor (shift_left (of_uint32 b0) 32) (of_uint32 b1)) in
    let b0 = peekDWord ~big_endian p at in
    let b1 = peekDWord ~big_endian p (at+4) in
    if big_endian then cat b0 b1 else cat b1 b0

  let peekOWord ?(big_endian=false) p at =
    let cat b0 b1 =
      Uint128.(logor (shift_left (of_uint64 b0) 64) (of_uint64 b1)) in
    let b0 = peekQWord ~big_endian p at in
    let b1 = peekQWord ~big_endian p (at+8) in
    if big_endian then cat b0 b1 else cat b1 b0

  let getBit p o =
    let b = peekByte p (o/8) in
    Uint8.(compare one (logand (shift_right_logical b (o mod 8)) one) = 0)

  let readByte p =
    peekByte p 0, skip p 1

  let readWord ?(big_endian=false) p =
    peekWord ~big_endian p 0, skip p 2

  let readDWord ?(big_endian=false) p =
    peekDWord ~big_endian p 0, skip p 4

  let readQWord ?(big_endian=false) p =
    peekQWord ~big_endian p 0, skip p 8

  let readOWord ?(big_endian=false) p =
    peekOWord ~big_endian p 0, skip p 16

  let readBytes p sz =
    Slice.make p.bytes p.offset sz,
    skip p sz

  let pokeByte p at v =
    Bytes.set p.bytes (p.offset + at) (Uint8.to_int v |> Char.chr)

  let pokeWord ?(big_endian=false) p at v =
    let fst, snd = v, Uint16.shift_right_logical v 8 in
    let fst, snd = if big_endian then snd, fst else fst, snd in
    if debug then
      Printf.eprintf "PokeWord 0x%04x at %d\n%!" (Uint16.to_int v) (p.offset + at) ;
    pokeByte p at (Uint16.to_uint8 fst) ;
    pokeByte p (at+1) (Uint16.to_uint8 snd)

  let pokeDWord ?(big_endian=false) p at v =
    let fst, snd = v, Uint32.shift_right_logical v 16 in
    let fst, snd = if big_endian then snd, fst else fst, snd in
    if debug then
      Printf.eprintf "PokeDWord 0x%08Lx at %d\n%!" (Uint32.to_int64 v) (p.offset + at) ;
    pokeWord ~big_endian p at (Uint32.to_uint16 fst) ;
    pokeWord ~big_endian p (at+2) (Uint32.to_uint16 snd)

  let pokeQWord ?(big_endian=false) p at v =
    let fst, snd = v, Uint64.shift_right_logical v 32 in
    let fst, snd = if big_endian then snd, fst else fst, snd in
    pokeDWord ~big_endian p at (Uint64.to_uint32 fst) ;
    pokeDWord ~big_endian p (at+4) (Uint64.to_uint32 snd)

  let pokeOWord ?(big_endian=false) p at v =
    let fst, snd = v, Uint128.shift_right_logical v 64 in
    let fst, snd = if big_endian then snd, fst else fst, snd in
    pokeQWord ~big_endian p at (Uint128.to_uint64 fst) ;
    pokeQWord ~big_endian p (at+8) (Uint128.to_uint64 snd)

  let setBit p o v =
    let bit = Uint8.(shift_left one (o mod 8)) in
    let at = o/8 in
    let b = peekByte p at in
    let b =
      if v then Uint8.(logor b bit)
           else Uint8.(logand b (lognot bit)) in
    pokeByte p at b

  let writeByte p v =
    pokeByte p 0 v ;
    skip p 1

  let writeWord ?(big_endian=false) p v =
    pokeWord ~big_endian p 0 v ;
    skip p 2

  let writeDWord ?(big_endian=false) p v =
    pokeDWord ~big_endian p 0 v ;
    skip p 4

  let writeQWord ?(big_endian=false) p v =
    pokeQWord ~big_endian p 0 v ;
    skip p 8

  let writeOWord ?(big_endian=false) p v =
    pokeOWord ~big_endian p 0 v ;
    skip p 16

  let writeBytes p v =
    let len = v.Slice.length in
    Bytes.blit v.bytes v.offset p.bytes p.offset len ;
    skip p len

  let blitBytes p v sz =
    let c = Char.chr (Uint8.to_int v) in
    for i = p.offset to p.offset + sz - 1 do
      Bytes.set p.bytes i c
    done ;
    skip p sz

  let push p =
    { p with stack = p.offset :: p.stack }

  let pop p =
    let offset, stack =
      match p.stack with
      | [] ->
          Printf.eprintf "Cannot pop pointer offset from empty stack\n%!" ;
          assert false
      | o :: s -> o, s in
    { p with offset ; stack }
end
