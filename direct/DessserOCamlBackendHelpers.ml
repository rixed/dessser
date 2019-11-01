open Stdint

let option_get = function
  | Some x -> x
  | None -> invalid_arg "option_get"

module Size =
struct
  type t = int

  let of_int v = v
  let to_int v = v
  let add = (+)
  let to_string = string_of_int
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
    else
      let length = s1.length + s2.length in
      let bytes = Bytes.create length in
      Bytes.blit s1.bytes s1.offset bytes 0 s1.length ;
      Bytes.blit s2.bytes s2.offset bytes s1.length s2.length ;
      { bytes ; offset = 0 ; length }

  let to_string s =
    Bytes.sub_string s.bytes s.offset s.length
  let of_string s =
    make (Bytes.of_string s) 0 (String.length s)
end

module Pointer =
struct
  type t = Bytes.t * int

  let make sz =
    (Bytes.create sz, 0)

  let of_bytes b o = b, o

  let of_string s =
    (Bytes.of_string s, 0)

  let skip (b, o) n =
    assert (o + n <= Bytes.length b) ;
    (b, o + n)

  let sub (b1, o1) (b2, o2) =
    assert (b1 == b2) ;
    assert (o1 >= o2) ;
    Size.of_int (o2 - o1)

  let remSize (b, o) =
    Size.of_int (Bytes.length b - o)

  let peekByte (b, o) at =
    let c = Bytes.get b (o + at) in
    Uint8.of_int (Char.code c)

  let peekWordLe p at =
    let cat b0 b1 =
      Uint16.(logor (shift_left (of_uint8 b0) 8) (of_uint8 b1)) in
    let b0 = peekByte p at in
    let b1 = peekByte p (at+1) in
    cat b1 b0

  let peekDWordLe p at =
    let cat b0 b1 =
      Uint32.(logor (shift_left (of_uint16 b0) 16) (of_uint16 b1)) in
    let b0 = peekWordLe p at in
    let b1 = peekWordLe p (at+2) in
    cat b1 b0

  let peekQWordLe p at =
    let cat b0 b1 =
      Uint64.(logor (shift_left (of_uint32 b0) 32) (of_uint32 b1)) in
    let b0 = peekDWordLe p at in
    let b1 = peekDWordLe p (at+4) in
    cat b1 b0

  let peekOWordLe p at =
    let cat b0 b1 =
      Uint128.(logor (shift_left (of_uint64 b0) 64) (of_uint64 b1)) in
    let b0 = peekQWordLe p at in
    let b1 = peekQWordLe p (at+8) in
    cat b1 b0

  let getBit p o =
    let b = peekByte p (o/8) in
    Uint8.(compare one (logand (shift_right_logical b (o mod 8)) one) = 0)

  let readByte p =
    peekByte p 0, skip p 1
  let readWordLe p =
    peekWordLe p 0, skip p 2
  let readDWordLe p =
    peekDWordLe p 0, skip p 4
  let readQWordLe p =
    peekQWordLe p 0, skip p 8
  let readOWordLe p =
    peekOWordLe p 0, skip p 16

  let readBytes (b, o as p) l =
    assert (Bytes.length b - o >= l) ;
    Slice.make b o l,
    skip p l

  let pokeByte (b, o) at v =
    Bytes.set b (o + at) (Uint8.to_int v |> Char.chr)

  let pokeWordLe p at v =
    let fst, snd = v, Uint16.shift_right_logical v 8 in
    pokeByte p at (Uint16.to_uint8 fst) ;
    pokeByte p (at+1) (Uint16.to_uint8 snd)

  let pokeDWordLe p at v =
    let fst, snd = v, Uint32.shift_right_logical v 16 in
    pokeWordLe p at (Uint32.to_uint16 fst) ;
    pokeWordLe p (at+2) (Uint32.to_uint16 snd)

  let pokeQWordLe p at v =
    let fst, snd = v, Uint64.shift_right_logical v 32 in
    pokeDWordLe p at (Uint64.to_uint32 fst) ;
    pokeDWordLe p (at+4) (Uint64.to_uint32 snd)

  let pokeOWordLe p at v =
    let fst, snd = v, Uint128.shift_right_logical v 64 in
    pokeQWordLe p at (Uint128.to_uint64 fst) ;
    pokeQWordLe p (at+8) (Uint128.to_uint64 snd)

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

  let writeWordLe p v =
    pokeWordLe p 0 v ;
    skip p 2

  let writeDWordLe p v =
    pokeDWordLe p 0 v ;
    skip p 4

  let writeQWordLe p v =
    pokeQWordLe p 0 v ;
    skip p 8

  let writeOWordLe p v =
    pokeOWordLe p 0 v ;
    skip p 16

  let writeBytes (b, o as p) v =
    let len = v.Slice.length in
    Bytes.blit v.bytes v.offset b o len ;
    skip p len

  let blitBytes (b, o as p) v l =
    let c = Char.chr (Uint8.to_int v) in
    for i = o to o + l - 1 do
      Bytes.set b i c
    done ;
    skip p l
end
