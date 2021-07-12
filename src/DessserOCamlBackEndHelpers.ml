open Stdint

open DessserFloatTools
open DessserTools

let debug = false

let string_of_char c = String.make 1 c

let print_dump oc x =
  BatPervasives.dump x |> BatString.print oc

(* Taken from BatString.find_from: *)
let string_find str sub =
  let len = String.length str in
  let sublen = String.length sub in
  if sublen = 0 then 0 else
    let rec find ~str ~sub i =
      if i > len - sublen then raise Not_found
      else
        (* 0 <= i <= length str - length sub *)
        let rec loop ~str ~sub i j =
          if j = sublen then i
          else
            (* 0 <= j < length sub *)
            (* ==>  0 <= i + j < length str *)
            if String.unsafe_get str (i + j) <> String.unsafe_get sub j
            then find ~str ~sub (i + 1)
            else loop ~str ~sub i (j + 1)
        in loop ~str ~sub i 0
    in find ~str ~sub 0

(* Taken from BatString.rfind_from: *)
let string_rfind str sub =
  let sublen = String.length sub
  and len = String.length str in
  (* 0 <= pos + 1 <= length str *)
  if sublen = 0 then len else
    (* length sub > 0 *)
    (* (pos + 1 - sublen) <= length str - length sub < length str *)
    let rec find ~str ~sub i =
      if i < 0 then raise Not_found
      else
        (* 0 <= i <= length str - length sub < length str *)
        let rec loop ~str ~sub i j =
          if j = sublen then i
          else
            (* 0 <= j < length sub *)
            (* ==> 0 <= i + j < length str *)
            if String.unsafe_get str (i + j) <> String.unsafe_get sub j
            then find ~str ~sub (i - 1)
            else loop ~str ~sub i (j + 1)
        in loop ~str ~sub i 0
    in find ~str ~sub (len - sublen)

let substring str start stop =
  let l = String.length str in
  let rec clamp x =
    if x < 0 then clamp (x + l) else min x l in
  let start, stop =
    if l = 0 then 0, 0 else clamp start, clamp stop in
  if start >= stop then "" else
  String.sub str start (stop - start)

let strftime ?(gmt=false) str tim =
  let open Unix in
  let name_of_month m =
    [| "January" ; "February" ; "March" ; "April" ; "May" ; "June" ;
       "July" ; "August" ; "September" ; "October" ; "November" ;
       "December" |].(m) in
  let abbr_of_month m =
    String.sub (name_of_month m) 0 3 in
  let name_of_day d =
    [| "Sunday" ; "Monday" ; "Tuesday" ; "Wednesday" ; "Thursday" ; "Friday" ;
       "Saturday" |].(d) in
  let abbr_of_day d =
    String.sub (name_of_day d) 0 3 in
  let tm = (if gmt then gmtime else localtime) tim in
  let replacements =
    [| "%%", "%" ; "%n", "\n" ; "%t", "\t" ;
       "%y", Printf.sprintf "%02d" ((tm.tm_year + 1900) mod 100) ;
       "%Y", string_of_int (tm.tm_year + 1900) ;
       "%C", Printf.sprintf "%02d" (((tm.tm_year + 1900) / 100) mod 100) ;
       "%b", abbr_of_month tm.tm_mon ;
       "%h", abbr_of_month tm.tm_mon ;
       "%B", name_of_month tm.tm_mon ;
       "%m", Printf.sprintf "%02d" (tm.tm_mon + 1) ;
       "%j", string_of_int tm.tm_yday ;
       "%d", Printf.sprintf "%02d" tm.tm_mday ;
       "%e", Printf.sprintf "%2d" tm.tm_mday ;
       "%a", abbr_of_day tm.tm_wday ;
       "%A", name_of_day tm.tm_wday ;
       "%w", string_of_int tm.tm_wday ;
       "%u", string_of_int (tm.tm_wday + 1) ;
       "%H", Printf.sprintf "%02d" tm.tm_hour ;
       "%I", Printf.sprintf "%02d" (tm.tm_hour mod 12) ;
       "%M", Printf.sprintf "%02d" tm.tm_min ;
       "%S", Printf.sprintf "%02d" tm.tm_sec ;
       "%XS", Printf.sprintf "%05.2f"
               (float_of_int tm.tm_sec +. mod_float tim 1.) ;
       "%p", if tm.tm_hour < 12 then "a.m." else "p.m." ;
       "%Xs", string_of_float tim |] in
  Array.fold_left (fun str (sub, by) ->
    BatString.nreplace ~str ~sub ~by
  ) str replacements

let array_of_list_rev l =
  let a = Array.of_list l in
  let len = Array.length a in
  for i = 0 to len/2 - 1 do
    let tmp = a.(i) in
    let j = len - i - 1 in
    a.(i) <- a.(j) ;
    a.(j) <- tmp
  done ;
  a

let ( % ) f g x = f (g x)

(*$= array_of_list_rev & ~printer:(BatIO.to_string (BatArray.print BatInt.print))
  [||] (array_of_list_rev [])
  [|1|] (array_of_list_rev [1])
  [| 2;1 |] (array_of_list_rev [ 1;2 ])
  [| 3;2;1 |] (array_of_list_rev [ 1;2;3 ])
*)

exception NotImplemented of string
(* Parameter is the minimum length of the missing part: *)
exception NotEnoughInput of { offset : int ; missing : int }

let () =
  Printexc.register_printer (function
    | NotImplemented s ->
        Some ("Not implemented: "^ s)
    | NotEnoughInput b ->
        Some (
          Printf.sprintf "NotEnoughInput: %d byte%s missing at offset %d"
            b.missing (if b.missing > 1 then "s" else "")
            b.offset)
    | _ ->
        None)

(* Nulls propagate outward. The quickest way to implement this is to fire an
 * exception when encountering a Null, to longjmp to the outward operation. *)
exception ImNull

let nullable_get ?what = function
  | None ->
      let what =
        match what with
        | None -> ""
        | Some msg -> " ("^ msg ^")" in
      invalid_arg ("nullable_get"^ what)
  | Some x ->
      x

let nullable_of_nan (x : float) =
  if x <> x then None else Some x

(*$= nullable_of_nan
  None (nullable_of_nan nan)
  (Some 0.) (nullable_of_nan 0.)
  (Some infinity) (nullable_of_nan infinity)
  (Some neg_infinity) (nullable_of_nan neg_infinity)
*)

let nullable_map f = function
  | None -> None
  | Some x ->
      (try Some (f x) with ImNull -> None)

let nullable_map_no_fail f = function
  | None -> None
  | Some x ->
      (try Some (f x) with _ -> None)

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

(* Persistent data container => not to be used as a buffer!
 * We use bytes but we could as well use strings, as they are
 * never externally modified. Bytes allow us to construct
 * values more efficiently though (initializing and blitting
 * in several steps). Thus the many unsafe operations. *)
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
      Bytes.unsafe_blit s1.bytes s1.offset bytes 0 s1.length ;
      Bytes.unsafe_blit s2.bytes s2.offset bytes s1.length s2.length ;
      { bytes ; offset = 0 ; length }

  let add s1 b =
    let b = Char.chr (Uint8.to_int b) in
    if s1.offset + s1.length < Bytes.length s1.bytes &&
       Bytes.unsafe_get s1.bytes (s1.offset + s1.length) = b
    then
      { s1 with length = s1.length + 1 }
    else
      let length = s1.length + 1 in
      let bytes = Bytes.create length in
      Bytes.unsafe_blit s1.bytes s1.offset bytes 0 s1.length ;
      Bytes.unsafe_set bytes s1.length b ;
      { bytes ; offset = 0 ; length }

  (* FIXME: the string type should be implemented as a slice *)
  let to_string s =
    if s.offset = 0 && s.length = Bytes.length s.bytes then
      Bytes.unsafe_to_string s.bytes
    else
      Bytes.sub_string s.bytes s.offset s.length

  let of_string s =
    make (Bytes.unsafe_of_string s) 0 (String.length s)

  let eq s1 s2 =
    s1.length = s2.length && (
      (s1.bytes == s2.bytes && s1.offset = s2.offset) ||
      (* Slow path *)
      (to_string s1 = to_string s2)
    )
end

(* Pointers
 *
 * We want a data pointer to either points into an OCaml Bytes.t as well as
 * into an out-of-ocaml-heap buffer, yet have an efficient implementation
 * that does not switch for every operation.
 *
 * Therefore, pointers are records of functions.
 * To sub them they must be able to probe whether another pointer uses the
 * same backend implementation (and byte sequence). For this, we use an integer
 * sequence that's incremented at each new pointer creation.
 *)

module Pointer =
struct
  let seq = ref 0

  let next_seq () =
    incr seq ;
    !seq

  type ptr =
    { stop : int ;
      seq : int ;
      impl : impl }

  and impl =
    { size : int ;
      peek1 : int -> Uint8.t ;
      peek2_le : int -> Uint16.t ;
      peek2_be : int -> Uint16.t ;
      peek4_le : int -> Uint32.t ;
      peek4_be : int -> Uint32.t ;
      peek8_le : int -> Uint64.t ;
      peek8_be : int -> Uint64.t ;
      peek16_le : int -> Uint128.t ;
      peek16_be : int -> Uint128.t ;
      peekn : int -> int -> Slice.t ;
      poke1 : int -> Uint8.t -> unit ;
      poke2_le : int -> Uint16.t -> unit ;
      poke2_be : int -> Uint16.t -> unit ;
      poke4_le : int -> Uint32.t -> unit ;
      poke4_be : int -> Uint32.t -> unit ;
      poke8_le : int -> Uint64.t -> unit ;
      poke8_be : int -> Uint64.t -> unit ;
      poke16_le : int -> Uint128.t -> unit ;
      poke16_be : int -> Uint128.t -> unit ;
      poken : int -> Slice.t -> unit ;
      to_string : unit -> string }

  type t = ptr * int (* offset *)

  let make impl =
    { stop = impl.size ;
      seq = next_seq () ;
      impl },
    0

  let of_pointer (p, _) start stop =
    assert (start <= stop) ;
    assert (stop <= p.stop) ;
    if stop = p.stop then p, start
    else { p with stop }, start

  (* Check that the given start is not past the end; But end position is OK *)
  let check_input_length o l =
    if o > l then raise (NotEnoughInput { missing = o - l ; offset = o })

  let skip (p, o) n =
    let o' = o + n in
    if debug then
      Printf.eprintf "Advance from %d to %d\n%!" o o' ;
    p, o'

  let sub (p1, o1) (p2, o2) =
    assert (p1.seq = p2.seq) ;
    assert (o1 >= o2) ;
    Size.of_int (o1 - o2)

  let remSize (p, o) =
    Size.of_int (p.stop - o)

  let offset (_, o) =
    Size.of_int o

  let peekU8 (p, o) at =
    check_input_length (o + at + 1) p.stop ;
    p.impl.peek1 (o + at)

  let peekU16Little (p, o) at =
    check_input_length (o + at + 2) p.stop ;
    p.impl.peek2_le (o + at)

  let peekU16Big (p, o) at =
    check_input_length (o + at + 2) p.stop ;
    p.impl.peek2_be (o + at)

  let peekU32Little (p, o) at =
    check_input_length (o + at + 4) p.stop ;
    p.impl.peek4_le (o + at)

  let peekU32Big (p, o) at =
    check_input_length (o + at + 4) p.stop ;
    p.impl.peek4_be (o + at)

  let peekU64Little (p, o) at =
    check_input_length (o + at + 8) p.stop ;
    p.impl.peek8_le (o + at)

  let peekU64Big (p, o) at =
    check_input_length (o + at + 8) p.stop ;
    p.impl.peek8_be (o + at)

  let peekU128Little (p, o) at =
    check_input_length (o + at + 8) p.stop ;
    p.impl.peek16_le (o + at)

  let peekU128Big (p, o) at =
    check_input_length (o + at + 8) p.stop ;
    p.impl.peek16_be (o + at)

  let getBit p_o bi =
    let b = peekU8 p_o (bi/8) in
    Uint8.(compare one (logand (shift_right_logical b (bi mod 8)) one) = 0)

  let readU8 p_o =
    peekU8 p_o 0, skip p_o 1

  let readU16Little p_o =
    peekU16Little p_o 0, skip p_o 2

  let readU16Big p_o =
    peekU16Big p_o 0, skip p_o 2

  let readU32Little p_o =
    peekU32Little p_o 0, skip p_o 4

  let readU32Big p_o =
    peekU32Big p_o 0, skip p_o 4

  let readU64Little p_o =
    peekU64Little p_o 0, skip p_o 8

  let readU64Big p_o =
    peekU64Big p_o 0, skip p_o 8

  let readU128Little p_o =
    peekU128Little p_o 0, skip p_o 16

  let readU128Big p_o =
    peekU128Big p_o 0, skip p_o 16

  let readBytes (p, o) sz =
    check_input_length (o + sz) p.stop ;
    p.impl.peekn o sz,
    (p, o + sz)

  let pokeU8 (p, o) v =
    check_input_length (o + 1) p.stop ;
    p.impl.poke1 o v

  let pokeU16Little (p, o) v =
    check_input_length (o + 2) p.stop ;
    if debug then
      Printf.eprintf "Poke word 0x%04x at %d\n%!" (Uint16.to_int v) o ;
    p.impl.poke2_le o v

  let pokeU16Big (p, o) v =
    check_input_length (o + 2) p.stop ;
    if debug then
      Printf.eprintf "Poke word 0x%04x at %d\n%!" (Uint16.to_int v) o ;
    p.impl.poke2_be o v

  let pokeU32Little (p, o) v =
    check_input_length (o + 4) p.stop ;
    if debug then
      Printf.eprintf "Poke U32 0x%08Lx at %d\n%!" (Uint32.to_int64 v) o ;
    p.impl.poke4_le o v

  let pokeU32Big (p, o) v =
    check_input_length (o + 4) p.stop ;
    if debug then
      Printf.eprintf "Poke U32 0x%08Lx at %d\n%!" (Uint32.to_int64 v) o ;
    p.impl.poke4_be o v

  let pokeU64Little (p, o) v =
    check_input_length (o + 8) p.stop ;
    p.impl.poke8_le o v

  let pokeU64Big (p, o) v =
    check_input_length (o + 8) p.stop ;
    p.impl.poke8_be o v

  let pokeU128Little (p, o) v =
    check_input_length (o + 16) p.stop ;
    p.impl.poke16_le o v

  let pokeU128Big (p, o) v =
    check_input_length (o + 16) p.stop ;
    p.impl.poke16_be o v

  let setBit (p, o) bi v =
    let bit = Uint8.(shift_left one (bi mod 8)) in
    let o' = o + bi/8 in
    let b = peekU8 (p, o') 0 in
    let b =
      if v then Uint8.(logor b bit)
           else Uint8.(logand b (lognot bit)) in
    pokeU8 (p, o') b

  let writeU8 p v =
    pokeU8 p v ;
    skip p 1

  let writeU16Little p v =
    pokeU16Little p v ;
    skip p 2

  let writeU16Big p v =
    pokeU16Big p v ;
    skip p 2

  let writeU32Little p v =
    pokeU32Little p v ;
    skip p 4

  let writeU32Big p v =
    pokeU32Big p v ;
    skip p 4

  let writeU64Little p v =
    pokeU64Little p v ;
    skip p 8

  let writeU64Big p v =
    pokeU64Big p v ;
    skip p 8

  let writeU128Little p v =
    pokeU128Little p v ;
    skip p 16

  let writeU128Big p v =
    pokeU128Big p v ;
    skip p 16

  let writeBytes (p, o) v =
    let len = v.Slice.length in
    p.impl.poken o v ;
    p, o + len

  let blitBytes (p, o) v sz =
    for i = o to o + sz - 1 do
      p.impl.poke1 i v
    done ;
    p, o + sz
end

let pointer_of_bytes t =
  let size = Bytes.length t in
  let peek1 at =
    Bytes.unsafe_get t at |> Char.code |> Uint8.of_int in
  let peek2_le at =
    let b0 = peek1 at
    and b1 = peek1 (at+1) in
    Uint16.(logor (shift_left (of_uint8 b1) 8) (of_uint8 b0)) in
  let peek2_be at =
    let b0 = peek1 at
    and b1 = peek1 (at+1) in
    Uint16.(logor (shift_left (of_uint8 b0) 8) (of_uint8 b1)) in
  let peek4_le at =
    let b0 = peek2_le at
    and b1 = peek2_le (at+2) in
    Uint32.(logor (shift_left (of_uint16 b1) 16) (of_uint16 b0)) in
  let peek4_be at =
    let b0 = peek2_be at
    and b1 = peek2_be (at+2) in
    Uint32.(logor (shift_left (of_uint16 b0) 16) (of_uint16 b1)) in
  let peek8_le at =
    let b0 = peek4_le at
    and b1 = peek4_le (at+4) in
    Uint64.(logor (shift_left (of_uint32 b1) 32) (of_uint32 b0)) in
  let peek8_be at =
    let b0 = peek4_be at
    and b1 = peek4_be (at+4) in
    Uint64.(logor (shift_left (of_uint32 b0) 32) (of_uint32 b1)) in
  let peek16_le at =
    let b0 = peek8_le at
    and b1 = peek8_le (at+8) in
    Uint128.(logor (shift_left (of_uint64 b1) 64) (of_uint64 b0)) in
  let peek16_be at =
    let b0 = peek8_be at
    and b1 = peek8_be (at+8) in
    Uint128.(logor (shift_left (of_uint64 b0) 64) (of_uint64 b1)) in
  let peekn at sz =
    Slice.make t at sz in
  let poke1 at v =
    Bytes.unsafe_set t at (Char.chr (Uint8.to_int v)) in
  let poke2_le at v =
    let fst, snd = v, Uint16.shift_right_logical v 8 in
    poke1 at (Uint16.to_uint8 fst) ;
    poke1 (at+1) (Uint16.to_uint8 snd) in
  let poke2_be at v =
    let fst, snd = v, Uint16.shift_right_logical v 8 in
    poke1 at (Uint16.to_uint8 snd) ;
    poke1 (at+1) (Uint16.to_uint8 fst) in
  let poke4_le at v =
    let fst, snd = v, Uint32.shift_right_logical v 16 in
    poke2_le at (Uint32.to_uint16 fst) ;
    poke2_le (at+2) (Uint32.to_uint16 snd) in
  let poke4_be at v =
    let fst, snd = v, Uint32.shift_right_logical v 16 in
    poke2_be at (Uint32.to_uint16 snd) ;
    poke2_be (at+2) (Uint32.to_uint16 fst) in
  let poke8_le at v =
    let fst, snd = v, Uint64.shift_right_logical v 32 in
    poke4_le at (Uint64.to_uint32 fst) ;
    poke4_le (at+4) (Uint64.to_uint32 snd) in
  let poke8_be at v =
    let fst, snd = v, Uint64.shift_right_logical v 32 in
    poke4_be at (Uint64.to_uint32 snd) ;
    poke4_be (at+4) (Uint64.to_uint32 fst) in
  let poke16_le at v =
    let fst, snd = v, Uint128.shift_right_logical v 64 in
    poke8_le at (Uint128.to_uint64 fst) ;
    poke8_le (at+8) (Uint128.to_uint64 snd) in
  let poke16_be at v =
    let fst, snd = v, Uint128.shift_right_logical v 64 in
    poke8_be at (Uint128.to_uint64 snd) ;
    poke8_be (at+8) (Uint128.to_uint64 fst) in
  let poken at slice =
    Bytes.blit slice.Slice.bytes slice.offset t at slice.length in
  let to_string () =
    Bytes.unsafe_to_string t in
  Pointer.make
    { peek1 ; peekn ;
      peek2_le ; peek4_le ; peek8_le ; peek16_le ;
      peek2_be ; peek4_be ; peek8_be ; peek16_be ;
      poke1 ; poken ;
      poke2_le ; poke4_le ; poke8_le ; poke16_le ;
      poke2_be ; poke4_be ; poke8_be ; poke16_be ;
      size ; to_string }

let pointer_of_buffer size =
  pointer_of_bytes (Bytes.create size)

let pointer_of_string str =
  pointer_of_bytes (Bytes.of_string str)

let peek_char (p, _) at =
  p.Pointer.impl.peek1 at |> Uint8.to_int |> Char.chr

(* A C impl of the above, that reads from any address *)

module ExtPointer =
struct
  type t (* abstract, represents an external pointer *)

  external make : Uint64.t -> Size.t -> t = "ext_pointer_new"
  external size : t -> int = "ext_pointer_size" [@@noalloc]
  external peek1 : t -> int -> Uint8.t = "ext_pointer_peek1" [@@noalloc]
  external peek2_le : t -> int -> Uint16.t = "ext_pointer_peek2_le" [@@noalloc]
  external peek2_be : t -> int -> Uint16.t = "ext_pointer_peek2_be" [@@noalloc]
  external peek4_le : t -> int -> Uint32.t = "ext_pointer_peek4_le"
  external peek4_be : t -> int -> Uint32.t = "ext_pointer_peek4_be"
  external peek8_le : t -> int -> Uint64.t = "ext_pointer_peek8_le"
  external peek8_be : t -> int -> Uint64.t = "ext_pointer_peek8_be"
  external peek16_le : t -> int -> Uint128.t = "ext_pointer_peek16_le"
  external peek16_be : t -> int -> Uint128.t = "ext_pointer_peek16_be"
  external peekn : t -> int -> int -> Slice.t = "ext_pointer_peekn"
  external poke1 : t -> int -> Uint8.t -> unit = "ext_pointer_poke1" [@@noalloc]
  external poke2_le : t -> int -> Uint16.t -> unit = "ext_pointer_poke2_le" [@@noalloc]
  external poke2_be : t -> int -> Uint16.t -> unit = "ext_pointer_poke2_be" [@@noalloc]
  external poke4_le : t -> int -> Uint32.t -> unit = "ext_pointer_poke4_le" [@@noalloc]
  external poke4_be : t -> int -> Uint32.t -> unit = "ext_pointer_poke4_be" [@@noalloc]
  external poke8_le : t -> int -> Uint64.t -> unit = "ext_pointer_poke8_le" [@@noalloc]
  external poke8_be : t -> int -> Uint64.t -> unit = "ext_pointer_poke8_be" [@@noalloc]
  external poke16_le : t -> int -> Uint128.t -> unit = "ext_pointer_poke16_le" [@@noalloc]
  external poke16_be : t -> int -> Uint128.t -> unit = "ext_pointer_poke16_be" [@@noalloc]
  external poken : t -> int -> Slice.t -> unit = "ext_pointer_poken" [@@noalloc]
  external to_string : t -> string = "ext_pointer_to_string"
end

let pointer_of_address addr size =
  let t = ExtPointer.make addr size in
  let peek1 at = ExtPointer.peek1 t at
  and peek2_le at = ExtPointer.peek2_le t at
  and peek2_be at = ExtPointer.peek2_be t at
  and peek4_le at = ExtPointer.peek4_le t at
  and peek4_be at = ExtPointer.peek4_be t at
  and peek8_le at = ExtPointer.peek8_le t at
  and peek8_be at = ExtPointer.peek8_be t at
  and peek16_le at = ExtPointer.peek16_le t at
  and peek16_be at = ExtPointer.peek16_be t at
  and peekn at sz = ExtPointer.peekn t at sz
  and poke1 at v = ExtPointer.poke1 t at v
  and poke2_le at v = ExtPointer.poke2_le t at v
  and poke2_be at v = ExtPointer.poke2_be t at v
  and poke4_le at v = ExtPointer.poke4_le t at v
  and poke4_be at v = ExtPointer.poke4_be t at v
  and poke8_le at v = ExtPointer.poke8_le t at v
  and poke8_be at v = ExtPointer.poke8_be t at v
  and poke16_le at v = ExtPointer.poke16_le t at v
  and poke16_be at v = ExtPointer.poke16_be t at v
  and poken at slice = ExtPointer.poken t at slice
  and to_string () = ExtPointer.to_string t in
  Pointer.make
    { peek1 ; peekn ;
      peek2_le ; peek4_le ; peek8_le ; peek16_le ;
      peek2_be ; peek4_be ; peek8_be ; peek16_be ;
      poke1 ; poken ;
      poke2_le ; poke4_le ; poke8_le ; poke16_le ;
      poke2_be ; poke4_be ; poke8_be ; poke16_be ;
      size ; to_string }

(* Once a set is constructed few operations are possible with it:
 * - insert an item
 * - iterate over its items
 * - compute the cardinality of the set
 * - delete minimum items (for ordered sets)
 *
 * Backend code generator might want various kind of sets depending on the
 * use cases:
 * - some sets automatically get rid of old data (based on cardinality, time,
 *   weight...);
 * - some sets order items and allow to retrieve/remove the smallest ones;
 * - some sets keep only a random sample of the inserted items;
 * - etc...
 *
 * All these sets share the same type "Set" (with a parameter indicating the
 * actual type of set and the implementation to choose), and some of the
 * operations (esp. Insert and Fold). The idea is that there should exist a
 * generic way to perform operations incrementally on all those sets.
 *)

module type SET =
sig
  type 'a t

  val insert : 'a t -> 'a -> unit

  val insert_weighted : 'a t -> float -> 'a -> unit

  val last_update : 'a t -> 'a * 'a list

  val cardinality : 'a t -> Uint32.t

  val member : 'a t -> 'a -> bool

  val fold : 'a t -> 'b -> ('b -> 'a -> 'b) -> 'b

  val get_min : 'a t -> 'a

  (* Remove N items: *)
  val del_min : 'a t -> int -> unit
end

module SimpleSet =
struct
  type 'a t = 'a list ref

  let make () = ref []

  (* When serializing with fold, the oldest value is written first.
   * When deserializing into a list, the first value (the oldest) end up
   * at the end of the list. *)
  let of_list l = ref l

  let cardinality t =
    List.length !t |> Uint32.of_int

  let insert t x =
    t := x :: !t

  let insert_weighted t _w x =
    insert t x

  let last_update t =
    List.hd !t, []

  let member t x =
    List.mem x !t

  let fold t u f =
    List.rev !t |>
    List.fold_left f u

  let get_min t =
    list_last !t

  let del_min t n =
    t := list_drop n !t
end

module SimpleSetCheck : SET = SimpleSet

(* Sliding window based on number of items: *)
module SlidingWindow =
struct
  type 'a t =
    { arr : 'a array ;
      mutable num_inserts : int ;
      (* Last value that was removed (either 0 or 1, list type used as it
       * matches last_update's return type) *)
      mutable last_removed : 'a list }

  let make def max_sz =
    { arr = Array.make max_sz def ;
      num_inserts = 0 ;
      last_removed = [] }

  let cardinality t =
    min t.num_inserts (Array.length t.arr) |> Uint32.of_int

  let insert t x =
    let i = t.num_inserts mod Array.length t.arr in
    if t.num_inserts >= Array.length t.arr then
      t.last_removed <- [ t.arr.(i) ] ;
    t.arr.(i) <- x ;
    t.num_inserts <- succ t.num_inserts

  let insert_weighted t _w x =
    insert t x

  let last_update t =
    let i = pred t.num_inserts mod Array.length t.arr in
    t.arr.(i), t.last_removed

  let member t x =
    let len = min t.num_inserts (Array.length t.arr) in
    let rec loop i =
      if i >= len then false else
      if t.arr.(i) = x then true else
      loop (i + 1) in
    loop 0

  let fold t u f =
    let len = min t.num_inserts (Array.length t.arr) in
    let rec loop u n =
      if n >= len then u else
      let i = (t.num_inserts + n) mod Array.length t.arr in
      loop (f u t.arr.(i)) (n + 1) in
    loop u 0

  let get_min _t =
    todo "SlidingWindow.get_min"

  let del_min _t =
    todo "SlidingWindow.del_min"
end

module SlidingWindowCheck : SET = SlidingWindow

(* Reservoir sampling: *)
module Sampling =
struct
  type 'a t =
    { (* The size of this array gives the reservoir max size.
       * Uninitialized values are initialized to a random value given by the
       * user: *)
      arr : 'a array  ;
      mutable cur_size : int ;
      (* Total number of items added: *)
      mutable count : int ;
      (* Last value that was added and removed, if any: *)
      mutable last_update : 'a * 'a list }

  let make def max_sz =
    { arr = Array.make max_sz def ;
      cur_size = 0 ; count = 0 ; last_update = def, [] }

  let cardinality t =
    t.cur_size |> Uint32.of_int

  let insert t x =
    let i = t.count in
    t.count <- i + 1 ;
    if t.cur_size < Array.length t.arr then (
      let idx = t.cur_size in
      t.cur_size <- t.cur_size + 1 ;
      t.arr.(idx) <- x
    ) else (
      let max_size = float_of_int (Array.length t.arr) in
      let keep = Random.float 1. < max_size /. float_of_int (i + 1) in
      if keep then (
        let idx = Random.int (Array.length t.arr) in
        t.last_update <- x, [ t.arr.(idx) ] ;
        t.arr.(idx) <- x
      )
    )

  let insert_weighted t _w x =
    insert t x

  let last_update t =
    t.last_update

  let member t x =
    let rec loop i =
      if i >= t.cur_size then false else
      if t.arr.(i) = x then true else
      loop (i + 1) in
    loop 0

  let fold t u f =
    let rec loop u i =
      if i >= t.cur_size then u else
      loop (f u t.arr.(i)) (i + 1) in
    loop u 0

  let get_min _t =
    todo "Sampling.get_min"

  let del_min _t =
    todo "Sampling.del_min"
end

module SamplingCheck : SET = Sampling

(* A Hash table for quick Member tests: *)
module HashTable =
struct
  type 'a t =
    { h : ('a, unit) Hashtbl.t ;
      mutable last_added : 'a option }

  let make init_sz =
    { h = Hashtbl.create init_sz ;
      last_added = None }

  let cardinality t =
    Hashtbl.length t.h |> Uint32.of_int

  let insert t x =
    Hashtbl.add t.h x () ;
    t.last_added <- Some x

  let insert_weighted t _w x =
    insert t x

  let last_update t =
    match t.last_added with
    | None -> failwith "HashTable.last_update: no items added"
    | Some x -> x, []

  let member t x =
    Hashtbl.mem t.h x

  let fold t u f =
    Hashtbl.fold (fun x () u -> f u x) t.h u

  let get_min _t =
    todo "HashTable.get_min"

  let del_min _t =
    todo "HashTable.del_min"
end

module HashTableCheck : SET = HashTable

(* A Heap for ordering elements: *)
module Heap =
struct
  module H = DessserLeftistHeap

  type 'a t =
    { mutable heap : 'a H.t ;
      cmp : 'a -> 'a -> int ;
      mutable length : int }

  let make cmp =
    { heap = H.empty ;
      cmp ;
      length = 0 }

  let insert t x =
    t.heap <- H.add t.cmp x t.heap ;
    t.length <- t.length + 1

  let insert_weighted t _w x =
    insert t x

  let last_update _ =
    failwith "Not implemented: Heap.last_update"

  let cardinality t =
    Uint32.of_int t.length

  let member t x =
    H.mem t.cmp x t.heap

  let fold t u f =
    H.fold_left t.cmp f u t.heap

  let get_min t =
    H.min t.heap

  let del_min t n =
    if n >= t.length then (
      t.heap <- H.empty ;
      t.length <- 0
    ) else (
      let rec loop n h =
        if n <= 0 then h else
        loop (n - 1) (snd (H.pop_min t.cmp h)) in
      t.heap <- loop n t.heap ;
      t.length <- t.length - n
    )
end

module HeapCheck : SET = Heap

module Top =
struct
  let debug = false

  (* Weight map: from weight to anything, ordered bigger weights first: *)
  module WMap = BatMap.Make (struct
    type t = float ref (* ref so we can downscale *)
    let compare w1 w2 = Float.compare !w2 !w1
  end)

  type 'a t =
    { size : int ;
      max_size : int ;
      mutable cur_size : int ;
      (* Optionally, select only those outliers above that many sigmas: *)
      sigmas : float ;
      mutable sum_weight1 : Kahan.t ;
      mutable sum_weight2 : Kahan.t ;
      mutable count : int64 ;
      (* value to weight and overestimation: *)
      mutable w_of_x : ('a, float * float) BatMap.t ;
      (* max weight to value to overestimation. Since we need iteration to go
       * from bigger to smaller weight we need a custom map: *)
      mutable xs_of_w : (('a, float) BatMap.t) WMap.t }

  let make size max_size sigmas =
    let sigmas = abs_float sigmas in
    if size < 1 then invalid_arg "Top.make size" ;
    { size ; max_size ; cur_size = 0 ; sigmas ;
      sum_weight1 = Kahan.init ; sum_weight2 = Kahan.init ; count = 0L ;
      w_of_x = BatMap.empty ; xs_of_w = WMap.empty }

  (* Scale all stored weight by [d].
   * It is OK to modify the map keys because relative ordering is not going
   * to change: *)
  let scale t d =
    t.w_of_x <-
      BatMap.map (fun (w, o) -> w *. d, o *. d) t.w_of_x ;
    WMap.iter (fun w _xs -> w := !w *. d) t.xs_of_w ;
    t.sum_weight1 <- Kahan.mul t.sum_weight1 d ;
    t.sum_weight2 <- Kahan.mul t.sum_weight2 (d *. d)

  let insert_weighted t w x =
    let add_in_xs_of_w x w o m =
      if debug then
        Printf.printf "TOP: add entry %s of weight %f\n"
          (BatPervasives.dump x) w ;
      WMap.modify_opt (ref w) (function
        | None -> Some (BatMap.singleton x o)
        | Some xs ->
            assert (not (BatMap.mem x xs)) ;
            Some (BatMap.add x o xs)
      ) m
    and rem_from_xs_of_w x w m =
      WMap.modify_opt (ref w) (function
        | None -> assert false
        | Some xs ->
            (match BatMap.extract x xs with
            | exception Not_found ->
                BatPrintf.eprintf "xs_of_w for w=%f does not have x=%s (only %a)\n"
                  w (BatPervasives.dump x)
                  (BatMap.print print_dump BatFloat.print) xs ;
                assert false
            | _, xs ->
                if BatMap.is_empty xs then None else Some xs)
      ) m
    in
    (* Shortcut for the frequent case when w=0: *)
    if w <> 0. then (
      let victim_x = ref None in
      t.w_of_x <-
        BatMap.modify_opt x (function
          | None ->
              let victim_w = ref 0. in
              if t.cur_size >= t.max_size then (
                (* pick the victim and remove it from xs_of_w: *)
                let victim_w', xs = WMap.max_binding t.xs_of_w in
                let (victim_x', _victim_o), xs' = BatMap.pop xs in
                victim_w := !victim_w' ;
                victim_x := Some victim_x' ;
                t.xs_of_w <-
                  if BatMap.is_empty xs' then
                    WMap.remove victim_w' t.xs_of_w
                  else
                    WMap.update victim_w' victim_w' xs' t.xs_of_w
              ) else t.cur_size <- t.cur_size + 1 ;
              let w = w +. !victim_w in
              t.xs_of_w <- add_in_xs_of_w x w !victim_w t.xs_of_w ;
              Some (w, !victim_w)
          | Some (w', o) ->
              let w = w +. w' in
              t.xs_of_w <-
                rem_from_xs_of_w x w' t.xs_of_w |>
                add_in_xs_of_w x w o ;
              Some (w, o)
        ) t.w_of_x ;
      BatOption.may (fun x ->
        match BatMap.extract x t.w_of_x with
        | exception Not_found ->
            BatPrintf.eprintf "w_of_x does not have x=%s, only %a\n"
              (BatPervasives.dump x)
              (BatEnum.print print_dump) (BatMap.keys t.w_of_x |>
               BatEnum.take 99) ;
            assert false
        | _, w_of_x ->
            t.w_of_x <- w_of_x
      ) !victim_x ;
      (* Also compute the mean if sigmas is not null: *)
      if t.sigmas > 0. then (
        t.sum_weight1 <- Kahan.add t.sum_weight1 w ;
        t.sum_weight2 <- Kahan.add t.sum_weight1 (w *. w) ;
        t.count <- Int64.succ t.count
      ) ;
      assert (t.cur_size <= t.max_size) (*;
      assert (BatMap.cardinal s.w_of_x = s.cur_size) ;
      assert (WMap.cardinal s.xs_of_w <= s.cur_size)*)
    ) (* w <> 0. *)

  let insert _ _ =
    todo "Top.insert"

  let last_update _ =
    todo "Top.last_update"

  (* For each monitored item of rank k <= n, we must ask ourselves: could there
   * be an item with rank k > n, or a non-monitored items, with more weight?  For
   * this we must compare guaranteed weight of items with the max weight of item
   * of rank n+1; but we don't know which item that is unless we order them. *)
  (* FIXME: super slow, maintain the entries in increased max weight order. *)

  (* Iter the entries in decreasing weight order.
   * Note: BatMap iterates in increasing keys order despite the doc says it's
   * unspecified, but since we reverse the comparison operator we fold from
   * heaviest to lightest. *)
  let fold_all t f u =
    WMap.fold (fun w xs u ->
      if debug then Printf.printf "TOP: folding over all entries of weight %f\n" !w ;
      BatMap.foldi (fun x o u ->
        if debug then Printf.printf "TOP:   ... %s\n" (BatPervasives.dump x) ;
        f !w x o u
      ) xs u
    ) t.xs_of_w u

  (* Iter over the top entries in order of weight, lightest first (so that it's
   * easy to build the reverse list), ignoring those entries below the
   * specified amount of sigmas: *)
  let fold t u f =
    let res = ref []
    and cutoff = ref None in
    let cutoff_fun () =
      if t.sigmas > 0. then
        let sum_weight1 = Kahan.finalize t.sum_weight1
        and sum_weight2 = Kahan.finalize t.sum_weight2
        and count = Int64.to_float t.count in
        let mean = sum_weight1 /. count in
        let sigma = sqrt (count *. sum_weight2 -. mean *. mean) /. count in
        let cutoff_sigma = mean +. t.sigmas *. sigma in
        match !cutoff with
        | None ->
            fun u (w, _min_w, x) ->
              if w >= cutoff_sigma then f u x else u
        | Some c ->
            fun u (w, min_w, x) ->
              if min_w >= c && w >= cutoff_sigma then f u x else u
      else
        match !cutoff with
        | None ->
            fun u (_w, _min_w, x) -> f u x
        | Some c ->
            fun u (_w, min_w, x) ->
              if min_w >= c then f u x else u
    in
    (try
      fold_all t (fun w x o rank ->
        (* We need item at rank n+1 to find top-n *)
        if rank <= t.size then (
          if debug then
            Printf.printf "TOP rank=%d<=%d is %s, weight %f\n"
              rank t.size (BatPervasives.dump x) w ;
          (* May be filtered once we know the cutoff: *)
          res := (w, (w -. o), x) :: !res ; (* res is lightest to heaviest *)
          rank + 1
        ) else (
          assert (rank = t.size + 1) ;
          if debug then
            Printf.printf "TOP rank=%d>%d is %s, weight %f\n"
              rank t.size (BatPervasives.dump x) w ;
          cutoff := Some w ;
          raise Exit
        )
      ) 1 |> ignore ;
      (* We reach here when we had less entries than [size], in which case we
       * do not need a cut-off since we know all the entries: *)
      if debug then
        Printf.printf "TOP: Couldn't reach rank %d, cur_size=%d\n" t.size t.cur_size ;
    with Exit -> ()) ;
    (* Now filter the entries if we have a cutoff, and build the result: *)
    List.fold_left (cutoff_fun ()) u !res

  let cardinality t =
    fold t 0 (fun c _ -> c + 1) |>
    Uint32.of_int

  (* Tells the rank of a given value in the top, or None: *)
  let rank t x =
    let res = ref None in
    (try
      fold t 1 (fun k x' ->
        if x = x' then (
          res := Some k ;
          raise Exit
        ) else k + 1
      ) |> ignore
    with Exit -> ()) ;
    !res

  let member t x =
    rank t x <> None

  let get_min _ =
    todo "Top.get_min"

  let del_min _ _ =
    todo "Top.del_min"
end

module TopCheck : SET = Top

let lst_lchop l n =
  if n >= Array.length l then [||] else
  Array.sub l n (Array.length l - n)

let lst_rchop l n =
  if n >= Array.length l then [||] else
  Array.sub l 0 (Array.length l - n)

(* Partial sort (inplace) of array at least indices [ks] of array [a], à la
 * quick-sort: *)

let partial_sort a ks =
  (* Swap two values of [a]: *)
  let swap i j =
    let tmp = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- tmp in
  (* Partition a slice of [a] around a given pivot value [p],
   * and return a position into the final array such that every
   * items up to and including that position are <= pivot and
   * every items after that are >= pivot. *)
  let partition lo hi p =
    let rec adv_i i j =
      (* Everything that's <= i is <= p,
       * everything that's >= j is >= p,
       * i < j *)
      let i = i + 1 in
      if i >= j then i - 1
      else if a.(i) < p then (adv_i [@tailcall]) i j
      else (rec_j [@tailcall]) i j
    and rec_j i j =
      (* Everything that's < i is <= p, i is > p,
       * everything that's >= j is >= p, j > i-1.
       * If i = 0 we know we have the actual pivot value before to stop j
       * from reading before the beginning of the array. *)
      let j = j - 1 in
      if a.(j) > p then (rec_j [@tailcall]) i j
      else if j <= i then i - 1
      else (
        swap i j ;
        (* we are back to adv_i precondition: *)
        (adv_i [@tailcall]) i j
      )
    in
    adv_i (lo - 1) (hi + 1) in
  let rec loop count lo hi ks =
    assert (count <= Array.length a) ;
    if hi - lo > 0 && ks <> [] then (
      (* We are going to use partition in that way:
       * First, we take one random value as the pivot and put it in front.
       * Then, we partition the rest of the array around
       * that pivot value. The returned position is the last of the first
       * partition. We can then put the pivot at the end of it, so we have
       * "sorted" that value. *)
      let pv = lo + (hi - lo) / 2 in (* not that random *)
      swap lo pv ;
      let m = a.(lo) in
      let pv = partition (lo + 1) hi m in
      swap lo pv ;
      let ks_lo, ks_hi = List.partition (fun k -> k <= pv) ks in
      let c = loop (count + 1) lo pv ks_lo in
      loop c (pv + 1) hi ks_hi
    ) else count in
  loop 0 0 (Array.length a - 1) ks |> ignore

(* Runtime Field Masks *)

let mask_get ma i =
  match ma with
  | DessserMasks.Recurse mas -> mas.(i)
  | (Copy | Skip | SetNull as ma) -> ma
  | Replace _ | Insert _ ->
      assert false (* no runtime representation (yet?) *)
