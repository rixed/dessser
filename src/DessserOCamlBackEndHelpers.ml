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

type 'a nullable = Null | NotNull of 'a

(* Nulls propagate outward. The quickest way to implement this is to fire an
 * exception when encountering a Null, to longjmp to the outward operation. *)
exception ImNull

module Nullable =
struct
  (*$< Nullable *)

  type 'a t = 'a nullable

  let map f = function
    | Null -> Null
    | NotNull x ->
        (try NotNull (f x) with ImNull -> Null)

  let may f = function
    | Null -> ()
    | NotNull x -> f x

  let map_no_fail f = function
    | Null -> Null
    | NotNull x ->
        (try NotNull (f x) with _ -> Null)

  let get ?what = function
    | Null ->
        let what =
          match what with
          | None -> ""
          | Some msg -> " ("^ msg ^")" in
        invalid_arg ("Nullable.get"^ what)
    | NotNull x ->
        x

  let (|!) a b =
    match a with Null -> b | NotNull a -> a

  let default d = function
    | Null -> d
    | NotNull x -> x

  let default_delayed f = function
    | Null -> f ()
    | NotNull x -> x

  let of_option = function
    | None -> Null
    | Some x -> NotNull x

  let to_option = function
    | Null -> None
    | NotNull x -> Some x

  (* The following 3 comparison functions are used to *order* values and are
   * therefore not nullable (in particular, [cmp Null Null] is not [Null]).
   * Null values are considered smaller than everything else. *)
  let compare cmp a b =
    match a, b with
    | Null, Null -> 0
    | Null, _ -> -1
    | _, Null -> 1
    | a, b -> cmp a b

  let compare_left cmp a b =
    match a with
    | Null -> -1
    | a -> cmp a b

  let compare_right cmp a b =
    match b with
    | Null -> 1
    | b -> cmp a b

  let of_nan (x : float) =
    if x <> x then Null else NotNull x

  (*$= of_nan
    Null (of_nan nan)
    (NotNull 0.) (of_nan 0.)
    (NotNull infinity) (of_nan infinity)
    (NotNull neg_infinity) (of_nan neg_infinity)
  *)

  (*$>*)
end

let (|!) = Nullable.(|!)

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

(* Persistent data container => not to be used as a buffer! *)
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

  (* FIXME: the string type should be implemented as a slice *)
  let to_string s =
    Bytes.sub_string s.bytes s.offset s.length

  let of_string s =
    make (Bytes.of_string s) 0 (String.length s)
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

  type t =
    { start : int ;
      stop : int ;
      stack : int list ;
      seq : int ;
      impl : impl }

  and impl =
    { size : int ;
      peek : int -> int ;
      peekn : int -> int -> Slice.t ;
      poke : int -> int -> unit ;
      poken : int -> Slice.t -> unit ;
      to_string : unit -> string }

  let make impl =
    { start = 0 ;
      stop = impl.size ;
      stack = [] ;
      seq = next_seq () ;
      impl }

  let of_pointer p start stop =
    assert (start <= stop) ;
    assert (stop <= p.stop) ;
    { p with start ; stop }

  (* Check that the given start is not past the end; But end position is OK *)
  let check_input_length o l =
    if o > l then raise (NotEnoughInput { missing = o - l ; offset = o })

  let skip p n =
    if debug then
      Printf.eprintf "Advance from %d to %d\n%!" p.start (p.start + n) ;
    check_input_length (p.start + n) p.stop ;
    { p with start = p.start + n }

  let sub p1 p2 =
    assert (p1.seq = p2.seq) ;
    assert (p1.start >= p2.start) ;
    Size.of_int (p1.start - p2.start)

  let remSize p =
    Size.of_int (p.stop - p.start)

  let offset p =
    Size.of_int p.start

  let peekByte p at =
    check_input_length (p.start + at + 1) p.stop ;
    let c = p.impl.peek (p.start + at) in
    if debug then
      Printf.eprintf "PeekByte 0x%02x at %d\n%!" c (p.start + at) ;
    Uint8.of_int c

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
    check_input_length (p.start + sz) p.stop ;
    p.impl.peekn p.start sz,
    skip p sz

  let pokeByte p at v =
    check_input_length (p.start + at) p.stop ;
    p.impl.poke (p.start + at) (Uint8.to_int v)

  let pokeWord ?(big_endian=false) p at v =
    let fst, snd = v, Uint16.shift_right_logical v 8 in
    let fst, snd = if big_endian then snd, fst else fst, snd in
    if debug then
      Printf.eprintf "PokeWord 0x%04x at %d\n%!" (Uint16.to_int v) (p.start + at) ;
    pokeByte p at (Uint16.to_uint8 fst) ;
    pokeByte p (at+1) (Uint16.to_uint8 snd)

  let pokeDWord ?(big_endian=false) p at v =
    let fst, snd = v, Uint32.shift_right_logical v 16 in
    let fst, snd = if big_endian then snd, fst else fst, snd in
    if debug then
      Printf.eprintf "PokeDWord 0x%08Lx at %d\n%!" (Uint32.to_int64 v) (p.start + at) ;
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
    p.impl.poken p.start v ;
    skip p len

  let blitBytes p v sz =
    let c = Uint8.to_int v in
    for i = p.start to p.start + sz - 1 do
      p.impl.poke i c
    done ;
    skip p sz

  let push p =
    { p with stack = p.start :: p.stack }

  let pop p =
    let start, stack =
      match p.stack with
      | [] ->
          Printf.eprintf "Cannot pop pointer offset from empty stack\n%!" ;
          assert false
      | o :: s -> o, s in
    { p with start ; stack }
end

let pointer_of_bytes t =
  let size = Bytes.length t
  and peek at =
    Bytes.unsafe_get t at |> Char.code
  and peekn at sz =
    Slice.make t at sz
  and poke at v =
    Bytes.unsafe_set t at (Char.chr v)
  and poken at slice =
    Bytes.blit slice.Slice.bytes slice.offset t at slice.length
  and to_string () =
    Bytes.unsafe_to_string t in
  Pointer.make { size ; peek ; peekn ; poke ; poken ; to_string }

let pointer_of_buffer size =
  pointer_of_bytes (Bytes.create size)

let pointer_of_string str =
  pointer_of_bytes (Bytes.of_string str)

let peek_char p o =
  p.Pointer.impl.peek o |> Char.chr

(* A C impl of the above, that reads from any address *)

module ExtPointer =
struct
  type t (* abstract, represents an external pointer *)

  external make : Uint64.t -> Size.t -> t = "ext_pointer_new"
  external size : t -> int = "ext_pointer_size" [@@noalloc]
  external peek : t -> int -> int = "ext_pointer_peek" [@@noalloc]
  external peekn : t -> int -> int -> Slice.t = "ext_pointer_peekn"
  external poke : t -> int -> int -> unit = "ext_pointer_poke" [@@noalloc]
  external poken : t -> int -> Slice.t -> unit = "ext_pointer_poken" [@@noalloc]
  external to_string : t -> string = "ext_pointer_to_string"
end

let pointer_of_address addr size =
  let t = ExtPointer.make addr size in
  let peek at =
    ExtPointer.peek t at
  and peekn at sz =
    ExtPointer.peekn t at sz
  and poke at v =
    ExtPointer.poke t at v
  and poken at slice =
    ExtPointer.poken t at slice
  and to_string () =
    ExtPointer.to_string t in
  Pointer.make { size ; peek ; peekn ; poke ; poken ; to_string }

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
   * When deserializing into an slist, the first value (the oldest) end up
   * at the end of the slist. *)
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
