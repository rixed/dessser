open Stdint

include DessserFloatTools

let debug = false

let string_of_char c = String.make 1 c

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

(* Persistent data container => no to be used as a buffer! *)
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

module Pointer =
struct
  type t =
    { bytes : Bytes.t ;
      start : int ;
      stop : int ; (* From the beginning of [bytes] not [start]! *)
      stack : int list }

  let make sz =
    { bytes = Bytes.create sz ;
      start = 0 ;
      stop = sz ;
      stack = [] }

  let of_bytes bytes start stop =
    { bytes ; start ; stop ; stack = [] }

  let of_string s =
    { bytes = Bytes.of_string s ;
      start = 0 ;
      stop = String.length s ;
      stack = [] }

  let of_buffer n =
    { bytes = Bytes.create n ;
      start = 0 ;
      stop = n ;
      stack = [] }

  let of_pointer p start stop =
    { p with start ; stop }

  let reset p =
    { p with start = 0 ;
             stop = Bytes.length p.bytes ;
             stack = [] }

  let contents p =
    Bytes.sub p.bytes 0 p.start

  (* Check that the given start is not past the end; But end position is OK *)
  let check_input_length o l =
    if o > l then raise (NotEnoughInput { missing = o - l ; offset = o })

  let skip p n =
    if debug then
      Printf.eprintf "Advance from %d to %d\n%!" p.start (p.start + n) ;
    check_input_length (p.start + n) p.stop ;
    { p with start = p.start + n }

  let sub p1 p2 =
    assert (p1.bytes == p2.bytes) ;
    assert (p1.start >= p2.start) ;
    Size.of_int (p1.start - p2.start)

  let remSize p =
    Size.of_int (p.stop - p.start)

  let offset p =
    Size.of_int p.start

  let peekByte p at =
    check_input_length (p.start + at + 1) p.stop ;
    let c = Bytes.unsafe_get p.bytes (p.start + at) in
    if debug then
      Printf.eprintf "PeekByte 0x%02x at %d\n%!" (Char.code c) (p.start+at) ;
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
    Slice.make p.bytes p.start sz,
    skip p sz

  let pokeByte p at v =
    Bytes.set p.bytes (p.start + at) (Uint8.to_int v |> Char.chr)

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
    Bytes.blit v.bytes v.offset p.bytes p.start len ;
    skip p len

  let blitBytes p v sz =
    let c = Char.chr (Uint8.to_int v) in
    for i = p.start to p.start + sz - 1 do
      Bytes.set p.bytes i c
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

(* Once a set is constructed only 2 operations are possible with it:
 * - insert an item
 * - iterate over its items
 *
 * Backend code generator might want various kind of sets depending on the
 * use cases:
 * 1. kind of sizing:
 * - last added items limited in size;
 * - last added items limited by "age" (being defined by some expression);
 * - last added items reset after some size is reached;
 * - reservoir sampling to keep max number of items.
 *
 * Kind of insert API:
 * - should skip over NULLs or not;
 * - should keep entry sorted ;
 * - inserting an item returns nothing;
 * - inserting an item returns the updates performed (added item, removed item).
 *)

module SimpleSet =
struct
  type 'a t = 'a list ref

  let make () = ref []

  let of_list l = ref l

  let length t =
    List.length !t |> Uint32.of_int

  let insert t x =
    t := x :: !t

  let last_update t =
    List.hd !t, []

  let member t x =
    List.mem x !t

  let fold t u f =
    List.rev !t |>
    List.fold_left f u

  let del_min t n =
    t := DessserTools.list_drop n !t
end

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

  let length t =
    min t.num_inserts (Array.length t.arr) |> Uint32.of_int

  let insert t x =
    let i = t.num_inserts mod Array.length t.arr in
    if t.num_inserts >= Array.length t.arr then
      t.last_removed <- [ t.arr.(i) ] ;
    t.arr.(i) <- x ;
    t.num_inserts <- succ t.num_inserts

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
end

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

  let length t =
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
end

(* A Hash table for quick Member tests: *)
module HashTable =
struct
  type 'a t =
    { h : ('a, unit) Hashtbl.t ;
      mutable last_added : 'a option }

  let make init_sz =
    { h = Hashtbl.create init_sz ;
      last_added = None }

  let length t =
    Hashtbl.length t.h |> Uint32.of_int

  let insert t x =
    Hashtbl.add t.h x () ;
    t.last_added <- Some x

  let last_update t =
    match t.last_added with
    | None -> failwith "HashTable.last_update: no items added"
    | Some x -> x, []

  let member t x =
    Hashtbl.mem t.h x

  let fold t u f =
    Hashtbl.fold (fun x () u -> f u x) t.h u
end

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

  let last_update _ =
    failwith "Not implemented: Heap.last_update"

  let length t =
    Uint32.of_int t.length

  let member t x =
    H.mem t.cmp x t.heap

  let fold t u f =
    H.fold_left t.cmp f u t.heap

  let del_min t n =
    let rec loop n h =
      if n <= 0 then h else
      loop (n - 1) (snd (H.pop_min t.cmp h)) in
    t.heap <- loop n t.heap
end

(* Finally, the generic set type: *)

type 'a set =
  { insert : 'a -> unit ;
    last_update : unit -> 'a * 'a list ;
    cardinality : unit -> Uint32.t ;
    member : 'a -> bool ;
    fold : 'b. 'b -> ('b -> 'a -> 'b) -> 'b ;
    (* Remove N items: *)
    del_min : int -> unit }

(* When serializing with fold, the oldest value is written first.
 * When deserializing into an slist, the first value (the oldest) end up
 * at the end of the slist. *)
let make_simple_set_of_slist sl =
  let s = SimpleSet.of_list sl in
  { insert = SimpleSet.insert s ;
    last_update = (fun () -> SimpleSet.last_update s) ;
    cardinality = (fun () -> SimpleSet.length s) ;
    member = SimpleSet.member s ;
    fold = (fun u f -> SimpleSet.fold s u f) ;
    del_min = SimpleSet.del_min s }

let make_simple_set () =
  make_simple_set_of_slist []

let make_sliding_window def max_sz =
  let s = SlidingWindow.make def max_sz in
  { insert = SlidingWindow.insert s ;
    last_update = (fun () -> SlidingWindow.last_update s) ;
    cardinality = (fun () -> SlidingWindow.length s) ;
    member = SlidingWindow.member s ;
    fold = (fun u f -> SlidingWindow.fold s u f) ;
    del_min = (fun _ -> DessserTools.todo "SlidingWindow.del_min") }

let make_sampling def max_sz =
  let s = Sampling.make def max_sz in
  { insert = Sampling.insert s ;
    last_update = (fun () -> Sampling.last_update s) ;
    cardinality = (fun () -> Sampling.length s) ;
    member = Sampling.member s ;
    fold = (fun u f -> Sampling.fold s u f) ;
    del_min = (fun _ -> DessserTools.todo "Sampling.del_min") }

let make_hash_table init_sz =
  let s = HashTable.make init_sz in
  { insert = HashTable.insert s ;
    last_update = (fun () -> HashTable.last_update s) ;
    cardinality = (fun () -> HashTable.length s) ;
    member = HashTable.member s ;
    fold = (fun u f -> HashTable.fold s u f) ;
    del_min = (fun _ -> DessserTools.todo "HashTable.del_min") }

let make_heap cmp =
  let s = Heap.make cmp in
  { insert = Heap.insert s ;
    last_update = (fun () -> Heap.last_update s) ;
    cardinality = (fun () -> Heap.length s) ;
    member = Heap.member s ;
    fold = (fun u f -> Heap.fold s u f) ;
    del_min = Heap.del_min s }

(* TODO: this operation needs to be faster, ideally a nop! *)
let list_of_set s =
  s.fold [] (fun lst x -> x :: lst) |>
  List.rev |>
  Array.of_list

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
