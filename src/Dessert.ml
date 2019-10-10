(* BerSerde is a library that generates serializers and deserializers
 * between a choice of several external formats and several internal
 * representations. *)
(* Objectives:
 * 1. Be able to add serialization format easily
 * 2. Do not rely too much on large code values with OCaml code but rather
 *    try to combine smaller ones so that it's possible to lift them in C
 *    expressions instead of printing OCaml.
 * 3. Integrate nicely with Ramen types and type checker (ie: no need to ask
 *    to much to OCaml's typechecker since we are going to typecheck with z3)
 *)

(* Serialized data are read from / written to memory buffers accessed via this
 * interface.
 * You will find several implementations:
 * - One reading/writing into a Bytes.t;
 * - One reading/writing into a file;
 * - Other should be easy to add. *)
module type SERDATA =
sig
  type pointer
  type size
  val add : pointer code -> size code -> pointer code
  val sub : pointer code -> pointer code -> size code
  (*val print_pointer : 'a BatIO.output -> pointer -> unit
  val print_data : 'a BatIO.output -> pointer -> size -> unit*)

  type bit
  type byte
  type word
  type dword
  type qword
  type bytes
  val int_of_byte : byte code -> int code
  val byte_of_int : int code -> byte code

  val test_bit : pointer code -> int code -> bit code
  (* read functions: *)
  val read_byte : pointer code -> (byte * pointer) code
  val read_word : ?be:bool -> pointer code -> (word * pointer) code
  val read_dword : ?be:bool -> pointer code -> (dword * pointer) code
  val read_qword : ?be:bool -> pointer code -> (qword * pointer) code
  val read_bytes : pointer code -> size code -> (bytes * pointer) code
  val set_bit : pointer code -> int code -> bit code -> unit code
  (* write functions: *)
  val write_byte : pointer code -> byte code -> pointer code
  val write_word : ?be:bool -> pointer code -> word code -> pointer code
  val write_dword : ?be:bool -> pointer code -> dword code -> pointer code
  val write_qword : ?be:bool -> pointer code -> qword code -> pointer code
  val write_bytes : pointer code -> bytes code -> pointer code
  (* Those two do not move the pointer: *)
  val peek_byte : pointer code -> byte code
  val poke_byte : pointer code -> byte code -> unit code

  val size_of_const : int -> size code
  val make_buffer : size code -> pointer code
  val of_string : string code -> pointer code
end

(* Now when we generate code expressions we want to generate ocaml code (or
 * C code) without carrying along the types nor the value type tags. The
 * generator must either generate working code of fail in an assert false
 * (we do not rely on the type checker to avoid the asserts but on z3). *)

(* For instance, if we know that the 3rd item of a tuple is a i8 value,
 * then the generated code to write this 3rd item must be: `write x_3` not
 *   `write (match x_3 with VI8 v -> v)`.
 * How to do that?
 * if v is the value for the tuple, then we can do:
 *   (i8v_of_value v.(3))
 * to get the i8v (int code).
 * So the only thing that's missing to implement tuple_get et vec_get is the
 * vector and tuple *types*, that could be part of the value.
 *
 * Now what if the item we want is only known at runtime. For vectors that
 * must not be a problem as the type is still known at compile time. For
 * a tuple this is just not possible.
 *)

type typ =
  | TPointer
  | TSize
  | TBool
  | TI8
  | TI16
  | TVec of int * typ
  | TTup of typ array

module type INTREPR =
sig
  module SerData : SERDATA
  type boolv and i8v and i16v

  val byte_of_i8v : i8v code -> SerData.byte code
  val i8v_of_byte : SerData.byte code -> i8v code
  val word_of_i16v : i16v code -> SerData.word code
  val i16v_of_word : SerData.word code -> i16v code

  val choose : boolv code -> 'a code -> 'a code -> 'a code
  val fst : ('a * 'b) code -> 'a code
  val snd : ('a * 'b) code -> 'b code
  val map_fst : ('a -> 'c) code -> ('a * 'b) code -> ('c * 'b) code
  val map_pair : fst:('a code -> 'c code) -> snd:('b code -> 'd code) ->
                 ('a * 'b) code -> ('c * 'd) code

  val boolv_of_const : bool -> boolv code
  val boolv_and : boolv code -> boolv code -> boolv code
  val i8v_of_const : int -> i8v code
  val i8v_eq : i8v code -> i8v code -> boolv code
  val i8v_ne : i8v code -> i8v code -> boolv code
  val i8v_ge : i8v code -> i8v code -> boolv code
  val i8v_gt : i8v code -> i8v code -> boolv code
  val i8v_add : i8v code -> i8v code -> i8v code
  val i8v_sub : i8v code -> i8v code -> i8v code
  val i8v_mul : i8v code -> i8v code -> i8v code
  val i8v_mod : i8v code -> i8v code -> i8v code
  val i8v_div : i8v code -> i8v code -> i8v code
  val i16v_of_const : int -> i16v code
  val i16v_gt : i16v code -> i16v code -> boolv code

  (* Peek next byte and if cond is true then accumulate it into the value. *)
  val read_while :
    cond:(SerData.byte -> boolv) code ->
    reduce:('a -> SerData.byte -> 'a) code ->
    ('a * SerData.pointer) code ->
      ('a * SerData.pointer) code

  (* Process a value of type ['a] in a loop while a condition on the loop
   * counter (of type ['b]) is true. The condition is tested initially. *)
  val do_while :
    cond:('a -> 'b -> boolv) code ->
    loop:('a -> 'b -> 'a * 'b) code ->
    'b code -> 'a code -> 'a code
end

(* Implementations must (de)serialize using only the functions above.
 * Possible implementations include:
 * - One for some user-friendly s-expression format;
 * - One for some user-friendly CSV format;
 * - One for some more efficient yet still simple binary encoding. *)
module type DES =
sig
  module IntRepr : INTREPR
  type pointer = IntRepr.SerData.pointer

  val dbool : (pointer -> IntRepr.boolv * pointer) code
  val di8 : (pointer -> IntRepr.i8v * pointer) code

  val tup_opn : typ array -> pointer code -> pointer code
  val tup_cls : typ array -> pointer code -> pointer code
  val tup_sep : typ array -> int (* before *) -> pointer code -> pointer code
  val vec_opn : int -> typ -> pointer code -> pointer code
  val vec_cls : int -> typ -> pointer code -> pointer code
  val vec_sep : int -> typ -> int (* before *) -> pointer code -> pointer code
end

module type SER =
sig
  module IntRepr : INTREPR
  type pointer = IntRepr.SerData.pointer

  val sbool: (IntRepr.boolv * pointer -> pointer) code
  val si8 : (IntRepr.i8v * pointer -> pointer) code

  val tup_opn : typ array -> pointer code -> pointer code
  val tup_cls : typ array -> pointer code -> pointer code
  val tup_sep : typ array -> int (* before *) -> pointer code -> pointer code
  val vec_opn : int -> typ -> pointer code -> pointer code
  val vec_cls : int -> typ -> pointer code -> pointer code
  val vec_sep : int -> typ -> int (* before *) -> pointer code -> pointer code
end

module DesSer (Des : DES) (Ser : SER with module IntRepr = Des.IntRepr) =
struct
  let dsbool src dst =
    let ser = Ser.sbool
    and des = Des.dbool in
    .<
      let v, sp = .~des .~src in
      let dp = .~ser (v, .~dst) in
      sp, dp >.

  let dsi8 src dst =
    let ser = Ser.si8
    and des = Des.di8 in
    .<
      let v, sp = .~des .~src in
      let dp = .~ser (v, .~dst) in
      sp, dp >.

  let rec dstup typs src dst =
    let src = Des.tup_opn typs src
    and dst = Ser.tup_opn typs dst in
    let src_dst =
      BatArray.fold_lefti (fun src_dst i typ ->
        if i = 0 then
          .< .~(desser typ) .~src_dst >.
        else
          .<
            let src, dst = .~src_dst in
            let src = .~(Des.tup_sep typs i .<src>.)
            and dst = .~(Ser.tup_sep typs i .<dst>.) in
            .~(desser typ) (src, dst)
          >.
      ) .< .~src, .~dst >. typs in
    .<
      let src, dst = .~src_dst in
      .~(Des.tup_cls typs .<src>.),
      .~(Ser.tup_cls typs .<dst>.)
    >.

  (* This will generates a long linear code with one block per array
   * item. Maybe have a IntRepr.loop instead? *)
  and dsvec dim typ src dst =
    let desser_typ = desser typ in
    let src = Des.vec_opn dim typ src
    and dst = Ser.vec_opn dim typ dst in
    let rec loop src_dst i =
      if i >= dim then
        .<
          let src, dst = .~src_dst in
          .~(Des.vec_cls dim typ .<src>.),
          .~(Ser.vec_cls dim typ .<dst>.)
        >.
      else if i = 0 then
        loop .< .~desser_typ .~src_dst >. (i + 1)
      else
        .<
          let src, dst = .~src_dst in
          let src = .~(Des.vec_sep dim typ i .<src>.)
          and dst = .~(Ser.vec_sep dim typ i .<dst>.) in
          let src_dst = .~desser_typ (src, dst) in
          .~(loop .<src_dst>. (i + 1))
        >.
    in
    loop .< .~src, .~dst >. 0

  and desser : typ -> (Des.pointer * Ser.pointer ->
                        Des.pointer * Ser.pointer) code =
    function
      | TBool -> .< fun (src, dst) -> .~(dsbool .<src>. .<dst>.) >.
      | TI8 -> .< fun (src, dst) -> .~(dsi8 .<src>. .<dst>.) >.
      | TTup typs -> .< fun (src, dst) -> .~(dstup typs .<src>. .<dst>.) >.
      | TVec (d, typ) -> .< fun (src, dst) -> .~(dsvec d typ .<src>. .<dst>.) >.
      | _ -> assert false
end

(* As the generated code is only temporary (and hard to follow), assertions
 * and backtraces are not that usefull. Rather print as much as possible when
 * failing: *)
let fail ~msg ~cond =
  if not cond then (
    Format.eprintf "FAILURE: %s@." msg ;
    assert false
  )
