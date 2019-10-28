open Batteries
open Stdint

module Types =
struct
  type t = { nullable : bool ; structure : structure }
  and structure =
    | TFloat
    | TString
    | TBool
    | TU8 | TU16 | TU32 | TU64 | TU128
    | TI8 | TI16 | TI32 | TI64 | TI128
    | TVec of int * t
    | TTup of t array
    (* Special purpose for serialization: *)
    | TPointer
    | TSize
    (* Data accessor, may be just pointer to the actual serialized object: *)
    | TBit | TByte | TWord | TDWord | TQWord | TOWord | TBytes

  let make ?(nullable=false) structure = { nullable ; structure }

  let rec print_structure oc = function
    | TFloat -> String.print oc "Float"
    | TString -> String.print oc "String"
    | TBool -> String.print oc "Bool"
    | TU8 -> String.print oc "U8"
    | TU16 -> String.print oc "U16"
    | TU32 -> String.print oc "U32"
    | TU64 -> String.print oc "U64"
    | TU128 -> String.print oc "U128"
    | TI8 -> String.print oc "I8"
    | TI16 -> String.print oc "I16"
    | TI32 -> String.print oc "I32"
    | TI64 -> String.print oc "I64"
    | TI128 -> String.print oc "I128"
    | TVec (d, typ) ->
        Printf.fprintf oc "%a[%d]" print typ d
    | TTup typs ->
        Printf.fprintf oc "%a"
          (Array.print ~first:"(" ~last:")" ~sep:";" print) typs
    | TPointer -> String.print oc "Pointer"
    | TSize -> String.print oc "Size"
    | TBit -> String.print oc "Bit"
    | TByte -> String.print oc "Byte"
    | TWord -> String.print oc "Word"
    | TDWord -> String.print oc "DWord"
    | TQWord -> String.print oc "QWord"
    | TOWord -> String.print oc "OWord"
    | TBytes -> String.print oc "Bytes"

  and print oc t =
    Printf.fprintf oc "%a%s"
      print_structure t.structure
      (if t.nullable then "?" else "")
end

(* Every expression is bind to an identifiers, and
 * only identifiers are passed around, thus limiting module recursion and
 * linearising the generated code (FWIW). *)
module Identifier :
  sig
    type t = private string
    val print : 'a IO.output -> t -> unit
    val make : string -> t
    val cat : t -> string -> t
  end =
struct
  type t = string

  let print = String.print

  let make =
    let seq = ref (-1) in
    fun pref ->
      incr seq ;
      pref ^ "_" ^ string_of_int !seq

  let cat = (^)
end

type id = Identifier.t

module type NUMERIC =
sig
  type output
  val eq : output -> id -> id -> id
  val ne : output -> id -> id -> id
  val gt : output -> id -> id -> id
  val ge : output -> id -> id -> id
  val add : output -> id -> id -> id
  val sub : output -> id -> id -> id
  val mul : output -> id -> id -> id
  val div : output -> id -> id -> id

  val of_const_int : output -> int -> id
  val of_byte : output -> id -> id
  val to_byte : output -> id -> id
  val of_word : output -> id -> id
  val to_word : output -> id -> id
  val of_dword : output -> id -> id
  val to_dword : output -> id -> id
  val of_qword : output -> id -> id
  val to_qword : output -> id -> id
  val of_oword : output -> id -> id
  val to_oword : output -> id -> id
  val of_string : output -> id -> id
  val to_string : output -> id -> id
end

module type INTEGER =
sig
  include NUMERIC

  val rem : output -> id -> id -> id
  val shift_left : output -> id -> id -> id
  val shift_right : output -> id -> id -> id
end

module type BACKEND =
sig
  (* Depending on the back-end, one might want to write in several sections
   * at the same time and combine them together at the end. For instance,
   * a section for global definitions, for local definitions, and for the
   * code proper. *)
  type output
  val make_output : unit -> output
  val print_function0 : output -> Types.t -> (output -> id) -> id
  val print_function1 : output -> Types.t -> Types.t -> (output -> id -> id) -> id
  val print_function2 : output -> Types.t -> Types.t -> Types.t -> (output -> id -> id -> id) -> id
  val print_output : 'a IO.output -> output -> unit

  val ignore : output -> id -> unit

  val dword_eq : output -> id -> id -> id
  val size_ge : output -> id -> id -> id
  val of_string : output -> string -> id
  val bytes_append : output -> id -> id -> id
  val make_bytes : output -> id
  val u8_of_byte : output -> id -> id
  val byte_of_u8 : output -> id -> id
  val test_bit : output -> id -> id -> id
  val set_bit : output -> id -> id -> id -> unit
  val read_byte : output -> id -> (id * id)
  val read_word : output -> ?be:bool -> id -> (id * id)
  val read_dword : output -> ?be:bool -> id -> (id * id)
  val read_qword : output -> ?be:bool -> id -> (id * id)
  val read_oword : output -> ?be:bool -> id -> (id * id)
  val read_bytes : output -> id -> id -> (id * id)
  val write_byte : output -> id -> id -> id
  val write_word : output -> ?be:bool -> id -> id -> id
  val write_dword : output -> ?be:bool -> id -> id -> id
  val write_qword : output -> ?be:bool -> id -> id -> id
  val write_oword : output -> ?be:bool -> id -> id -> id
  val write_bytes : output -> id -> id -> id
  val peek_byte : output -> ?at:id -> id -> id
  val peek_word : output -> ?be:bool -> ?at:id -> id -> id
  val peek_dword : output -> ?be:bool -> ?at:id -> id -> id
  val peek_qword : output -> ?be:bool -> ?at:id -> id -> id
  val peek_oword : output -> ?be:bool -> ?at:id -> id -> id
  val poke_byte : output -> id -> id -> unit

  val pointer_add : output -> id -> id -> id
  val pointer_sub : output -> id -> id -> id
  val rem_size : output -> id -> id
  val byte_of_const : output -> int -> id
  val word_of_const : output -> int -> id
  val dword_of_const : output -> Uint32.t -> id
  val qword_of_const : output -> Uint64.t -> id
  val oword_of_const : output -> Uint128.t -> id
  val size_of_const : output -> int -> id
  val make_pointer : output -> id -> id
  val float_of_i8 : output -> id -> id
  val float_of_qword : output -> id -> id
  val bytes_of_float : output -> id -> id (* binary repr of the float *)
  val string_of_float : output -> id -> id (* human readable *)
  val cat_string : output -> id -> id -> id
  val and_ : output -> id -> id -> id
  val or_ : output -> id -> id -> id
  val make_tuple : output -> Types.t -> id array -> id
  val tuple_get : output -> id -> int -> id

  val length_of_string : output -> id -> id
  val string_of_bytes : output -> id -> id
  val bytes_of_string : output -> id -> id

  val bool_of_const : output -> bool -> id
  val bool_and : output -> id -> id -> id
  val bool_or : output -> id -> id -> id
  val bool_not : output -> id -> id

  (* [cond] must be a function from byte to bool and each alternative
   * must return an identifier. *)
  val choose :
    output -> cond:id -> (output -> id) -> (output -> id) -> id
  (* [cond] must be a function from byte to bool.
   * [reduce] must be a function from any value and byte into any value. *)
  val read_while : output -> cond:id -> reduce:id -> id -> id -> id * id
  (* [cond] must be a function from byte to bool *)
  val do_while : output -> cond:id -> loop:id -> id -> id -> id

  module Float : NUMERIC with type output = output
  module U8 : INTEGER with type output = output
  module I8 : INTEGER with type output = output
  module U16 : INTEGER with type output = output
  module I16 : INTEGER with type output = output
  module U32 : INTEGER with type output = output
  module I32 : INTEGER with type output = output
  module U64 : INTEGER with type output = output
  module I64 : INTEGER with type output = output
  module U128 : INTEGER with type output = output
  module I128 : INTEGER with type output = output
end

module Expr (BackEnd : BACKEND) =
struct
  module BackEnd = BackEnd
  open BackEnd

  (* Base values: *)
  type value =
    | VFloat of float
    | VString of string
    | VBool of bool
    | VI8 of Int8.t
    | VTuple of value array

  (* Although it is possible to call directly a backend function to
   * generate any code, it is safer to use those typed expressions.
   * Quite verbose though, so not sure it's worth the maintenance
   * effort. *)
  type any_expr =
    | PointerExpr of pointer_expr
    | SizeExpr of size_expr
    | FloatExpr of float_expr
    | I8Expr of i8_expr
    | StringExpr of string_expr
    | BoolExpr of bool_expr
    | TupleExpr of tuple_expr
  (* Expressions of type pointer: *)
  and pointer_expr =
    | ObjPointer of id
    | MakePointer of size_expr
    | AddPointer of pointer_expr * size_expr
  (* Expressions of type size: *)
  and size_expr =
    | ConstSize of int
    | ObjSize of id
    | SubPointer of pointer_expr * pointer_expr
    | RemSize of pointer_expr
  (* Expressions of type float: *)
  and float_expr =
    | ObjFloat of id
    | AddFloat of float_expr * float_expr
    | SubFloat of float_expr * float_expr
    | FloatOfI8 of i8_expr
  and i8_expr =
    | ObjI8 of id
    (* etc, for now let's convert everything into float *)
  (* Expressions of type string: *)
  and string_expr =
    | ObjString of id
    | Concat of string_expr * string_expr
  (* Expressions of type bool: *)
  and bool_expr =
    | ObjBool of id
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr
  and tuple_expr =
    | ObjTuple of id * Types.t array
    | MakeTuple of any_expr array (* Only identifiers *)

  (* Each of these printer returns the id and type *)
  let rec print_any_expr oc = function
    | PointerExpr e -> print_pointer_expr oc e, Types.(make TPointer)
    | SizeExpr e -> print_size_expr oc e, Types.(make TSize)
    | FloatExpr e -> print_float_expr oc e, Types.(make TFloat)
    | I8Expr e -> print_i8_expr oc e, Types.(make TI8)
    | StringExpr e -> print_string_expr oc e, Types.(make TString)
    | BoolExpr e -> print_bool_expr oc e, Types.(make TBool)
    | TupleExpr e -> print_tuple_expr oc e

  and print_pointer_expr oc = function
    | ObjPointer t ->
        t
    | MakePointer s ->
        let st = print_size_expr oc s in
        BackEnd.make_pointer oc st
    | AddPointer (p, s) ->
        let pt = print_pointer_expr oc p
        and st = print_size_expr oc s in
        BackEnd.pointer_add oc pt st

  and print_size_expr oc = function
    | ConstSize s ->
        BackEnd.size_of_const oc s
    | ObjSize t ->
        t
    | SubPointer (p1, p2) ->
        let p1t = print_pointer_expr oc p1
        and p2t = print_pointer_expr oc p2 in
        BackEnd.pointer_sub oc p1t p2t
    | RemSize p ->
        let pt = print_pointer_expr oc p in
        BackEnd.rem_size oc pt

  and print_float_expr oc = function
    | ObjFloat t ->
        t
    | AddFloat (n1, n2) ->
        let n1t = print_float_expr oc n1
        and n2t = print_float_expr oc n2 in
        BackEnd.Float.add oc n1t n2t
    | SubFloat (n1, n2) ->
        let n1t = print_float_expr oc n1
        and n2t = print_float_expr oc n2 in
        BackEnd.Float.sub oc n1t n2t
    | FloatOfI8 n ->
        let nt = print_i8_expr oc n in
        BackEnd.float_of_i8 oc nt

  and print_i8_expr _oc = function
    | ObjI8 t ->
        t

  and print_string_expr oc = function
    | ObjString t ->
        t
    | Concat (s1, s2) ->
        let s1t = print_string_expr oc s1
        and s2t = print_string_expr oc s2 in
        BackEnd.cat_string oc s1t s2t

  and print_bool_expr oc = function
    | ObjBool t ->
        t
    | And (b1, b2) ->
        let b1t = print_bool_expr oc b1
        and b2t = print_bool_expr oc b2 in
        BackEnd.and_ oc b1t b2t
    | Or (b1, b2) ->
        let b1t = print_bool_expr oc b1
        and b2t = print_bool_expr oc b2 in
        BackEnd.or_ oc b1t b2t

  and print_tuple_expr oc = function
    | ObjTuple (id, typs) ->
        id, Types.(make (TTup typs))
    | MakeTuple exprs ->
        let fields =
          Array.map (fun any_e ->
            print_any_expr oc any_e
          ) exprs in
        let ids = Array.map fst fields in
        let typ = Types.(make (TTup (Array.map snd fields))) in
        BackEnd.make_tuple oc typ ids,
        typ
end

module type DES =
sig
  module BE : BACKEND

  type pointer = Identifier.t
  type 'a des = BE.output -> pointer -> 'a * pointer

  val dfloat : Identifier.t des
  val dstring : Identifier.t des
  val dbool : Identifier.t des
  val di8 : Identifier.t des
  val di16 : Identifier.t des
  val di32 : Identifier.t des
  val di64 : Identifier.t des
  val di128 : Identifier.t des
  val du8 : Identifier.t des
  val du16 : Identifier.t des
  val du32 : Identifier.t des
  val du64 : Identifier.t des
  val du128 : Identifier.t des

  val tup_opn : Types.t array -> BE.output -> pointer -> pointer
  val tup_cls : Types.t array -> BE.output -> pointer -> pointer
  val tup_sep : Types.t array -> int (* before *) -> BE.output -> pointer -> pointer
  val vec_opn : int -> Types.t -> BE.output -> pointer -> pointer
  val vec_cls : int -> Types.t -> BE.output -> pointer -> pointer
  val vec_sep : int -> Types.t -> int (* before *) -> BE.output -> pointer -> pointer

  val is_null : Types.structure -> BE.output -> pointer -> Identifier.t
  val dnull : BE.output -> pointer -> pointer
  val dnotnull : BE.output -> pointer -> pointer
end

module type SER =
sig
  module BE : BACKEND

  type pointer = Identifier.t
  type 'a ser = BE.output -> 'a -> pointer -> pointer

  val sfloat : Identifier.t ser
  val sstring : Identifier.t ser
  val sbool: Identifier.t ser
  val si8 : Identifier.t ser
  val si16 : Identifier.t ser
  val si32 : Identifier.t ser
  val si64 : Identifier.t ser
  val si128 : Identifier.t ser
  val su8 : Identifier.t ser
  val su16 : Identifier.t ser
  val su32 : Identifier.t ser
  val su64 : Identifier.t ser
  val su128 : Identifier.t ser

  val tup_opn : Types.t array -> BE.output -> pointer -> pointer
  val tup_cls : Types.t array -> BE.output -> pointer -> pointer
  val tup_sep : Types.t array -> int (* before *) -> BE.output -> pointer -> pointer
  val vec_opn : int -> Types.t -> BE.output -> pointer -> pointer
  val vec_cls : int -> Types.t -> BE.output -> pointer -> pointer
  val vec_sep : int -> Types.t -> int (* before *) -> BE.output -> pointer -> pointer

  val snull : BE.output -> pointer -> pointer
  val snotnull : BE.output -> pointer -> pointer
end

(* Many return values have the type of a pair or src*dst pointers: *)
let t_pair_ptrs = Types.(make (TTup [| make TPointer ; make TPointer |]))

module DesSer (Des : DES) (Ser : SER with module BE = Des.BE) =
struct
  module BE = Des.BE

  let ds ser des oc src dst =
    let v, src = des oc src in
    let dst = ser oc v dst in
    BE.make_tuple oc t_pair_ptrs [| src ; dst |]

  let dsfloat = ds Ser.sfloat Des.dfloat
  let dsstring = ds Ser.sstring Des.dstring
  let dsbool = ds Ser.sbool Des.dbool
  let dsi8 = ds Ser.si8 Des.di8
  let dsi16 = ds Ser.si16 Des.di16
  let dsi32 = ds Ser.si32 Des.di32
  let dsi64 = ds Ser.si64 Des.di64
  let dsi128 = ds Ser.si128 Des.di128
  let dsu8 = ds Ser.su8 Des.du8
  let dsu16 = ds Ser.su16 Des.du16
  let dsu32 = ds Ser.su32 Des.du32
  let dsu64 = ds Ser.su64 Des.du64
  let dsu128 = ds Ser.su128 Des.du128

  let dsnull oc src dst =
    Des.dnull oc src,
    Ser.snull oc dst

  let dsnotnull oc src dst =
    Des.dnotnull oc src,
    Ser.snotnull oc dst

  let rec dstup typs oc src dst =
    let src = Des.tup_opn typs oc src
    and dst = Ser.tup_opn typs oc dst in
    let src, dst =
      BatArray.fold_lefti (fun (src, dst) i typ ->
        if i = 0 then
          desser typ oc src dst
        else
          let src = Des.tup_sep typs i oc src
          and dst = Ser.tup_sep typs i oc dst in
          desser typ oc src dst
      ) (src, dst) typs in
    BE.make_tuple oc t_pair_ptrs [|
      Des.tup_cls typs oc src ;
      Ser.tup_cls typs oc dst |]

  (* This will generates a long linear code with one block per array
   * item. Maybe have a IntRepr.loop instead? *)
  and dsvec dim typ oc src dst =
    let desser_typ = desser typ oc in
    let src = Des.vec_opn dim typ oc src
    and dst = Ser.vec_opn dim typ oc dst in
    let rec loop src dst i =
      if i >= dim then
        BE.make_tuple oc t_pair_ptrs [|
          Des.vec_cls dim typ oc src ;
          Ser.vec_cls dim typ oc dst |]
      else if i = 0 then
        let src, dst = desser_typ src dst in
        loop src dst (i + 1)
      else
        let src = Des.vec_sep dim typ i oc src
        and dst = Ser.vec_sep dim typ i oc dst in
        let src, dst = desser_typ src dst in
        loop src dst (i + 1)
    in
    loop src dst 0

  and desser : Types.t -> BE.output -> Identifier.t -> Identifier.t ->
                 (Identifier.t * Identifier.t) = fun typ oc src dst ->
    let desser_structure = function
      | Types.TFloat -> dsfloat
      | Types.TString -> dsstring
      | Types.TBool -> dsbool
      | Types.TI8 -> dsi8
      | Types.TI16 -> dsi16
      | Types.TI32 -> dsi32
      | Types.TI64 -> dsi64
      | Types.TI128 -> dsi128
      | Types.TU8 -> dsu8
      | Types.TU16 -> dsu16
      | Types.TU32 -> dsu32
      | Types.TU64 -> dsu64
      | Types.TU128 -> dsu128
      | Types.TTup typs -> dstup typs
      | Types.TVec (d, typ) -> dsvec d typ
      | _ -> assert false
    in
    let pair =
      if typ.nullable then
        let cond = Des.is_null typ.structure oc src in
        BE.choose oc ~cond
          (fun oc ->
            let s, d = dsnull oc src dst in
            BE.make_tuple oc t_pair_ptrs [| s ; d |])
          (fun oc ->
            let s, d = dsnotnull oc src dst in
            desser_structure typ.structure oc s d)
      else
        desser_structure typ.structure oc src dst in
    (* Returns src and dest: *)
    BE.tuple_get oc pair 0,
    BE.tuple_get oc pair 1
end
