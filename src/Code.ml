(* Manual variant without metaocaml: custom code type (hereafter named cod to
 * not conflict with MetaOCaml code type) *)
open Batteries
open Stdint

(* Every expression is bind to an identifiers, and
 * only identifiers are passed around, thus limiting module recursion and
 * linearising the generated code (FWIW).
 * Identifiers have a hantom type attached so type checking is not relaxed. *)
module Identifier :
  sig
    type +'a u = private string

    module Pointer : sig
      type t = [`Pointer] u
      val make : unit -> t
      val print : 'a IO.output -> t -> unit
    end
    module Size : sig
      type t = [`Size] u
      val make : unit -> t
      val print : 'a IO.output -> t -> unit
    end
    module Float : sig
      type t = [`Float] u
      val make : unit -> t
      val print : 'a IO.output -> t -> unit
    end
    module String : sig
      type t = [`String] u
      val make : unit -> t
      val print : 'a IO.output -> t -> unit
    end
    module Bool : sig
      type t = [`Bool] u
      val make : unit -> t
      val print : 'a IO.output -> t -> unit
    end
    module I8 : sig
      type t = [`I8] u
      val make : unit -> t
      val print : 'a IO.output -> t -> unit
    end
  end =
struct
  let identifier =
    let seq = ref (-1) in
    fun pref ->
      incr seq ;
      pref ^ "_" ^ string_of_int !seq

  type 'a u = string

  module Pointer =
  struct
    type t = [`Pointer] u
    let make () = identifier "ptr"
    let print = String.print
  end
  module Size =
  struct
    type t = [`Size] u
    let make () = identifier "sz"
    let print = String.print
  end
  module Float =
  struct
    type t = [`Float] u
    let make () = identifier "flt"
    let print = String.print
  end
  module String =
  struct
    type t = [`String] u
    let make () = identifier "str"
    let print = String.print
  end
  module Bool =
  struct
    type t = [`Bool] u
    let make () = identifier "bool"
    let print = String.print
  end
  module I8 =
  struct
    type t = [`I8] u
    let make () = identifier "i8"
    let print = String.print
  end
end

module type BACKEND =
sig
  (* Depending on the back-end, one might want to write in several sections
   * at the same time and combine them together at the end. For instance,
   * a section for global definitions, for local definitions, and for the
   * code proper. *)
  type output
  val make_output : unit -> output
  val print_output : 'a IO.output -> output -> unit

  val add_pointer : output -> Identifier.Pointer.t -> Identifier.Size.t -> Identifier.Pointer.t
  val sub_pointer : output -> Identifier.Pointer.t -> Identifier.Pointer.t -> Identifier.Size.t
  val rem_size : output -> Identifier.Pointer.t -> Identifier.Size.t
  val size_of_const : output -> int -> Identifier.Size.t
  val make_pointer : output -> Identifier.Size.t -> Identifier.Pointer.t
  val add_float : output -> Identifier.Float.t -> Identifier.Float.t -> Identifier.Float.t
  val sub_float : output -> Identifier.Float.t -> Identifier.Float.t -> Identifier.Float.t
  val float_of_i8 : output -> Identifier.I8.t -> Identifier.Float.t
  val cat_string : output -> Identifier.String.t -> Identifier.String.t -> Identifier.String.t
  val and_ : output -> Identifier.Bool.t -> Identifier.Bool.t -> Identifier.Bool.t
  val or_ : output -> Identifier.Bool.t -> Identifier.Bool.t -> Identifier.Bool.t
end

module BackEnd_C : BACKEND =
struct
  type output = string IO.output

  let make_output () = IO.output_string ()

  let print_output oc output =
    String.print oc (IO.close_out output)

  (* All operations create a new object (and return its identifier),
   * initialized from the given parameters (also identifiers), relying
   * on runtime constructors to perform the actual operation. *)
  let emit_pointer oc p =
    let id = Identifier.Pointer.make () in
    Printf.fprintf oc "Pointer %a(%t); "
      Identifier.Pointer.print id p ;
    id

  let emit_size oc p =
    let id = Identifier.Size.make () in
    Printf.fprintf oc "Size %a(%t); "
      Identifier.Size.print id p ;
    id

  let emit_float oc p =
    let id = Identifier.Float.make () in
    Printf.fprintf oc "double %a(%t); "
      Identifier.Float.print id p ;
    id

  let emit_string oc p =
    let id = Identifier.String.make () in
    Printf.fprintf oc "std::string %a(%t); "
      Identifier.String.print id p ;
    id

  let emit_bool oc p =
    let id = Identifier.Bool.make () in
    Printf.fprintf oc "bool %a(%t); "
      Identifier.Bool.print id p ;
    id

  (* With the idea that pointers are actually C++ objects with an idea of their
   * base, current position and length. *)
  let make_pointer oc s =
    emit_pointer oc (fun oc -> Printf.fprintf oc "%a"
      Identifier.Size.print s)

  let add_pointer oc p s =
    emit_pointer oc (fun oc -> Printf.fprintf oc "%a + %a"
      Identifier.Pointer.print p Identifier.Size.print s)

  let sub_pointer oc p1 p2 =
    emit_size oc (fun oc -> Printf.fprintf oc "%a - %a"
      Identifier.Pointer.print p1 Identifier.Pointer.print p2)

  let rem_size oc p =
    emit_size oc (fun oc -> Printf.fprintf oc "(%a).remSize()"
      Identifier.Pointer.print p)

  let size_of_const oc s =
    emit_size oc (fun oc -> Int.print oc s)

  let add_float oc t1 t2 =
    emit_float oc (fun oc -> Printf.fprintf oc "%a + %a"
      Identifier.Float.print t1 Identifier.Float.print t2)

  let sub_float oc t1 t2 =
    emit_float oc (fun oc -> Printf.fprintf oc "%a - %a"
      Identifier.Float.print t1 Identifier.Float.print t2)

  let float_of_i8 oc i =
    emit_float oc (fun oc -> Identifier.I8.print oc i)

  let cat_string oc s1 s2 =
    emit_string oc (fun oc -> Printf.fprintf oc "%a + %a"
      Identifier.String.print s1 Identifier.String.print s2)

  let and_ oc b1 b2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a && %a"
      Identifier.Bool.print b1 Identifier.Bool.print b2)

  let or_ oc b1 b2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a || %a"
      Identifier.Bool.print b1 Identifier.Bool.print b2)
end

module Expr (BackEnd : BACKEND) =
struct
  module BackEnd = BackEnd
  open BackEnd

  (* Base values: *)
  type value =
    (* The expression can be an immediate value from the user *)
    | VFloat of float
    | VString of string
    | VBool of bool
    | VI8 of Int8.t

  (* Expressions of type pointer: *)
  type pointer_expr =
    | ObjPointer of Identifier.Pointer.t
    | MakePointer of size_expr
    | AddPointer of pointer_expr * size_expr
  (* Expressions of type size: *)
  and size_expr =
    | ConstSize of int
    | ObjSize of Identifier.Size.t
    | SubPointer of pointer_expr * pointer_expr
    | RemSize of pointer_expr
  (* Expressions of type float: *)
  and  float_expr =
    | ObjFloat of Identifier.Float.t
    | AddFloat of float_expr * float_expr
    | SubFloat of float_expr * float_expr
    | FloatOfI8 of i8_expr
  and i8_expr =
    | ObjI8 of Identifier.I8.t
    (* etc, for now let's convert everything into float *)
  (* Expressions of type string: *)
  and string_expr =
    | ObjString of Identifier.String.t
    | Concat of string_expr * string_expr
  (* Expressions of type bool: *)
  and bool_expr =
    | ObjBool of Identifier.Bool.t
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

  let rec print_pointer_expr oc = function
    | ObjPointer t ->
        t
    | MakePointer s ->
        let st = print_size_expr oc s in
        BackEnd.make_pointer oc st
    | AddPointer (p, s) ->
        let pt = print_pointer_expr oc p
        and st = print_size_expr oc s in
        BackEnd.add_pointer oc pt st

  and print_size_expr oc = function
    | ConstSize s ->
        BackEnd.size_of_const oc s
    | ObjSize t ->
        t
    | SubPointer (p1, p2) ->
        let p1t = print_pointer_expr oc p1
        and p2t = print_pointer_expr oc p2 in
        BackEnd.sub_pointer oc p1t p2t
    | RemSize p ->
        let pt = print_pointer_expr oc p in
        BackEnd.rem_size oc pt

  and print_float_expr oc = function
    | ObjFloat t ->
        t
    | AddFloat (n1, n2) ->
        let n1t = print_float_expr oc n1
        and n2t = print_float_expr oc n2 in
        BackEnd.add_float oc n1t n2t
    | SubFloat (n1, n2) ->
        let n1t = print_float_expr oc n1
        and n2t = print_float_expr oc n2 in
        BackEnd.sub_float oc n1t n2t
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
end
