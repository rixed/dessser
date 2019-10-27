(* Manual variant without metaocaml: custom code type (hereafter named cod to
 * not conflict with MetaOCaml code type) *)
open Batteries
open Stdint

module Type =
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

let gen_float () = Identifier.make "flt"
let gen_string () = Identifier.make "str"
let gen_bool () = Identifier.make "bool"
let gen_i8 () = Identifier.make "i8"
let gen_u8 () = Identifier.make "u8"
let gen_i16 () = Identifier.make "i16"
let gen_u16 () = Identifier.make "u16"
let gen_u32 () = Identifier.make "u32"
let gen_i32 () = Identifier.make "i32"
let gen_u64 () = Identifier.make "u64"
let gen_i64 () = Identifier.make "i64"
let gen_u128 () = Identifier.make "u128"
let gen_i128 () = Identifier.make "i128"
let gen_tuple () = Identifier.make "tup"
let gen_pointer () = Identifier.make "ptr"
let gen_size () = Identifier.make "sz"
let gen_bit () = Identifier.make "bit"
let gen_byte () = Identifier.make "byte"
let gen_word () = Identifier.make "word"
let gen_dword () = Identifier.make "dword"
let gen_qword () = Identifier.make "qword"
let gen_bytes () = Identifier.make "bytes"
let gen_pair () = Identifier.make "pair"
let gen_auto () = Identifier.make "auto"
let gen_function () = Identifier.make "func"
let gen_param n = Identifier.make ("param"^ string_of_int n)

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
  val print_function0 : output -> Type.t -> (output -> id) -> id
  val print_function1 : output -> Type.t -> Type.t -> (output -> id -> id) -> id
  val print_function2 : output -> Type.t -> Type.t -> Type.t -> (output -> id -> id -> id) -> id
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
  val cat_string : output -> id -> id -> id
  val and_ : output -> id -> id -> id
  val or_ : output -> id -> id -> id
  val make_tuple : output -> Type.t -> id array -> id
  val tuple_get : output -> id -> int -> id

  val length_of_string : output -> id -> id
  val string_of_bytes : output -> id -> id
  val bytes_of_string : output -> id -> id

  val bool_of_const : output -> bool -> id
  val bool_and : output -> id -> id -> id
  val bool_or : output -> id -> id -> id
  val bool_not : output -> id -> id

  (* [cond] must be a function from byte to bool *)
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

module BackEnd_C : BACKEND =
struct
  type output =
    { decl : string IO.output ;
      mutable code : string IO.output ; (* current function body *)
      mutable indent : string ; (* current line prefix *)
      mutable entry_point : bool ;
      mutable funs : (Identifier.t * string) list }

  (* Compound types have to be declared (once) *)

  let type_decls = ref Set.String.empty

  let c_type_of_scalar typ =
    let open Type in
    match typ.structure with
    | TFloat -> "double"
    | TString -> "std::string"
    | TBool -> "bool"
    | TI8 -> "int8_t"
    | TU8 -> "uint8_t"
    | TI16 -> "int16_t"
    | TU16 -> "uint16_t"
    | TI32 -> "int32_t"
    | TU32 -> "uint32_t"
    | TI64 -> "int64_t"
    | TU64 -> "uint64_t"
    | TI128 -> "int128_t"
    | TU128 -> "uint128_t"
    | TPointer -> "Pointer"
    | TSize -> "Size"
    | TBit -> "bool"
    | TByte -> "uint8_t"
    | TWord -> "uint16_t";
    | TDWord -> "uint32_t";
    | TQWord -> "uint64_t";
    | TOWord -> "uint128_t";
    | TBytes
    | TTup _
    | TVec _ ->
        assert false

  let ignore oc id =
    Printf.fprintf oc.code "%s(void)%a;\n" oc.indent
      Identifier.print id

  (* TODO: nullable types should be std::optionals *)
  let rec print_type_decl oc id typ =
    match typ.Type.structure with
    | Type.TTup typs ->
        (* Beware that we might need to print recursively into oc.delc before
         * we are ready to print this one *)
        let s = IO.output_string () in
        Printf.fprintf s "struct %s {\n" id ;
        Array.iteri (fun i typ ->
          let typ_id = find_or_define_type oc typ in
          Printf.fprintf s "  %s field_%d;\n" typ_id i
        ) typs ;
        Printf.fprintf s "};\n\n" ;
        String.print oc.decl (IO.close_out s)
    | _ ->
        ()

  (* Returns the name of the C type for typ: *)
  and find_or_define_type oc typ =
    let uniq_id =
      Printf.sprintf "typ_%d" (Hashtbl.hash typ) in
    if Set.String.mem uniq_id !type_decls then
      uniq_id
    else
      let do_decl () =
        print_type_decl oc uniq_id typ ;
        type_decls := Set.String.add uniq_id !type_decls ;
        uniq_id
      in
      match typ.Type.structure with
      | Type.TTup _ -> do_decl ()
      | _ -> c_type_of_scalar typ

  (* Output: make, print... *)

  let make_output () =
    { decl = IO.output_string () ;
      code = IO.output_string () ;
      indent = "" ;
      entry_point = true ; (* First function emitted will be public *)
      funs = [] }

  let print_output oc output =
    Printf.fprintf oc
      "/* Declarations */\n\n%s\n\n/* Functions */\n\n"
      (IO.close_out output.decl) ;
    List.rev output.funs |>
    List.iter (fun (_fun_id, str) ->
      Printf.fprintf oc "%s\n" str)

  (* - [p] is a printer that returns the id of the result;
   * - [typ] is the return type of that function.
   * Returns the function id. *)
  let print_function0 oc out_typ p =
    let fun_id = gen_function () in
    let out_tname = find_or_define_type oc out_typ in
    let cur_code = oc.code and cur_indent = oc.indent
    and cur_entry_point = oc.entry_point in
    oc.code <- IO.output_string () ;
    oc.indent <- "  " ;
    oc.entry_point <- false ;
    let res_id = p oc in
    let str =
      Printf.sprintf2 "%s%s %a() {\n%s%sreturn %a;\n}\n"
        (if cur_entry_point then "" else "static ")
        out_tname
        Identifier.print fun_id
        (IO.close_out oc.code)
        oc.indent
        Identifier.print res_id in
    oc.code <- cur_code ;
    oc.indent <- cur_indent ;
    oc.funs <- (fun_id, str) :: oc.funs ;
    fun_id

  let print_function1 oc out_typ in_typ0 p =
    let fun_id = gen_function () in
    let out_tname = find_or_define_type oc out_typ in
    let in_tname0 = find_or_define_type oc in_typ0 in
    let param0 = gen_param 0 in
    let cur_code = oc.code and cur_indent = oc.indent
    and cur_entry_point = oc.entry_point in
    oc.code <- IO.output_string () ;
    oc.indent <- "  " ;
    oc.entry_point <- false ;
    let res_id = p oc param0 in
    let str =
      Printf.sprintf2 "%s%s %a(%s %a) {\n%s%sreturn %a;\n}\n"
        (if cur_entry_point then "" else "static ")
        out_tname
        Identifier.print fun_id
        in_tname0
        Identifier.print param0
        (IO.close_out oc.code)
        oc.indent
        Identifier.print res_id in
    oc.code <- cur_code ;
    oc.indent <- cur_indent ;
    oc.funs <- (fun_id, str) :: oc.funs ;
    fun_id

  let print_function2 oc out_typ in_typ0 in_typ1 p =
    let fun_id = gen_function () in
    let out_tname = find_or_define_type oc out_typ in
    let in_tname0 = find_or_define_type oc in_typ0 in
    let in_tname1 = find_or_define_type oc in_typ1 in
    let param0 = gen_param 0 in
    let param1 = gen_param 1 in
    let cur_code = oc.code and cur_indent = oc.indent
    and cur_entry_point = oc.entry_point in
    oc.code <- IO.output_string () ;
    oc.indent <- "  " ;
    oc.entry_point <- false ;
    let res_id = p oc param0 param1 in
    let str =
      Printf.sprintf2 "%s%s %a(%s %a, %s %a) {\n%s%sreturn %a;\n}\n"
        (if cur_entry_point then "" else "static ")
        out_tname
        Identifier.print fun_id
        in_tname0
        Identifier.print param0
        in_tname1
        Identifier.print param1
        (IO.close_out oc.code)
        oc.indent
        Identifier.print res_id in
    oc.code <- cur_code ;
    oc.indent <- cur_indent ;
    oc.funs <- (fun_id, str) :: oc.funs ;
    fun_id

  (* All operations create a new object (and return its identifier),
   * initialized from the given parameters (also identifiers), relying
   * on runtime constructors to perform the actual operation. *)
  let emit_pointer oc p =
    let id = gen_pointer () in
    Printf.fprintf oc.code "%sPointer %a(%t);\n" oc.indent
      Identifier.print id p ;
    id

  let emit_size oc p =
    let id = gen_size () in
    Printf.fprintf oc.code "%sSize %a(%t);\n" oc.indent
      Identifier.print id p ;
    id

  let emit_bit oc b =
    let id = gen_bit () in
    Printf.fprintf oc.code "%sBit %a(%t);\n" oc.indent
      Identifier.print id b ;
    id

  let emit_byte oc b =
    let id = gen_byte () in
    Printf.fprintf oc.code "%sByte %a(%t);\n" oc.indent
      Identifier.print id b ;
    id

  let emit_word oc b =
    let id = gen_word () in
    Printf.fprintf oc.code "%sWord %a(%t);\n" oc.indent
      Identifier.print id b ;
    id

  let emit_dword oc b =
    let id = gen_word () in
    Printf.fprintf oc.code "%sDWord %a(%t);\n" oc.indent
      Identifier.print id b ;
    id

  let emit_qword oc b =
    let id = gen_word () in
    Printf.fprintf oc.code "%sQWord %a(%t);\n" oc.indent
      Identifier.print id b ;
    id

  let emit_oword oc b =
    let id = gen_word () in
    Printf.fprintf oc.code "%sOWord %a(%t);\n" oc.indent
      Identifier.print id b ;
    id

  let emit_bytes oc bs =
    let id = gen_bytes () in
    Printf.fprintf oc.code "%sBytes %a(%t);\n" oc.indent
      Identifier.print id bs ;
    id

  let emit_u8 oc u =
    let id = gen_u8 () in
    Printf.fprintf oc.code "%suint8_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_i8 oc u =
    let id = gen_i8 () in
    Printf.fprintf oc.code "%sint8_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_u16 oc u =
    let id = gen_u16 () in
    Printf.fprintf oc.code "%suint16_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_i16 oc u =
    let id = gen_i16 () in
    Printf.fprintf oc.code "%sint16_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_u32 oc u =
    let id = gen_u32 () in
    Printf.fprintf oc.code "%suint32_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_i32 oc u =
    let id = gen_i32 () in
    Printf.fprintf oc.code "%sint32_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_u64 oc u =
    let id = gen_u64 () in
    Printf.fprintf oc.code "%suint64_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_i64 oc u =
    let id = gen_i64 () in
    Printf.fprintf oc.code "%sint64_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_u128 oc u =
    let id = gen_u128 () in
    Printf.fprintf oc.code "%suint128_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_i128 oc u =
    let id = gen_i128 () in
    Printf.fprintf oc.code "%sint128_t %a(%t);\n" oc.indent
      Identifier.print id u ;
    id

  let emit_float oc p =
    let id = gen_float () in
    Printf.fprintf oc.code "%sdouble %a(%t);\n" oc.indent
      Identifier.print id p ;
    id

  let emit_string oc p =
    let id = gen_string () in
    Printf.fprintf oc.code "%sstd::string %a(%t);\n" oc.indent
      Identifier.print id p ;
    id

  let emit_bool oc p =
    let id = gen_bool () in
    Printf.fprintf oc.code "%sbool %a(%t);\n" oc.indent
      Identifier.print id p ;
    id

  let emit_unit oc p =
    Printf.fprintf oc.code "%s%t;\n" oc.indent p

  let emit_auto oc p =
    let id = gen_auto () in
    Printf.fprintf oc.code "%sauto %a(%t);\n" oc.indent
      Identifier.print id p ;
    id

  let emit_pair typnames oc p =
    let id = gen_pair () in
    Printf.fprintf oc.code "%sstd::pair<%s> %a(%t);\n" oc.indent
      typnames Identifier.print id p ;
    Identifier.cat id ".first", Identifier.cat id ".second"

  (* With the idea that pointers are actually C++ objects with an idea of their
   * base, current position and length. *)
  let make_pointer oc s =
    emit_pointer oc (fun oc -> Identifier.print oc s)

  let pointer_add oc p s =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.skip(%a)"
        Identifier.print p Identifier.print s)

  let pointer_sub oc p1 p2 =
    emit_size oc (fun oc ->
      Printf.fprintf oc "%a - %a"
        Identifier.print p1 Identifier.print p2)

  let rem_size oc p =
    emit_size oc (fun oc ->
      Printf.fprintf oc "%a.remSize()"
        Identifier.print p)

  let byte_of_const oc b =
    emit_byte oc (fun oc -> Int.print oc b)

  let word_of_const oc w =
    emit_word oc (fun oc -> Int.print oc w)

  let dword_of_const oc w =
    emit_dword oc (fun oc -> Uint32.to_string w |> String.print oc)

  let qword_of_const oc w =
    emit_qword oc (fun oc -> Uint64.to_string w |> String.print oc)

  let oword_of_const oc w =
    emit_oword oc (fun oc -> Uint128.to_string w |> String.print oc)

  let size_of_const oc s =
    emit_size oc (fun oc -> Int.print oc s)

  let dword_eq oc w1 w2 =
    emit_bool oc (fun oc ->
      Printf.fprintf oc "%a == %a" Identifier.print w1 Identifier.print w2)

  let size_ge oc s1 s2 =
    emit_bool oc (fun oc ->
      Printf.fprintf oc "%a >= %a" Identifier.print s1 Identifier.print s2)

  let of_string oc s =
    emit_pointer oc (fun oc -> String.print_quoted oc s)

  let bytes_append oc bs b =
    emit_bytes oc (fun oc ->
      Printf.fprintf oc "%a, %a"
        Identifier.print bs
        Identifier.print b)

  let make_bytes oc =
    emit_bytes oc (fun _ -> ())

  let u8_of_byte oc b =
    emit_u8 oc (fun oc ->
      Printf.fprintf oc "%a.to_uint8()"
        Identifier.print b)

  let byte_of_u8 oc u =
    emit_byte oc (fun oc -> Identifier.print oc u)

  let test_bit oc p u =
    (* TODO: rather, try to make as many functions possibles mere
     * constructor, so here have a Bit constructor from Pointer and U8. *)
    emit_bit oc (fun oc ->
      Printf.fprintf oc "%a.getBit(%a)"
        Identifier.print p
        Identifier.print u)

  let read_byte oc p =
    emit_pair "Byte, Pointer" oc (fun oc ->
      Printf.fprintf oc "%a.readByte()"
        Identifier.print p)

  let read_word oc ?(be=false) p =
    emit_pair "Word, Pointer" oc (fun oc ->
      Printf.fprintf oc "%a.readWord%s()"
        Identifier.print p
        (if be then "Be" else "Le"))

  let read_dword oc ?(be=false) p =
    emit_pair "DWord, Pointer" oc (fun oc ->
      Printf.fprintf oc "%a.readDWord%s()"
        Identifier.print p
        (if be then "Be" else "Le"))

  let read_qword oc ?(be=false) p =
    emit_pair "QWord, Pointer" oc (fun oc ->
      Printf.fprintf oc "%a.readQWord%s()"
        Identifier.print p
        (if be then "Be" else "Le"))

  let read_oword oc ?(be=false) p =
    emit_pair "OWord, Pointer" oc (fun oc ->
      Printf.fprintf oc "%a.readOWord%s()"
        Identifier.print p
        (if be then "Be" else "Le"))

  let read_bytes oc p sz =
    emit_pair "Bytes, Pointer" oc (fun oc ->
      Printf.fprintf oc "%a.readBytes(%a)"
        Identifier.print p
        Identifier.print sz)

  let set_bit oc p u b =
    emit_unit oc (fun oc ->
      Printf.fprintf oc "%a.setBit(%a, %a)"
        Identifier.print p
        Identifier.print u
        Identifier.print b)

  let write_byte oc p b =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.writeByte(%a)"
        Identifier.print p
        Identifier.print b)

  let write_word oc ?(be=false) p w =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.writeWord%s(%a)"
        Identifier.print p
        (if be then "Be" else "Le")
        Identifier.print w)

  let write_dword oc ?(be=false) p w =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.writeDWord%s(%a)"
        Identifier.print p
        (if be then "Be" else "Le")
        Identifier.print w)

  let write_qword oc ?(be=false) p w =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.writeQWord%s(%a)"
        Identifier.print p
        (if be then "Be" else "Le")
        Identifier.print w)

  let write_oword oc ?(be=false) p w =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.writeOWord%s(%a)"
        Identifier.print p
        (if be then "Be" else "Le")
        Identifier.print w)

  let write_bytes oc p b =
    emit_pointer oc (fun oc ->
      Printf.fprintf oc "%a.writeBytes(%a)"
        Identifier.print p
        Identifier.print b)

  let peek_byte oc ?at p =
    emit_byte oc (fun oc ->
      Printf.fprintf oc "%a.peekByte(%s)"
        Identifier.print p
        (match at with None -> "0" | Some at -> (at : id :> string)))

  let peek_word oc ?(be=false) ?at p =
    emit_word oc (fun oc ->
      Printf.fprintf oc "%a.peekWord%s(%s)"
        Identifier.print p
        (if be then "Be" else "Le")
        (match at with None -> "0" | Some at -> (at : id :> string)))

  let peek_dword oc ?(be=false) ?at p =
    emit_dword oc (fun oc ->
      Printf.fprintf oc "%a.peekDWord%s(%s)"
        Identifier.print p
        (if be then "Be" else "Le")
        (match at with None -> "0" | Some at -> (at : id :> string)))

  let peek_qword oc ?(be=false) ?at p =
    emit_qword oc (fun oc ->
      Printf.fprintf oc "%a.peekQWord%s(%s)"
        Identifier.print p
        (if be then "Be" else "Le")
        (match at with None -> "0" | Some at -> (at : id :> string)))

  let peek_oword oc ?(be=false) ?at p =
    emit_oword oc (fun oc ->
      Printf.fprintf oc "%a.peekOWord%s(%s)"
        Identifier.print p
        (if be then "Be" else "Le")
        (match at with None -> "0" | Some at -> (at : id :> string)))

  let poke_byte oc p b =
    emit_unit oc (fun oc ->
      Printf.fprintf oc "%a.pokeByte(%a)"
        Identifier.print p
        Identifier.print b)

  module type Emitter =
  sig
    val emit : output -> (string IO.output -> unit) -> id
  end
  module MakeNum (M : Emitter) =
  struct
    let emit = M.emit
    type o = output
    type output = o

    let eq oc t1 t2 =
      emit_bool oc (fun oc -> Printf.fprintf oc "%a == %a"
        Identifier.print t1 Identifier.print t2)

    let ne oc t1 t2 =
      emit_bool oc (fun oc -> Printf.fprintf oc "%a != %a"
        Identifier.print t1 Identifier.print t2)

    let gt oc t1 t2 =
      emit_bool oc (fun oc -> Printf.fprintf oc "%a > %a"
        Identifier.print t1 Identifier.print t2)

    let ge oc t1 t2 =
      emit_bool oc (fun oc -> Printf.fprintf oc "%a >= %a"
        Identifier.print t1 Identifier.print t2)

    let add oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a + %a"
        Identifier.print t1 Identifier.print t2)

    let sub oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a - %a"
        Identifier.print t1 Identifier.print t2)

    let mul  oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a * %a"
        Identifier.print t1 Identifier.print t2)

    let div oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a / %a"
        Identifier.print t1 Identifier.print t2)

    let rem oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a %% %a"
        Identifier.print t1 Identifier.print t2)

    let of_const_int oc d =
      M.emit oc (fun oc -> Int.print oc d)

    let of_byte oc b =
      M.emit oc (fun oc -> Identifier.print oc b)

    let to_byte oc b =
      emit_byte oc (fun oc -> Identifier.print oc b)

    let of_word oc b =
      M.emit oc (fun oc -> Identifier.print oc b)

    let to_word oc b =
      emit_word oc (fun oc -> Identifier.print oc b)

    let of_dword oc b =
      M.emit oc (fun oc -> Identifier.print oc b)

    let to_dword oc b =
      emit_dword oc (fun oc -> Identifier.print oc b)

    let of_qword oc b =
      M.emit oc (fun oc -> Identifier.print oc b)

    let to_qword oc b =
      emit_qword oc (fun oc -> Identifier.print oc b)

    let of_oword oc b =
      M.emit oc (fun oc -> Identifier.print oc b)

    let to_oword oc b =
      emit_oword oc (fun oc -> Identifier.print oc b)
  end

  module MakeInt (M : Emitter) =
  struct
    include MakeNum (M)

    let shift_left oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a << %a"
        Identifier.print t1 Identifier.print t2)

    let shift_right oc t1 t2 =
      M.emit oc (fun oc -> Printf.fprintf oc "%a >> %a"
        Identifier.print t1 Identifier.print t2)
  end

  module Float = MakeNum (struct let emit = emit_float end)
  module U8 = MakeInt (struct let emit = emit_u8 end)
  module I8 = MakeInt (struct let emit = emit_i8 end)
  module U16 = MakeInt (struct let emit = emit_u16 end)
  module I16 = MakeInt (struct let emit = emit_i16 end)
  module U32 = MakeInt (struct let emit = emit_u32 end)
  module I32 = MakeInt (struct let emit = emit_i32 end)
  module U64 = MakeInt (struct let emit = emit_u64 end)
  module I64 = MakeInt (struct let emit = emit_i64 end)
  module U128 = MakeInt (struct let emit = emit_u128 end)
  module I128 = MakeInt (struct let emit = emit_i128 end)

  let float_of_i8 oc i =
    emit_float oc (fun oc -> Identifier.print oc i)

  let float_of_qword oc w =
    emit_float oc (fun oc -> Identifier.print oc w)

  let cat_string oc s1 s2 =
    emit_string oc (fun oc -> Printf.fprintf oc "%a + %a"
      Identifier.print s1 Identifier.print s2)

  let and_ oc b1 b2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a && %a"
      Identifier.print b1 Identifier.print b2)

  let or_ oc b1 b2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a || %a"
      Identifier.print b1 Identifier.print b2)

  let indent_more oc f =
    let cur_indent = oc.indent in
    oc.indent <- oc.indent ^ "  " ;
    f () ;
    oc.indent <- cur_indent

  let make_tuple oc typ fields =
    let id = gen_tuple () in
    (* Construct the type of the result: *)
    let typname = find_or_define_type oc typ in
    Printf.fprintf oc.code "%s%s %a = {" oc.indent
      typname Identifier.print id ;
    indent_more oc (fun () ->
      Array.iteri (fun i id ->
        Printf.fprintf oc.code "%s\n%s%a"
          (if i > 0 then "," else "")
          oc.indent
          Identifier.print id
      ) fields) ;
    Printf.fprintf oc.code "\n%s};\n" oc.indent ;
    id

  let tuple_get oc id n =
    emit_auto oc (fun oc ->
      Printf.fprintf oc "%a.field_%d"
        Identifier.print id n)

  let length_of_string oc s =
    emit_size oc (fun oc ->
      Printf.fprintf oc "%a.size()" Identifier.print s)

  let string_of_bytes oc bs =
    emit_string oc (fun oc ->
      Printf.fprintf oc "%a.toString()" Identifier.print bs)

  let bytes_of_string oc s =
    emit_bytes oc (fun oc -> Identifier.print oc s)

  let bool_of_const oc b =
    emit_bool oc (fun oc -> Bool.print oc b)

  let bool_and oc b1 b2 =
    emit_bool oc (fun oc ->
      Printf.fprintf oc "%a && %a" Identifier.print b1 Identifier.print b2)

  let bool_or oc b1 b2 =
    emit_bool oc (fun oc ->
      Printf.fprintf oc "%a || %a" Identifier.print b1 Identifier.print b2)

  let bool_not oc b =
    emit_bool oc (fun oc ->
      Printf.fprintf oc "!%a" Identifier.print b)

  (* [c] and [a] must have been resolved into indentifiers already, ie. their
   * side effect must have happened already (in practice: only for [c] and [a]
   * with no side effects.) *)
  let choose oc ~cond c a =
    let res_id = gen_auto () in
    (* Lambdas are a trick to allow [c] and [a] to use the local vars the use
     * of auto: *)
    Printf.fprintf oc.code "%sauto %a = (%a ?\n" oc.indent
      Identifier.print res_id
      Identifier.print cond ;
    indent_more oc (fun () ->
      Printf.fprintf oc.code "%s[&]() {\n" oc.indent ;
      indent_more oc (fun () ->
        let res_c = c oc in
        Printf.fprintf oc.code "%sreturn %a;\n" oc.indent
          Identifier.print res_c) ;
      Printf.fprintf oc.code "%s}() : [&]() {\n" oc.indent ;
      indent_more oc (fun () ->
        let res_a = a oc in
        Printf.fprintf oc.code "%sreturn %a;\n" oc.indent
          Identifier.print res_a)) ;
    Printf.fprintf oc.code "%s}());\n" oc.indent ;
    res_id

  (* - [cond] is the identifier of a function from byte to bool;
   * - [reduce] is the identifier of a function from any value and byte to
   *   any value;
   * - [v0] is the initial value;
   * Returns the rediced value and the new pointer. *)
  let read_while oc ~cond ~reduce v0 p =
    let id_ptr = gen_pointer () in
    Printf.fprintf oc.code "%sPointer %a(%a);\n" oc.indent
      Identifier.print id_ptr
      Identifier.print p ;
    let id_res = gen_auto () in
    Printf.fprintf oc.code "%sauto %a = %a;\n" oc.indent
      Identifier.print id_res
      Identifier.print v0 ;
    Printf.fprintf oc.code "%swhile (true) {\n" oc.indent ;
    indent_more oc (fun () ->
      Printf.fprintf oc.code "%sByte nextByte(%a.peekByte(0));\n" oc.indent
        Identifier.print p ;
      Printf.fprintf oc.code "%sif (! %a(nextByte)) break;\n" oc.indent
        Identifier.print cond ;
      Printf.fprintf oc.code "%s%a = %a(%a, nextByte);\n" oc.indent
        Identifier.print id_res
        Identifier.print reduce
        Identifier.print id_res ;
      Printf.fprintf oc.code "%s%a.skip(1);\n" oc.indent
        Identifier.print id_ptr) ;
    Printf.fprintf oc.code "%s}\n" oc.indent ;
    id_res, id_ptr

  (* - [cond] is the name of a function that takes the current value and
   *   cond value and returns a boolean
   * - [loop] is a function that takes the current value and condition
   *   and return the next value and condition value as a pair.
   * - [cond0] and [v0] are the initial values of those values.
   * Returns the aggregated [v0]. *)
  let do_while oc ~cond ~loop cond0 v0 =
    let id_res = gen_auto () in
    Printf.fprintf oc.code "%sauto %a = %a;\n" oc.indent
      Identifier.print id_res
      Identifier.print v0 ;
    Printf.fprintf oc.code "%sauto cond0 = %a;\n" oc.indent
      Identifier.print cond0 ;
    Printf.fprintf oc.code "%swhile (%a(%a, cond0)) {\n" oc.indent
      Identifier.print cond
      Identifier.print id_res ;
    indent_more oc (fun () ->
      Printf.fprintf oc.code "%sauto loop_res = %a(%a, cond0);\n" oc.indent
        Identifier.print loop
        Identifier.print id_res ;
      Printf.fprintf oc.code "%s%a = loop_res.first;\n" oc.indent
        Identifier.print id_res ;
      Printf.fprintf oc.code "%scond0 = loop_res.second;\n" oc.indent) ;
    Printf.fprintf oc.code "%s};\n" oc.indent ;
    id_res

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
    | ObjTuple of id * Type.t array
    | MakeTuple of any_expr array (* Only identifiers *)

  (* Each of these printer returns the id and type *)
  let rec print_any_expr oc = function
    | PointerExpr e -> print_pointer_expr oc e, Type.(make TPointer)
    | SizeExpr e -> print_size_expr oc e, Type.(make TSize)
    | FloatExpr e -> print_float_expr oc e, Type.(make TFloat)
    | I8Expr e -> print_i8_expr oc e, Type.(make TI8)
    | StringExpr e -> print_string_expr oc e, Type.(make TString)
    | BoolExpr e -> print_bool_expr oc e, Type.(make TBool)
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
        id, Type.(make (TTup typs))
    | MakeTuple exprs ->
        let fields =
          Array.map (fun any_e ->
            print_any_expr oc any_e
          ) exprs in
        let ids = Array.map fst fields in
        let typ = Type.(make (TTup (Array.map snd fields))) in
        BackEnd.make_tuple oc typ ids,
        typ
end
