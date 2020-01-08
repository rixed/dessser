open Batteries
open Stdint
open Dessser

let preferred_file_extension = "cc"

type output =
  { mutable code : string IO.output ; (* current function body *)
    mutable indent : string ; (* current line prefix *)
    mutable entry_point : bool ;
    (* When building heap values, remember the base type name (for casts): *)
    mutable value_typname : string ;
    (* name and definition: *)
    mutable funs : (string * string) list ;
    (* type names and declarations: *)
    mutable type_decls : (string * string) list }

let rec c_type_of_scalar = function
  | Type.TValue ValueType.(NotNullable TFloat | Nullable TFloat) -> "double"
  | Type.TValue ValueType.(NotNullable TString | Nullable TString) -> "std::string"
  | Type.TValue ValueType.(NotNullable TBool | Nullable TBool) -> "bool"
  | Type.TValue ValueType.(NotNullable TChar | Nullable TChar) -> "char"
  | Type.TValue ValueType.(NotNullable TI8 | Nullable TI8) -> "int8_t"
  | Type.TValue ValueType.(NotNullable TU8 | Nullable TU8) -> "uint8_t"
  | Type.TValue ValueType.(NotNullable TI16 | Nullable TI16) -> "int16_t"
  | Type.TValue ValueType.(NotNullable TU16 | Nullable TU16) -> "uint16_t"
  | Type.TValue ValueType.(NotNullable TI32 | Nullable TI32) -> "int32_t"
  | Type.TValue ValueType.(NotNullable TU32 | Nullable TU32) -> "uint32_t"
  | Type.TValue ValueType.(NotNullable TI64 | Nullable TI64) -> "int64_t"
  | Type.TValue ValueType.(NotNullable TU64 | Nullable TU64) -> "uint64_t"
  | Type.TValue ValueType.(NotNullable TI128 | Nullable TI128) -> "int128_t"
  | Type.TValue ValueType.(NotNullable TU128 | Nullable TU128) -> "uint128_t"
  (* Not scalars: *)
  | Type.TValue ValueType.(NotNullable (TTup _ | TRec _) | Nullable (TTup _ | TRec _))
  | Type.TPair _
  | Type.TFunction0 _
  | Type.TFunction1 _
  | Type.TFunction2 _
  | Type.TFunction3 _ ->
      assert false
  (* Treated as a scalar here: *)
  | Type.TValue ValueType.(NotNullable TVec (dim, typ) | Nullable TVec (dim, typ)) ->
      Printf.sprintf "Vec<%d, %s>" dim (c_type_of_scalar (Type.TValue typ))
  | Type.TValue ValueType.(NotNullable TList typ | Nullable TList typ) ->
      Printf.sprintf "List<%s>" (c_type_of_scalar (Type.TValue typ))
  (* The caller does not know if it's a pointer used for reading/writing bytes
   * or setting/getting subfields, which is a good thing as it allows to
   * combine freely actual serializers and value "reifiers".
   * So here both types of pointer should be allowed.  *)
  | Type.TPointer -> "Pointer"
  | Type.TSize -> "Size"
  | Type.TBit -> "bool"
  | Type.TByte -> "uint8_t"
  | Type.TWord -> "uint16_t"
  | Type.TDWord -> "uint32_t"
  | Type.TQWord -> "uint64_t"
  | Type.TOWord -> "uint128_t"
  | Type.TBytes -> "Bytes"

let ignore oc id =
  Printf.fprintf oc.code "%s(void)%a;\n" oc.indent
    Identifier.print id

let comment oc fmt =
  Printf.fprintf oc.code ("%s/* " ^^ fmt ^^ " */\n") oc.indent

let dump oc lst =
  Printf.fprintf oc.code "%sstd::cout << %a << std::endl;\n" oc.indent
    (List.print ~first:"" ~last:"" ~sep:" << " Identifier.print) lst

(* TODO: *)
let make_valid_identifier s = s

(* Returns the name and valuetype of that subfield of the given valuetype: *)
let subfield_info vtyp idx =
  match vtyp with
  | ValueType.(Nullable (TTup typs) | NotNullable (TTup typs)) ->
      "field_"^ string_of_int idx,
      typs.(idx)
  | ValueType.(Nullable (TRec typs) | NotNullable (TRec typs)) ->
      make_valid_identifier (fst typs.(idx)),
      snd typs.(idx)
  | ValueType.(Nullable (TVec (dim, typ)) | NotNullable (TVec (dim, typ))) ->
      assert (idx < dim) ;
      "["^ string_of_int idx ^"]",
      typ
  | _ ->
      (* Scalar types have no subfields: *)
      assert false

let rec declare_type oc id typ =
  (* Beware that we might need to print recursively into oc.decl before
   * we are ready to print this one *)
  let s = IO.output_string () in
  let print_record vtyp vsubtyps =
    Printf.fprintf s "struct %s {\n" id ;
    Array.iteri (fun i subtyp ->
      let typ_id = find_or_declare_type oc (Type.TValue subtyp) in
      let fname, subtyp = subfield_info vtyp i in
      if ValueType.is_nullable subtyp then
        Printf.fprintf s "  std::optional<%s> %s;\n" typ_id fname
      else
        Printf.fprintf s "  %s %s;\n" typ_id fname
    ) vsubtyps ;
    Printf.fprintf s "};\n\n"
  in
  (match typ with
  | Type.TValue (ValueType.(NotNullable TTup vsubtyps | Nullable TTup vsubtyps) as vtyp) ->
      print_record vtyp vsubtyps
  | Type.TValue (ValueType.(NotNullable TRec vsubtyps | Nullable TRec vsubtyps) as vtyp) ->
      print_record vtyp (Array.map snd vsubtyps)
  | Type.TPair (t1, t2) ->
      Printf.fprintf s "typedef std::pair<%s, %s> %s;\n\n"
        (find_or_declare_type oc t1)
        (find_or_declare_type oc t2)
        id
  | _ ->
      ()) ;
  oc.type_decls <- (id, IO.close_out s) :: oc.type_decls

(* Returns the name of the C type for typ: *)
and find_or_declare_type oc typ =
  let uniq_id =
    Printf.sprintf "typ_%d" (Hashtbl.hash typ) in
  if List.mem_assoc uniq_id oc.type_decls then
    uniq_id
  else
    let do_decl () =
      declare_type oc uniq_id typ ;
      uniq_id
    in
    match typ with
    | Type.TValue ValueType.(NotNullable TTup _ | Nullable TTup _)
    | Type.TValue ValueType.(NotNullable TRec _ | Nullable TRec _)
    | Type.TPair _ ->
        do_decl ()
    | _ ->
        c_type_of_scalar typ

(* Output: make, print... *)

let make_output () =
  { code = IO.output_string () ;
    indent = "" ;
    entry_point = true ; (* First function emitted will be public *)
    value_typname = "" ;
    funs = [] ;
    type_decls = [] }

let print_output oc output =
  Printf.fprintf oc "#include \"dessser/runtime.h\"" ;
  Printf.fprintf oc "\n/* Type Declarations */\n\n" ;
  List.rev output.type_decls |>
  List.iter (fun (_type_id, str) ->
    Printf.fprintf oc "%s\n" str) ;
  Printf.fprintf oc "\n/* Functions */\n\n" ;
  List.rev output.funs |>
  List.iter (fun (_fun_id, str) ->
    Printf.fprintf oc "%s\n" str)

(* - [p] is a printer that returns the id of the result;
 * - [typ] is the return type of that function.
 * Returns the function id. *)
let function0 oc out_typ p =
  let fun_id = Identifier.func0 () in
  let out_tname = find_or_declare_type oc out_typ in
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
  oc.funs <- ((fun_id : _ Identifier.t :> string), str) :: oc.funs ;
  oc.entry_point <- cur_entry_point ;
  fun_id

let function1 oc in_typ0 out_typ p =
  let out_tname = find_or_declare_type oc out_typ in
  let in_tname0 = find_or_declare_type oc in_typ0 in
  let param0 = Identifier.param 0 () in
  let cur_code = oc.code and cur_indent = oc.indent
  and cur_entry_point = oc.entry_point in
  oc.code <- IO.output_string () ;
  oc.indent <- "  " ;
  oc.entry_point <- false ;
  let res_id = p oc param0 in
  let fun_id = Identifier.func1 param0 res_id in
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
  oc.funs <- ((fun_id : _ Identifier.t :> string), str) :: oc.funs ;
  oc.entry_point <- cur_entry_point ;
  fun_id

let function2 oc in_typ0 in_typ1 out_typ p =
  let out_tname = find_or_declare_type oc out_typ in
  let in_tname0 = find_or_declare_type oc in_typ0 in
  let in_tname1 = find_or_declare_type oc in_typ1 in
  let param0 = Identifier.param 0 () in
  let param1 = Identifier.param 1 () in
  let cur_code = oc.code and cur_indent = oc.indent
  and cur_entry_point = oc.entry_point in
  oc.code <- IO.output_string () ;
  oc.indent <- "  " ;
  oc.entry_point <- false ;
  let res_id = p oc param0 param1 in
  let fun_id = Identifier.func2 param0 param1 res_id in
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
  oc.funs <- ((fun_id : _ Identifier.t :> string), str) :: oc.funs ;
  oc.entry_point <- cur_entry_point ;
  fun_id

(* All operations create a new object (and return its identifier),
 * initialized from the given parameters (also identifiers), relying
 * on runtime constructors to perform the actual operation. *)
let emit_pointer oc p =
  let id = Identifier.pointer () in
  Printf.fprintf oc.code "%sPointer %a(%t);\n" oc.indent
    Identifier.print id p ;
  id

let emit_size oc p =
  let id = Identifier.size () in
  Printf.fprintf oc.code "%sSize %a(%t);\n" oc.indent
    Identifier.print id p ;
  id

let emit_bit oc b =
  let id = Identifier.bit () in
  Printf.fprintf oc.code "%sBit %a(%t);\n" oc.indent
    Identifier.print id b ;
  id

let emit_byte oc b =
  let id = Identifier.byte () in
  Printf.fprintf oc.code "%sByte %a(%t);\n" oc.indent
    Identifier.print id b ;
  id

let emit_word oc b =
  let id = Identifier.word () in
  Printf.fprintf oc.code "%sWord %a(%t);\n" oc.indent
    Identifier.print id b ;
  id

let emit_dword oc b =
  let id = Identifier.dword () in
  Printf.fprintf oc.code "%sDWord %a(%t);\n" oc.indent
    Identifier.print id b ;
  id

let emit_qword oc b =
  let id = Identifier.qword () in
  Printf.fprintf oc.code "%sQWord %a(%t);\n" oc.indent
    Identifier.print id b ;
  id

let emit_oword oc b =
  let id = Identifier.oword () in
  Printf.fprintf oc.code "%sOWord %a(%t);\n" oc.indent
    Identifier.print id b ;
  id

let emit_bytes oc bs =
  let id = Identifier.bytes () in
  Printf.fprintf oc.code "%sBytes %a(%t);\n" oc.indent
    Identifier.print id bs ;
  id

let emit_char oc u =
  let id = Identifier.char () in
  Printf.fprintf oc.code "%suint8_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_u8 oc u =
  let id = Identifier.u8 () in
  Printf.fprintf oc.code "%suint8_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_i8 oc u =
  let id = Identifier.i8 () in
  Printf.fprintf oc.code "%sint8_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_u16 oc u =
  let id = Identifier.u16 () in
  Printf.fprintf oc.code "%suint16_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_i16 oc u =
  let id = Identifier.i16 () in
  Printf.fprintf oc.code "%sint16_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_u32 oc u =
  let id = Identifier.u32 () in
  Printf.fprintf oc.code "%suint32_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_i32 oc u =
  let id = Identifier.i32 () in
  Printf.fprintf oc.code "%sint32_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_u64 oc u =
  let id = Identifier.u64 () in
  Printf.fprintf oc.code "%suint64_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_i64 oc u =
  let id = Identifier.i64 () in
  Printf.fprintf oc.code "%sint64_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_u128 oc u =
  let id = Identifier.u128 () in
  Printf.fprintf oc.code "%suint128_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_i128 oc u =
  let id = Identifier.i128 () in
  Printf.fprintf oc.code "%sint128_t %a(%t);\n" oc.indent
    Identifier.print id u ;
  id

let emit_float oc p =
  let id = Identifier.float () in
  Printf.fprintf oc.code "%sdouble %a(%t);\n" oc.indent
    Identifier.print id p ;
  id

let emit_string oc p =
  let id = Identifier.string () in
  Printf.fprintf oc.code "%sstd::string %a(%t);\n" oc.indent
    Identifier.print id p ;
  id

let emit_bool oc p =
  let id = Identifier.bool () in
  Printf.fprintf oc.code "%sbool %a(%t);\n" oc.indent
    Identifier.print id p ;
  id

let emit_unit oc p =
  Printf.fprintf oc.code "%s%t;\n" oc.indent p

let emit_auto oc p =
  let id = Identifier.auto () in
  Printf.fprintf oc.code "%sauto %a(%t);\n" oc.indent
    Identifier.print id p ;
  id

let emit_pair t1 t2 oc p =
  let id = Identifier.pair () in
  let tname1 = find_or_declare_type oc t1 in
  let tname2 = find_or_declare_type oc t2 in
  Printf.fprintf oc.code "%sstd::pair<%s, %s> %a(%t);\n" oc.indent
    tname1 tname2 Identifier.print id p ;
  Identifier.(modify "" id ".first" |> of_any t1),
  Identifier.(modify "" id ".second" |> of_any t2)

let pointer_add oc p s =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "%a.skip(%a)"
      Identifier.print p Identifier.print s)

let pointer_sub oc p1 p2 =
  emit_size oc (fun oc ->
    Printf.fprintf oc "%a - %a"
      Identifier.print p1 Identifier.print p2)

let size_add oc s1 s2 =
  emit_size oc (fun oc ->
    Printf.fprintf oc "%a + %a"
      Identifier.print s1 Identifier.print s2)

let size_to_string oc s =
  emit_string oc (fun oc -> Printf.fprintf oc "std::to_string(%a)"
    Identifier.print s)

let rem_size oc p =
  emit_size oc (fun oc ->
    Printf.fprintf oc "%a.remSize()"
      Identifier.print p)

let bit_of_const oc b =
  emit_bit oc (fun oc -> String.print oc (if b then "true" else "false"))

let byte_of_const oc b =
  emit_byte oc (fun oc -> Int.print oc b)

let word_of_const oc w =
  emit_word oc (fun oc -> Int.print oc w)

let dword_of_const oc w =
  emit_dword oc (fun oc -> Printf.fprintf oc "%sU" (Uint32.to_string w))

let qword_of_const oc w =
  emit_qword oc (fun oc -> Printf.fprintf oc "%sUL" (Uint64.to_string w))

let oword_of_const oc w =
  let lo = Uint128.to_uint64 w
  and hi = Uint128.(to_uint64 (shift_right_logical w 64)) in
  emit_oword oc (fun oc ->
    Printf.fprintf oc "((((uint128_t)%sULL) << 64U) | %sULL)"
      (Uint64.to_string hi)
      (Uint64.to_string lo))

let size_of_const oc s =
  emit_size oc (fun oc -> Int.print oc s)

let size_of_u32 oc v =
  emit_size oc (fun oc -> Identifier.print oc v)

let u32_of_size oc v =
  emit_u32 oc (fun oc -> Identifier.print oc v)

let dword_eq oc w1 w2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a == %a" Identifier.print w1 Identifier.print w2)

let size_ge oc s1 s2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a >= %a" Identifier.print s1 Identifier.print s2)

let pointer_of_string oc s =
  emit_pointer oc (fun oc -> String.print_quoted oc s)

let bytes_append oc bs b =
  emit_bytes oc (fun oc ->
    Printf.fprintf oc "%a, %a"
      Identifier.print bs
      Identifier.print b)

let u8_of_byte oc b =
  emit_u8 oc (fun oc ->
    Printf.fprintf oc "%a.to_uint8()"
      Identifier.print b)

let byte_of_u8 oc u =
  emit_byte oc (fun oc -> Identifier.print oc u)

let test_bit oc p u =
  (* TODO: rather, try to make as many functions as possibles mere
   * constructor, so here have a Bit constructor from Pointer and U8. *)
  emit_bit oc (fun oc ->
    Printf.fprintf oc "%a.getBit(%a)"
      Identifier.print p
      Identifier.print u)

let read_byte oc p =
  emit_pair TByte TPointer oc (fun oc ->
    Printf.fprintf oc "%a.readByte()"
      Identifier.print p)

let read_word oc ?(be=false) p =
  emit_pair TWord TPointer oc (fun oc ->
    Printf.fprintf oc "%a.readWord%s()"
      Identifier.print p
      (if be then "Be" else "Le"))

let read_dword oc ?(be=false) p =
  emit_pair TDWord TPointer oc (fun oc ->
    Printf.fprintf oc "%a.readDWord%s()"
      Identifier.print p
      (if be then "Be" else "Le"))

let read_qword oc ?(be=false) p =
  emit_pair TQWord TPointer oc (fun oc ->
    Printf.fprintf oc "%a.readQWord%s()"
      Identifier.print p
      (if be then "Be" else "Le"))

let read_oword oc ?(be=false) p =
  emit_pair TOWord TPointer oc (fun oc ->
    Printf.fprintf oc "%a.readOWord%s()"
      Identifier.print p
      (if be then "Be" else "Le"))

let read_bytes oc p sz =
  emit_pair TBytes TPointer oc (fun oc ->
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

let blit_bytes oc p b sz =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "%a.blitBytes(%a, %a)"
      Identifier.print p
      Identifier.print b
      Identifier.print sz)

let peek_byte oc ?at p =
  emit_byte oc (fun oc ->
    Printf.fprintf oc "%a.peekByte(%s)"
      Identifier.print p
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_word oc ?(be=false) ?at p =
  emit_word oc (fun oc ->
    Printf.fprintf oc "%a.peekWord%s(%s)"
      Identifier.print p
      (if be then "Be" else "Le")
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_dword oc ?(be=false) ?at p =
  emit_dword oc (fun oc ->
    Printf.fprintf oc "%a.peekDWord%s(%s)"
      Identifier.print p
      (if be then "Be" else "Le")
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_qword oc ?(be=false) ?at p =
  emit_qword oc (fun oc ->
    Printf.fprintf oc "%a.peekQWord%s(%s)"
      Identifier.print p
      (if be then "Be" else "Le")
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_oword oc ?(be=false) ?at p =
  emit_oword oc (fun oc ->
    Printf.fprintf oc "%a.peekOWord%s(%s)"
      Identifier.print p
      (if be then "Be" else "Le")
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let poke_byte oc p b =
  emit_unit oc (fun oc ->
    Printf.fprintf oc "%a.pokeByte(%a)"
      Identifier.print p
      Identifier.print b)

module type Emitter =
sig
  type mid
  val emit : output -> (string IO.output -> unit) -> mid
  val print : string BatIO.output -> mid -> unit
end
module MakeNum (M : Emitter) =
struct
  type o = output
  type output = o
  include M

  let eq oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a == %a"
      print t1 print t2)

  let ne oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a != %a"
      print t1 print t2)

  let gt oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a > %a"
      print t1 print t2)

  let ge oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a >= %a"
      print t1 print t2)

  let add oc (t1 : mid) (t2 : mid) =
    emit oc (fun oc -> Printf.fprintf oc "%a + %a"
      print t1 print t2)

  let sub oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a - %a"
      print t1 print t2)

  let mul  oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a * %a"
      print t1 print t2)

  let div oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a / %a"
      print t1 print t2)

  let of_const_int oc d =
    emit oc (fun oc -> Int.print oc d)

  let of_byte oc b =
    emit oc (fun oc -> Identifier.print oc b)

  let to_byte oc b =
    emit_byte oc (fun oc -> print oc b)

  let of_word oc b =
    emit oc (fun oc -> Identifier.print oc b)

  let to_word oc b =
    emit_word oc (fun oc -> print oc b)

  let of_dword oc b =
    emit oc (fun oc -> Identifier.print oc b)

  let to_dword oc b =
    emit_dword oc (fun oc -> print oc b)

  let of_qword oc b =
    emit oc (fun oc -> Identifier.print oc b)

  let to_qword oc b =
    emit_qword oc (fun oc -> print oc b)

  let of_oword oc b =
    emit oc (fun oc -> Identifier.print oc b)

  let to_oword oc b =
    emit_oword oc (fun oc -> print oc b)

  let to_string oc b =
    emit_string oc (fun oc -> Printf.fprintf oc "std::to_string(%a)"
      print b)
end

module MakeInt (M : Emitter) =
struct
  include MakeNum (M)

  let rem oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a %% %a"
      print t1 print t2)

  let log_and oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a & %a"
      print t1 print t2)

  let log_or oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a | %a"
      print t1 print t2)

  let log_xor oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a ^ %a"
      print t1 print t2)

  let log_not oc t =
    emit oc (fun oc -> Printf.fprintf oc "~%a" print t)

  let shift_left oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a << %a"
      print t1 Identifier.print t2)

  let shift_right oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%a >> %a"
      print t1 Identifier.print t2)

  (* override the above *)
  let of_string oc s =
    emit oc (fun oc -> Printf.fprintf oc "std::stoll(%a)"
      Identifier.print s)

  let to_u8 oc t =
    emit_u8 oc (fun oc -> Identifier.print oc t)

  let of_u8 oc u8 =
    emit oc (fun oc -> Identifier.print oc u8)
end

module Float =
struct
  include MakeNum (
    struct
      type mid = [`Float] Identifier.t
      let emit = emit_float
      let print = Identifier.print
    end)

  let of_string oc s =
    emit_float oc (fun oc -> Printf.fprintf oc "std::stod(%a)"
      Identifier.print s)
end

module U8 = MakeInt (struct type mid = [`U8] Identifier.t let emit = emit_u8 let print = Identifier.print end)
module I8 = MakeInt (struct type mid = [`I8] Identifier.t let emit = emit_i8 let print = Identifier.print end)
module U16 = MakeInt (struct type mid = [`U16] Identifier.t let emit = emit_u16 let print = Identifier.print end)
module I16 = MakeInt (struct type mid = [`I16] Identifier.t let emit = emit_i16 let print = Identifier.print end)
module U32 = MakeInt (struct type mid = [`U32] Identifier.t let emit = emit_u32 let print = Identifier.print end)
module I32 = MakeInt (struct type mid = [`I32] Identifier.t let emit = emit_i32 let print = Identifier.print end)
module U64 = MakeInt (struct type mid = [`U64] Identifier.t let emit = emit_u64 let print = Identifier.print end)
module I64 = MakeInt (struct type mid = [`I64] Identifier.t let emit = emit_i64 let print = Identifier.print end)

module U128 =
struct
  include MakeInt (struct type mid = [`U128] Identifier.t let emit = emit_u128 let print = Identifier.print end)

  let to_string oc _b =
    emit_string oc (fun oc -> String.print oc "\"TODO: to_string(uint128)\"")
end

module I128 =
struct
  include MakeInt (struct type mid = [`I128] Identifier.t let emit = emit_i128 let print = Identifier.print end)

  let to_string oc _b =
    emit_string oc (fun oc -> String.print oc "\"TODO: to_string(int128)\"")
end

let char_of_byte oc b =
  emit_char oc (fun oc -> Identifier.print oc b)

let byte_of_char oc u =
  emit_byte oc (fun oc -> Identifier.print oc u)

let float_of_i8 oc i =
  emit_float oc (fun oc -> Identifier.print oc i)

let float_of_qword oc w =
  emit_float oc (fun oc ->
    Printf.fprintf oc "floatOfQword(%a)" Identifier.print w)

let qword_of_float oc v =
  emit_qword oc (fun oc ->
    Printf.fprintf oc "qwordOfFloat(%a)" Identifier.print v)

let string_of_float oc v =
  emit_string oc (fun oc -> Printf.fprintf oc "std::to_string(%a)"
    Identifier.print v)

let cat_string oc s1 s2 =
  emit_string oc (fun oc -> Printf.fprintf oc "%a + %a"
    Identifier.print s1 Identifier.print s2)

let indent_more oc f =
  let cur_indent = oc.indent in
  oc.indent <- oc.indent ^ "  " ;
  f () ;
  oc.indent <- cur_indent

let make_pair oc typ id1 id2 =
  let id = Identifier.pair () in
  (* Construct the type of the result: *)
  let typname = find_or_declare_type oc typ in
  Printf.fprintf oc.code "%s%s %a = { %a, %a };\n" oc.indent
    typname Identifier.print id
    Identifier.print id1
    Identifier.print id2 ;
  id

let pair_fst oc id =
  emit_auto oc (fun oc ->
    Printf.fprintf oc "%a.first" Identifier.print id)

let pair_snd oc id =
  emit_auto oc (fun oc ->
    Printf.fprintf oc "%a.second" Identifier.print id)

let length_of_string oc s =
  emit_size oc (fun oc ->
    Printf.fprintf oc "%a.size()" Identifier.print s)

let string_of_bytes oc bs =
  emit_string oc (fun oc ->
    Printf.fprintf oc "%a.toString()" Identifier.print bs)

let bytes_of_string oc s =
  emit_bytes oc (fun oc -> Identifier.print oc s)

let string_of_const oc s =
  emit_string oc (fun oc -> String.print_quoted oc s)

(* Lists are actually implemented as vectors: *)
let length_of_list oc l =
  emit_u32 oc (fun oc ->
    Printf.fprintf oc "%a.size()" Identifier.print l)

let bool_of_const oc b =
  emit_bool oc (fun oc -> Bool.print oc b)

let print_float_literal oc v =
  if v = infinity then
    String.print oc "std::numeric_limits<double>::infinity"
  else if v = neg_infinity then
    String.print oc "-std::numeric_limits<double>::infinity"
  else
    Legacy.Printf.sprintf "%h" v |> String.print oc

let float_of_const oc v =
  emit_float oc (fun oc -> print_float_literal oc v)

let u8_of_bool oc b =
  emit_u8 oc (fun oc -> Identifier.print oc b)

let bool_and oc b1 b2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a && %a" Identifier.print b1 Identifier.print b2)

let bool_or oc b1 b2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a || %a" Identifier.print b1 Identifier.print b2)

let bool_not oc b =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "!%a" Identifier.print b)

let i32_of_u32 oc n =
  emit_i32 oc (fun oc ->
    Printf.fprintf oc "(int32_t)%a" Identifier.print n)

let u32_of_i32 oc n =
  emit_u32 oc (fun oc ->
    Printf.fprintf oc "(uint32_t)%a" Identifier.print n)

let u8_of_const oc v =
  emit_u8 oc (fun oc -> String.print oc (Uint8.to_string v))

let u16_of_const oc v =
  emit_u16 oc (fun oc -> String.print oc (Uint16.to_string v))

let u32_of_const oc v =
  emit_u32 oc (fun oc -> String.print oc (Uint32.to_string v))

let u64_of_const oc v =
  emit_u64 oc (fun oc -> String.print oc (Uint64.to_string v))

let u128_of_const oc v =
  emit_u128 oc (fun oc -> String.print oc (Uint128.to_string v))

let i8_of_const oc v =
  emit_i8 oc (fun oc -> String.print oc (Int8.to_string v))

let i16_of_const oc v =
  emit_i16 oc (fun oc -> String.print oc (Int16.to_string v))

let i32_of_const oc v =
  emit_i32 oc (fun oc -> String.print oc (Int32.to_string v))

let i64_of_const oc v =
  emit_i64 oc (fun oc -> String.print oc (Int64.to_string v))

let i128_of_const oc v =
  emit_i128 oc (fun oc -> String.print oc (Int128.to_string v))

(* [c] and [a] must have been resolved into indentifiers already, ie. their
 * side effect must have happened already (in practice: only for [c] and [a]
 * with no side effects.) *)
let choose oc ~cond c a =
  let res_id = Identifier.auto () in
  (* Lambdas are a trick to allow [c] and [a] to use the local vars the use
   * of auto. Still, [choose] is limited to return an identifier instead of
   * any type (like the MetaOcaml version): *)
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
  let id_ptr = Identifier.pointer () in
  Printf.fprintf oc.code "%sPointer %a(%a);\n" oc.indent
    Identifier.print id_ptr
    Identifier.print p ;
  let id_res = Identifier.auto () in
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
let loop_while oc ~cond ~loop v0 =
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%sauto %a = %a;\n" oc.indent
    Identifier.print id_res
    Identifier.print v0 ;
  Printf.fprintf oc.code "%swhile (%a(%a)) {\n" oc.indent
    Identifier.print cond
    Identifier.print id_res ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code "%s%a = %a(%a);\n" oc.indent
      Identifier.print id_res
      Identifier.print loop
      Identifier.print id_res) ;
  Printf.fprintf oc.code "%s};\n" oc.indent ;
  id_res

let loop_until oc ~loop ~cond v0 =
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%sauto %a = %a;\n" oc.indent
    Identifier.print id_res
    Identifier.print v0 ;
  Printf.fprintf oc.code "%sdo {\n" oc.indent ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code "%s%a = %a(%a);\n" oc.indent
      Identifier.print id_res
      Identifier.print loop
      Identifier.print id_res) ;
  Printf.fprintf oc.code "%s} while (%a(%a));\n" oc.indent
    Identifier.print cond
    Identifier.print id_res ;
  id_res

let loop_repeat oc ~from ~to_ ~loop v0 =
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%sauto %a = %a;\n" oc.indent
    Identifier.print id_res
    Identifier.print v0 ;
  Printf.fprintf oc.code "%sfor (auto n = %a; n < %a; n++) {\n" oc.indent
    Identifier.print from
    Identifier.print to_ ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code "%s%a = %a(n, %a);\n" oc.indent
      Identifier.print id_res
      Identifier.print loop
      Identifier.print id_res) ;
  Printf.fprintf oc.code "%s};\n" oc.indent ;
  id_res

(* For heap allocated values, all subtypes are unboxed so we can perform a
 * single allocation. *)
let alloc_value oc vtyp =
  let typ = Type.TValue vtyp in
  oc.value_typname <- find_or_declare_type oc typ ;
  emit_pointer oc (fun oc' ->
    (* Build the shared_ptr here that the type is known: *)
    Printf.fprintf oc' "std::shared_ptr<%s>(new %s)"
      oc.value_typname
      oc.value_typname)

let subfield_name typ idx =
  fst (subfield_info typ idx)

(* Note: [p] despite being a Pointer identifier is actually the identifier of
 * the address of a value. *)
let id_of_path oc frames (p : [`Pointer] id) =
  let base =
    Printf.sprintf2 "(*((%s *)((%a).value.get())))"
      oc.value_typname
      Identifier.print p in
  let rec loop = function
    | [] ->
        assert false
    | [ _ ] ->
        base
    | top :: (parent :: _ as rest) ->
        (* Dereference parent: *)
        let accessor =
          match parent.typ with
          | ValueType.(Nullable (TTup _) | NotNullable (TTup _)) -> "."
          | ValueType.(Nullable (TRec _) | NotNullable (TRec _)) -> "."
          | ValueType.(Nullable (TVec _) | NotNullable (TVec _)) -> ""
          | _ -> assert false in
        let subfield = subfield_name parent.typ top.index in
        let denullify =
          if ValueType.is_nullable parent.typ then ".value()" else "" in
        (loop rest) ^ denullify ^ accessor ^ subfield
  in
  loop frames

let set_field oc frames p v =
  let id = id_of_path oc frames p in
  Printf.fprintf oc.code "%s%s = %a;\n" oc.indent
    id
    Identifier.print v

let set_nullable_field oc typ idx v =
  match v with
  | Some v ->
      set_field oc typ idx v
  | None ->
      set_field oc typ idx (Identifier.of_string "std::nullopt")

let get_field oc frames p =
  let id = id_of_path oc frames p in
  Identifier.of_string id

let get_nullable_field oc frames p =
  let id = id_of_path oc frames p in
  Identifier.of_string (id ^".value()")

let field_is_set oc frames p =
  let id = id_of_path oc frames p in
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%s.has_value()" id)
