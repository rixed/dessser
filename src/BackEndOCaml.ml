open Batteries
open Stdint
open Dessser

let preferred_file_extension = "ml"

type output =
  { mutable code : string IO.output ; (* current function body *)
    mutable indent : string ; (* current line prefix *)
    (* When building heap values, remember the base type name (for casts): *)
    mutable value_typname : string ;
    (* Used while constructing a heap value: *)
    mutable value_last_frames : frame list ;
    (* While constructing a value the initialization of the fields is delayed
     * until the end of the function: *)
    mutable value_init : string IO.output ;
    (* name and definition (order matter!): *)
    mutable funs : (string * string) list ;
    (* type names and declarations (order matters!): *)
    mutable type_decls : (string * string) list }

(* Compound types have to be declared (once) *)

let rec ml_type_of_scalar = function
  | Type.TValue ValueType.(NotNullable TFloat | Nullable TFloat) -> "float"
  | Type.TValue ValueType.(NotNullable TString | Nullable TString) -> "string"
  | Type.TValue ValueType.(NotNullable TBool | Nullable TBool) -> "bool"
  | Type.TValue ValueType.(NotNullable TChar | Nullable TChar) -> "char"
  | Type.TValue ValueType.(NotNullable TI8 | Nullable TI8) -> "Int8.t"
  | Type.TValue ValueType.(NotNullable TU8 | Nullable TU8) -> "Uint8.t"
  | Type.TValue ValueType.(NotNullable TI16 | Nullable TI16) -> "Int16.t"
  | Type.TValue ValueType.(NotNullable TU16 | Nullable TU16) -> "Uint16.t"
  | Type.TValue ValueType.(NotNullable TI32 | Nullable TI32) -> "Int32.t"
  | Type.TValue ValueType.(NotNullable TU32 | Nullable TU32) -> "Uint32.t"
  | Type.TValue ValueType.(NotNullable TI64 | Nullable TI64) -> "Int64.t"
  | Type.TValue ValueType.(NotNullable TU64 | Nullable TU64) -> "Uint64.t"
  | Type.TValue ValueType.(NotNullable TI128 | Nullable TI128) -> "Int128.t"
  | Type.TValue ValueType.(NotNullable TU128 | Nullable TU128) -> "Uint128.t"
  (* Not scalars: *)
  | Type.TValue ValueType.(NotNullable (TTup _ | TRec _) | Nullable (TTup _ | TRec _))
  | Type.TPair _
  | Type.TFunction0 _
  | Type.TFunction1 _
  | Type.TFunction2 _
  | Type.TFunction3 _ ->
      assert false
  (* Treated as a scalar here: *)
  | Type.TValue ValueType.(NotNullable TVec (_dim, typ) | Nullable TVec (_dim, typ)) ->
      Printf.sprintf "%s array" (ml_type_of_scalar (Type.TValue typ))
  | Type.TValue ValueType.(NotNullable TList typ | Nullable TList typ) ->
      Printf.sprintf "%s array" (ml_type_of_scalar (Type.TValue typ))
  (* The caller does not know if it's a pointer used for reading/writing bytes
   * or setting/getting subfields, which is a good thing as it allow to
   * combine freely actual serializers and value "reifiers".
   * So here both types of pointer should be allowed.  *)
  | TPointer -> "Pointer.t"
  | TSize -> "Size.t"
  | TBit -> "bool"
  | TByte -> "Uint8.t"
  | TWord -> "Uint16.t"
  | TDWord -> "Uint32.t"
  | TQWord -> "Uint64.t"
  | TOWord -> "Uint128.t"
  | TBytes -> "Slice.t"

let ignore oc id =
  Printf.fprintf oc.code "%signore %a;\n" oc.indent
    Identifier.print id

let comment oc fmt =
  Printf.fprintf oc.code ("%s(* " ^^ fmt ^^ " *)\n") oc.indent

let dump oc lst =
  List.iter (fun id ->
    Printf.fprintf oc.code "%sprint_string %a;\n" oc.indent
      Identifier.print id
  ) lst ;
  Printf.fprintf oc.code "%sprint_newline ();\n" oc.indent

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
      "("^ string_of_int idx ^")",
      typ
  | _ ->
      (* Scalar types have no subfields: *)
      assert false

let rec declare_type oc id typ =
  let s = IO.output_string () in
  let print_record vtyp vsubtyps =
    Printf.fprintf s "type %s = {\n" id ;
    Array.iteri (fun i subtyp ->
      let typ_id = find_or_declare_type oc (Type.TValue subtyp) in
      let fname, subtyp = subfield_info vtyp i in
      Printf.fprintf s "  mutable %s : %s%s;\n"
        fname typ_id
        (if ValueType.is_nullable subtyp then " option" else "")
    ) vsubtyps ;
    Printf.fprintf s "}\n\n"
  in
  (match typ with
  | Type.TValue (ValueType.(NotNullable TTup vsubtyps | Nullable TTup vsubtyps) as vtyp) ->
      print_record vtyp vsubtyps
  | Type.TValue (ValueType.(NotNullable TRec vsubtyps | Nullable TRec vsubtyps) as vtyp) ->
      print_record vtyp (Array.map snd vsubtyps)
  | Type.TPair (t1, t2) ->
      Printf.fprintf s "type %s = %s * %s\n\n"
        id
        (find_or_declare_type oc t1)
        (find_or_declare_type oc t2)
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
        ml_type_of_scalar typ

(* Output: make, print... *)

let make_output () =
  { code = IO.output_string () ;
    indent = "" ;
    value_typname = "" ;
    value_last_frames = [] ;
    value_init = IO.output_string () ;
    funs = [] ;
    type_decls = [] }

let print_output oc output =
  Printf.fprintf oc "open Stdint\n" ;
  Printf.fprintf oc "open DessserOCamlBackendHelpers\n" ;
  Printf.fprintf oc "\n(* Type Declarations *)\n\n" ;
  List.rev output.type_decls |>
  List.iter (fun (_type_id, str) ->
    Printf.fprintf oc "%s\n" str) ;
  Printf.fprintf oc "\n(* Functions *)\n\n" ;
  List.rev output.funs |>
  List.iter (fun (_fun_id, str) ->
    Printf.fprintf oc "%s\n" str)

(* - [p] is a printer that returns the id of the result;
 * - [typ] is the return type of that function.
 * Returns the function id. *)
let function0 oc out_typ p =
  let fun_id = Identifier.func0 () in
  let out_tname = find_or_declare_type oc out_typ in
  let cur_code = oc.code and cur_indent = oc.indent in
  oc.code <- IO.output_string () ;
  oc.indent <- "  " ;
  let res_id = p oc in
  let str =
    Printf.sprintf2 "let %a () : %s =\n%s%s%a\n"
      Identifier.print fun_id
      out_tname
      (IO.close_out oc.code)
      oc.indent
      Identifier.print res_id in
  oc.code <- cur_code ;
  oc.indent <- cur_indent ;
  oc.funs <- ((fun_id : _ Identifier.t :> string), str) :: oc.funs ;
  fun_id

let function1 oc in_typ0 out_typ p =
  let out_tname = find_or_declare_type oc out_typ in
  let in_tname0 = find_or_declare_type oc in_typ0 in
  let param0 = Identifier.param 0 () in
  let cur_code = oc.code and cur_indent = oc.indent in
  oc.code <- IO.output_string () ;
  oc.indent <- "  " ;
  let res_id = p oc param0 in
  let fun_id = Identifier.func1 param0 res_id in
  let str =
    Printf.sprintf2 "let %a (%a : %s) : %s =\n%s%s%a\n"
      Identifier.print fun_id
      Identifier.print param0
      in_tname0
      out_tname
      (IO.close_out oc.code)
      oc.indent
      Identifier.print res_id in
  oc.code <- cur_code ;
  oc.indent <- cur_indent ;
  oc.funs <- ((fun_id : _ Identifier.t :> string), str) :: oc.funs ;
  fun_id

let function2 oc in_typ0 in_typ1 out_typ p =
  let out_tname = find_or_declare_type oc out_typ in
  let in_tname0 = find_or_declare_type oc in_typ0 in
  let in_tname1 = find_or_declare_type oc in_typ1 in
  let param0 = Identifier.param 0 () in
  let param1 = Identifier.param 1 () in
  let cur_code = oc.code and cur_indent = oc.indent in
  oc.code <- IO.output_string () ;
  oc.indent <- "  " ;
  let res_id = p oc param0 param1 in
  let fun_id = Identifier.func2 param0 param1 res_id in
  let str =
    Printf.sprintf2 "let %a (%a : %s) (%a : %s) : %s =\n%s%s%a\n"
      Identifier.print fun_id
      Identifier.print param0
      in_tname0
      Identifier.print param1
      in_tname1
      out_tname
      (IO.close_out oc.code)
      oc.indent
      Identifier.print res_id in
  oc.code <- cur_code ;
  oc.indent <- cur_indent ;
  oc.funs <- ((fun_id : _ Identifier.t :> string), str) :: oc.funs ;
  fun_id

(* Pointers can be actual Pointer.t or also any heap value, so let's not
 * annotate the type and let the OCaml compiler find out which is which. *)
let emit_pointer oc p =
  let id = Identifier.pointer () in
  Printf.fprintf oc.code "%slet %a = %t in\n" oc.indent
    Identifier.print id p ;
  id

let emit_size oc p =
  let id = Identifier.size () in
  Printf.fprintf oc.code "%slet %a : Size.t = %t in\n" oc.indent
    Identifier.print id p ;
  id

let emit_bit oc b =
  let id = Identifier.bit () in
  Printf.fprintf oc.code "%slet %a : bool = %t in\n" oc.indent
    Identifier.print id b ;
  id

let emit_byte oc b =
  let id = Identifier.byte () in
  Printf.fprintf oc.code "%slet %a : Uint8.t = %t in\n" oc.indent
    Identifier.print id b ;
  id

let emit_word oc b =
  let id = Identifier.word () in
  Printf.fprintf oc.code "%slet %a : Uint16.t = %t in\n" oc.indent
    Identifier.print id b ;
  id

let emit_dword oc b =
  let id = Identifier.dword () in
  Printf.fprintf oc.code "%slet %a : Uint32.t = %t in\n" oc.indent
    Identifier.print id b ;
  id

let emit_qword oc b =
  let id = Identifier.qword () in
  Printf.fprintf oc.code "%slet %a : Uint64.t = %t in\n" oc.indent
    Identifier.print id b ;
  id

let emit_oword oc b =
  let id = Identifier.oword () in
  Printf.fprintf oc.code "%slet %a : Uint128.t = %t in\n" oc.indent
    Identifier.print id b ;
  id

let emit_bytes oc bs =
  let id = Identifier.bytes () in
  Printf.fprintf oc.code "%slet %a : Slice.t = %t in\n" oc.indent
    Identifier.print id bs ;
  id

let emit_char oc u =
  let id = Identifier.char () in
  Printf.fprintf oc.code "%slet %a : char = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_u8 oc u =
  let id = Identifier.u8 () in
  Printf.fprintf oc.code "%slet %a : Uint8.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_i8 oc u =
  let id = Identifier.i8 () in
  Printf.fprintf oc.code "%slet %a : Int8.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_u16 oc u =
  let id = Identifier.u16 () in
  Printf.fprintf oc.code "%slet %a : Uint16.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_i16 oc u =
  let id = Identifier.i16 () in
  Printf.fprintf oc.code "%slet %a : Int16.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_u32 oc u =
  let id = Identifier.u32 () in
  Printf.fprintf oc.code "%slet %a : Uint32.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_i32 oc u =
  let id = Identifier.i32 () in
  Printf.fprintf oc.code "%slet %a : Int32.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_u64 oc u =
  let id = Identifier.u64 () in
  Printf.fprintf oc.code "%slet %a : Uint64.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_i64 oc u =
  let id = Identifier.i64 () in
  Printf.fprintf oc.code "%slet %a : Int64.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_u128 oc u =
  let id = Identifier.u128 () in
  Printf.fprintf oc.code "%slet %a : Uint128.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_i128 oc u =
  let id = Identifier.i128 () in
  Printf.fprintf oc.code "%slet %a : Int128.t = %t in\n" oc.indent
    Identifier.print id u ;
  id

let emit_float oc p =
  let id = Identifier.float () in
  Printf.fprintf oc.code "%slet %a : float = %t in\n" oc.indent
    Identifier.print id p ;
  id

let emit_string oc p =
  let id = Identifier.string () in
  Printf.fprintf oc.code "%slet %a : string = %t in\n" oc.indent
    Identifier.print id p ;
  id

let emit_bool oc p =
  let id = Identifier.bool () in
  Printf.fprintf oc.code "%slet %a : bool = %t in\n" oc.indent
    Identifier.print id p ;
  id

let emit_unit oc p =
  Printf.fprintf oc.code "%s%t;\n" oc.indent p

let emit_auto oc p =
  let id = Identifier.auto () in
  Printf.fprintf oc.code "%slet %a = %t in\n" oc.indent
    Identifier.print id p ;
  id

let emit_pair t1 t2 oc p =
  let id = Identifier.pair () in
  let tname1 = find_or_declare_type oc t1 in
  let tname2 = find_or_declare_type oc t2 in
  Printf.fprintf oc.code "%slet %a : (%s * %s) = %t in\n" oc.indent
    Identifier.print id tname1 tname2 p ;
  Identifier.(modify "(fst " id ")" |> of_any t1),
  Identifier.(modify "(snd " id ")" |> of_any t2)

let pointer_add oc p s =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.skip %a %a"
      Identifier.print p Identifier.print s)

let pointer_sub oc p1 p2 =
  emit_size oc (fun oc ->
    Printf.fprintf oc "Pointer.sub %a %a"
      Identifier.print p1 Identifier.print p2)

let size_add oc s1 s2 =
  emit_size oc (fun oc ->
    Printf.fprintf oc "Size.add %a %a"
      Identifier.print s1 Identifier.print s2)

let size_to_string oc s =
  emit_string oc (fun oc ->
    Printf.fprintf oc "Size.to_string %a" Identifier.print s)

let rem_size oc p =
  emit_size oc (fun oc ->
    Printf.fprintf oc "Pointer.remSize %a"
      Identifier.print p)

let bit_of_const oc b =
  emit_bit oc (fun oc -> String.print oc (if b then "true" else "false"))

let byte_of_const oc b =
  emit_byte oc (fun oc -> Printf.fprintf oc "Uint8.of_int %d" b)

let size_of_const oc s =
  emit_size oc (fun oc -> Printf.fprintf oc "Size.of_int %d" s)

let size_of_u32 oc v =
  emit_size oc (fun oc ->
    Printf.fprintf oc "Size.of_int (Uint32.to_int %a)" Identifier.print v)

let u32_of_size oc v =
  emit_u32 oc (fun oc ->
    Printf.fprintf oc "Uint32.of_int (Size.to_int %a)" Identifier.print v)

let dword_eq oc w1 w2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a = %a" Identifier.print w1 Identifier.print w2)

let size_ge oc s1 s2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a >= %a" Identifier.print s1 Identifier.print s2)

let pointer_of_string oc s =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.of_string %S" s)

let bytes_append oc bs b =
  emit_bytes oc (fun oc ->
    Printf.fprintf oc "Slice.append %a %a"
      Identifier.print bs
      Identifier.print b)

let u8_of_byte oc b =
  emit_u8 oc (fun oc -> Identifier.print oc b)

let byte_of_u8 oc u =
  emit_byte oc (fun oc -> Identifier.print oc u)

let test_bit oc p u =
  emit_bit oc (fun oc ->
    Printf.fprintf oc "Pointer.getBit %a (Uint32.to_int %a)"
      Identifier.print p
      Identifier.print u)

let read_byte oc p =
  emit_pair TByte TPointer oc (fun oc ->
    Printf.fprintf oc "Pointer.readByte %a"
      Identifier.print p)

let read_word oc ?(be=false) p =
  emit_pair TWord TPointer oc (fun oc ->
    Printf.fprintf oc "Pointer.readWord%s %a"
      (if be then "Be" else "Le")
      Identifier.print p)

let read_dword oc ?(be=false) p =
  emit_pair TDWord TPointer oc (fun oc ->
    Printf.fprintf oc "Pointer.readDWord%s %a"
      (if be then "Be" else "Le")
      Identifier.print p)

let read_qword oc ?(be=false) p =
  emit_pair TQWord TPointer oc (fun oc ->
    Printf.fprintf oc "Pointer.readQWord%s %a"
      (if be then "Be" else "Le")
      Identifier.print p)

let read_oword oc ?(be=false) p =
  emit_pair TOWord TPointer oc (fun oc ->
    Printf.fprintf oc "Pointer.readOWord%s %a"
      (if be then "Be" else "Le")
      Identifier.print p)

let read_bytes oc p sz =
  emit_pair TBytes TPointer oc (fun oc ->
    Printf.fprintf oc "Pointer.readBytes %a %a"
      Identifier.print p
      Identifier.print sz)

let set_bit oc p u b =
  emit_unit oc (fun oc ->
    Printf.fprintf oc "Pointer.setBit %a (Uint32.to_int %a) %a"
      Identifier.print p
      Identifier.print u
      Identifier.print b)

let write_byte oc p b =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.writeByte %a %a"
      Identifier.print p
      Identifier.print b)

let write_word oc ?(be=false) p w =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.writeWord%s %a %a"
      (if be then "Be" else "Le")
      Identifier.print p
      Identifier.print w)

let write_dword oc ?(be=false) p w =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.writeDWord%s %a %a"
      (if be then "Be" else "Le")
      Identifier.print p
      Identifier.print w)

let write_qword oc ?(be=false) p w =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.writeQWord%s %a %a"
      (if be then "Be" else "Le")
      Identifier.print p
      Identifier.print w)

let write_oword oc ?(be=false) p w =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.writeOWord%s %a %a"
      (if be then "Be" else "Le")
      Identifier.print p
      Identifier.print w)

let write_bytes oc p b =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.writeBytes %a %a"
      Identifier.print p
      Identifier.print b)

let blit_bytes oc p b sz =
  emit_pointer oc (fun oc ->
    Printf.fprintf oc "Pointer.blitBytes %a %a %a"
      Identifier.print p
      Identifier.print b
      Identifier.print sz)

let peek_byte oc ?at p =
  emit_byte oc (fun oc ->
    Printf.fprintf oc "Pointer.peekByte %a %s"
      Identifier.print p
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_word oc ?(be=false) ?at p =
  emit_word oc (fun oc ->
    Printf.fprintf oc "Pointer.peekWord%s %a %s"
      (if be then "Be" else "Le")
      Identifier.print p
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_dword oc ?(be=false) ?at p =
  emit_dword oc (fun oc ->
    Printf.fprintf oc "Pointer.peekDWord%s %a %s"
      (if be then "Be" else "Le")
      Identifier.print p
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_qword oc ?(be=false) ?at p =
  emit_qword oc (fun oc ->
    Printf.fprintf oc "Pointer.peekQWord%s %a %s"
      (if be then "Be" else "Le")
      Identifier.print p
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let peek_oword oc ?(be=false) ?at p =
  emit_oword oc (fun oc ->
    Printf.fprintf oc "Pointer.peekOWord%s %a %s"
      (if be then "Be" else "Le")
      Identifier.print p
      (match at with None -> "0" | Some at -> (at : [`Size] id :> string)))

let poke_byte oc p b =
  emit_unit oc (fun oc ->
    Printf.fprintf oc "Pointer.pokeByte %a %a"
      Identifier.print p
      Identifier.print b)

module type Emitter =
sig
  type mid
  val mod_name : string
  val emit : output -> (string IO.output -> unit) -> mid
  val print : string BatIO.output -> mid -> unit
end
module MakeNum (M : Emitter) =
struct
  type o = output
  type output = o
  include M

  let eq oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a = %a"
      print t1 print t2)

  let ne oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a <> %a"
      print t1 print t2)

  let gt oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a > %a"
      print t1 print t2)

  let ge oc t1 t2 =
    emit_bool oc (fun oc -> Printf.fprintf oc "%a >= %a"
      print t1 print t2)

  let add oc (t1 : mid) (t2 : mid) =
    emit oc (fun oc -> Printf.fprintf oc "%s.add %a %a"
      mod_name print t1 print t2)

  let sub oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.sub %a %a"
      mod_name print t1 print t2)

  let mul  oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.mul %a %a"
      mod_name print t1 print t2)

  let div oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.div %a %a"
      mod_name print t1 print t2)

  let of_const_int oc d =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_int %d"
      mod_name d)

  let to_string oc b =
    emit_string oc (fun oc -> Printf.fprintf oc "%s.to_string %a"
      mod_name print b)

  (* Those functions are about _casting_ not converting: *)
  let of_byte oc b =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_uint8 %a"
      mod_name Identifier.print b)

  let to_byte oc b =
    emit_byte oc (fun oc -> Printf.fprintf oc "%s.to_uint8 %a"
      mod_name print b)

  let of_word oc b =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_uint16 %a"
      mod_name Identifier.print b)

  let to_word oc b =
    emit_word oc (fun oc -> Printf.fprintf oc "%s.to_uint16 %a"
      mod_name print b)

  let of_dword oc b =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_uint32 %a"
      mod_name Identifier.print b)

  let to_dword oc b =
    emit_dword oc (fun oc -> Printf.fprintf oc "%s.to_uint32 %a"
      mod_name print b)

  let of_qword oc b =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_uint64 %a"
      mod_name Identifier.print b)

  let to_qword oc b =
    emit_qword oc (fun oc -> Printf.fprintf oc "%s.to_uint64 %a"
      mod_name print b)

  let of_oword oc b =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_uint128 %a"
      mod_name Identifier.print b)

  let to_oword oc b =
    emit_oword oc (fun oc -> Printf.fprintf oc "%s.to_uint128 %a"
      mod_name print b)
end

module MakeInt (M : Emitter) =
struct
  include MakeNum (M)

  let rem oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.rem %a %a"
      mod_name print t1 print t2)

  let log_and oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.logand %a %a"
      mod_name print t1 print t2)

  let log_or oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.logor %a %a"
      mod_name print t1 print t2)

  let log_xor oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.logxor %a %a"
      mod_name print t1 print t2)

  let log_not oc t =
    emit oc (fun oc -> Printf.fprintf oc "%s.lognot %a"
      mod_name print t)

  let shift_left oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.shift_left %a (Uint8.to_int %a)"
      mod_name print t1 Identifier.print t2)

  let shift_right oc t1 t2 =
    emit oc (fun oc -> Printf.fprintf oc "%s.shift_right_logical %a (Uint8.to_int %a)"
      mod_name print t1 Identifier.print t2)

  (* override the above *)
  let of_string oc s =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_string %a"
      mod_name Identifier.print s)

  let to_u8 oc t =
    emit_u8 oc (fun oc -> Printf.fprintf oc "%s.to_uint8 %a"
      mod_name Identifier.print t)

  let of_u8 oc u8 =
    emit oc (fun oc -> Printf.fprintf oc "%s.of_uint8 %a"
      mod_name Identifier.print u8)
end

module Float =
struct
  include MakeNum (
    struct
      type mid = [`Float] Identifier.t
      let emit = emit_float
      let print = Identifier.print
      let mod_name = "Float"
    end)

  (* Casting in OCaml is not that simple: *)
  let of_byte oc _ = emit oc (fun oc -> Printf.fprintf oc "raise (NotImplemented \"Float.of_byte\")")
  let to_byte oc _ = emit_byte oc (fun oc -> Printf.fprintf oc "raise (NotImplemented \"Float.to_byte\")")
  let of_word oc _ = emit oc (fun oc -> Printf.fprintf oc "raise (NotImplemented \"Float.of_word\")")
  let to_word oc _ = emit_word oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.to_word\")")
  let of_dword oc _ = emit oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.of_dword\")")
  let to_dword oc _ = emit_dword oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.to_dword\")")
  let of_qword oc _ = emit oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.of_qword\")")
  let to_qword oc _ = emit_qword oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.to_qword\")")
  let of_oword oc _ = emit oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.of_oword\")")
  let to_oword oc _ = emit_oword oc (fun oc ->  Printf.fprintf oc "raise (NotImplemented \"Float.to_oword\")")
end

module U8 = MakeInt (struct let mod_name = "Uint8" type mid = [`U8] Identifier.t let emit = emit_u8 let print = Identifier.print end)
module I8 = MakeInt (struct let mod_name = "Int8" type mid = [`I8] Identifier.t let emit = emit_i8 let print = Identifier.print end)
module U16 = MakeInt (struct let mod_name = "Uint16" type mid = [`U16] Identifier.t let emit = emit_u16 let print = Identifier.print end)
module I16 = MakeInt (struct let mod_name = "Int16" type mid = [`I16] Identifier.t let emit = emit_i16 let print = Identifier.print end)
module U32 = MakeInt (struct let mod_name = "Uint32" type mid = [`U32] Identifier.t let emit = emit_u32 let print = Identifier.print end)
module I32 = MakeInt (struct let mod_name = "Int32" type mid = [`I32] Identifier.t let emit = emit_i32 let print = Identifier.print end)
module U64 = MakeInt (struct let mod_name = "Uint64" type mid = [`U64] Identifier.t let emit = emit_u64 let print = Identifier.print end)
module I64 = MakeInt (struct let mod_name = "Int64" type mid = [`I64] Identifier.t let emit = emit_i64 let print = Identifier.print end)
module U128 = MakeInt (struct let mod_name = "Uint128" type mid = [`U128] Identifier.t let emit = emit_u128 let print = Identifier.print end)
module I128 = MakeInt (struct let mod_name = "Int128" type mid = [`I128] Identifier.t let emit = emit_i128 let print = Identifier.print end)

let char_of_byte oc b =
  emit_char oc (fun oc ->
    Printf.fprintf oc "Char.chr (Uint8.to_int %a)" Identifier.print b)

let byte_of_char oc u =
  emit_byte oc (fun oc ->
    Printf.fprintf oc "Uint8.of_int (Char.code %a)" Identifier.print u)

let float_of_i8 oc i =
  emit_float oc (fun oc ->
    Printf.fprintf oc "Uint8.to_float %a" Identifier.print i)

(* Those functions encode the float as a qword for serialiation.
 * This is not a conversion from float to integer. *)
let float_of_qword oc w =
  emit_float oc (fun oc ->
    Printf.fprintf oc
      "BatInt64.float_of_bits (Uint64.to_int64 %a)" Identifier.print w)

let qword_of_float oc v =
  emit_qword oc (fun oc ->
    Printf.fprintf oc
      "Uint64.of_int64 (BatInt64.bits_of_float %a)" Identifier.print v)

let string_of_float oc v =
  emit_string oc (fun oc ->
    Printf.fprintf oc "string_of_float %a" Identifier.print v)

let cat_string oc s1 s2 =
  emit_string oc (fun oc ->
    Printf.fprintf oc "String.append %a %a"
      Identifier.print s1 Identifier.print s2)

let indent_more oc f =
  let cur_indent = oc.indent in
  oc.indent <- oc.indent ^ "  " ;
  f () ;
  oc.indent <- cur_indent

let subfield_name typ idx =
  fst (subfield_info typ idx)

(* As these pairs are used to transport pointers, do not be too explicit about
 * typing: *)
let make_pair oc _typ id1 id2 =
  let id = Identifier.pair () in
  Printf.fprintf oc.code "%slet %a = %a, %a in\n" oc.indent
    Identifier.print id
    Identifier.print id1
    Identifier.print id2 ;
  id

let pair_fst oc id =
  emit_auto oc (fun oc ->
    Printf.fprintf oc "(fst %a)" Identifier.print id)

let pair_snd oc id =
  emit_auto oc (fun oc ->
    Printf.fprintf oc "(snd %a)" Identifier.print id)

let length_of_string oc s =
  emit_size oc (fun oc ->
    Printf.fprintf oc "String.length %a" Identifier.print s)

let string_of_bytes oc bs =
  emit_string oc (fun oc ->
    Printf.fprintf oc "Slice.to_string %a" Identifier.print bs)

let bytes_of_string oc s =
  emit_bytes oc (fun oc ->
    Printf.fprintf oc "Slice.of_string %a" Identifier.print s)

let string_of_const oc s =
  emit_string oc (fun oc -> String.print_quoted oc s)

(* Lists are actually implemented as vectors: *)
let length_of_list oc l =
  emit_u32 oc (fun oc ->
    Printf.fprintf oc "Array.length %a" Identifier.print l)

let bool_of_const oc b =
  emit_bool oc (fun oc -> Bool.print oc b)

let print_float_literal oc v =
  (* printf "%F" would not work for infinity:
   * https://caml.inria.fr/mantis/view.php?id=7685
   * and "%h" not for neg_infinity. *)
  if v = infinity then String.print oc "infinity"
  else if v = neg_infinity then String.print oc "neg_infinity"
  else Legacy.Printf.sprintf "%h" v |> String.print oc

let float_of_const oc v =
  emit_float oc (fun oc -> print_float_literal oc v)

let u8_of_bool oc b =
  emit_u8 oc (fun oc ->
    Printf.fprintf oc "Uint8.(if %a then one else zero)" Identifier.print b)

let bool_and oc b1 b2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a && %a" Identifier.print b1 Identifier.print b2)

let bool_or oc b1 b2 =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%a || %a" Identifier.print b1 Identifier.print b2)

let bool_not oc b =
  emit_bool oc (fun oc ->
    Printf.fprintf oc "not %a" Identifier.print b)

let i32_of_u32 oc n =
  emit_i32 oc (fun oc ->
    Printf.fprintf oc "Uint32.to_int32 %a" Identifier.print n)

let u32_of_i32 oc n =
  emit_u32 oc (fun oc ->
    Printf.fprintf oc "Int32.to_uint32 %a" Identifier.print n)

let u8_of_const oc v =
  emit_u8 oc (fun oc ->
    Printf.fprintf oc "Uint8.of_int %d" (Uint8.to_int v))

let lift_u16 v oc =
  Printf.fprintf oc "Uint16.of_int %d" (Uint16.to_int v)

let u16_of_const oc v =
  emit_u16 oc (lift_u16 v)

let word_of_const oc v =
  emit_word oc (fun oc -> Printf.fprintf oc "Uint16.of_int %d" v)

let lift_u32 v oc =
  if v >= Uint32.of_int min_int && v <= Uint32.of_int max_int then
    Printf.fprintf oc "Uint32.of_int %d" (Uint32.to_int v)
  else
    Printf.fprintf oc "Uint32.of_string %S" (Uint32.to_string v)

let u32_of_const oc v =
  emit_u32 oc (lift_u32 v)

let dword_of_const oc v =
  emit_dword oc (lift_u32 v)

let lift_u64 v oc =
  if v >= Uint64.of_int min_int && v <= Uint64.of_int max_int then
    Printf.fprintf oc "Uint64.of_int %d" (Uint64.to_int v)
  else
    Printf.fprintf oc "Uint64.of_string %S" (Uint64.to_string v)

let u64_of_const oc v =
  emit_u64 oc (lift_u64 v)

let qword_of_const oc v =
  emit_qword oc (lift_u64 v)

let lift_u128 v oc =
  if v >= Uint128.of_int min_int && v <= Uint128.of_int max_int then
    Printf.fprintf oc "Uint128.of_int %d" (Uint128.to_int v)
  else
    Printf.fprintf oc "Uint128.of_string %S" (Uint128.to_string v)

let u128_of_const oc v =
  emit_u128 oc (lift_u128 v)

let oword_of_const oc v =
  emit_oword oc (lift_u128 v)

let i8_of_const oc v =
  emit_i8 oc (fun oc ->
    Printf.fprintf oc "Int8.of_int %d" (Int8.to_int v))

let i16_of_const oc v =
  emit_i16 oc (fun oc ->
    Printf.fprintf oc "Int16.of_int %d" (Int16.to_int v))

let i32_of_const oc v =
  emit_i32 oc (fun oc ->
    if v >= Int32.of_int min_int && v <= Int32.of_int max_int then
      Printf.fprintf oc "Int32.of_int %d" (Int32.to_int v)
    else
      Printf.fprintf oc "Int32.of_string %S" (Int32.to_string v))

let i64_of_const oc v =
  emit_i64 oc (fun oc ->
    if v >= Int64.of_int min_int && v <= Int64.of_int max_int then
      Printf.fprintf oc "Int64.of_int %d" (Int64.to_int v)
    else
      Printf.fprintf oc "Int64.of_string %S" (Int64.to_string v))

let i128_of_const oc v =
  emit_i128 oc (fun oc ->
    if v >= Int128.of_int min_int && v <= Int128.of_int max_int then
      Printf.fprintf oc "Int128.of_int %d" (Int128.to_int v)
    else
      Printf.fprintf oc "Int128.of_string %S" (Int128.to_string v))

(* [c] and [a] must have been resolved into indentifiers already, ie. their
 * side effect must have happened already (in practice: only for [c] and [a]
 * with no side effects.) *)
let choose oc ~cond c a =
  let res_id = Identifier.auto () in
  (* Lambdas are a trick to allow [c] and [a] to use the local vars the use
   * of auto. Still, [choose] is limited to return an identifier instead of
   * any type (like the MetaOcaml version): *)
  Printf.fprintf oc.code "%slet %a = if %a then (\n" oc.indent
    Identifier.print res_id
    Identifier.print cond ;
  indent_more oc (fun () ->
    indent_more oc (fun () ->
      let res_c = c oc in
      Printf.fprintf oc.code "%s%a\n" oc.indent
        Identifier.print res_c) ;
    Printf.fprintf oc.code "%s) else (\n" oc.indent ;
    indent_more oc (fun () ->
      let res_a = a oc in
      Printf.fprintf oc.code "%s%a\n" oc.indent
        Identifier.print res_a) ;
    Printf.fprintf oc.code "%s) in\n" oc.indent) ;
  res_id

(* - [cond] is the identifier of a function from byte to bool;
 * - [reduce] is the identifier of a function from any value and byte to
 *   any value;
 * - [v0] is the initial value;
 * Returns the rediced value and the new pointer. *)
let read_while oc ~cond ~reduce v0 p =
  let id_ptr = Identifier.pointer () in
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%slet rec read_while_loop accum ptr =\n" oc.indent ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code
      "%slet nextByte : Uint8.t = Pointer.peekByte ptr 0 in\n" oc.indent ;
    Printf.fprintf oc.code "%sif not (%a nextByte) then (accum, ptr) else\n" oc.indent
      Identifier.print cond ;
    Printf.fprintf oc.code "%slet accum = %a accum nextByte in\n" oc.indent
      Identifier.print reduce ;
    Printf.fprintf oc.code "%slet ptr = Pointer.skip ptr 1 in\n" oc.indent ;
    Printf.fprintf oc.code "%sread_while_loop accum ptr in\n" oc.indent) ;
  Printf.fprintf oc.code "%slet %a, %a = read_while_loop %a %a in\n" oc.indent
    Identifier.print id_res
    Identifier.print id_ptr
    Identifier.print v0
    Identifier.print p ;
  id_res, id_ptr

(* - [cond] is the name of a function that takes the current value and returns
 *   a boolean.
 * - [loop] is a function that takes the current value and returns the next.
 * - [v0] is the initial value.
 * Returns the last value. *)
let loop_while oc ~cond ~loop v0 =
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%slet rec while_loop accum =\n" oc.indent ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code "%sif not (%a accum) then accum else\n" oc.indent
      Identifier.print cond ;
    Printf.fprintf oc.code "%slet accum = %a accum in\n" oc.indent
      Identifier.print loop ;
    Printf.fprintf oc.code "%swhile_loop accum in\n" oc.indent) ;
  Printf.fprintf oc.code "%slet %a = while_loop %a in\n" oc.indent
      Identifier.print id_res
      Identifier.print v0 ;
  id_res

(* Same as while_ but check the condition only after the loop body: *)
let loop_until oc ~loop ~cond v0 =
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%slet rec until_loop accum =\n" oc.indent ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code "%slet accum = %a accum in\n" oc.indent
      Identifier.print loop ;
    Printf.fprintf oc.code
      "%sif %a accum then until_loop accum else accum in\n" oc.indent
      Identifier.print cond) ;
  Printf.fprintf oc.code "%slet %a = until_loop %a in\n" oc.indent
    Identifier.print id_res
    Identifier.print v0 ;
  id_res

let loop_repeat oc ~from ~to_ ~loop v0 =
  let id_res = Identifier.auto () in
  Printf.fprintf oc.code "%slet rec repeat_loop n accum =\n" oc.indent ;
  indent_more oc (fun () ->
    Printf.fprintf oc.code "%sif n >= %a then accum else\n" oc.indent
      Identifier.print to_ ;
    Printf.fprintf oc.code "%slet accum = %a accum in\n" oc.indent
      Identifier.print loop ;
    Printf.fprintf oc.code "%srepeat_loop (Int32.succ n) accum in\n" oc.indent) ;
  Printf.fprintf oc.code "%slet %a = repeat_loop %a %a in\n" oc.indent
    Identifier.print id_res
    Identifier.print v0
    Identifier.print from ;
  id_res

let rec print_default_value indent oc typ =
  let open Type in
  let wrap_nullable vtyp p x =
    if ValueType.is_nullable vtyp then
      (* Unfortunately we cannot start with None as we want the whole tree
       * of values to be populated. *)
      Printf.fprintf oc "Some (%a)" p x
    else
      p oc x
  in
  match typ with
  | Type.TValue ValueType.((Nullable TFloat | NotNullable TFloat) as vtyp) ->
      wrap_nullable vtyp String.print "0."
  | Type.TValue ValueType.((Nullable TString | NotNullable TString) as vtyp) ->
      wrap_nullable vtyp String.print "\"\""
  | Type.TValue ValueType.((Nullable TBool | NotNullable TBool) as vtyp) ->
      wrap_nullable vtyp String.print "false"
  | Type.TValue ValueType.((Nullable TChar | NotNullable TChar) as vtyp) ->
      wrap_nullable vtyp String.print "'\\000'"
  | Type.TValue ValueType.((Nullable TI8 | NotNullable TI8) as vtyp) ->
      wrap_nullable vtyp String.print "Int8.zero"
  | Type.TValue ValueType.((Nullable TI16 | NotNullable TI16) as vtyp) ->
      wrap_nullable vtyp String.print "Int16.zero"
  | Type.TValue ValueType.((Nullable TI32 | NotNullable TI32) as vtyp) ->
      wrap_nullable vtyp String.print "Int32.zero"
  | Type.TValue ValueType.((Nullable TI64 | NotNullable TI64) as vtyp) ->
      wrap_nullable vtyp String.print "Int64.zero"
  | Type.TValue ValueType.((Nullable TI128 | NotNullable TI128) as vtyp) ->
      wrap_nullable vtyp String.print "Int128.zero"
  | Type.TValue ValueType.((Nullable TU8 | NotNullable TU8) as vtyp) ->
      wrap_nullable vtyp String.print "Uint8.zero"
  | Type.TValue ValueType.((Nullable TU16 | NotNullable TU16) as vtyp) ->
      wrap_nullable vtyp String.print "Uint16.zero"
  | Type.TValue ValueType.((Nullable TU32 | NotNullable TU32) as vtyp) ->
      wrap_nullable vtyp String.print "Uint32.zero"
  | Type.TValue ValueType.((Nullable TU64 | NotNullable TU64) as vtyp) ->
      wrap_nullable vtyp String.print "Uint64.zero"
  | Type.TValue ValueType.((Nullable TU128 | NotNullable TU128) as vtyp) ->
      wrap_nullable vtyp String.print "Uint128.zero"
  | Type.TValue ValueType.((Nullable (TTup typs) | NotNullable (TTup typs)) as vtyp) ->
      wrap_nullable vtyp
        (Array.print ~first:("{\n"^indent) ~last:(indent^"}") ~sep:(";\n"^indent)
          (fun oc (i, t) ->
            let fname = subfield_name vtyp i in
            Printf.fprintf oc "%s=%a"
              fname (print_default_value (indent^"  ")) (Type.TValue t)))
        (Array.mapi (fun i t -> (i, t)) typs)
  | Type.TValue ValueType.((Nullable (TRec typs) | NotNullable (TRec typs)) as vtyp) ->
      wrap_nullable vtyp
        (Array.print ~first:("{\n"^indent) ~last:(indent^"}") ~sep:(";\n"^indent)
          (fun oc (fname, t) ->
            Printf.fprintf oc "%s=%a"
              fname (print_default_value (indent^"  ")) (Type.TValue t)))
        typs
  | Type.TValue ValueType.((Nullable (TVec (dim, t)) | NotNullable (TVec (dim, t))) as vtyp) ->
      wrap_nullable vtyp (fun oc () ->
        Printf.fprintf oc "[| " ;
        for i = 0 to dim - 1 do
          Printf.fprintf oc "%a; "
            (print_default_value (indent^"  ")) (Type.TValue t)
        done ;
        Printf.fprintf oc "%s|]" indent) ()
  | Type.TValue ValueType.((Nullable (TList _) | NotNullable (TList _)) as vtyp) ->
      wrap_nullable vtyp String.print "[]"
  | Type.TPair (t1, t2) ->
      Printf.fprintf oc "(%a, %a)"
        (print_default_value indent) t1
        (print_default_value indent) t2
  | Type.TPointer | Type.TSize | Type.TBit | Type.TByte | Type.TWord
  | Type.TDWord | Type.TQWord | Type.TOWord | Type.TBytes
  | Type.TFunction0 _ | Type.TFunction1 _ | Type.TFunction2 _ | Type.TFunction3 _ ->
      assert false

(* For heap allocated values, all subtypes are unboxed so we can perform a
 * single allocation. *)
let alloc_value oc vtyp =
  let typ = Type.TValue vtyp in
  oc.value_typname <- find_or_declare_type oc typ ;
  emit_pointer oc (fun oc' ->
    (* Build the shared_ptr here that the type is known: *)
    Printf.fprintf oc' "(%a : %s)"
      (print_default_value oc.indent) typ
      oc.value_typname)

let id_of_path frames p =
  let base = (p : _ Identifier.t :> string) in
  let rec loop = function
    | [] ->
        assert false
    | [ _ ] ->
        base
    | top :: (parent :: _ as rest) ->
        (* Dereference parent: *)
        let subfield = subfield_name parent.typ top.index in
        let base = loop rest in
        let base =
          if ValueType.is_nullable parent.typ then
            "(option_get "^ base ^")"
          else
            base in
        base ^"."^ subfield
  in
  loop frames

let set_field oc frames p v =
  let id = id_of_path frames p in
  Printf.fprintf oc.code "%s%s <- %a;\n" oc.indent
    id
    Identifier.print v

let set_nullable_field oc frames idx v =
  match v with
  | Some v ->
      set_field oc frames idx (Identifier.modify "(Some " v ")")
  | None ->
      set_field oc frames idx (Identifier.of_string "None")

let get_field _oc frames p =
  let id = id_of_path frames p in
  Identifier.of_string id

let get_nullable_field _oc frames p =
  let id = id_of_path frames p in
  Identifier.of_string ("(option_get "^ id ^")")

let field_is_set oc frames p =
  let id = id_of_path frames p in
  emit_bool oc (fun oc ->
    Printf.fprintf oc "%s <> None" id)
