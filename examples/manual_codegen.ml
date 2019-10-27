open Batteries

module C = Code.BackEnd_C
module E = Code.Expr (C)
module T = Code.Type
module Identifier = Code.Identifier

module RowBinary =
struct
  type pointer = Identifier.t
  type 'a des = C.output -> pointer -> 'a * pointer

  let dfloat oc p =
    let w, p = C.read_qword oc p in
    C.float_of_qword oc w, p

  let read_leb128 oc p =
    let t_pair_u32_u8 = T.(make (TTup [| make TU32 ; make TU8 |]))
    and t_byte = T.(make TByte)
    and t_bool = T.(make TBool)
    in
    let cond =
      C.print_function1 oc t_bool t_byte (fun oc b ->
        C.U8.gt oc (C.U8.of_byte oc b) (C.U8.of_const_int oc 128))
    and reduce =
      C.print_function2 oc t_pair_u32_u8 t_pair_u32_u8 t_byte (fun oc leb_shft_tup byte ->
        let leb = C.tuple_get oc leb_shft_tup 0
        and shft = C.tuple_get oc leb_shft_tup 1 in
        C.make_tuple oc t_pair_u32_u8 [|
          C.U32.add oc (C.U32.shift_left oc (C.U32.of_byte oc leb) shft) byte ;
          C.U8.add oc shft (C.U8.of_const_int oc 7)
        |])
    in
    let u32_zero = C.U32.of_const_int oc 0
    and u8_zero = C.U8.of_const_int oc 0 in
    let init = C.make_tuple oc t_pair_u32_u8 [| u32_zero ; u8_zero |] in
    let leb_shft_tup, p = C.read_while oc ~cond ~reduce init p in
    (* Still have to add the last byte: *)
    let last_b, p = C.read_byte oc p in
    let leb = C.tuple_get oc leb_shft_tup 0
    and shft = C.tuple_get oc leb_shft_tup 1 in
    C.U32.add oc (C.U32.shift_left oc (C.U32.of_byte oc last_b) shft) leb,
    p

  (* Given a list of fields * typ, generate a function that takes a pointer and
   * a size, and deserialize a RowBinary tuple into a non-nullable value of
   * TTup type: *)
  let des typ = ignore typ

  let dstring oc p =
    let len, p = read_leb128 oc p in
    let bs, p = C.read_bytes oc p len in
    C.string_of_bytes oc bs,
    p

  let dbool oc p =
    let b, p = C.read_byte oc p in
    C.(bool_not oc U8.(eq oc (of_byte oc b) (of_const_int oc 0))),
    p

  let di8 oc p =
    let b, p = C.read_byte oc p in
    C.I8.of_byte oc b,
    p

  let du8 oc p =
    let b, p = C.read_byte oc p in
    C.U8.of_byte oc b,
    p

  let di16 oc p =
    let w, p = C.read_word oc p in
    C.I16.of_word oc w,
    p

  let du16 oc p =
    let w, p = C.read_word oc p in
    C.U16.of_word oc w,
    p

  let di32 oc p =
    let w, p = C.read_dword oc p in
    C.I32.of_dword oc w,
    p

  let du32 oc p =
    let w, p = C.read_dword oc p in
    C.U32.of_dword oc w,
    p

  let di64 oc p =
    let w, p = C.read_qword oc p in
    C.I64.of_qword oc w,
    p

  let du64 oc p =
    let w, p = C.read_qword oc p in
    C.U64.of_qword oc w,
    p

  let di128 oc p =
    let w, p = C.read_oword oc p in
    C.I128.of_oword oc w,
    p

  let du128 oc p =
    let w, p = C.read_oword oc p in
    C.U128.of_oword oc w,
    p

  (* Items of a tuples are just concatenated together: *)
  let tup_opn _typs _oc p = p
  let tup_cls _typs _oc p = p
  let tup_sep _typs _n _oc p = p

  (* Vectors: ClickHouse does not distinguish between vectors (or known
   * dimension) and lists (of variable length). But it has varchars, which
   * are close to our vectors, and that come without any length on the wire.
   * So we assume vectors are not prefixed by any length, and out lists are
   * what ClickHouse refers to as arrays. *)
  let vec_opn _dim _typ _oc p = p
  let vec_cls _dim _typ _oc p = p
  let vec_sep _dim _typ _oc _n p = p

  (* TODO: lists *)

  (* "For NULL support, an additional byte containing 1 or 0 is added before
   * each Nullable value. If 1, then the value is NULL and this byte is
   * interpreted as a separate value. If 0, the value after the byte is not
   * NULL." *)
  let is_null _typ oc p =
    let b = C.peek_byte oc p in
    C.U8.(eq oc (of_byte oc b) (of_const_int oc 1))

  let dnull oc p = C.pointer_add oc p (C.size_of_const oc 1)
  let dnotnull = dnull
end

module type DES =
sig
  type pointer = Identifier.t
  type 'a des = C.output -> pointer -> 'a * pointer

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

  val tup_opn : T.t array -> C.output -> pointer -> pointer
  val tup_cls : T.t array -> C.output -> pointer -> pointer
  val tup_sep : T.t array -> int (* before *) -> C.output -> pointer -> pointer
  val vec_opn : int -> T.t -> C.output -> pointer -> pointer
  val vec_cls : int -> T.t -> C.output -> pointer -> pointer
  val vec_sep : int -> T.t -> int (* before *) -> C.output -> pointer -> pointer

  val is_null : T.structure -> C.output -> pointer -> Identifier.t
  val dnull : C.output -> pointer -> pointer
  val dnotnull : C.output -> pointer -> pointer
end

module type SER =
sig
  type pointer = Identifier.t
  type 'a ser = C.output -> 'a -> pointer -> pointer

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

  val tup_opn : T.t array -> C.output -> pointer -> pointer
  val tup_cls : T.t array -> C.output -> pointer -> pointer
  val tup_sep : T.t array -> int (* before *) -> C.output -> pointer -> pointer
  val vec_opn : int -> T.t -> C.output -> pointer -> pointer
  val vec_cls : int -> T.t -> C.output -> pointer -> pointer
  val vec_sep : int -> T.t -> int (* before *) -> C.output -> pointer -> pointer

  val snull : C.output -> pointer -> pointer
  val snotnull : C.output -> pointer -> pointer
end


(* Many return values have the type of a pair or src*dst pointers: *)
let t_pair_ptrs = T.(make (TTup [| make TPointer ; make TPointer |]))

module DesSer (Des : DES) (Ser : SER) =
struct
  let ds ser des oc src dst =
    let v, src = des oc src in
    let dst = ser oc v dst in
    C.make_tuple oc t_pair_ptrs [| src ; dst |]

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
    C.make_tuple oc t_pair_ptrs [|
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
        C.make_tuple oc t_pair_ptrs [|
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

  and desser : T.t -> C.output -> Identifier.t -> Identifier.t ->
                 (Identifier.t * Identifier.t) = fun typ oc src dst ->
    let desser_structure = function
      | T.TFloat -> dsfloat
      | T.TString -> dsstring
      | T.TBool -> dsbool
      | T.TI8 -> dsi8
      | T.TI16 -> dsi16
      | T.TI32 -> dsi32
      | T.TI64 -> dsi64
      | T.TI128 -> dsi128
      | T.TU8 -> dsu8
      | T.TU16 -> dsu16
      | T.TU32 -> dsu32
      | T.TU64 -> dsu64
      | T.TU128 -> dsu128
      | T.TTup typs -> dstup typs
      | T.TVec (d, typ) -> dsvec d typ
      | _ -> assert false
    in
    let pair =
      if typ.nullable then
        let cond = Des.is_null typ.structure oc src in
        C.choose oc ~cond
          (fun oc ->
            let s, d = dsnull oc src dst in
            C.make_tuple oc t_pair_ptrs [| s ; d |])
          (fun oc ->
            let s, d = dsnotnull oc src dst in
            desser_structure typ.structure oc s d)
      else
        desser_structure typ.structure oc src dst in
    (* Returns src and dest: *)
    C.tuple_get oc pair 0,
    C.tuple_get oc pair 1
end

module DevNull =
struct
  type pointer = Identifier.t
  type 'a ser = C.output -> 'a -> pointer -> pointer

  let sfloat oc x p = C.ignore oc x ; p
  let sstring oc x p = C.ignore oc x ; p
  let sbool oc x p = C.ignore oc x ; p
  let si8 oc x p = C.ignore oc x ; p
  let su8 oc x p = C.ignore oc x ; p
  let si16 oc x p = C.ignore oc x ; p
  let su16 oc x p = C.ignore oc x ; p
  let si32 oc x p = C.ignore oc x ; p
  let su32 oc x p = C.ignore oc x ; p
  let si64 oc x p = C.ignore oc x ; p
  let su64 oc x p = C.ignore oc x ; p
  let si128 oc x p = C.ignore oc x ; p
  let su128 oc x p = C.ignore oc x ; p
  let tup_opn _ _ p = p
  let tup_cls _ _ p = p
  let tup_sep _ _ _ p = p
  let vec_opn _ _ _ p = p
  let vec_cls _ _ _ p = p
  let vec_sep _ _ _ _ p = p
  let snull _ p = p
  let snotnull _ p = p
end

module DS = DesSer (RowBinary) (DevNull)

let compile_output output =
  let mode = [ `create ; `excl ; `text ] in
  let fname =
    File.with_temporary_out ~mode ~suffix:".cpp" (fun oc fname ->
      Printf.fprintf oc "#include \"runtime.h\"\n\n" ;
      C.print_output oc output ;
      fname) in
  Printf.printf "Output in %s\n" fname ;
  let cmd = Printf.sprintf "g++ -W -Wall -I src -c %s" fname in
  match Unix.system cmd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code ->
      Printf.eprintf "%s failed with code %d\n" cmd code
  | Unix.WSIGNALED s ->
      Printf.eprintf "%s killed with signal %d" cmd s
  | Unix.WSTOPPED s ->
      Printf.eprintf "%s stopped by signal %d" cmd s


let () =
(*  let expr =
    E.(RemSize
        (AddPointer
           (MakePointer (ConstSize 1000),
                         ConstSize 10))) in *)
  let typ =
    let nullable = true in
    T.(make (TTup [|
      make TString ;
      make TU64 ;
      make TU64 ;
      make TU8 ;
      make TString ;
      make TU8 ;
      make TString ;
      make ~nullable TU32 ;
      make ~nullable TU32 ;
      make TU64 ;
      make TU64 ;
      make TU32 ;
      make TU32 ;
      make ~nullable TU32 ;
      make ~nullable TString ;
      make ~nullable TU32 ;
      make ~nullable TString ;
      make ~nullable TU32 ;
      make ~nullable TString ;
      make TU16 ;
      make TU16 ;
      make TU8 ;
      make TU8 ;
      make ~nullable TU32 ;
      make ~nullable TU32 ;
      make TU32 ;
      make TString ;
      make TU64 ;
      make TU64 ;
      make TU64 ;  (* Should be U32 *)
      make TU64 ;  (* Should be U32 *)
      make TU64 ;
      make TU64 ;
      make ~nullable TString
    |])) in
  let output = C.make_output () in
  let _read_tuple =
    C.print_function2 output t_pair_ptrs T.(make TPointer) T.(make TPointer) (fun oc src dst ->
      let src, dst = DS.desser typ oc src dst in
      C.make_tuple oc t_pair_ptrs [| src ; dst |]) in
  compile_output output
