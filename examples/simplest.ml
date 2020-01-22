open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open DessserTools

(* The simplest possible deserializer *)
module TestDes : DES =
struct
  type state = unit
  let ptr _mtyp = dataptr

  let start _mtyp src = (), src
  let stop () src = src
  type des = state -> (*dataptr*) e -> (* (nn * dataptr) *) e

  let from_byte v1 v2 =
    fun src ->
      let b_src = ReadByte src in
      MapPair (b_src,
        func [|byte; dataptr|] (fun fid ->
          Pair (
            Choose (BoolOfU8 (U8OfByte (Param (fid, 0))), v1, v2),
            Param (fid, 1))))

  let dfloat () = from_byte (Float 1.) (Float 0.)
  let dstring () = from_byte (String "x") (String "")
  let dbool () = from_byte (Bool true) (Bool false)
  let dchar () src =
    MapPair (ReadByte src,
      func [|byte; dataptr|] (fun fid ->
        Pair (CharOfU8 (U8OfByte (Param (fid, 0))), Param (fid, 1))))
  let di8 () = from_byte (I8 1) (I8 0)
  let di16 () = from_byte (I16 1) (I16 0)
  let di24 () = from_byte (I24 1) (I24 0)
  let di32 () = from_byte (I32 1l) (I32 0l)
  let di40 () = from_byte (I40 1L) (I40 0L)
  let di48 () = from_byte (I48 1L) (I48 0L)
  let di56 () = from_byte (I56 1L) (I56 0L)
  let di64 () = from_byte (I64 1L) (I64 0L)
  let di128 () = from_byte (I128 Int128.one) (I128 Int128.zero)
  let du8 () = from_byte (U8 1) (U8 0)
  let du16 () = from_byte (U16 1) (U16 0)
  let du24 () = from_byte (U24 1) (U24 0)
  let du32 () = from_byte (U32 Uint32.one) (U32 Uint32.zero)
  let du40 () = from_byte (U40 Uint40.one) (U40 Uint40.zero)
  let du48 () = from_byte (U48 Uint48.one) (U48 Uint48.zero)
  let du56 () = from_byte (U56 Uint56.one) (U56 Uint56.zero)
  let du64 () = from_byte (U64 Uint64.one) (U64 Uint64.zero)
  let du128 () = from_byte (U128 Uint128.one) (U128 Uint128.zero)
  let tup_opn () _ src = src
  let tup_cls () src = src
  let tup_sep _ () src = src
  let rec_opn () _ src = src
  let rec_cls () src = src
  let rec_sep _ () src = src
  let vec_opn () _ _ src = src
  let vec_cls () src = src
  let vec_sep _ () src = src
  let list_opn () _ src =
    let b_src = ReadByte src in
    MapPair (b_src,
      func [|byte; dataptr|] (fun fid ->
        Pair (
          ToU32 (U8OfByte (Param (fid, 0))),
          Param (fid, 1))))
  let list_cls () src = src
  let list_sep () src = src
  let is_null () src =
    BoolOfU8 (U8OfByte (PeekByte (src, Size 0)))
  let dnull _t () src = DataPtrAdd (src, Size 1)
  let dnotnull = dnull
end

(* The simplest possible serializer *)
module TestSer : SER =
struct
  type state = unit
  let ptr _mtyp = dataptr

  let start _v dst = (), dst
  let stop () dst = dst
  type ser = state -> (*nn*) e -> (*dataptr*) e -> (*dataptr*) e

  let from_bool b dst =
    WriteByte (dst, ByteOfU8 (U8OfBool b))
  let from_eq v1 v = from_bool (Eq (v1, v))
  let sfloat () = from_eq (Float 1.)
  let sstring () v = from_bool (Ge (StringLength v, U32 Uint32.zero))
  let sbool () = from_bool

  let schar () b dst = WriteByte (dst, ByteOfU8 (U8OfChar b))
  let si8 () = from_eq (I8 1)
  let si16 () = from_eq (I16 1)
  let si24 () = from_eq (I24 1)
  let si32 () = from_eq (I32 1l)
  let si40 () = from_eq (I40 1L)
  let si48 () = from_eq (I48 1L)
  let si56 () = from_eq (I56 1L)
  let si64 () = from_eq (I64 1L)
  let si128 () = from_eq (I128 Int128.one)
  let su8 () = from_eq (U8 1)
  let su16 () = from_eq (U16 1)
  let su24 () = from_eq (U24 1)
  let su32 () = from_eq (U32 Uint32.one)
  let su40 () = from_eq (U40 Uint40.one)
  let su48 () = from_eq (U48 Uint48.one)
  let su56 () = from_eq (U56 Uint56.one)
  let su64 () = from_eq (U64 Uint64.one)
  let su128 () = from_eq (U128 Uint128.one)
  let tup_opn () _ dst = dst
  let tup_cls () dst = dst
  let tup_sep _i () dst = dst
  let rec_opn () _ dst = dst
  let rec_cls () dst = dst
  let rec_sep _i () dst = dst
  let vec_opn () _ _ dst = dst
  let vec_cls () dst = dst
  let vec_sep _i () dst = dst
  let list_opn () _ dst _n = dst
  let list_cls () dst = dst
  let list_sep () dst = dst
  let nullable () dst = dst
  let snull _t () dst = WriteByte (dst, Byte 1)
  let snotnull _t () dst = WriteByte (dst, Byte 0)

  type ssizer = maybe_nullable -> path -> (*valueptr*) e -> ssize
  let ssize_of_float _ _ _ = ConstSize 1
  let ssize_of_string _ _ _ = ConstSize 1
  let ssize_of_bool _ _ _ = ConstSize 1
  let ssize_of_char _ _ _ = ConstSize 1
  let ssize_of_i8 _ _ _ = ConstSize 1
  let ssize_of_i16 _ _ _ = ConstSize 1
  let ssize_of_i24 _ _ _ = ConstSize 1
  let ssize_of_i32 _ _ _ = ConstSize 1
  let ssize_of_i40 _ _ _ = ConstSize 1
  let ssize_of_i48 _ _ _ = ConstSize 1
  let ssize_of_i56 _ _ _ = ConstSize 1
  let ssize_of_i64 _ _ _ = ConstSize 1
  let ssize_of_i128 _ _ _ = ConstSize 1
  let ssize_of_u8 _ _ _ = ConstSize 1
  let ssize_of_u16 _ _ _ = ConstSize 1
  let ssize_of_u24 _ _ _ = ConstSize 1
  let ssize_of_u32 _ _ _ = ConstSize 1
  let ssize_of_u40 _ _ _ = ConstSize 1
  let ssize_of_u48 _ _ _ = ConstSize 1
  let ssize_of_u56 _ _ _ = ConstSize 1
  let ssize_of_u64 _ _ _ = ConstSize 1
  let ssize_of_u128 _ _ _ = ConstSize 1
  let ssize_of_tup _ _ _ = ConstSize 1
  let ssize_of_rec _ _ _ = ConstSize 1
  let ssize_of_vec _ _ _ = ConstSize 1
  let ssize_of_list _ _ _ = ConstSize 1
  let ssize_of_null _ _ = ConstSize 1
end

(* The simplest possible converter: *)
module TestDesSer = DesSer (TestDes) (TestSer)

let test_desser () =
  let mtyp = NotNullable (TTup [| Nullable (Mac TU8) ;
                                  NotNullable (Mac TChar) |]) in
  let src = DataPtrOfString "\001X"
  and dst = DataPtrOfString "_____" in
  Let ("e", TestDesSer.desser mtyp src dst,
         Seq [
          Dump (Identifier "e") ;
          Dump (String "\n") ;
          Identifier "e" ])

(* Test: generate the source for test_desser and compile it: *)
let test_backend () =
  let e = test_desser () in
  type_check [] e ;
  let backend, exe_ext, outro =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "ocaml" then
      (module BackEndOCaml : BACKEND), ".opt", ""
    else if Array.length Sys.argv > 1 && Sys.argv.(1) = "c++" then
      (module BackEndCPP : BACKEND), ".exe", "int main() { return 0; }\n"
    else (
      Printf.eprintf "%s ocaml|c++\n" Sys.argv.(0) ;
      exit 1
    ) in
  let module BE = (val backend : BACKEND) in
  let state = BE.make_state () in
  let state, _, _entry_point =
    BE.identifier_of_expression state ~name:"entry_point" e in
  let exe_fname = "/tmp/simplest_gen"^ exe_ext in
  let src_fname = change_ext BE.preferred_def_extension exe_fname in
  write_source ~src_fname (fun oc ->
      BE.print_definitions state oc ;
      String.print oc outro) ;
  compile ~link:true backend src_fname exe_fname

let main =
  test_backend ()
