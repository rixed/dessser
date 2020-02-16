open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open DessserTools
open DessserDSTools
open Ops
module T = DessserTypes

(* The simplest possible deserializer *)
module TestDes : DES =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _vtyp src = (), src
  let stop () src = src
  type des = state -> maybe_nullable -> path -> (*dataptr*) e -> (* (v * dataptr) *) e

  let from_byte v1 v2 _ _ src =
    let b_src = read_byte src in
    with_sploded_pair "from_byte" b_src (fun b src ->
      pair
        (choose ~cond:(bool_of_u8 (u8_of_byte b)) v1 v2)
        src)

  let dfloat () = from_byte (float 1.) (float 0.)
  let dstring () = from_byte (string "x") (string "")
  let dbool () = from_byte (bool true) (bool false)
  let dchar () _ _ src =
    with_sploded_pair "dchar" (read_byte src) (fun b src ->
      pair (char_of_u8 (u8_of_byte b)) src)
  let di8 () = from_byte (i8 1) (i8 0)
  let di16 () = from_byte (i16 1) (i16 0)
  let di24 () = from_byte (i24 1) (i24 0)
  let di32 () = from_byte (i32 1l) (i32 0l)
  let di40 () = from_byte (i40 1L) (i40 0L)
  let di48 () = from_byte (i48 1L) (i48 0L)
  let di56 () = from_byte (i56 1L) (i56 0L)
  let di64 () = from_byte (i64 1L) (i64 0L)
  let di128 () = from_byte (i128 Int128.one) (i128 Int128.zero)
  let du8 () = from_byte (u8 1) (u8 0)
  let du16 () = from_byte (u16 1) (u16 0)
  let du24 () = from_byte (u24 1) (u24 0)
  let du32 () = from_byte (u32 Uint32.one) (u32 Uint32.zero)
  let du40 () = from_byte (u40 Uint40.one) (u40 Uint40.zero)
  let du48 () = from_byte (u48 Uint48.one) (u48 Uint48.zero)
  let du56 () = from_byte (u56 Uint56.one) (u56 Uint56.zero)
  let du64 () = from_byte (u64 Uint64.one) (u64 Uint64.zero)
  let du128 () = from_byte (u128 Uint128.one) (u128 Uint128.zero)
  let tup_opn () _ _ _ src = src
  let tup_cls () _ _ src = src
  let tup_sep _ () _ _ src = src
  let rec_opn () _ _ _ src = src
  let rec_cls () _ _ src = src
  let rec_sep _ () _ _ src = src
  let vec_opn () _ _ _ _ src = src
  let vec_cls () _ _ src = src
  let vec_sep _ () _ _ src = src
  let list_opn = KnownSize (fun () _ _ _ src ->
    let b_src = read_byte src in
    map_pair b_src
      (func2 T.byte T.dataptr (fun _l b p ->
        pair (to_u32 (u8_of_byte b)) p)))
  let list_cls () _ _ src = src
  let list_sep () _ _ src = src
  let is_null () _ _ src =
    bool_of_u8 (u8_of_byte (peek_byte src (size 0)))
  let dnull _t () _ _ src = data_ptr_add src (size 1)
  let dnotnull = dnull
end

(* The simplest possible serializer *)
module TestSer : SER =
struct
  type state = unit
  let ptr _vtyp = dataptr

  let start _v dst = (), dst
  let stop () dst = dst
  type ser = state -> maybe_nullable -> path -> (*v*) e -> (*dataptr*) e -> (*dataptr*) e

  let from_bool b dst =
    write_byte dst (byte_of_u8 (u8_of_bool b))
  let from_eq v1 v = from_bool (eq v1 v)
  let sfloat () _ _ = from_eq (float 1.)
  let sstring () _ _ v = from_bool (ge (string_length v) (u32 Uint32.zero))
  let sbool () _ _ = from_bool

  let schar () _ _ b dst = write_byte dst (byte_of_u8 (u8_of_char b))
  let si8 () _ _ = from_eq (i8 1)
  let si16 () _ _ = from_eq (i16 1)
  let si24 () _ _ = from_eq (i24 1)
  let si32 () _ _ = from_eq (i32 1l)
  let si40 () _ _ = from_eq (i40 1L)
  let si48 () _ _ = from_eq (i48 1L)
  let si56 () _ _ = from_eq (i56 1L)
  let si64 () _ _ = from_eq (i64 1L)
  let si128 () _ _ = from_eq (i128 Int128.one)
  let su8 () _ _ = from_eq (u8 1)
  let su16 () _ _ = from_eq (u16 1)
  let su24 () _ _ = from_eq (u24 1)
  let su32 () _ _ = from_eq (u32 Uint32.one)
  let su40 () _ _ = from_eq (u40 Uint40.one)
  let su48 () _ _ = from_eq (u48 Uint48.one)
  let su56 () _ _ = from_eq (u56 Uint56.one)
  let su64 () _ _ = from_eq (u64 Uint64.one)
  let su128 () _ _ = from_eq (u128 Uint128.one)
  let tup_opn () _ _ _ dst = dst
  let tup_cls () _ _ dst = dst
  let tup_sep _i () _ _ dst = dst
  let rec_opn () _ _ _ dst = dst
  let rec_cls () _ _ dst = dst
  let rec_sep _i () _ _ dst = dst
  let vec_opn () _ _ _ _ dst = dst
  let vec_cls () _ _ dst = dst
  let vec_sep _i () _ _ dst = dst
  let list_opn () _ _ _ _n dst = dst
  let list_cls () _ _ dst = dst
  let list_sep () _ _ dst = dst
  let nullable () _ _ dst = dst
  let snull _t () _ _ dst = write_byte dst (byte 1)
  let snotnull _t () _ _ dst = write_byte dst (byte 0)

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
  let vtyp = NotNullable (TTup [| Nullable (Mac TU8) ;
                                  NotNullable (Mac TChar) |]) in
  let src = data_ptr_of_string "\001X"
  and dst = data_ptr_of_string "_____" in
  let1 (TestDesSer.desser vtyp src dst) (fun e ->
    seq [ dump e ;
          dump (string "\n") ;
          e ])

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
