open Batteries
open Stdint
open Dessser
open DessserTools

let run_cmd cmd =
  match Unix.system cmd with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code ->
      Printf.sprintf "%s failed with code %d\n" cmd code |>
      failwith
  | Unix.WSIGNALED s ->
      Printf.sprintf "%s killed with signal %d" cmd s |>
      failwith
  | Unix.WSTOPPED s ->
      Printf.sprintf "%s stopped by signal %d" cmd s |>
      failwith

let () =
  let m x = ValueType.NotNullable x
  and n x = ValueType.Nullable x in
  let udp_typ =
    m (Tup [|
      m String ; m U64 ; m U64 ; m U8 ; m String ; m U8 ; m String ; n U32 ;
      n U32 ; m U64 ; m U64 ; m U32 ; m U32 ; n U32 ; n String ; n U32 ;
      n String ; n U32 ; n String ; m U16 ; m U16 ; m U8 ; m U8 ; n U32 ;
      n U32 ; m U32 ; m String ; m U64 ; m U64 ; m U64 ; (* Should be U32 *) m U64 ;
      (* Should be U32 *) m U64 ; m U64 ; n String
    |])
  and _http_typ =
    m (Tup [|
      m String ; m U64 ; m U64 ; m U8 ; m String ; m U8 ; m String ;
      n U32 ; n U32 ; m U64 ; m U64 ; m U32 ; m U32 ;
      n U32 ; n (Vec (16, m Char)) ;
      n U32 ; n (Vec (16, m Char)) ;
      m U16 ; m U16 ; m U128 ; m U128 ; m U128 ; n U128 ;
      m U8 ; m U8 ; m U8 ; n String ; n String ;
      n String (* url *) ; n String ; m U8 ; m U8 ; m U8 ;
      n U32 ; n (Vec (16, m Char)) ;
      m U8 ; m U8 ; m U64 ; m U64 ; m U8 ; m U32 ; m U32 ; m U32 ;
      n String ; m U32 ; m U8 ; n String ;
      n U64 ; n U64 ; n U32 ;
      m U32 ; m U32 ; m U32 ;
      n String ; m U32 ; m U8 ; n String ;
      m U32 ; m U32 ; m U16 ; m U16 ; m U16 ;
      m U64 ; m U64 ; m U64 ; m Float ; m U8 ; m I64 ; m Float ;
      m I64 ; m Float ; m I64 ; m Float ; m U32 |]) in
  let typ = udp_typ in
  let backend, exe_ext =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "ocaml" then
      (module BackEndOCaml : BACKEND), ".opt"
    else if Array.length Sys.argv > 1 && Sys.argv.(1) = "c++" then
      (module BackEndCPP : BACKEND), ".exe"
    else (
      Printf.eprintf "%s ocaml|c++\n" Sys.argv.(0) ;
      exit 1
    ) in
  let module BE = (val backend : BACKEND) in
  let convert_only = false in
  let convert =
    if convert_only then (
      (* Just convert the rowbinary to s-expr: *)
      let module DS = DesSer (RowBinary.Des) (SExpr.Ser) in
      let tptr = Type.DataPtr in
      let open Expression in
      func [|tptr; tptr|] (fun fid ->
        let src = Param (fid, 0) and dst = Param (fid, 1) in
        Comment ("Convert from RowBinary into S-Expression:",
          DS.desser typ src dst))
    ) else (
      (* convert from RowBinary into a heapvalue, compute its serialization
       * size in RamenringBuf format, then convert it into S-Expression: *)
      let module DS1 = DesSer (RowBinary.Des) (HeapValue.Ser) in
      let module DS2 = DesSer (HeapValue.Des) (SExpr.Ser) (*(RamenRingBuffer.Ser (BE))*) in
      let module Sizer = HeapValue.SerSizer (RamenRingBuffer.Ser) in

      let tptr = Type.DataPtr in
      let open Expression in
      func [|tptr; tptr|] (fun fid ->
        let src = Param (fid, 0) and dst = Param (fid, 1) in
        Comment ("Convert from RowBinary into a heap value:",
          let vptr = AllocValue typ in
          let src_valueptr = DS1.desser typ src vptr in
          with_sploded_pair "src_valueptr" src_valueptr (fun src valueptr ->
            Comment ("Compute the serialized size of this tuple:",
              let const_dyn_sz = Sizer.sersize typ valueptr in
              with_sploded_pair "read_tuple" const_dyn_sz (fun const_sz dyn_sz ->
                Seq [
                  Dump (String "Constant size: ") ;
                  Dump const_sz ;
                  Dump (String ", dynamic size: ") ;
                  Dump dyn_sz ;
                  Dump (String "\n") ;
                  Comment ("Now convert the heap value into an SExpr:",
                    let src_dst' = DS2.desser typ valueptr dst in
                    Pair (src, Snd (src_dst'))) ])))))
    ) in
  Printf.printf "convert = %a\n%!" (Expression.print ?max_depth:None) convert ;
  Expression.type_check [] convert ;
  let state = BE.make_state  () in
  let state, _, entry_point =
    BE.identifier_of_expression state ~name:"convert" convert in
  let exe_fname = "examples/rowbinary2sexpr"^ exe_ext in
  compile ~optim:3 BE.preferred_file_extension exe_fname (fun oc ->
    BE.print_source state oc ;
    if BE.preferred_file_extension = "cc" then
      Printf.fprintf oc {|
static std::string readWholeFile(std::string const fname)
{
  std::ifstream t(fname);
  std::string str(std::istreambuf_iterator<char>(t),
                  (std::istreambuf_iterator<char>()));
  return str;
}

int main(int numArgs, char **args)
{
  char const *fname = "/dev/stdin";
  char delim = '\n';

  for (int a = 1; a < numArgs; a++) {
    if (a < numArgs - 1 && (
          0 == strcasecmp(args[a], "--delim") ||
          0 == strcasecmp(args[a], "-d")
        )) {
      delim = args[a+1][0];
    } else {
      fname = args[a];
    }
  }

  std::string input = readWholeFile(fname);
  Pointer src(input);

  while (src.rem() > 0) {
    Size outSz(1024);
    Pointer dst(outSz);

    std::pair<Pointer, Pointer> ptrs = %s(src, dst);

    // Print serialized:
    assert(ptrs.second.offset < ptrs.second.size-1);
    if (ptrs.second.buffer) {
      fwrite(ptrs.second.buffer.get(), 1, ptrs.second.offset, stdout);
      if (delim != '\0') fwrite(&delim, sizeof(delim), 1, stdout);
    } // else it's a heap value

    src = ptrs.first;
  }

  return 0;
}
|} entry_point
    else
      Printf.fprintf oc {|
let read_whole_file fname =
  File.with_file_in ~mode:[`text] fname IO.read_all

let () =
  let fname = ref "/dev/stdin" in
  let delim = ref '\n' in
  Array.iteri (fun i arg ->
    if i < Array.length Sys.argv - 1 && (
         String.icompare arg "--delim" = 0 ||
         String.icompare arg "-d" = 0
       ) then
      delim := Sys.argv.(i + 1).[0]
    else if i > 0 then
      fname := arg
  ) Sys.argv ;

  let input = read_whole_file !fname in
  let src = Pointer.of_string input in

  let rec loop src =
    if Pointer.remSize src <= 0 then src else (
      let sz = 1024 in
      let dst = Pointer.make sz in
      let src, dst = %s src dst in
      let b, o, l = dst in
      assert (o < l) ;
      String.print stdout (Bytes.sub_string b 0 o) ;
      Char.print stdout !delim ;
      flush stdout ;
      loop src
    ) in
  loop src |> ignore
|} entry_point)
