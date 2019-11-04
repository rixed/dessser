open Batteries
open Stdint
open Dessser

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
  let typ =
    let nullable = true in
    Types.(
      make (TTup [|
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

  let backend =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "ocaml" then (module BackEndOCaml : BACKEND)
    else if Array.length Sys.argv > 1 && Sys.argv.(1) = "c++" then (module BackEndCPP : BACKEND)
    else (
      Printf.eprintf "%s ocaml|c++\n" Sys.argv.(0) ;
      exit 1
    ) in
  let module BE = (val backend : BACKEND) in
  let module DS1 = DesSer (RowBinary.Des (BE)) (HeapValue.Ser (BE)) in
  let module DS2 = DesSer (HeapValue.Des (BE)) (SExpr.Ser (BE)) (*(RamenRingBuffer.Ser (BE))*) in
  let module Sizer = HeapValue.SerSizer (RamenRingBuffer.Ser (BE)) in

  let output = BE.make_output () in
  let _read_tuple =
    let tptr = Types.(make TPointer) in
    BE.print_function2 output tptr tptr t_pair_ptrs (fun oc src dst ->
      BE.comment oc "Convert from RowBinary into a heap value:" ;
      let dummy = Identifier.pointer () in (* Will be ignored by HeapValue.Ser *)
      let src, heap_value = DS1.desser typ oc src dummy in
      BE.comment oc "Compute the serialized size of this tuple:" ;
      let const_sz, dyn_sz = Sizer.sersize typ oc heap_value in
      BE.dump oc [
        BE.string_of_const oc "Constant size: " ;
        BE.size_to_string oc const_sz ;
        BE.string_of_const oc ", dynamic size: " ;
        BE.size_to_string oc dyn_sz ] ;
      BE.comment oc "Now convert the heap value into an SExpr:" ;
      let src', dst = DS2.desser typ oc heap_value dst in
      BE.ignore oc src' ;
      BE.make_pair oc t_pair_ptrs src dst) in
  let optim = 3 in
  let mode = [ `create ; `text ; `trunc ] in
  let fname = "examples/example."^ BE.preferred_file_extension in
  File.with_file_out ~mode fname (fun oc ->
    BE.print_output oc output) ;
  let cmd =
    if BE.preferred_file_extension = "cc" then
      Printf.sprintf "g++ -std=c++17 -g -O%d -W -Wall -I src %s examples/rowbinary2sexpr.cpp -o examples/rowbinary2sexpr" optim fname
    else if BE.preferred_file_extension = "ml" then
      Printf.sprintf "ocamlfind ocamlopt -g -annot -O%d -I src -I examples -package stdint,batteries -linkpkg src/dessser.cmxa %s examples/rowbinary2sexpr.ml -o examples/rowbinary2sexpr.opt" optim fname
    else
      "true"
    in
  run_cmd cmd
