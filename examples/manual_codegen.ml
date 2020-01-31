open Batteries
open Stdint
open Dessser
open DessserTypes
open DessserExpressions
open DessserTools
open DessserDSTools
open Ops
module T = DessserTypes

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
  let m x = NotNullable (Mac x)
  and n x = Nullable (Mac x) in
  let udp_typ =
    NotNullable (TTup [|
      m TString ; m TU64 ; m TU64 ; m TU8 ; m TString ; m TU8 ; m TString ; n TU32 ;
      n TU32 ; m TU64 ; m TU64 ; m TU32 ; m TU32 ; n TU32 ; n TString ; n TU32 ;
      n TString ; n TU32 ; n TString ; m TU16 ; m TU16 ; m TU8 ; m TU8 ; n TU32 ;
      n TU32 ; m TU32 ; m TString ; m TU64 ; m TU64 ; m TU64 ; (* Should be U32 *)
      m TU64 ; (* Should be U32 *) m TU64 ; m TU64 ; n TString
    |])
  and _http_typ =
    NotNullable (TTup [|
      m TString ; m TU64 ; m TU64 ; m TU8 ; m TString ; m TU8 ; m TString ;
      n TU32 ; n TU32 ; m TU64 ; m TU64 ; m TU32 ; m TU32 ;
      n TU32 ; Nullable (TVec (16, m TChar)) ;
      n TU32 ; Nullable (TVec (16, m TChar)) ;
      m TU16 ; m TU16 ; m TU128 ; m TU128 ; m TU128 ; n TU128 ;
      m TU8 ; m TU8 ; m TU8 ; n TString ; n TString ;
      n TString (* url *) ; n TString ; m TU8 ; m TU8 ; m TU8 ;
      n TU32 ; Nullable (TVec (16, m TChar)) ;
      m TU8 ; m TU8 ; m TU64 ; m TU64 ; m TU8 ; m TU32 ; m TU32 ; m TU32 ;
      n TString ; m TU32 ; m TU8 ; n TString ;
      n TU64 ; n TU64 ; n TU32 ;
      m TU32 ; m TU32 ; m TU32 ;
      n TString ; m TU32 ; m TU8 ; n TString ;
      m TU32 ; m TU32 ; m TU16 ; m TU16 ; m TU16 ;
      m TU64 ; m TU64 ; m TU64 ; m TFloat ; m TU8 ; m TI64 ; m TFloat ;
      m TI64 ; m TFloat ; m TI64 ; m TFloat ; m TU32 |]) in
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
      func2 TDataPtr TDataPtr (fun src dst ->
        comment "Convert from RowBinary into S-Expression:"
          (DS.desser typ src dst))
    ) else (
      (* convert from RowBinary into a heapvalue, compute its serialization
       * size in RamenringBuf format, then convert it into S-Expression: *)
      let module DS1 = DesSer (RowBinary.Des) (HeapValue.Ser) in
      let module DS2 = DesSer (HeapValue.Des) (SExpr.Ser) (*(RamenRingBuffer.Ser (BE))*) in
      let module Sizer = HeapValue.SerSizer (RamenRingBuffer.Ser) in

      func2 TDataPtr TDataPtr (fun src dst ->
        comment "Convert from RowBinary into a heap value:" (
          let vptr = alloc_value typ in
          let src_valueptr = DS1.desser typ src vptr in
          with_sploded_pair "src_valueptr" src_valueptr (fun src valueptr ->
            comment "Compute the serialized size of this tuple:" (
              let const_dyn_sz = Sizer.sersize typ valueptr in
              with_sploded_pair "read_tuple" const_dyn_sz (fun const_sz dyn_sz ->
                seq [
                  dump (string "Constant size: ") ;
                  dump const_sz ;
                  dump (string ", dynamic size: ") ;
                  dump dyn_sz ;
                  dump (string "\n") ;
                  comment "Now convert the heap value into an SExpr:" (
                    let src_dst' = DS2.desser typ valueptr dst in
                    pair src (snd src_dst')) ])))))
    ) in
  (*Printf.printf "convert = %a\n%!" (print_expr ?max_depth:None) convert ;*)
  let exe_fname = "examples/rowbinary2sexpr"^ exe_ext in
  let exe_fname = make_converter ~exe_fname backend convert in
  Printf.printf "executable in %s" exe_fname
