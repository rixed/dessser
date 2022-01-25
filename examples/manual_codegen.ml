open Batteries
open Stdint
open Dessser
open DessserTools
open DessserDSTools
module T = DessserTypes
module E = DessserExpressions
open E.Ops

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
  let udp_typ =
    T.(required (tup [|
      string ; u64 ; u64 ; u8 ; string ; u8 ; string ; nu32 ;
      nu32 ; u64 ; u64 ; u32 ; u32 ; nu32 ; nstring ; nu32 ;
      nstring ; nu32 ; nstring ; u16 ; u16 ; u8 ; u8 ; nu32 ;
      nu32 ; u32 ; string ; u64 ; u64 ; u64 ; (* Should be U32 *)
      u64 ; (* Should be U32 *) u64 ; u64 ; nstring
    |]))
  and _http_typ =
    T.(required (tup [|
      string ; u64 ; u64 ; u8 ; string ; u8 ; string ;
      nu32 ; nu32 ; u64 ; u64 ; u32 ; u32 ;
      nu32 ; optional (vec 16 char) ;
      nu32 ; optional (vec 16 char) ;
      u16 ; u16 ; u128 ; u128 ; u128 ; nu128 ;
      u8 ; u8 ; u8 ; nstring ; nstring ;
      nstring (* url *) ; nstring ; u8 ; u8 ; u8 ;
      nu32 ; optional (vec 16 char) ;
      u8 ; u8 ; u64 ; u64 ; u8 ; u32 ; u32 ; u32 ;
      nstring ; u32 ; u8 ; nstring ;
      nu64 ; nu64 ; nu32 ;
      u32 ; u32 ; u32 ;
      nstring ; u32 ; u8 ; nstring ;
      u32 ; u32 ; u16 ; u16 ; u16 ;
      u64 ; u64 ; u64 ; float ; u8 ; i64 ; float ;
      i64 ; float ; i64 ; float ; u32 |])) in
  let typ = udp_typ in
  let backend =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "ocaml" then
      (module DessserBackEndOCaml : BACKEND)
    else if Array.length Sys.argv > 1 && Sys.argv.(1) = "c++" then
      (module DessserBackEndCPP : BACKEND)
    else (
      Printf.eprintf "%s ocaml|c++\n" Sys.argv.(0) ;
      exit 1
    ) in
  let module BE = (val backend : BACKEND) in
  let sexpr_config = { DessserSExpr.default_config with newline = Some '\n' } in
  let convert_only = false in
  let compunit = U.make "manual_codegen" in
  let compunit, convert =
    if convert_only then (
      (* Just convert the rowbinary to s-expr: *)
      let module DS = DesSer (DessserRowBinary.Des) (DessserSExpr.Ser) in
      compunit,
      func2 T.ptr T.ptr (fun src dst ->
        comment "Convert from RowBinary into S-Expression:"
          (DS.desser ~ser_config:sexpr_config typ src dst))
    ) else (
      (* convert from RowBinary into a heapvalue, compute its serialization
       * size in RamenringBuf format, then convert it into S-Expression: *)
      let module ToValue =
        DessserHeapValue.Materialize (DessserRowBinary.Des) in
      (* To compute sersize in RingBuffer: *)
      let module OfValue1 =
        DessserHeapValue.Serialize (DessserRamenRingBuffer.Ser) in
      (* To serialize into S-Expr: *)
      let module OfValue2 =
        DessserHeapValue.Serialize (DessserSExpr.Ser) in

      let ma = copy_field in
      let compunit, des, _ =
        ToValue.make "t" typ compunit in
      let compunit, ser_func, _ =
        OfValue2.serialize ~config:sexpr_config typ compunit in
      let compunit, sersize, _ =
        OfValue1.sersize typ compunit in
      compunit,
      func2 T.ptr T.ptr (fun src dst ->
        comment "Convert from RowBinary into a heap value:" (
          let v_src = apply des [ src ] in
          E.with_sploded_pair "v_src" v_src (fun v src ->
            comment "Compute the serialized size of this tuple:" (
              let sz = apply sersize [ ma ; v ] in
              E.let_ ~name:"sz" sz (fun sz ->
                seq [
                  dump (string "Size: ") ;
                  dump sz ;
                  dump (string "\n") ;
                  comment "Now convert the heap value into an SExpr:" (
                    let dst' = apply ser_func [ ma ; v ; dst ] in
                    make_pair src dst') ])))))
    ) in
  (*Printf.printf "convert = %a\n%!" (print_expr ?max_depth:None) convert ;*)
  let dst_fname = "examples/rowbinary2sexpr."^ BE.preferred_comp_extension Executable in
  let dst_fname =
    make_converter ~dev_mode:true ~dst_fname compunit backend convert in
  Printf.printf "executable in %s\n" dst_fname
