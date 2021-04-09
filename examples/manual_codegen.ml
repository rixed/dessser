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
  let m x = T.{ vtyp = Mac x ; nullable = false }
  and n x = T.{ vtyp = Mac x ; nullable = true } in
  let udp_typ =
    T.make (Tup [|
      m String ; m T.mac_u64 ; m T.mac_u64 ; m T.mac_u8 ; m String ; m T.mac_u8 ; m String ; n T.mac_u32 ;
      n T.mac_u32 ; m T.mac_u64 ; m T.mac_u64 ; m T.mac_u32 ; m T.mac_u32 ; n T.mac_u32 ; n String ; n T.mac_u32 ;
      n String ; n T.mac_u32 ; n String ; m T.mac_u16 ; m T.mac_u16 ; m T.mac_u8 ; m T.mac_u8 ; n T.mac_u32 ;
      n T.mac_u32 ; m T.mac_u32 ; m String ; m T.mac_u64 ; m T.mac_u64 ; m T.mac_u64 ; (* Should be T.mac_u32 *)
      m T.mac_u64 ; (* Should be T.mac_u32 *) m T.mac_u64 ; m T.mac_u64 ; n String
    |])
  and _http_typ =
    T.make (Tup [|
      m String ; m T.mac_u64 ; m T.mac_u64 ; m T.mac_u8 ; m String ; m T.mac_u8 ; m String ;
      n T.mac_u32 ; n T.mac_u32 ; m T.mac_u64 ; m T.mac_u64 ; m T.mac_u32 ; m T.mac_u32 ;
      n T.mac_u32 ; T.optional (Vec (16, m Char)) ;
      n T.mac_u32 ; T.optional (Vec (16, m Char)) ;
      m T.mac_u16 ; m T.mac_u16 ; m T.mac_u128 ; m T.mac_u128 ; m T.mac_u128 ; n T.mac_u128 ;
      m T.mac_u8 ; m T.mac_u8 ; m T.mac_u8 ; n String ; n String ;
      n String (* url *) ; n String ; m T.mac_u8 ; m T.mac_u8 ; m T.mac_u8 ;
      n T.mac_u32 ; T.optional (Vec (16, m Char)) ;
      m T.mac_u8 ; m T.mac_u8 ; m T.mac_u64 ; m T.mac_u64 ; m T.mac_u8 ; m T.mac_u32 ; m T.mac_u32 ; m T.mac_u32 ;
      n String ; m T.mac_u32 ; m T.mac_u8 ; n String ;
      n T.mac_u64 ; n T.mac_u64 ; n T.mac_u32 ;
      m T.mac_u32 ; m T.mac_u32 ; m T.mac_u32 ;
      n String ; m T.mac_u32 ; m T.mac_u8 ; n String ;
      m T.mac_u32 ; m T.mac_u32 ; m T.mac_u16 ; m T.mac_u16 ; m T.mac_u16 ;
      m T.mac_u64 ; m T.mac_u64 ; m T.mac_u64 ; m Float ; m T.mac_u8 ; m T.mac_i64 ; m Float ;
      m T.mac_i64 ; m Float ; m T.mac_i64 ; m Float ; m T.mac_u32 |]) in
  let typ = udp_typ in
  let backend, exe_ext =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "ocaml" then
      (module DessserBackEndOCaml : BACKEND), ".opt"
    else if Array.length Sys.argv > 1 && Sys.argv.(1) = "c++" then
      (module DessserBackEndCPP : BACKEND), ".exe"
    else (
      Printf.eprintf "%s ocaml|c++\n" Sys.argv.(0) ;
      exit 1
    ) in
  let module BE = (val backend : BACKEND) in
  let sexpr_config = { DessserSExpr.default_config with newline = Some '\n' } in
  let convert_only = false in
  let convert =
    if convert_only then (
      (* Just convert the rowbinary to s-expr: *)
      let module DS = DesSer (DessserRowBinary.Des) (DessserSExpr.Ser) in
      E.func2 DataPtr DataPtr (fun l src dst ->
        comment "Convert from RowBinary into S-Expression:"
          (DS.desser ~ser_config:sexpr_config typ l src dst))
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
      E.func2 DataPtr DataPtr (fun l src dst ->
        comment "Convert from RowBinary into a heap value:" (
          let v_src = ToValue.make typ l src in
          E.with_sploded_pair ~l "v_src" v_src (fun l v src ->
            comment "Compute the serialized size of this tuple:" (
              let const_dyn_sz = OfValue1.sersize typ l ma v in
              E.with_sploded_pair ~l "read_tuple" const_dyn_sz (fun l const_sz dyn_sz ->
                seq [
                  dump (string "Constant size: ") ;
                  dump const_sz ;
                  dump (string ", dynamic size: ") ;
                  dump dyn_sz ;
                  dump (string "\n") ;
                  comment "Now convert the heap value into an SExpr:" (
                    let dst' =
                      OfValue2.serialize ~config:sexpr_config typ l ma v dst in
                    pair src dst') ])))))
    ) in
  (*Printf.printf "convert = %a\n%!" (print_expr ?max_depth:None) convert ;*)
  let exe_fname = "examples/rowbinary2sexpr"^ exe_ext in
  let exe_fname = make_converter ~exe_fname backend convert in
  Printf.printf "executable in %s\n" exe_fname
