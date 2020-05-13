(* Examples:
 *
 *   dessser --schema (FILE|TYPE) --in ENCODING --out ENCODING \
 *           [--language ocaml|c++] --target libs|file-converter|lmdb-dump
 *
 *   In case only a lib is generated, then also include converters from/to
 *   heapvalues.  *)
open Batteries
open Dessser
open DessserTools
open DessserDSTools
open DessserCompilConfig
module T = DessserTypes
module E = DessserExpressions
open E.Ops

let debug = true

(*
 * Code generators
 *)

(* Generate just the code to convert from in to out and from
 * in to a heap value and from a heap value to out, then link into a library. *)
let target_lib schema backend encoding_in encoding_out dest_fname =
  let module BE = (val backend : BACKEND) in
  let module Des = (val encoding_in : DES) in
  let module Ser = (val encoding_out : SER) in
  let module DS = DesSer (Des) (Ser) in
  let module ToValue = HeapValue.Materialize (Des) in
  let module OfValue = HeapValue.Serialize (Ser) in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    E.func2 TDataPtr TDataPtr (fun _l -> DS.desser schema ?transform:None) in
  let to_value =
    (* convert from encoding_in into a heapvalue: *)
    E.func1 TDataPtr (fun _l src ->
      first (ToValue.make schema src)) in
  let ma = copy_field in
  let value_sersize =
    (* compute the serialization size of a heap value: *)
    E.func1 (TValue schema) (fun _l v ->
      OfValue.sersize schema ma v) in
  let of_value =
    (* convert from a heapvalue into encoding_out. *)
    E.func2 (TValue schema) TDataPtr (fun _l v dst ->
      OfValue.serialize schema ma v dst) in
  if debug then (
    E.type_check [] convert ;
    E.type_check [] to_value ;
    E.type_check [] value_sersize ;
    E.type_check [] of_value) ;
  let state = BE.make_state  () in
  let state, _, _convert_id =
    BE.identifier_of_expression state ~name:"convert" convert in
  let state, _, _to_value_id =
    BE.identifier_of_expression state ~name:"to_value" to_value in
  let state, _, _value_sersize_id =
    BE.identifier_of_expression state ~name:"value_sersize" value_sersize in
  let state, _, _of_value_id =
    BE.identifier_of_expression state ~name:"of_value" of_value in
  let def_fname = change_ext BE.preferred_def_extension dest_fname in
  let decl_fname = change_ext BE.preferred_decl_extension dest_fname in
  write_source ~src_fname:def_fname (BE.print_definitions state) ;
  write_source ~src_fname:decl_fname (BE.print_declarations state) ;
  Printf.printf "declarations in %S\n" decl_fname ;
  Printf.printf "definitions in %S\n" def_fname

let convert_main_for ext entry_point =
  (if ext = "cc" then DessserDSTools_FragmentsCPP.converter
                 else DessserDSTools_FragmentsOCaml.converter)
    entry_point

let target_converter schema backend encoding_in encoding_out dest_fname =
  let module BE = (val backend : BACKEND) in
  let module Des = (val encoding_in : DES) in
  let module Ser = (val encoding_out : SER) in
  let module DS = DesSer (Des) (Ser) in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    E.func2 TDataPtr TDataPtr (fun _l -> DS.desser schema ?transform:None) in
  if debug then E.type_check [] convert ;
  let state = BE.make_state  () in
  let state, _, convert_id =
    BE.identifier_of_expression state ~name:"convert" convert in
  let def_fname = change_ext BE.preferred_def_extension dest_fname in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions state oc ;
    String.print oc (convert_main_for BE.preferred_def_extension convert_id)
  ) ;
  compile ~optim:3 ~link:true backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

(* Build two binaries, to dump and restore an lmdb from/to the given
 * encodings *)
let target_lmdb _schema _backend _encoding_in _encoding_out _dest_fname =
  assert false (* TODO *)

(*
 * Command line
 *)

open Cmdliner

let schema =
  let doc = "file or inline schema" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options ["schema"] in
  Arg.(required (opt (some string) None i))

let encoding_in =
  let row_binary = (module RowBinary.Des : DES) in
  let s_expr = (module SExpr.Des : DES) in
  let encodings =
    [ "row-binary", row_binary ;
      "s-expression", s_expr ] in
  let doc = "encoding format for input" in
  let docv = "row-binary" in
  let i = Arg.info ~doc ~docv [ "input-encoding" ] in
  Arg.(value (opt (enum encodings) row_binary i))

let encoding_out =
  let row_binary = (module RowBinary.Ser : SER) in
  let null = (module DevNull.Ser : SER) in
  let ringbuf = (module RamenRingBuffer.Ser : SER) in
  let s_expr = (module SExpr.Ser : SER) in
  let encodings =
    [ "null", null ;
      "ringbuf", ringbuf ;
      "row-binary", row_binary ;
      "s-expression", s_expr ] in
  let doc = "encoding format for output" in
  let docv = "null|ringbuf|row-binary|s-expression" in
  let i = Arg.info ~doc ~docv [ "output-encoding" ] in
  Arg.(value (opt (enum encodings) s_expr i))

(* cmdliner must be given enum values that are comparable, therefore not
 * functions: *)
type targets = Converter | Lib | Lmdb

let function_of_target = function
  | Converter -> target_converter
  | Lib -> target_lib
  | Lmdb -> target_lmdb

let target =
  let targets =
    [ "converter", Converter ;
      "lib", Lib ;
      "lmdb", Lmdb ] in
  let doc = "What binary to generate" in
  let docv = "converter|lib|lmdb" in
  let i = Arg.info ~doc ~docv [ "target" ] in
  Arg.(value (opt (enum targets) Converter i))

type backends = Cpp | OCaml

let module_of_backend = function
  | Cpp -> (module BackEndCPP : BACKEND)
  | OCaml -> (module BackEndOCaml : BACKEND)

let backend =
  let languages =
    [ "C++", Cpp ;
      "OCaml", OCaml ] in
  let doc = "Language to generate code for" in
  let docv = "C++|OCaml" in
  let i = Arg.info ~doc ~docv [ "backend" ] in
  Arg.(value (opt (enum languages) Cpp i))

let dest_fname =
  let doc = "Output file" in
  let docv = "FILE" in
  let i = Arg.info ~doc ~docv [ "o" ; "output-file" ] in
  Arg.(required (opt (some string) None i))

let maybe_nullable_of_string str =
  let p = T.Parser.maybe_nullable_of_string ~what:"schema" in
  let parse_as_string str = p str in
  (* First try to parse that file, then to parse that string: *)
  try (
    let str = read_whole_file str in
    parse_as_string str
  ) with _ ->
    parse_as_string str

let start target schema backend encoding_in encoding_out dest_fname =
  let schema = maybe_nullable_of_string schema in
  let target = function_of_target target in
  let backend = module_of_backend backend in
  target schema backend encoding_in encoding_out dest_fname

let () =
  let doc = "Dessser code generator" in
  Term.((
    (const start
     $ target
     $ schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ dest_fname),
    info "dessserc" ~version ~doc) |>
  eval |> exit)
