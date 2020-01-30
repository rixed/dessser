(* Examples:
 *
 *   dessser --schema (FILE|TYPE) --in ENCODING --out ENCODING \
 *           [--language ocaml|c++] --target libs|file-converter|lmdb-dump
 *
 *   In case only a lib is generated, then also include converters from/to
 *   heapvalues.  *)
open Batteries
open Dessser
open DessserTypes
open DessserExpressions
open DessserTools
open DessserCompilConfig
open Ops

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
  let module ToValue = DesSer (Des) (HeapValue.Ser) in
  let module OfValue = DesSer (HeapValue.Des) (Ser) in
  let module Sizer = HeapValue.SerSizer (Ser) in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    func2 TDataPtr TDataPtr (DS.desser schema) in
  let to_value =
    (* convert from encoding_in into a heapvalue: *)
    func1 TDataPtr (fun src ->
      let vptr = alloc_value schema in
      ToValue.desser schema src vptr) in
  let value_sersize =
    (* compute the serialization size of a heap value: *)
    func1 (TValuePtr schema) (fun vptr ->
      Sizer.sersize schema vptr) in
  let of_value =
    (* convert from a heapvalue into encoding_out. *)
    func2 (TValuePtr schema) TDataPtr (fun vptr dst ->
      let src_dst = OfValue.desser schema vptr dst in
      snd src_dst) in
  if debug then (
    type_check [] convert ;
    type_check [] to_value ;
    type_check [] value_sersize ;
    type_check [] of_value) ;
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

let convert_main_for_cpp =
  format_of_string {|
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
|}

let convert_main_for_ocaml =
  format_of_string {|
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
|}

let convert_main_for ext =
  if ext = "cc" then convert_main_for_cpp
  else convert_main_for_ocaml

let target_converter schema backend encoding_in encoding_out dest_fname =
  let module BE = (val backend : BACKEND) in
  let module Des = (val encoding_in : DES) in
  let module Ser = (val encoding_out : SER) in
  let module DS = DesSer (Des) (Ser) in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    func2 TDataPtr TDataPtr (DS.desser schema) in
  if debug then type_check [] convert ;
  let state = BE.make_state  () in
  let state, _, convert_id =
    BE.identifier_of_expression state ~name:"convert" convert in
  let def_fname = change_ext BE.preferred_def_extension dest_fname in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions state oc ;
    Printf.fprintf oc (convert_main_for BE.preferred_def_extension) convert_id
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
  let doc = "file or online schema" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options ["schema"] in
  Arg.(required (opt (some string) None i))

let encoding_in =
  let row_binary = (module RowBinary.Des : DES) in
  let encodings =
    [ "row-binary", row_binary ] in
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
