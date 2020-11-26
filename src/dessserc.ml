(* Examples:
 *
 *   dessser --schema (FILE|TYPE) --in ENCODING --out ENCODING \
 *           --target libs|file-converter|dump-lmdb|load-lmdb|query-lmdb \
 *           [--language ocaml|c++]
 *
 * In case only a lib is generated, then also include converters from/to
 * heapvalues. *)
open Batteries
open Dessser
open DessserTools
open DessserDSTools
open DessserCompilConfig
module T = DessserTypes
module E = DessserExpressions
module M = DessserMasks
open E.Ops

let debug = true

(*
 * Code generators
 *)

let check_no_key_schema = function
  | None -> ()
  | Some _ ->
      failwith "--key-schema makes no sense here"

let check_key_schema = function
  | None ->
      failwith "--key-schema is required"
  | Some s -> s

(* Generate just the code to convert from in to out and from
 * in to a heap value and from a heap value to out, then link into a library. *)
let target_lib
      key_schema schema backend encoding_in encoding_out _fieldmask
      _modifier_expr dest_fname =
  check_no_key_schema key_schema ;
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

let target_converter
      key_schema schema backend encoding_in encoding_out _fieldmask
      modifier_expr dest_fname =
  check_no_key_schema key_schema ;
  let module BE = (val backend : BACKEND) in
  let module Des = (val encoding_in : DES) in
  let module Ser = (val encoding_out : SER) in
  let module DS = DesSer (Des) (Ser) in
  let transform _mn0 path v =
    match modifier_expr, path with
    | Some f, [] -> apply f [v]
    | _ -> v in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    E.func2 TDataPtr TDataPtr (fun _l -> DS.desser schema ~transform) in
  if debug then E.type_check [] convert ;
  let state = BE.make_state  () in
  let state, _, convert_id =
    BE.identifier_of_expression state ~name:"convert" convert in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  let convert_main_for convert_id = function
    | "cc" -> DessserDSTools_FragmentsCPP.converter convert_id
    | "ml" -> DessserDSTools_FragmentsOCaml.converter convert_id
    | "dil" -> ""
    | _ -> assert false in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions state oc ;
    String.print oc (convert_main_for convert_id BE.preferred_def_extension)
  ) ;
  compile ~optim:3 ~link:true backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

let destruct_pair = function
  | T.{ vtyp = TTup [| k ; v |] ; _ } ->
      k, v
  | t ->
      Printf.sprintf2 "Not a pair: %a" T.print_maybe_nullable t |>
      failwith

let check_is_pair = ignore % destruct_pair

let target_lmdb main
      key_schema val_schema backend encoding_in encoding_out _fieldmask
      _modifier_expr dest_fname =
  let key_schema = check_key_schema key_schema in
  let module BE = (val backend : BACKEND) in
  let module Des = (val encoding_in : DES) in
  let module Ser = (val encoding_out : SER) in
  let module DS = DesSer (Des) (Ser) in
  let convert_key =
    (* convert from encoding_in to encoding_out: *)
    E.func2 TDataPtr TDataPtr (fun _l -> DS.desser key_schema ?transform:None) in
  let convert_val =
    E.func2 TDataPtr TDataPtr (fun _l -> DS.desser val_schema ?transform:None) in
  if debug then (
    E.type_check [] convert_key ;
    E.type_check [] convert_val
  ) ;
  let state = BE.make_state  () in
  let state, _, convert_key_id =
    BE.identifier_of_expression state ~name:"convert_key" convert_key in
  let state, _, convert_val_id =
    BE.identifier_of_expression state ~name:"convert_val" convert_val in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions state oc ;
    main BE.preferred_def_extension convert_key_id convert_val_id |>
    String.print oc
  ) ;
  compile ~optim:3 ~link:true backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

let target_lmdb_dump =
  let main ext convert_key_id convert_val_id =
    (if ext = "cc" then DessserDSTools_FragmentsCPP.dumper
                   else DessserDSTools_FragmentsOCaml.dumper)
      convert_key_id convert_val_id in
  target_lmdb main

let target_lmdb_load =
  let main ext convert_key_id convert_val_id =
    (if ext = "cc" then DessserDSTools_FragmentsCPP.loader
                   else DessserDSTools_FragmentsOCaml.loader)
      convert_key_id convert_val_id in
  target_lmdb main

let target_lmdb_query _ _ _ _ _ _ _ _ =
  todo "target_lmdb_query"


(*
 * Command line
 *)

open Cmdliner

let val_schema =
  let doc = "file or inline schema for values" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options
            [ "schema" ; "value-schema" ] in
  Arg.(required (opt (some string) None i))

let key_schema =
  let doc = "file or inline schema for keys" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options [ "key-schema" ] in
  Arg.(value (opt string "" i))

(* cmdliner must be given enum values that are comparable, therefore not
 * functions: *)
type encodings = Null | RowBinary | SExpr | RingBuff

let des_of_encoding = function
  | RingBuff -> (module RamenRingBuffer.Des : DES)
  | RowBinary -> (module RowBinary.Des : DES)
  | SExpr -> (module SExpr.Des : DES)
  | _ -> failwith "No desserializer for that encoding"

let ser_of_encoding = function
  | Null -> (module DevNull.Ser : SER)
  | RingBuff -> (module RamenRingBuffer.Ser : SER)
  | RowBinary -> (module RowBinary.Ser : SER)
  | SExpr -> (module SExpr.Ser : SER)

let docv_of_enum l =
  IO.to_string (
    List.print ~first:"" ~last:"" ~sep:"|" (fun oc (n, _) ->
      String.print oc n)
  ) l

let known_inputs =
  [ "ringbuf", RingBuff ;
    "row-binary", RowBinary ;
    "s-expression", SExpr ]

let encoding_in =
  let doc = "encoding format for input" in
  let docv = docv_of_enum known_inputs in
  let i = Arg.info ~doc ~docv [ "input-encoding" ] in
  Arg.(value (opt (enum known_inputs) RowBinary i))

let known_outputs =
  [ "null", Null ;
    "ringbuf", RingBuff ;
    "row-binary", RowBinary ;
    "s-expression", SExpr ]

let encoding_out =
  let doc = "encoding format for output" in
  let docv = docv_of_enum known_outputs in
  let i = Arg.info ~doc ~docv [ "output-encoding" ] in
  Arg.(value (opt (enum known_outputs) SExpr i))

let known_inouts =
  let encoding_cmp (_, e1) (_, e2) = compare e1 e2 in
  let l1 = List.fast_sort encoding_cmp known_inputs in
  let l2 = List.fast_sort encoding_cmp known_outputs in
  let encoding_less a b = encoding_cmp a b <= 0 in
  Enum.merge encoding_less (List.enum l1) (List.enum l2) |>
  List.of_enum

type targets = Converter | Lib | LmdbDump | LmdbLoad | LmdbQuery

let function_of_target = function
  | Converter -> target_converter
  | Lib -> target_lib
  | LmdbDump -> target_lmdb_dump
  | LmdbLoad -> target_lmdb_load
  | LmdbQuery -> target_lmdb_query

let target =
  let targets =
    [ "converter", Converter ;
      "lib", Lib ;
      "dump-lmdb", LmdbDump ;
      "load-lmdb", LmdbLoad ;
      "query-lmdb", LmdbQuery ] in
  let doc = "What binary to generate" in
  let docv = docv_of_enum targets  in
  let i = Arg.info ~doc ~docv [ "target" ] in
  Arg.(value (opt (enum targets) Converter i))

type backends = DIL | Cpp | OCaml

let module_of_backend = function
  | DIL -> (module BackEndDIL : BACKEND)
  | Cpp -> (module BackEndCPP : BACKEND)
  | OCaml -> (module BackEndOCaml : BACKEND)

let backend =
  let languages =
    [ "DIL", DIL ;
      "C++", Cpp ;
      "OCaml", OCaml ] in
  let doc = "Language to generate code for" in
  let docv = docv_of_enum languages in
  let i = Arg.info ~doc ~docv [ "language" ; "backend" ] in
  Arg.(value (opt (enum languages) Cpp i))

let fieldmask =
  let parse s =
    try Stdlib.Ok (M.Parser.action_of_string s)
    with e -> Stdlib.Error (`Msg (Printexc.to_string e))
  and print fmt ma =
    Format.fprintf fmt "%s" (M.string_of_action ma)
  in
  Arg.conv ~docv:"MASK" (parse, print)

(* One day when there are compile time masks this option will be needed.
 * For now we simulate compile time masks with runtime ones, which comes
 * handy for testing. *)
let comptime_fieldmask =
  let doc = "Compile-time fieldmask to apply when serializing" in
  let docv = "MASK" in
  let i = Arg.info ~doc ~docv [ "mask" ; "field-mask" ] in
  Arg.(value (opt fieldmask M.Copy i))

let expression =
  let parse s =
    match E.Parser.expr s with
    | exception e ->
        Stdlib.Error (`Msg (Printexc.to_string e))
    | [ s ] ->
        Stdlib.Ok s
    | _ ->
        Stdlib.Error (`Msg "A single s-expression must be provided")
  in
  Arg.conv ~docv:"EXPRESSION" (parse, E.pretty_print)

let modifier_expr =
  let doc =
    "Expression computing an alternative value to write (function of the \
     input value)" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "e" ; "expression" ] in
  Arg.(value (opt (some expression) None i))

let dest_fname =
  let doc = "Output file" in
  let docv = "FILE" in
  let i = Arg.info ~doc ~docv [ "o" ; "output-file" ] in
  Arg.(required (opt (some string) None i))

let maybe_nullable_of_string str =
  let p = T.maybe_nullable_of_string ~what:"schema" in
  let parse_as_string str = p str in
  (* First try to parse that file, then to parse that string: *)
  try (
    let str = read_whole_file str in
    parse_as_string str
  ) with _ ->
    parse_as_string str

let start target key_schema val_schema backend encoding_in encoding_out
          fieldmask modifier_expr dest_fname =
  let target = function_of_target target in
  let key_schema =
    if key_schema = "" then None
    else Some (maybe_nullable_of_string key_schema) in
  let val_schema = maybe_nullable_of_string val_schema in
  let backend = module_of_backend backend in
  let encoding_in = des_of_encoding encoding_in in
  let encoding_out = ser_of_encoding encoding_out in
  target key_schema val_schema backend encoding_in encoding_out fieldmask
         modifier_expr dest_fname

let () =
  let doc = "Dessser code generator" in
  Term.((
    (const start
     $ target
     $ key_schema
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ comptime_fieldmask
     $ modifier_expr
     $ dest_fname),
    info "dessserc" ~version ~doc) |>
  eval |> exit)
