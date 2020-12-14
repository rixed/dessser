(* Tool to generate various piece of code to manipulate data *)
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

type backends = DIL | Cpp | OCaml

let module_of_backend = function
  | DIL -> (module BackEndDIL : BACKEND)
  | Cpp -> (module BackEndCPP : BACKEND)
  | OCaml -> (module BackEndOCaml : BACKEND)

(* cmdliner must be given enum values that are comparable, therefore not
 * functions: *)
type encodings = Null | RowBinary | SExpr | RingBuff | CSV

let des_of_encoding = function
  | RingBuff -> (module RamenRingBuffer.Des : DES)
  | RowBinary -> (module RowBinary.Des : DES)
  | SExpr -> (module SExpr.Des : DES)
  | CSV -> (module Csv.Des : DES)
  | _ -> failwith "No desserializer for that encoding"

let ser_of_encoding = function
  | Null -> (module DevNull.Ser : SER)
  | RingBuff -> (module RamenRingBuffer.Ser : SER)
  | RowBinary -> (module RowBinary.Ser : SER)
  | SExpr -> (module SExpr.Ser : SER)
  | CSV -> (module Csv.Ser : SER)

(* Generate just the code to convert from in to out and from
 * in to a heap value and from a heap value to out, then link into a library. *)
let lib schema backend encoding_in encoding_out _fieldmask dest_fname () =
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
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
  let state, _, _ =
    BE.identifier_of_expression state ~name:"convert" convert in
  let state, _, _ =
    BE.identifier_of_expression state ~name:"to_value" to_value in
  let state, _, _ =
    BE.identifier_of_expression state ~name:"value_sersize" value_sersize in
  let state, _, _ =
    BE.identifier_of_expression state ~name:"of_value" of_value in
  let def_fname = change_ext BE.preferred_def_extension dest_fname in
  let decl_fname = change_ext BE.preferred_decl_extension dest_fname in
  write_source ~src_fname:def_fname (BE.print_definitions state) ;
  write_source ~src_fname:decl_fname (BE.print_declarations state) ;
  Printf.printf "declarations in %S\n" decl_fname ;
  Printf.printf "definitions in %S\n" def_fname

let converter
      schema backend encoding_in encoding_out _fieldmask
      modifier_exprs dest_fname () =
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module DS = DesSer (Des) (Ser) in
  let transform _mn0 path v =
    match List.find (fun (p, _) -> p = path) modifier_exprs with
    | exception Not_found -> v
    | _p, e -> apply e [v] in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    E.func2 TDataPtr TDataPtr (fun _l -> DS.desser schema ~transform) in
  if debug then E.type_check [] convert ;
  let state = BE.make_state  () in
  let state, _, convert_name =
    BE.identifier_of_expression state ~name:"convert" convert in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  let convert_main_for = function
    | "cc" -> DessserDSTools_FragmentsCPP.converter convert_name
    | "ml" -> DessserDSTools_FragmentsOCaml.converter convert_name
    | "dil" -> ""
    | _ -> assert false in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions state oc ;
    String.print oc (convert_main_for BE.preferred_def_extension)
  ) ;
  compile ~optim:3 ~link:true backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

let destruct_pair = function
  | T.{ vtyp = TTup [| k ; v |] ; _ } ->
      k, v
  | t ->
      Printf.sprintf2 "Not a pair: %a" T.print_maybe_nullable t |>
      failwith

let lmdb main
      key_schema val_schema backend encoding_in encoding_out dest_fname () =
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
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
  let state, _, convert_key_name =
    BE.identifier_of_expression state ~name:"convert_key" convert_key in
  let state, _, convert_val_name =
    BE.identifier_of_expression state ~name:"convert_val" convert_val in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions state oc ;
    main BE.preferred_def_extension convert_key_name convert_val_name |>
    String.print oc
  ) ;
  compile ~optim:3 ~link:true backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

let lmdb_dump =
  let main ext convert_key_id convert_val_id =
    (if ext = "cc" then DessserDSTools_FragmentsCPP.dumper
                   else DessserDSTools_FragmentsOCaml.dumper)
      convert_key_id convert_val_id in
  lmdb main

let lmdb_load =
  let main ext convert_key_id convert_val_id =
    (if ext = "cc" then DessserDSTools_FragmentsCPP.loader
                   else DessserDSTools_FragmentsOCaml.loader)
      convert_key_id convert_val_id in
  lmdb main

let lmdb_query _ _ _ _ _ _ () =
  todo "lmdb_query"

(* In dessser IL we have to explicitly describe the initial state, the update
 * function and the finalizer function. Ramen will have to keep working hard
 * to convert simple RaQL aggregation functions into DIL programs. *)
let aggregator
      schema backend encoding_in encoding_out
      init_expr update_expr finalize_expr
      dest_fname () =
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module ToValue = HeapValue.Materialize (Des) in
  (* Let's start with a function that's reading input values from a given
   * source pointer and returns the heap value and the new source pointer: *)
  let to_value =
    E.func1 TDataPtr (fun _l -> ToValue.make schema) in
  (* Check the function that creates the initial state that will be used by
   * the update function: *)
  E.type_check [] init_expr ;
  let state_t = E.type_of [] init_expr in
  (* Then check the update expression, that must be a function of the state_t
   * and the input_t: *)
  E.type_check [] update_expr ;
  let update_t = E.type_of [] update_expr in
  if update_t <> T.TFunction ([| state_t ; TValue schema |], T.void) then
    Printf.sprintf2 "Aggregation updater (%a) must be a function of the \
                     aggregation state and the input value and returning \
                     nothing (not %a)"
                     (E.print ~max_depth:4) update_expr
                     T.print update_t |>
    failwith ;
  (* Then check the finalizer: *)
  E.type_check [] finalize_expr ;
  let output_t =
    match E.type_of [] finalize_expr with
    | T.TFunction ([| a1 |], TValue mn) when a1 = state_t -> mn
    | t ->
        Printf.sprintf2 "Aggregation finalizer must be a function of the \
                         aggregation state (not %a)" T.print t |>
        failwith in
  (* Finally, a function to convert the output value on the heap into stdout
   * in the given encoding: *)
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module OfValue = HeapValue.Serialize (Ser) in
  let ma = copy_field in
  let of_value =
    E.func2 (TValue output_t) TDataPtr (fun _l v dst ->
      OfValue.serialize output_t ma v dst) in
  (* Let's now assemble all this into just three functions:
   * - init_expr, that we already have;
   * - input_expr, that deserialize and then update and return the new source
   *   pointer;
   * - output_expr, that finalize the value and serialize it. *)
  let code = BE.make_state () in
  let code, state_id, state_name =
    BE.identifier_of_expression code ~name:"state" init_expr in
  let input_expr =
    E.func1 ~l:(BE.environment code) TDataPtr (fun _l src ->
      let v_src = apply to_value [ src ] in
      E.with_sploded_pair "input_expr" v_src (fun v src ->
        seq [ apply update_expr [ state_id ; v ] ;
              src ])) in
  let code, _, input_name =
    BE.identifier_of_expression code ~name:"input" input_expr in
  let output_expr =
    E.func1 ~l:(BE.environment code) TDataPtr (fun _l dst ->
      let v = apply finalize_expr [ state_id ] in
      apply of_value [ v ; dst ]) in
  let code, _, output_name =
    BE.identifier_of_expression code ~name:"output" output_expr in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  let main_for = function
    | "cc" -> DessserDSTools_FragmentsCPP.aggregator state_name input_name
                                                     output_name
    | "ml" -> DessserDSTools_FragmentsOCaml.aggregator state_name input_name
                                                       output_name
    | "dil" -> ""
    | _ -> assert false in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions code oc ;
    String.print oc (main_for BE.preferred_def_extension)
  ) ;
  compile ~optim:3 ~link:true backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

(*
 * Command line
 *)

open Cmdliner

let maybe_nullable =
  let parse s =
    try (
      let p = T.maybe_nullable_of_string ~what:"schema" in
      let parse_as_string s = Stdlib.Ok (p s) in
      (* First try to parse that file, then to parse that string: *)
      try (
        let s = read_whole_file s in
        parse_as_string s
      ) with _ ->
        parse_as_string s
    ) with e -> Stdlib.Error (`Msg (Printexc.to_string e))
  and print fmt mn =
    Format.fprintf fmt "%s" (T.string_of_maybe_nullable mn)
  in
  Arg.conv ~docv:"TYPE" (parse, print)

let val_schema =
  let doc = "file or inline schema for values" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options
            [ "schema" ; "value-schema" ] in
  Arg.(required (opt (some maybe_nullable) None i))

let key_schema =
  let doc = "file or inline schema for keys" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options [ "key-schema" ] in
  Arg.(required (opt (some maybe_nullable) None i))

let docv_of_enum l =
  IO.to_string (
    List.print ~first:"" ~last:"" ~sep:"|" (fun oc (n, _) ->
      String.print oc n)
  ) l

let known_inputs =
  [ "ringbuf", RingBuff ;
    "row-binary", RowBinary ;
    "s-expression", SExpr ;
    "csv", CSV ]

let encoding_in =
  let doc = "encoding format for input" in
  let docv = docv_of_enum known_inputs in
  let i = Arg.info ~doc ~docv [ "input-encoding" ] in
  Arg.(value (opt (enum known_inputs) RowBinary i))

let known_outputs =
  [ "null", Null ;
    "ringbuf", RingBuff ;
    "row-binary", RowBinary ;
    "s-expression", SExpr ;
    "csv", CSV ]

let encoding_out =
  let doc = "encoding format for output" in
  let docv = docv_of_enum known_outputs in
  let i = Arg.info ~doc ~docv [ "output-encoding" ] in
  Arg.(value (opt (enum known_outputs) SExpr i))

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

let parse_expression s =
  match E.Parser.expr s with
  | exception e ->
      Stdlib.Error (`Msg (Printexc.to_string e))
  | [ e ] ->
      Stdlib.Ok e
  | _ ->
      Stdlib.Error (`Msg "A single s-expression must be provided")

let expression =
  Arg.conv ~docv:"EXPRESSION" (parse_expression, E.pretty_print)

let path_expression =
  let parse s =
    let path, expr =
      match String.split s ~by:":" with
      | exception Not_found -> [], s
      | p, e -> String.trim p |> T.path_of_string, e in
    match parse_expression expr with
    | Stdlib.Error _ as err -> err
    | Stdlib.Ok e -> Stdlib.Ok (path, e)
  and print fmt (path, expr) =
    Legacy.Format.fprintf fmt "%s:%a"
      (T.string_of_path path)
      E.pretty_print expr in
  Arg.conv ~docv:"PATH:FUNCTION" (parse, print)

let modifier_exprs =
  let doc =
    "Expression computing an alternative value to write (function of the \
     input value)" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "e" ; "expression" ] in
  Arg.(value (opt_all path_expression [] i))

let aggr_init =
  let doc = "Initial valueof the aggregation" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "init" ] in
  Arg.(required (opt (some expression) None i))

let aggr_update =
  let doc = "Function updating the aggregation value with the current input \
             value" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "update" ] in
  (* TODO: by default, a function returning the first argument (prev state) *)
  Arg.(required (opt (some expression) None i))

let aggr_finalize =
  let doc = "Function building the output value from the aggregation value" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "finalize" ] in
  Arg.(required (opt (some expression) None i))

let dest_fname =
  let doc = "Output file" in
  let docv = "FILE" in
  let i = Arg.info ~doc ~docv [ "o" ; "output-file" ] in
  Arg.(required (opt (some string) None i))

let converter_cmd =
  let doc = "Generate a converter from in to out encodings" in
  Term.(
    (const converter
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ comptime_fieldmask
     $ modifier_exprs
     $ dest_fname),
    info "converter" ~doc)

let lib_cmd =
  let doc = "Generate a library with various converters from in to out \
             encodings" in
  Term.(
    (const lib
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ comptime_fieldmask
     $ dest_fname),
    info "lib" ~doc)

let lmdb_dump_cmd =
  let doc = "Generate a tool to dump an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_dump
     $ key_schema
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ dest_fname),
    info "lmdb-dump" ~doc)

let lmdb_load_cmd =
  let doc = "Generate a tool to load an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_load
     $ key_schema
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ dest_fname),
    info "lmdb-load" ~doc)

let lmdb_query_cmd =
  let doc = "Generate a tool to query an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_query
     $ key_schema
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ dest_fname),
    info "lmdb-query" ~doc)

let aggregator_cmd =
  let doc = "Generate a tool to compute an aggregated value of its input" in
  Term.(
    (const aggregator
     $ val_schema
     $ backend
     $ encoding_in
     $ encoding_out
     $ aggr_init
     $ aggr_update
     $ aggr_finalize
     $ dest_fname),
    info "aggregator" ~doc)

let default_cmd =
  let sdocs = Manpage.s_common_options in
  let doc = "Ramen Stream Processor" in
  Term.((ret (const (`Help (`Pager, None)))),
        info "ramen" ~version ~doc ~sdocs)

let () =
  match
    Term.eval_choice default_cmd [
      converter_cmd ; lib_cmd ; lmdb_dump_cmd ; lmdb_load_cmd ; lmdb_query_cmd ;
      aggregator_cmd ] with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok f -> f ()
