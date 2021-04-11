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
  | DIL -> (module DessserBackEndDIL : BACKEND)
  | Cpp -> (module DessserBackEndCPP : BACKEND)
  | OCaml -> (module DessserBackEndOCaml : BACKEND)

(* cmdliner must be given enum values that are comparable, therefore not
 * functions: *)
type encodings = Null | RowBinary | SExpr | RingBuff | CSV

let des_of_encoding = function
  | RingBuff -> (module DessserRamenRingBuffer.Des : DES)
  | RowBinary -> (module DessserRowBinary.Des : DES)
  | SExpr -> (module DessserSExpr.Des : DES)
  | CSV -> (module DessserCsv.Des : DES)
  | _ -> failwith "No desserializer for that encoding"

let ser_of_encoding = function
  | Null -> (module DessserDevNull.Ser : SER)
  | RingBuff -> (module DessserRamenRingBuffer.Ser : SER)
  | RowBinary -> (module DessserRowBinary.Ser : SER)
  | SExpr -> (module DessserSExpr.Ser : SER)
  | CSV -> (module DessserCsv.Ser : SER)

(* Generate just the code to convert from in to out (if they differ) and from
 * in to a heap value and from a heap value to out, then link into a library. *)
let lib schema backend encoding_in encoding_out _fieldmask dest_fname
        type_name () =
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module ToValue = DessserHeapValue.Materialize (Des) in
  let module OfValue = DessserHeapValue.Serialize (Ser) in
  let has_convert = encoding_in <> encoding_out in
  let convert =
    if has_convert then
      (* convert from encoding_in to encoding_out: *)
      E.func2 DataPtr DataPtr (fun l p1 p2 ->
        let module DS = DesSer (Des) (Ser) in
        DS.desser schema ?transform:None l p1 p2)
    else nop in
  let to_value =
    (* convert from encoding_in into a heapvalue: *)
    E.func1 DataPtr (fun l src ->
      first (ToValue.make schema l src)) in
  let ma = copy_field in
  let value_sersize =
    (* compute the serialization size of a heap value: *)
    E.func1 (Value schema) (fun l v ->
      OfValue.sersize schema l ma v) in
  let of_value =
    (* convert from a heapvalue into encoding_out. *)
    E.func2 (Value schema) DataPtr (fun l v dst ->
      OfValue.serialize schema l ma v dst) in
  if debug then (
    if has_convert then E.type_check [] convert ;
    E.type_check [] to_value ;
    E.type_check [] value_sersize ;
    E.type_check [] of_value) ;
  let compunit = U.make () in
  (* Christen the schema type with the user provided name: *)
  let compunit = U.name_type compunit schema.T.vtyp (type_name |? "t") in
  let compunit =
    if has_convert then
      let c, _, _ =
        U.add_identifier_of_expression compunit ~name:"convert" convert in c
    else compunit in
  let compunit, _, _ =
    U.add_identifier_of_expression compunit ~name:"to_value" to_value in
  let compunit, _, _ =
    U.add_identifier_of_expression compunit ~name:"value_sersize" value_sersize in
  let compunit, _, _ =
    U.add_identifier_of_expression compunit ~name:"of_value" of_value in
  let def_fname = change_ext BE.preferred_def_extension dest_fname in
  let decl_fname = change_ext BE.preferred_decl_extension dest_fname in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions oc compunit) ;
  write_source ~src_fname:decl_fname (fun oc ->
    BE.print_declarations oc compunit) ;
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
  let transform _mn0 path _l v =
    match List.find (fun (p, _) -> p = path) modifier_exprs with
    | exception Not_found -> v
    | _p, e -> apply e [v] in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    E.func2 DataPtr DataPtr (fun l -> DS.desser schema ~transform l) in
  if debug then E.type_check [] convert ;
  let compunit = U.make () in
  let compunit, _, convert_name =
    U.add_identifier_of_expression compunit ~name:"convert" convert in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  let convert_main_for = function
    | "cc" -> DessserDSTools_FragmentsCPP.converter convert_name
    | "ml" -> DessserDSTools_FragmentsOCaml.converter convert_name
    | "dil" -> ""
    | _ -> assert false in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions oc compunit ;
    String.print oc (convert_main_for BE.preferred_def_extension)
  ) ;
  compile ~optim:3 ~link:Executable backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

let destruct_pair = function
  | T.{ vtyp = Tup [| k ; v |] ; _ } ->
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
    E.func2 DataPtr DataPtr (fun l -> DS.desser key_schema l) in
  let convert_val =
    E.func2 DataPtr DataPtr (fun l -> DS.desser val_schema l) in
  if debug then (
    E.type_check [] convert_key ;
    E.type_check [] convert_val
  ) ;
  let compunit = U.make () in
  let compunit, _, convert_key_name =
    U.add_identifier_of_expression compunit ~name:"convert_key" convert_key in
  let compunit, _, convert_val_name =
    U.add_identifier_of_expression compunit ~name:"convert_val" convert_val in
  let def_fname =
    change_ext BE.preferred_def_extension dest_fname |>
    BE.valid_source_name in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions oc compunit ;
    main BE.preferred_def_extension convert_key_name convert_val_name |>
    String.print oc
  ) ;
  compile ~optim:3 ~link:Executable backend def_fname dest_fname ;
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
  let module ToValue = DessserHeapValue.Materialize (Des) in
  (* Let's start with a function that's reading input values from a given
   * source pointer and returns the heap value and the new source pointer: *)
  let to_value =
    E.func1 DataPtr (fun l -> ToValue.make schema l) in
  (* Check the function that creates the initial state that will be used by
   * the update function: *)
  E.type_check [] init_expr ;
  let state_t = E.type_of [] init_expr in
  (* Then check the update expression, that must be a function of the state_t
   * and the input_t: *)
  E.type_check [] update_expr ;
  let update_t = E.type_of [] update_expr in
  if not (T.eq update_t (T.Function ([| state_t ; Value schema |], T.void)))
  then
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
    | T.Function ([| a1 |], Value mn) when a1 = state_t -> mn
    | t ->
        Printf.sprintf2 "Aggregation finalizer must be a function of the \
                         aggregation state (not %a)" T.print t |>
        failwith in
  (* Finally, a function to convert the output value on the heap into stdout
   * in the given encoding: *)
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module OfValue = DessserHeapValue.Serialize (Ser) in
  let ma = copy_field in
  let of_value =
    E.func2 (Value output_t) DataPtr (fun l v dst ->
      OfValue.serialize output_t l ma v dst) in
  (* Let's now assemble all this into just three functions:
   * - init_expr, that we already have;
   * - input_expr, that deserialize and then update and return the new source
   *   pointer;
   * - output_expr, that finalize the value and serialize it. *)
  let compunit = U.make () in
  let compunit, state_id, state_name =
    U.add_identifier_of_expression compunit ~name:"init" init_expr in
  let input_expr =
    E.func1 ~l:(U.environment compunit) DataPtr (fun l src ->
      let v_src = apply to_value [ src ] in
      E.with_sploded_pair ~l "input_expr" v_src (fun _l v src ->
        seq [ apply update_expr [ state_id ; v ] ;
              src ])) in
  let compunit, _, input_name =
    U.add_identifier_of_expression compunit ~name:"input" input_expr in
  let output_expr =
    E.func1 ~l:(U.environment compunit) DataPtr (fun _l dst ->
      let v = apply finalize_expr [ state_id ] in
      apply of_value [ v ; dst ]) in
  let compunit, _, output_name =
    U.add_identifier_of_expression compunit ~name:"output" output_expr in
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
    BE.print_definitions oc compunit ;
    String.print oc (main_for BE.preferred_def_extension)
  ) ;
  compile ~optim:3 ~link:Executable backend def_fname dest_fname ;
  Printf.printf "executable in %S\n" dest_fname

(*
 * Command line
 *)

open Cmdliner

let maybe_nullable =
  let parse s =
    try (
      let s =
        if String.starts_with s "@" then
          (* That's a file *)
          let filename = String.lchop s in
          read_whole_file filename
        else
          s in
      Stdlib.Ok (T.maybe_nullable_of_string ~what:"schema" s)
    ) with e ->
      Stdlib.Error (`Msg (Printexc.to_string e))
  and print fmt mn =
    Format.fprintf fmt "%s" (T.string_of_maybe_nullable mn)
  in
  Arg.conv ~docv:"TYPE" (parse, print)

let val_schema =
  let doc = "schema for values (inline or @file)" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options
            [ "schema" ; "value-schema" ] in
  Arg.(opt (some maybe_nullable) None i)

let key_schema =
  let doc = "file or inline schema for keys" in
  let i = Arg.info ~doc ~docs:Manpage.s_common_options [ "key-schema" ] in
  Arg.(opt (some maybe_nullable) None i)

let type_name =
  let doc = "name for the schema" in
  let i = Arg.info ~doc [ "type-name" ; "name" ] in
  Arg.(opt (some string) None i)

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
  Arg.(opt (enum known_inputs) RowBinary i)

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
  Arg.(opt (enum known_outputs) SExpr i)

let backend =
  let languages =
    [ "DIL", DIL ;
      "C++", Cpp ;
      "OCaml", OCaml ] in
  let doc = "Language to generate code for" in
  let docv = docv_of_enum languages in
  let i = Arg.info ~doc ~docv [ "language" ; "backend" ] in
  Arg.(opt (some (enum languages)) None i)

let fieldmask =
  let parse s =
    try Stdlib.Ok (M.Parser.action_of_string s)
    with e -> Stdlib.Error (`Msg (Printexc.to_string e))
  and print fmt ma =
    Format.fprintf fmt "%s" (M.string_of_mask ma)
  in
  Arg.conv ~docv:"MASK" (parse, print)

(* One day when there are compile time masks this option will be needed.
 * For now we simulate compile time masks with runtime ones, which comes
 * handy for testing. *)
let comptime_fieldmask =
  let doc = "Compile-time fieldmask to apply when serializing" in
  let docv = "MASK" in
  let i = Arg.info ~doc ~docv [ "mask" ; "field-mask" ] in
  Arg.(opt fieldmask M.Copy i)

let parse_expression s =
  match E.Parser.expr s with
  | exception e ->
      Stdlib.Error (`Msg (Printexc.to_string e))
  | [ e ] ->
      Stdlib.Ok e
  | _ ->
      Stdlib.Error (`Msg "A single s-expression must be provided")

let expression =
  Arg.conv ~docv:"EXPRESSION" (parse_expression,
  E.pretty_print ?max_depth:None)

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
      (E.pretty_print ?max_depth:None) expr in
  Arg.conv ~docv:"PATH:FUNCTION" (parse, print)

let modifier_exprs =
  let doc =
    "Expression computing an alternative value to write (function of the \
     input value)" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "e" ; "expression" ] in
  Arg.(opt_all path_expression [] i)

let aggr_init =
  let doc = "Initial valueof the aggregation" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "init" ] in
  Arg.(opt (some expression) None i)

let aggr_update =
  let doc = "Function updating the aggregation value with the current input \
             value" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "update" ] in
  (* TODO: by default, a function returning the first argument (prev compunit) *)
  Arg.(opt (some expression) None i)

let aggr_finalize =
  let doc = "Function building the output value from the aggregation value" in
  let docv = "EXPRESSION" in
  let i = Arg.info ~doc ~docv [ "finalize" ] in
  Arg.(opt (some expression) None i)

let dest_fname =
  let doc = "Output file" in
  let docv = "FILE" in
  let i = Arg.info ~doc ~docv [ "o" ; "output-file" ] in
  Arg.(opt (some string) None i)

let converter_cmd =
  let doc = "Generate a converter from in to out encodings" in
  Term.(
    (const converter
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.value comptime_fieldmask
     $ Arg.value modifier_exprs
     $ Arg.required dest_fname),
    info "converter" ~doc)

let lib_cmd =
  let doc = "Generate a library with various converters from in to out \
             encodings" in
  Term.(
    (const lib
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.value comptime_fieldmask
     $ Arg.required dest_fname
     $ Arg.value type_name),
    info "lib" ~doc)

let lmdb_dump_cmd =
  let doc = "Generate a tool to dump an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_dump
     $ Arg.required key_schema
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required dest_fname),
    info "lmdb-dump" ~doc)

let lmdb_load_cmd =
  let doc = "Generate a tool to load an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_load
     $ Arg.required key_schema
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required dest_fname),
    info "lmdb-load" ~doc)

let lmdb_query_cmd =
  let doc = "Generate a tool to query an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_query
     $ Arg.required key_schema
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required dest_fname),
    info "lmdb-query" ~doc)

let aggregator_cmd =
  let doc = "Generate a tool to compute an aggregated value of its input" in
  Term.(
    (const aggregator
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required aggr_init
     $ Arg.required aggr_update
     $ Arg.required aggr_finalize
     $ Arg.required dest_fname),
    info "aggregator" ~doc)

let default_cmd =
  let sdocs = Manpage.s_common_options in
  let doc = "Ramen Stream Processor" in
  Term.((ret (const (`Help (`Pager, None)))),
        info "dessserc" ~version ~doc ~sdocs)

let () =
  match
    Term.eval_choice default_cmd [
      converter_cmd ; lib_cmd ; lmdb_dump_cmd ; lmdb_load_cmd ; lmdb_query_cmd ;
      aggregator_cmd ] with
  | `Error _ -> exit 1
  | `Version | `Help -> exit 0
  | `Ok f -> f ()
