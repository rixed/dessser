(* Tool to generate various piece of code to manipulate data *)
open Batteries

open Dessser
open DessserCompilConfig
open DessserDSTools
open DessserMiscTypes
open DessserTools
module T = DessserTypes
module E = DessserExpressions
module M = DessserMasks
open E.Ops

let debug = ref false
let quiet = ref false

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
  | _ -> failwith "No serializer for that encoding"

(* Some back-ends cannot deal with some schema, or need to adapt it somewhat.
 * For instance, OCaml backend does not like non unique record field names
 * or sum constructor names, so it can uniquify them: *)
let init_backend backend schema =
  let module BE = (val backend : BACKEND) in
  match BE.id with
  | OCaml -> DessserBackEndOCaml.init schema
  | _ -> ()

(* Generate just the code to convert from in to out (if they differ) and from
 * in to a heap value and from a heap value to out, then link into a library. *)
let lib dbg quiet_ schema backend encodings_in encodings_out converters
        with_fieldmask dest_fname optim () =
  if encodings_in = [] && encodings_out = [] then
    failwith "No encoding specified" ;
  if List.exists (fun (i, o) -> i = o) converters then
    failwith "Cannot convert from an encoding to itself" ;
  debug := dbg ;
  quiet := quiet_ ;
  (* Make "this" refers to top-level type: *)
  T.add_type_as "" schema.T.typ ;
  DessserEval.inline_level := optim ;
  let backend = module_of_backend backend in
  init_backend backend schema ;
  let compunit = U.make () in
  (* Christen the schema type the global type name "t" *)
  let type_name = "t" in
  let compunit = U.name_type compunit schema.T.typ type_name in
  (* Then declare all referenced external types as if we had the same methods
   * than those we are generating for this type: *)
  let compunit =
    T.fold_mn compunit (fun compunit -> function
      | T.Ext name ->
          U.register_external_type compunit name (fun _p -> function
            | DIL ->
                Printf.sprintf "%S" ("$" ^ name)
            | OCaml ->
                let m = DessserBackEndOCaml.valid_module_name name in
                m ^".DessserGen.t"
            | Cpp ->
                "*"^ DessserBackEndCPP.valid_identifier name)
      | _ ->
          compunit
    ) schema in
  let module BE = (val backend : BACKEND) in
  let add_decoder compunit encoding_in =
    let module Des = (val (des_of_encoding encoding_in) : DES) in
    let module ToValue = DessserHeapValue.Materialize (Des) in
    (* convert from encoding_in into a heapvalue: *)
    let compunit, des = ToValue.make schema compunit in
    if !debug then E.type_check E.no_env des ;
    let compunit, _, _ =
      let name = E.string_of_type_method (E.DesNoMask encoding_in) in
      U.add_identifier_of_expression compunit ~name des in
    compunit
  and add_encoder compunit encoding_out =
    let module Ser = (val (ser_of_encoding encoding_out) : SER) in
    let module OfValue = DessserHeapValue.Serialize (Ser) in
    let compunit, sersize =
      (* compute the serialization size of a heap value: *)
      OfValue.sersize ~with_fieldmask schema compunit in
    let compunit, ser =
      (* convert from a heapvalue into encoding_out. *)
      OfValue.serialize ~with_fieldmask schema compunit in
    if !debug then (
      E.type_check E.no_env sersize ;
      E.type_check E.no_env ser) ;
    let compunit, _, _ =
      let name =
        (if with_fieldmask then E.SSizeWithMask encoding_out
                           else E.SSizeNoMask encoding_out) |>
        E.string_of_type_method in
      U.add_identifier_of_expression compunit ~name sersize in
    let compunit, _, _ =
      let name =
        (if with_fieldmask then E.SerWithMask encoding_out
                           else E.SerNoMask encoding_out) |>
        E.string_of_type_method in
      U.add_identifier_of_expression compunit ~name ser in
    compunit
  and add_converter compunit (encoding_in, encoding_out) =
    let module Des = (val (des_of_encoding encoding_in) : DES) in
    let module Ser = (val (ser_of_encoding encoding_out) : SER) in
    assert (encoding_in <> encoding_out) ;
    let convert =
      (* convert from encoding_in to encoding_out: *)
      E.func2 T.ptr T.ptr (fun p1 p2 ->
        let module DS = DesSer (Des) (Ser) in
        DS.desser schema ?transform:None p1 p2) in
    if !debug then E.type_check E.no_env convert ;
    let compunit, _, _ =
      let name =
        E.string_of_type_method (E.Convert (encoding_in, encoding_out)) in
      U.add_identifier_of_expression compunit ~name convert in
    compunit in
  let compunit = List.fold_left add_decoder compunit encodings_in in
  let compunit = List.fold_left add_encoder compunit encodings_out in
  let compunit = List.fold_left add_converter compunit converters in
  let def_fname = change_ext BE.preferred_def_extension dest_fname in
  let decl_fname = change_ext BE.preferred_decl_extension dest_fname in
  write_source ~src_fname:def_fname (fun oc ->
    BE.print_definitions oc compunit) ;
  write_source ~src_fname:decl_fname (fun oc ->
    BE.print_declarations oc compunit) ;
  if not !quiet then (
    Printf.printf "declarations in %S\n" decl_fname ;
    Printf.printf "definitions in %S\n" def_fname)

let converter
      dbg quiet_ schema backend encoding_in encoding_out
      modifier_exprs dest_fname dev_mode optim () =
  debug := dbg ;
  quiet := quiet_ ;
  (* Make "this" refers to top-level type: *)
  T.add_type_as "" schema.T.typ ;
  DessserEval.inline_level := optim ;
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module DS = DesSer (Des) (Ser) in
  init_backend backend schema ;
  let transform _mn0 path v =
    match List.find (fun (p, _) -> p = path) modifier_exprs with
    | exception Not_found -> v
    | _p, e -> apply e [ v ] in
  let convert =
    (* convert from encoding_in to encoding_out: *)
    E.func2 T.ptr T.ptr (DS.desser schema ~transform) in
  if !debug then E.type_check E.no_env convert ;
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
  compile ~dev_mode ~optim ~link:Executable backend def_fname dest_fname ;
  if not !quiet then Printf.printf "executable in %S\n" dest_fname

let destruct_pair = function
  | T.{ typ = Tup [| k ; v |] ; _ } ->
      k, v
  | t ->
      Printf.sprintf2 "Not a pair: %a" T.print_mn t |>
      failwith

let lmdb main
      dbg quiet_ key_schema val_schema backend encoding_in encoding_out
      dest_fname dev_mode optim () =
  debug := dbg ;
  quiet := quiet_ ;
  T.add_type_as "val" val_schema.T.typ ;
  T.add_type_as "key" key_schema.T.typ ;
  DessserEval.inline_level := optim ;
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module DS = DesSer (Des) (Ser) in
  init_backend backend T.(required (Rec [| "key", key_schema ;
                                           "value", val_schema |])) ;
  let convert_key =
    (* convert from encoding_in to encoding_out: *)
    E.func2 T.ptr T.ptr (DS.desser key_schema) in
  let convert_val =
    E.func2 T.ptr T.ptr (DS.desser val_schema) in
  if !debug then (
    E.type_check E.no_env convert_key ;
    E.type_check E.no_env convert_val
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
  compile ~dev_mode ~optim ~link:Executable backend def_fname dest_fname ;
  if not !quiet then Printf.printf "executable in %S\n" dest_fname

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

let lmdb_query _ _ _ _ _ _ _ _ () =
  todo "lmdb_query"

(* In dessser IL we have to explicitly describe the initial state, the update
 * function and the finalizer function. Ramen will have to keep working hard
 * to convert simple RaQL aggregation functions into DIL programs. *)
let aggregator
      dbg quiet_ schema backend encoding_in encoding_out
      init_expr update_expr finalize_expr
      dest_fname dev_mode optim () =
  debug := dbg ;
  quiet := quiet_ ;
  (* Make "this" refers to top-level type: *)
  T.add_type_as "" schema.T.typ ;
  DessserEval.inline_level := optim ;
  let backend = module_of_backend backend in
  let module BE = (val backend : BACKEND) in
  let module Des = (val (des_of_encoding encoding_in) : DES) in
  let module ToValue = DessserHeapValue.Materialize (Des) in
  init_backend backend schema ;
  (* Let's start with a function that's reading input values from a given
   * source pointer and returns the heap value and the new source pointer: *)
  let compunit = U.make () in
  let compunit, des = ToValue.make schema compunit in
  (* Check the function that creates the initial state that will be used by
   * the update function: *)
  E.type_check E.no_env init_expr ;
  let state_t = E.type_of E.no_env init_expr in
  (* Then check the update expression, that must be a function of the state_t
   * and the input_t: *)
  E.type_check E.no_env update_expr ;
  let update_t = E.type_of E.no_env update_expr in
  if not (T.eq_mn update_t (T.func [| state_t ; schema |] T.void))
  then
    Printf.sprintf2
      "Aggregation updater (%a) must be a function of the aggregation state \
       (%a) and the input value (%a) and returning nothing (not %a)"
      (E.print ~max_depth:4) update_expr
      T.print_mn state_t
      T.print_mn schema
      T.print_mn update_t |>
    failwith ;
  (* Then check the finalizer: *)
  E.type_check E.no_env finalize_expr ;
  let output_t =
    match E.type_of E.no_env finalize_expr with
    | T.{ typ = Function ([| a1 |], mn) ; nullable = false }
      when T.eq_mn a1 state_t -> mn
    | mn ->
        Printf.sprintf2 "Aggregation finalizer must be a function of the \
                         aggregation state (not %a)" T.print_mn mn |>
        failwith in
  (* Finally, a function to convert the output value on the heap into stdout
   * in the given encoding: *)
  let module Ser = (val (ser_of_encoding encoding_out) : SER) in
  let module OfValue = DessserHeapValue.Serialize (Ser) in
  let compunit, ser = OfValue.serialize output_t compunit in
  (* Let's now assemble all this into just three functions:
   * - init_expr, that we already have;
   * - input_expr, that deserialize and then update and return the new source
   *   pointer;
   * - output_expr, that finalize the value and serialize it. *)
  let compunit, state_id, state_name =
    U.add_identifier_of_expression compunit ~name:"init" init_expr in
  let input_expr =
    E.func1 T.ptr (fun src ->
      let v_src = apply des [ src ] in
      E.with_sploded_pair "input_expr" v_src (fun v src ->
        seq [ apply update_expr [ state_id ; v ] ;
              src ])) in
  let compunit, _, input_name =
    U.add_identifier_of_expression compunit ~name:"input" input_expr in
  let output_expr =
    E.func1 T.ptr (fun dst ->
      let v = apply finalize_expr [ state_id ] in
      apply ser [ copy_field ; v ; dst ]) in
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
  compile ~dev_mode ~optim ~link:Executable backend def_fname dest_fname ;
  if not !quiet then Printf.printf "executable in %S\n" dest_fname

(*
 * Command line
 *)

open Cmdliner

let debug =
  let doc = "Enable debugging output on stdout and additional checks" in
  let env = Term.env_info "DESSSER_DEBUG" in
  let i = Arg.info ~env ~doc [ "debug" ] in
  Arg.flag i

let quiet =
  let doc = "Suppress all output but errors" in
  let env = Term.env_info "DESSSER_QUIET" in
  let i = Arg.info ~env ~doc [ "quiet" ] in
  Arg.flag i

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
      Stdlib.Ok (T.mn_of_string ~what:"schema" s)
    ) with e ->
      Stdlib.Error (`Msg (Printexc.to_string e))
  and print fmt mn =
    Format.fprintf fmt "%s" (T.mn_to_string mn)
  in
  Arg.conv ~docv:"TYPE" (parse, print)

let val_schema =
  let doc = "schema for values (inline or @file)" in
  let i = Arg.info ~doc [ "schema" ; "value-schema" ] in
  Arg.(opt (some maybe_nullable) None i)

let key_schema =
  let doc = "file or inline schema for keys" in
  let i = Arg.info ~doc [ "key-schema" ] in
  Arg.(opt (some maybe_nullable) None i)

let docv_of_enum l =
  IO.to_string (
    List.print ~first:"" ~last:"" ~sep:"|" (fun oc (n, _) ->
      String.print oc n)
  ) l

let known_inputs =
  T.[ RingBuff ; RowBinary ; SExpr ; CSV ] |>
  List.map (fun enc -> string_of_encoding enc, enc)

let encoding_in =
  let doc = "encoding format for input" in
  let docv = docv_of_enum known_inputs in
  let i = Arg.info ~doc ~docv [ "input-encoding" ] in
  Arg.(opt (enum known_inputs) RowBinary i)

let encodings_in =
  let doc = "encoding format for input" in
  let docv = docv_of_enum known_inputs in
  let i = Arg.info ~doc ~docv [ "input-encoding" ] in
  Arg.(opt_all (enum known_inputs) [] i)

let known_outputs =
  T.[ Null ; RingBuff ; RowBinary ; SExpr ; CSV ] |>
  List.map (fun enc -> string_of_encoding enc, enc)

let encoding_out =
  let doc = "encoding format for output" in
  let docv = docv_of_enum known_outputs in
  let i = Arg.info ~doc ~docv [ "output-encoding" ] in
  Arg.(opt (enum known_outputs) SExpr i)

let encodings_out =
  let doc = "encoding format for output" in
  let docv = docv_of_enum known_outputs in
  let i = Arg.info ~doc ~docv [ "output-encoding" ] in
  Arg.(opt_all (enum known_outputs) [] i)

let converter_in_out =
  let parse s =
    match String.split ~by:":" s with
    | exception Not_found ->
        Stdlib.Error (`Msg "Converter format must be IN:OUT")
    | i, o ->
        if not (List.mem_assoc i known_inputs) then
          Stdlib.Error (`Msg ("Unknown input encoder: "^ i))
        else if not (List.mem_assoc o known_outputs) then
          Stdlib.Error (`Msg ("Unknown output encoder: "^ o))
        else
          Stdlib.Ok (List.assoc i known_inputs,
                     List.assoc o known_outputs)
  and print fmt (i, o) =
    Format.fprintf fmt "%s:%s" (string_of_encoding i) (string_of_encoding o)
  in
  Arg.conv ~docv:"IN:OUT" (parse, print)

let converters =
  let doc = "in:out encodings to write a converter" in
  let docv = "IN:OUT" in
  let i = Arg.info ~doc ~docv [ "converter" ] in
  Arg.(opt_all converter_in_out [] i)

let backend =
  let languages =
    [ "DIL", DIL ;
      "C++", Cpp ;
      "OCaml", OCaml ] in
  let doc = "Language to generate code for" in
  let docv = docv_of_enum languages in
  let i = Arg.info ~doc ~docv [ "language" ; "backend" ] in
  Arg.(opt (some (enum languages)) None i)

let with_fieldmask =
  let doc = "Generate code that accept a runtime fieldmask" in
  let i = Arg.info ~doc [ "with-mask" ; "with-fieldmask" ;
                          "with-field-mask" ] in
  Arg.flag i

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
      | p, e -> String.trim p |> Path.of_string, e in
    match parse_expression expr with
    | Stdlib.Error _ as err -> err
    | Stdlib.Ok e -> Stdlib.Ok (path, e)
  and print fmt (path, expr) =
    Legacy.Format.fprintf fmt "%s:%a"
      (Path.to_string path)
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

let dev_mode =
  let doc = "Compile in development mode (using local files rather than \
             installed libraries)" in
  let env = Term.env_info "DESSSER_DEV_MODE" in
  let i = Arg.info ~env ~doc [ "dev-mode" ] in
  Arg.flag i

let optim =
  let doc = "Optimization level" in
  let env = Term.env_info "DESSSER_OPTIMIZATION_LEVEL" in
  let i = Arg.info ~env ~doc [ "O" ] in
  Arg.(opt int 3 i)

let converter_cmd =
  let doc = "Generate a converter from in to out encodings" in
  Term.(
    (const converter
     $ Arg.value debug
     $ Arg.value quiet
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.value modifier_exprs
     $ Arg.required dest_fname
     $ Arg.value dev_mode
     $ Arg.value optim),
    info "converter" ~doc)

let lib_cmd =
  let doc = "Generate a library with various converters from in to out \
             encodings" in
  Term.(
    (const lib
     $ Arg.value debug
     $ Arg.value quiet
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encodings_in
     $ Arg.value encodings_out
     $ Arg.value converters
     $ Arg.value with_fieldmask
     $ Arg.required dest_fname
     $ Arg.value optim),
    info "lib" ~doc)

let lmdb_dump_cmd =
  let doc = "Generate a tool to dump an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_dump
     $ Arg.value debug
     $ Arg.value quiet
     $ Arg.required key_schema
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required dest_fname
     $ Arg.value dev_mode
     $ Arg.value optim),
    info "lmdb-dump" ~doc)

let lmdb_load_cmd =
  let doc = "Generate a tool to load an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_load
     $ Arg.value debug
     $ Arg.value quiet
     $ Arg.required key_schema
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required dest_fname
     $ Arg.value dev_mode
     $ Arg.value optim),
    info "lmdb-load" ~doc)

let lmdb_query_cmd =
  let doc = "Generate a tool to query an LMDB storing values of the given \
             types" in
  Term.(
    (const lmdb_query
     $ Arg.value debug
     $ Arg.value quiet
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
     $ Arg.value debug
     $ Arg.value quiet
     $ Arg.required val_schema
     $ Arg.required backend
     $ Arg.value encoding_in
     $ Arg.value encoding_out
     $ Arg.required aggr_init
     $ Arg.required aggr_update
     $ Arg.required aggr_finalize
     $ Arg.required dest_fname
     $ Arg.value dev_mode
     $ Arg.value optim),
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
