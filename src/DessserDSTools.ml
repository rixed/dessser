(* Some tools above the Dessser modules: *)
open Batteries
open DessserTools
open DessserExpressions
open Dessser

module FragmentsCPP = DessserDSTools_FragmentsCPP
module FragmentsOCaml = DessserDSTools_FragmentsOCaml

let compile ?(optim=3) ~link backend src_fname dest_fname =
  let module BE = (val backend : BACKEND) in
  let cmd = BE.compile_cmd ~optim ~link src_fname dest_fname in
  run_cmd cmd

(* [convert] is a filter, aka a function from a pair of src*dst data-ptrs to
 * another such pair, as returned by DesSer.desser function. From that a
 * program from stdin to stdout is created. For simplicity, it will also
 * work in single-entry mode where it converts just one value from argv[1]
 * into stdout and stops (for tests). *)
let make_converter ?exe_fname ?mn backend convert =
  let module BE = (val backend : BACKEND) in
  type_check [] convert ;
  let state = BE.make_state () in
  let state, _, entry_point =
    BE.identifier_of_expression state convert in
  let exe_fname = match exe_fname with
    | Some fname -> fname
    | None -> Filename.temp_file "dessser_converter_" "" in
  let src_fname = change_ext BE.preferred_def_extension exe_fname in
  write_source ~src_fname (fun oc ->
    Option.may (fun mn ->
      BE.print_comment oc "Converter for values of type:\n  %a\n"
        T.print_maybe_nullable mn
    ) mn ;
    BE.print_comment oc "Compile with:\n  %s\n"
      (BE.compile_cmd ~optim:0 ~link:true src_fname exe_fname) ;
    BE.print_definitions state oc ;
    if BE.preferred_def_extension = "cc" then
      String.print oc (FragmentsCPP.converter entry_point)
    else
      String.print oc (FragmentsOCaml.converter entry_point)) ;
  compile ~optim:3 ~link:true backend src_fname exe_fname ;
  exe_fname

(* Write an input to some single-shot converter program and return its
 * output: *)
let run_converter ?timeout exe param =
  let str = IO.output_string () in
  let cmd, args = match timeout with
    | None -> exe, [| exe ; param |]
    | Some t ->
        let timeout_cmd = "/usr/bin/timeout" in
        timeout_cmd, [| timeout_cmd ; string_of_int t ; exe ; param |] in
  with_stdout_from_command cmd args (fun ic ->
    let i = IO.input_channel ic in
    IO.copy i str) ;
  IO.close_out str