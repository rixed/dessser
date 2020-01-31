(* Some tools above the Dessser modules: *)
open Batteries
open DessserTools
open DessserExpressions
open Dessser

module FragmentsCPP = DessserDSTools_FragmentsCPP
module FragmentsOCaml = DessserDSTools_FragmentsOCaml

(* [convert] is a filter, aka a function from a pair of src*dst data-ptrs to
 * another such pair, as returned by DesSer.desser function. From that a
 * program from stdin to stdout is created. For simplicity, it will also
 * work in single-entry mode where it converts just one value from argv[1]
 * into stdout and stops (for tests). *)
let make_converter ?exe_fname backend convert =
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
    BE.print_definitions state oc ;
    if BE.preferred_def_extension = "cc" then
      String.print oc (FragmentsCPP.converter entry_point)
    else
      String.print oc (FragmentsOCaml.converter entry_point)) ;
  compile ~optim:3 ~link:true backend src_fname exe_fname ;
  exe_fname

(* Write an input to some single-shot converter program and return its
 * output: *)
let run_converter exe param =
  let str = IO.output_string () in
  with_stdout_from_command exe [| exe ; param |] (fun ic ->
    let i = IO.input_channel ic in
    IO.copy i str) ;
  IO.close_out str
