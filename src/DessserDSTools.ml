(* Some tools above the Dessser modules: *)
open Batteries
open DessserTools
open Dessser
module E = DessserExpressions
module TC = DessserTypeCheck
module U = DessserCompilationUnit

let timeout_cmd = ref "/usr/bin/timeout"

let has_timeout () =
  let open Unix in
  match stat !timeout_cmd with
  | exception Unix_error (ENOENT, _, _) ->
      false
  | s ->
      s.st_kind = S_REG &&
      s.st_perm land 1 = 1

module FragmentsCPP = DessserDSTools_FragmentsCPP
module FragmentsOCaml = DessserDSTools_FragmentsOCaml

(* [convert] is a filter, aka a function from a pair of src*dst ptrs to
 * another such pair, as returned by DesSer.desser function. From that a
 * program from stdin to stdout is created. For simplicity, it will also
 * work in single-entry mode where it converts just one value from argv[1]
 * into stdout and stops (for tests). *)
let make_converter
      ?dev_mode ?optim ?dst_fname ?mn ?keep_temp_files compunit backend convert =
  let module BE = (val backend : BACKEND) in
  let convert = TC.type_check U.(environment compunit) convert in
  let compunit, _, entry_point =
    U.add_identifier_of_expression compunit ~name:"convert" convert in
  let comment =
    Option.map (fun mn ->
      Printf.sprintf2 "Converter for values of type:\n  %a\n"
        T.print_mn mn
    ) mn in
  let outro =
    let module_name = compunit.U.module_name in
    match BE.preferred_def_extension with
    | "cc" -> FragmentsCPP.converter module_name entry_point
    | "ml" -> FragmentsOCaml.converter module_name entry_point
    | "dil" -> ""
    | _ -> assert false in
  BE.compile ?dev_mode ?optim ~link:Executable ?comment ~outro ?dst_fname
             ?keep_temp_files compunit

(* Write an input to some single-shot converter program and return its
 * output: *)
let run_converter ?timeout exe param =
  let str = IO.output_string () in
  let cmd, args =
    match timeout with
    | Some t when has_timeout () ->
        !timeout_cmd, [| !timeout_cmd ; string_of_int t ; exe ; param |]
    | _ ->
        if timeout <> None then
          Printf.eprintf "Cannot set a timeout without %s.\n" !timeout_cmd ;
        exe, [| exe ; param |] in
  let env = [| "OCAMLRUNPARAM=b" |] in
  (try
    with_stdout_from_command ~env cmd args (fun ic ->
      let i = IO.input_channel ic in
      IO.copy i str)
  with _ -> ()) ;
  IO.close_out str
