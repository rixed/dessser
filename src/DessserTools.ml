open Batteries

(*
 * Helper functions
 *)

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

let compile ?(optim=3) ?src_fname preferred_file_extension dest_fname f =
  let mode = [ `create ; `text ; `trunc ] in
  let src_fname =
    Option.default_delayed (fun () ->
      Printf.sprintf "/tmp/dessser_%d.%s"
        0 (*(Unix.getpid ())*)
        preferred_file_extension
    ) src_fname in
  File.with_file_out ~mode src_fname f ;
  let cmd =
    if preferred_file_extension = "cc" then
      (*Printf.sprintf "g++ -std=c++17 -g -O%d -W -Wall -I src -c %S -o %S" optim src_fname dest_fname*)
      Printf.sprintf "g++ -std=c++17 -g -O%d -W -Wall -I src %S -o %S" optim src_fname dest_fname
    else if preferred_file_extension = "ml" then
      (*Printf.sprintf "ocamlfind ocamlopt -g -annot -O%d -I src -package stdint,batteries -linkpkg -c %S -o %S" optim src_fname dest_fname*)
      Printf.sprintf "ocamlfind ocamlopt -g -annot -O%d -I src -package stdint,batteries -linkpkg src/DessserOCamlBackendHelpers.cmx %S -o %S" optim src_fname dest_fname
    else
      "true"
    in
  run_cmd cmd ;
  Printf.printf "output produced in %s\n" dest_fname

let array_print_i ?first ?last ?sep p oc a =
  let i = ref 0 in
  Array.print ?first ?last ?sep (fun oc x ->
    p !i oc x ; incr i) oc a
