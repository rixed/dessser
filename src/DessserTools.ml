open Batteries
open Dessser

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

let write_source ~src_fname f =
  let mode = [ `create ; `text ; `trunc ] in
  File.with_file_out ~mode src_fname f

let compile ?(optim=3) ~link backend src_fname dest_fname =
  let module BE = (val backend : BACKEND) in
  let cmd = BE.compile_cmd ~optim ~link src_fname dest_fname in
  run_cmd cmd ;
  Printf.printf "output produced in %s\n" dest_fname

let array_print_i ?first ?last ?sep p oc a =
  let i = ref 0 in
  Array.print ?first ?last ?sep (fun oc x ->
    p !i oc x ; incr i) oc a

let read_whole_file fname =
  File.with_file_in ~mode:[`text] fname IO.read_all

let change_ext new_ext fname =
  assert (new_ext <> "") ;
  assert (new_ext.[0] <> '.') ;
  (Filename.remove_extension fname) ^"."^ new_ext
