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

let write_source ~src_fname f =
  let mode = [ `create ; `text ; `trunc ] in
  File.with_file_out ~mode src_fname f

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

let list_rev_filter f lst =
  let rec loop acc = function
    | [] -> acc
    | x :: rest -> loop (if f x then x :: acc else acc) rest
  in loop [] lst

let list_split_last lst =
  match List.rev lst with
  | [] -> invalid_arg "list_split_last"
  | hd :: tl -> List.rev tl, hd

let hexstring_of_float = Legacy.Printf.sprintf "%h"
let float_of_hexstring s = Legacy.Scanf.sscanf s "%h" identity
let float_of_anystring s =
  try float_of_hexstring s with _ -> float_of_string s

let cap mi ma x =
  if x < mi then mi else if x > ma then ma else x
