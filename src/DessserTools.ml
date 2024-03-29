(* General purpose helper functions with no dependencies on the rest of
 * Dessser *)
open Batteries

(*$inject
  open Batteries *)

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

let array_assoc n a =
  Array.find (fun (n', _) -> n' = n) a |> snd

let array_for_alli f a =
  try
    for i = 0 to Array.length a - 1 do
      if not (f i a.(i)) then raise Exit
    done ;
    true
  with Exit ->
    false

let array_find_map f a =
  let res = ref None in
  try
    for i = 0 to Array.length a - 1 do
      match f a.(i) with
      | Some x ->
          res := Some x ;
          raise Exit
      | None ->
          ()
    done ;
    raise Not_found
  with Exit ->
    Option.get !res

let enum_filter_mapi f e =
  Enum.mapi f e |>
  Enum.filter_map identity

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

let list_split_last_rev lst =
  match List.rev lst with
  | [] -> invalid_arg "list_split_last"
  | hd :: tl -> tl, hd

let list_split_last lst =
  let hds_rev, tl = list_split_last_rev lst in
  List.rev hds_rev, tl

(*$= list_split_last & ~printer:(fun (li, la) -> Printf.sprintf2 "%a..%d" (List.print Int.print) li la)
  ([ 1 ; 2; 3 ], 4) (list_split_last [ 1 ; 2 ; 3 ; 4 ])
  ([], 1) (list_split_last [ 1 ])
*)

let list_drop_last lst =
  fst (list_split_last lst)

let list_rev_mapi f l =
  List.fold_lefti (fun r i x -> f i x :: r) [] l

let list_drop n l =
  let rec loop n l =
    if n <= 0 || l = [] then l else
    loop (n - 1) (List.tl l) in
  loop n l

let rec list_assoc_eq ~eq k = function
  | [] -> raise Not_found
  | (k', v) :: rest -> if eq k k' then v else list_assoc_eq ~eq k rest

let rec list_last = function
  | [] -> invalid_arg "list_last"
  | x :: [] -> x
  | _ :: rest -> list_last rest

let rec list_find_after f = function
  | [] | [ _ ] -> raise Not_found
  | x :: n :: _ when f x -> n
  | _ :: _ :: rest -> list_find_after f rest

let assoc_merge l1 l2 =
  let rec loop l = function
    | [] -> l
    | (k, _) as x :: rest ->
        let l = if List.mem_assoc k l then l else x :: l in
        loop l rest in
  loop l1 l2

(*$= assoc_merge & ~printer:(IO.to_string (List.print (Tuple2.print Int.print String.print)))
  [ 3, "baz" ; 1, "foo" ; 2, "bar" ] \
    (assoc_merge [ 1, "foo" ; 2, "bar" ] [ 3, "baz" ])
  [ 3, "baz" ; 1, "foo" ; 2, "bar" ] \
    (assoc_merge [ 1, "foo" ; 2, "bar" ] [ 1, "foo" ; 3, "baz" ])
  [ 1, "foo" ; 2, "bar" ; 3, "baz" ] \
    (assoc_merge [ 1, "foo" ; 2, "bar" ; 3, "baz" ] [])
  [ 3, "baz" ; 2, "bar" ; 1, "foo" ] \
    (assoc_merge [] [ 1, "foo" ; 2, "bar" ; 3, "baz" ])
*)

let array_for_all2_no_exc f a b =
  try Array.for_all2 f a b
  with Invalid_argument _ -> false

let clamp mi ma x =
  if x < mi then mi else if x > ma then ma else x

let is_missing_symbol = function
  | '(' | ')' | '[' | ']' | '{' | '}' | ';'
  | '\'' | '"' | ',' | '.' | '_' |  ' ' ->
      true
  | _ ->
      false

let char_is_printable c =
  let open Char in
  is_letter c || is_digit c || is_symbol c || is_missing_symbol c

exception Not_implemented of string

let todo what =
  raise (Not_implemented what)

let () =
  Printexc.register_printer (function
    | Not_implemented what ->
        Some ("Not implemented: "^ what)
    | _ ->
        None)

(*
 * Print hex strings
 *)

let hex_of =
  let zero = Char.code '0'
  and ten = Char.code 'a' - 10 in
  fun n ->
    if n < 10 then Char.chr (zero + n)
    else Char.chr (ten + n)

let hexify_string s =
  match String.length s with
  | 0 ->
      ""
  | l1 ->
      let l2 = 2*l1 + (l1-1) in
      String.init l2 (fun i2 ->
        match i2 mod 3 with
        | 0 ->
            (* high digit *)
            hex_of (Char.code s.[i2 / 3] lsr 4)
        | 1 ->
            (* low digit *)
            hex_of (Char.code s.[i2 / 3] land 15)
        | _ ->
            (* separator *)
            ' ')

(*$= hexify_string & ~printer:identity
  "01 02 03" (hexify_string "\001\002\003")
*)

(*
 * Prettier printers
 *)

let pretty_enum_print ?(uppercase=false) p oc e =
  let and_ = if uppercase then " AND " else " and " in
  let rec loop first x =
    match Enum.get e with
    | None ->
        Printf.fprintf oc "%s%a" (if first then "" else and_) p x
    | Some next ->
        Printf.fprintf oc "%s%a" (if first then "" else ", ") p x ;
        loop false next in
  match Enum.get e with
  | None -> String.print oc "<empty>"
  | Some x -> loop true x

let pretty_list_print ?uppercase p oc =
  pretty_enum_print ?uppercase p oc % List.enum

let pretty_array_print ?uppercase p oc =
  pretty_enum_print ?uppercase p oc % Array.enum

(*
 * Subprocesses
 *)

(* Trick from LWT: how to exit without executing the at_exit hooks: *)
external sys_exit : int -> 'a = "caml_sys_exit"

(* Should be in batteries: *)
let name_of_signal s =
  let open Sys in
  if s = sigabrt then "ABORT"
  else if s = sigalrm then "ALRM"
  else if s = sigfpe then "FPE"
  else if s = sighup then "HUP"
  else if s = sigill then "ILL"
  else if s = sigint then "INT"
  else if s = sigkill then "KILL"
  else if s = sigpipe then "PIPE"
  else if s = sigquit then "QUIT"
  else if s = sigsegv then "SEGV"
  else if s = sigterm then "TERM"
  else if s = sigusr1 then "USR1"
  else if s = sigusr2 then "USR2"
  else if s = sigchld then "CHLD"
  else if s = sigcont then "CONT"
  else if s = sigstop then "STOP"
  else if s = sigtstp then "TSTP"
  else if s = sigttin then "TTIN"
  else if s = sigttou then "TTOU"
  else if s = sigvtalrm then "VTALRM"
  else if s = sigprof then "PROF"
  else if s = sigbus then "BUS"
  else if s = sigpoll then "POLL"
  else if s = sigsys then "SYS"
  else if s = sigtrap then "TRAP"
  else if s = sigurg then "URG"
  else if s = sigxcpu then "XCPU"
  else if s = sigxfsz then "XFSZ"
  else "Unknown OCaml signal number "^ string_of_int s

let wait_log pid =
  let open Unix in
  match restart_on_EINTR waitpid [] pid with
  | _, WEXITED 126 ->
      Printf.eprintf "couldn't execve after fork\n%!"
  | _, WEXITED code ->
      if code <> 0 then
        Printf.eprintf "Child process exited with status code %d\n%!" code
  | _, WSIGNALED sign ->
      Printf.eprintf "Child process killed by signal %s\n%!" (name_of_signal sign)
  | _, WSTOPPED sign ->
      Printf.eprintf "Child process stopped by signal %s\n%!" (name_of_signal sign)

let with_subprocess ?env cmd args k =
  let open Legacy.Unix in
  let env = env |? environment () in
  let his_in, my_in = pipe ~cloexec:false ()
  and my_out, his_out = pipe ~cloexec:false ()
  and my_err, his_err = pipe ~cloexec:false () in
  let close _what fd =
    try Unix.close fd
    with _e ->
      () (*Printf.eprintf "close %s failed: %s\n" what (Printexc.to_string e)*) in
  let rec fork_loop n =
    flush_all () ;
    match Unix.fork () with
    | exception (Unix.Unix_error (Unix.EAGAIN, _, _) as e) ->
        if n >= 10 then raise e ;
        Printf.eprintf "Cannot fork, too many processes, waiting...\n" ;
        Unix.sleep 1 ;
        fork_loop (n + 1)
    | 0 -> (* Child *)
      (try
        (* Move the fd in pos 0, 1 and 2: *)
        let move_fd s d =
          dup2 ~cloexec:false s d ;
          Unix.close s in
        move_fd his_err stderr ;
        move_fd his_in stdin ;
        move_fd his_out stdout ;
        execve cmd args env
      with e ->
        Printf.eprintf "Cannot execve: %s\n%!" (Printexc.to_string e) ;
        sys_exit 126)
    | pid -> (* Parent *)
      close "his_in" his_in ;
      close "his_out" his_out ;
      close "his_err" his_err ;
      let close_all () =
        close "my_in" my_in ;
        close "my_out" my_out ;
        close "my_err" my_err in
      let close_wait () =
        close_all () ;
        wait_log pid in
      finally close_wait
        k (out_channel_of_descr my_in,
           in_channel_of_descr my_out,
           in_channel_of_descr my_err)
  in
  fork_loop 0

let read_whole_channel ic =
  let read_chunk = 1000 in
  let rec loop buf o =
    if Bytes.length buf - o < read_chunk then
      loop (Bytes.extend buf 0 (5 * read_chunk)) o
    else
      let ret = Legacy.input ic buf o read_chunk in
      if ret = 0 then Bytes.(sub buf 0 o |> to_string)
      else loop buf (o + ret)
  in
  loop (Bytes.create (5 * read_chunk)) 0

let with_stdout_from_command ?env cmd args k =
  with_subprocess ?env cmd args (fun (_ic, oc, ec) ->
    let dump_stderr () =
      let last_words = read_whole_channel ec in
      if last_words <> "" then
        Printf.eprintf "%s: %s\n" cmd last_words in
    finally dump_stderr k oc)

(*$= with_stdout_from_command & ~printer:identity
  "glop" (with_stdout_from_command "/bin/echo" [|"/bin/echo";"glop"|] \
            Legacy.input_line)
*)

(*
 * Kahan Sums:
 *)

module Kahan =
struct
  type t = float * float

  let init = 0., 0.

  (* Add [x] to [sum] ([c] is carried along and must be added too eventually): *)
  let add (sum, c) x =
    let t = sum +. x in
    let c =
      c +.
      if Float.abs sum >= Float.abs x then (sum -. t) +. x
                                      else (x -. t) +. sum in
    t, c

  (* In some rare cases we might want to scale the counter: *)
  let mul (sum, c) s =
    sum *. s, c *. s

  let finalize (sum, c) = sum +. c
end
