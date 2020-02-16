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

let array_assoc n a =
  Array.find (fun (n', _) -> n' = n) a |> snd

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

let list_drop_last lst =
  fst (list_split_last lst)

let list_rev_mapi f l =
  List.fold_lefti (fun r i x -> f i x :: r) [] l

let cap mi ma x =
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

let wait_log () =
  let open Unix in
  match restart_on_EINTR wait () with
  | _, WEXITED 126 ->
      Printf.eprintf "couldn't execve after fork\n%!"
  | _, WEXITED code ->
      if code <> 0 then
        Printf.eprintf "Child process existed with status code %d\n%!" code
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
  flush_all () ;
  match Unix.fork () with
  | 0 -> (* Child *)
    (try
      (* Move the fd in pos 0, 1 and 2: *)
      let move_fd s d =
        dup2 ~cloexec:false s d ;
        close s in
      move_fd his_err stderr ;
      move_fd his_in stdin ;
      move_fd his_out stdout ;
      execve cmd args env
    with e ->
      Printf.eprintf "Cannot execve: %s\n%!" (Printexc.to_string e) ;
      sys_exit 126)
  | _pid -> (* Parent *)
    close his_in ; close his_out ; close his_err ;
    let close_all () =
      close my_in ;
      close my_out ;
      close my_err in
    let close_wait () =
      close_all () ;
      wait_log () in
    finally close_wait
      k (out_channel_of_descr my_in,
         in_channel_of_descr my_out,
         in_channel_of_descr my_err)

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
