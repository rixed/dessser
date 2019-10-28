(* As the generated code is only temporary (and hard to follow), assertions
 * and backtraces are not that usefull. Rather print as much as possible when
 * failing: *)
let fail msg =
  Format.eprintf "FAILURE: %s@." msg ;
  assert false

let fail_if ~cond ~msg =
  if not cond then fail msg

let todo what =
  fail ("TODO: "^ what)
