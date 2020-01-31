(* Float conversion utilities both useful for the OCaml backend and for the
 * dessser lib itself.*)

let hexstring_of_float = Printf.sprintf "%h"
let float_of_hexstring s = Scanf.sscanf s "%h" (fun f -> f)
let float_of_anystring s =
  try float_of_hexstring s with _ -> float_of_string s
