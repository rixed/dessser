(* Float conversion utilities both useful for the OCaml backend and for the
 * dessser lib itself.*)

let hexstring_of_float = Printf.sprintf "%h"
let float_of_hexstring s = Scanf.sscanf s "%h" (fun f -> f)
let float_of_anystring s =
  try float_of_hexstring s with _ -> float_of_string s

(* The original Float.to_string adds a useless dot at the end of
 * round numbers, and likes to end with lots of zeroes: *)
let string_of_float v =
  let s = Float.to_string v in
  assert (String.length s > 0) ;
  match String.index s '.' with
  | exception Not_found -> s
  | i ->
      let last_non_zero =
        let rec loop j =
          assert (j >= i) ;
          if s.[j] <> '0' then j else loop (j - 1) in
        loop (String.length s - 1) in
      let has_trailling_dot = s.[last_non_zero] = '.' in
      let n =
        (String.length s - last_non_zero) - 1 +
        (if has_trailling_dot then 1 else 0) in
      BatString.rchop ~n s
