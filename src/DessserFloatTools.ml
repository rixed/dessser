(* Float conversion utilities both useful for the OCaml backend and for the
 * dessser lib itself.*)

let hexstring_of_float = Printf.sprintf "%h"
let float_of_hexstring s = Scanf.sscanf s "%h" (fun f -> f)
let float_of_anystring s =
  try float_of_hexstring s with _ -> float_of_string s

(* The original Float.to_string adds a useless dot at the end of
 * round numbers, and likes to end with lots of zeroes. Also, we
 * want enough digits to be able to parse the exact same float back
 * from that result string., while avoiding ending sequence of 9s. *)
let string_of_float v =
  (* First try simple nice floats with no infinite 9s development,
   * as long as they can be parsed back: *)
  let s = string_of_float v in
  let s =
    if float_of_string s = v then s else
    (* Or go with as many digits as required: *)
    Printf.sprintf "%.17g" v in
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

(*$= string_of_float & ~printer:(fun x -> x)
  "1.234"    (string_of_float 1.234)
  "1.001"    (string_of_float 1.001)
  "1"        (string_of_float 1.)
  "31536000" (string_of_float 31536000.)
  "0"        (string_of_float 0.)
  "2972.98300573" (string_of_float 2972.98300573)
*)

(*$inject
  let test_desser_dec_float f =
    f = float_of_anystring (string_of_float f)
*)
(*$T
  test_desser_dec_float 0.
  test_desser_dec_float 1.
  test_desser_dec_float (1./.5.)
  test_desser_dec_float (1./.3.)
  test_desser_dec_float (1./.7.)
  test_desser_dec_float ~-.1.
  test_desser_dec_float ~-.(1./.5.)
  test_desser_dec_float ~-.(1./.3.)
  test_desser_dec_float ~-.(1./.7.)
  test_desser_dec_float Float.infinity
  test_desser_dec_float 2972.98300573
*)
(*$Q
  Q.float test_desser_dec_float
*)
