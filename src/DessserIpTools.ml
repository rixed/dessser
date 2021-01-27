open Stdint

open DessserTools

(*$inject
  open Stdint *)

module V4 =
struct
  (*$< V4 *)
  let to_string =
    let mask = Uint32.of_int 255 in
    let digit n shf =
      Uint32.(shift_right_logical n shf |>
              logand mask |>
              to_int) in
    let digit_sz n =
      if n < 10 then 1 else if n < 100 then 2 else 3 in
    let zero = Char.code '0' in
    let char_of n =
      Char.chr (zero + n) in
    fun n ->
      let ns = List.map (digit n) [24; 16; 8; 0] in
      let sz = List.fold_left (fun s n -> digit_sz n + s) 3 ns in
      let s = Bytes.create sz in
      List.fold_left (fun si n ->
        let si =
          if si = 0 then 0 else (Bytes.set s si '.' ; si + 1) in
        let si =
          if n < 100 then si else (
            Bytes.set s si (char_of (n / 100)) ; si + 1) in
        let si =
          if n < 10 then si else (
            Bytes.set s si (char_of ((n / 10) mod 10)) ; si + 1) in
        let si =
          Bytes.set s si (char_of (n mod 10)) ; si + 1 in
        si
      ) 0 ns |> ignore ;
      Bytes.to_string s

  (*$= to_string & ~printer:(fun x -> x)
    "123.45.67.89" \
      (to_string (Uint32.of_string "0x7B2D4359"))
   *)
  (*$>*)
end

module V6 =
struct
  (*$< V6 *)
  (* Used to compress 0s in IPv6: *)
  type word_type = Zeros of int | Word of int

  (* Used for *)
  let string_of_words lst =
    let s = Bytes.create (8*(4+1)) in
    let rec loop i (* last index in bytes s *) need_sep = function
      | [] -> i
      | Word v :: lst ->
          let i =
            if need_sep then (
              Bytes.set s i ':' ; i + 1
            ) else i in
          let i =
            if v < 0x1000 then i else (
              Bytes.set s i (hex_of (v lsr 12)) ; i + 1) in
          let i =
            if v < 0x100 then i else (
              Bytes.set s i (hex_of ((v lsr 8) land 0xf)) ; i + 1) in
          let i =
            if v < 0x10 then i else (
              Bytes.set s i (hex_of ((v lsr 4) land 0xf)) ; i + 1) in
          let i =
            Bytes.set s i (hex_of (v land 0xf)) ; i + 1 in
          loop i true lst
      | Zeros _ :: lst ->
          Bytes.set s i ':' ;
          Bytes.set s (i+1) ':' ;
          loop (i + 2) false lst
    in
    let len = loop 0 false lst in
    Bytes.sub_string s 0 len

  (*$= string_of_words & ~printer:(fun x -> x)
    "1" (string_of_words [ Word 1 ])
    "1:2" (string_of_words [ Word 1 ; Word 2 ])
    "1::2" (string_of_words [ Word 1 ; Zeros 42 ; Word 2 ])
    "::2a" (string_of_words [ Zeros 32 ; Word 42 ])
   *)


  let to_string =
    let mask = Uint128.of_int 65535 in
    let word n shf =
      Uint128.(shift_right_logical n shf |>
               logand mask |>
               to_int) in
    fun n ->
      let rec loop lst max_zeros zeros shf =
        if shf < 0 then
          let lst = if zeros > 0 then Zeros zeros :: lst else lst in
          (* replace all Zeros by Word 0s but the first one with max_zeros: *)
          let _, lst =
            List.fold_left (fun (max_zeros, res) -> function
              | Zeros z as zeros ->
                  if z = max_zeros then (* The chosen one *)
                    max_int, zeros :: res
                  else
                    let rec loop res z =
                      if z <= 0 then res else
                      loop (Word 0 :: res) (z - 1) in
                    max_zeros, loop res z
              | x -> max_zeros, x :: res
            ) (max_zeros, []) lst in
          string_of_words lst
        else
          let w = word n shf in
          let shf = shf - 16 in
          if w = 0 then
            let zeros = zeros + 1 in
            loop lst (max max_zeros zeros) zeros shf
          else
            let lst = if zeros > 0 then Zeros zeros :: lst else lst in
            let lst = Word w :: lst in
            loop lst max_zeros 0 shf
      in
      loop [] 0 0 112

  (*$= to_string & ~printer:(fun x -> x)
    "2001:db8::ff00:42:8329" \
      (to_string (Uint128.of_string \
        "0x20010DB8000000000000FF0000428329"))
    "::2001" \
      (to_string (Uint128.of_string "0x2001"))
    "2001::" \
      (to_string (Uint128.of_string \
        "0x20010000000000000000000000000000"))
    "1:23:456::7:0:8" \
      (to_string (Uint128.of_string \
        "0x00010023045600000000000700000008"))
  *)
  (*$>*)
end
