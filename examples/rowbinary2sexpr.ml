open Batteries
open DessserOCamlBackendHelpers

let read_whole_file fname =
  File.with_file_in ~mode:[`text] fname IO.read_all

let () =
  let fname = ref "/dev/stdin" in
  let delim = ref '\n' in
  Array.iteri (fun i arg ->
    if i < Array.length Sys.argv - 1 && (
         String.icompare arg "--delim" = 0 ||
         String.icompare arg "-d" = 0
       ) then
      delim := Sys.argv.(i + 1).[0]
    else if i > 0 then
      fname := arg
  ) Sys.argv ;

  let input = read_whole_file !fname in
  let src = Pointer.of_string input in

  let rec loop src =
    if Pointer.remSize src <= 0 then src else
      let sz = 1024 in
      let dst = Pointer.make sz in
      let src, dst = Example.func2_0 src dst in
      let b, o = dst in
      assert (o < Bytes.length b) ;
      String.print stdout (Bytes.sub_string b 0 o) ;
      Char.print stdout !delim ;
      loop src in
  loop src |> ignore
