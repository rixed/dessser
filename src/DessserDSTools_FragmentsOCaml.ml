let converter ?(out_buf_size=50_000) entry_point =
  Printf.sprintf {|
let read_whole_file fname =
  File.with_file_in ~mode:[`text] fname IO.read_all

let () =
  let fname = ref "/dev/stdin"
  and delim = ref '\n'
  and single_input = ref "" in

  Array.iteri (fun i arg ->
    if i < Array.length Sys.argv - 1 &&
       (String.icompare arg "--delim" = 0 ||
        String.icompare arg "-d" = 0)
    then
      delim := Sys.argv.(i + 1).[0]
    else if i < Array.length Sys.argv - 1 &&
            (String.icompare arg "--input" = 0 ||
             String.icompare arg "-i" = 0)
    then
      fname := argv.(i + 1)
    else if i > 0 then
      single_input := arg
  ) Sys.argv ;

  let input =
    if !single_input <> "" then
      !single_input
    else
      read_whole_file !fname in
  let src = Pointer.of_string input in

  let rec loop src =
    if Pointer.remSize src <= 0 then src else (
      let sz = %d in
      let dst = Pointer.make sz in
      let src, dst = %s src dst in
      assert (dst.offset < dst.length) ;
      String.print stdout (Bytes.sub_string dst.bytes 0 dst.offset) ;
      Char.print stdout !delim ;
      flush stdout ;
      if !single_input <> "" && Pointer.remSize src > 0 then
        Printf.sprintf "%%d bytes left of input" (Pointer.remSize src) |>
        failwith ;
      loop src
    ) in
  loop src |> ignore
|} out_buf_size entry_point
