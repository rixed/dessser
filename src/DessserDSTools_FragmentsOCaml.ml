let run_main = {|
let () =
  try
    main ()
  with Failure msg ->
    Printf.eprintf "Failure: %s\n" msg ;
    exit 1
|}


let converter ?(out_buf_size=50_000) convert_id =
  Printf.sprintf {|
open Batteries

let main () =
  let fname = ref "/dev/stdin"
  and delim = ref '\n'
  and single_input = ref "" in

  let syntax () =
    String.print stdout
      "Syntax: [(--delim|-d) DELIM] [(--input|-i) FILE | INPUT]\n" ;
    exit 1 in

  let read_whole_file fname =
    File.with_file_in ~mode:[`text] fname IO.read_all in

  let rec loop_args i =
    if i < Array.length Sys.argv then (
      let arg = Sys.argv.(i) in
      if i < Array.length Sys.argv - 1 &&
         (String.icompare arg "--delim" = 0 ||
          String.icompare arg "-d" = 0)
      then (
        delim := Sys.argv.(i + 1).[0] ;
        loop_args (i + 2)
      ) else if i < Array.length Sys.argv - 1 &&
                (String.icompare arg "--input" = 0 ||
                 String.icompare arg "-i" = 0)
      then (
        fname := Sys.argv.(i + 1) ;
        loop_args (i + 2)
      ) else if !single_input = "" then (
        single_input := arg ;
        loop_args (i + 1)
      ) else (
        syntax ()
      )
    ) in
  loop_args 1 ;

  let input =
    if !single_input <> "" then
      !single_input
    else
      read_whole_file !fname in

  let rec loop src =
    if Pointer.remSize src <= 0 then src else (
      let dst = Pointer.make %d in
      let src, dst = %s src dst in
      assert (dst.Pointer.start <= dst.Pointer.stop) ;
      String.print stdout (Bytes.sub_string dst.bytes 0 dst.start) ;
      Char.print stdout !delim ;
      flush stdout ;
      if !single_input <> "" && Pointer.remSize src > 0 then
        Printf.sprintf "%%d bytes left of input" (Pointer.remSize src) |>
        failwith ;
      loop src
    ) in
  let src = Pointer.of_string input in
  loop src |> ignore

%s
|} out_buf_size convert_id run_main


let dumper ?(out_buf_size=50_000) convert_key_id convert_val_id =
  Printf.sprintf {|
open Batteries

let main () =
  let envname = ref ""  (* mandatory *)
  and mapname = ref ""  (* empty is OK *)
  and kv_delim = ref "\n"
  and eov_delim = ref "\n" in

  let syntax () =
    String.print stdout
      "Syntax: [(--delim|-d) DELIM] [(--kv-delim|-k) DELIM] envpath [subdb]\n" ;
    exit 1 in

  let rec loop_args i =
    if i < Array.length Sys.argv then (
      let arg = Sys.argv.(i) in
      if i < Array.length Sys.argv - 1 &&
         (String.icompare arg "--delim" = 0 ||
          String.icompare arg "-d" = 0)
      then (
        eov_delim := Sys.argv.(i + 1) ;
        loop_args (i + 2)
      ) else if i < Array.length Sys.argv - 1 &&
                (String.icompare arg "--kv-delim" = 0 ||
                 String.icompare arg "-k" = 0)
      then (
        kv_delim := Sys.argv.(i + 1) ;
        loop_args (i + 2)
      ) else if String.length Sys.argv.(i) = 0 then (
        syntax ()
      ) else if Sys.argv.(i).[0] = '-' then (
        syntax ()
      ) else if !envname = "" then (
        envname := Sys.argv.(i) ;
        loop_args (i + 1)
      ) else if !mapname = "" then (
        mapname := Sys.argv.(i) ;
        loop_args (i + 1)
      ) else (
        failwith "Must specify only the environment path and then the map name"
      )
    ) in
  loop_args 1 ;

  if !envname = "" then
    failwith "LMDB environment name is missing" ;

  let open Lmdb in
  let env = Env.create Ro !envname in
  let name = if !mapname = "" then None else Some !mapname in
  (* We first serialize into a string for simplicity: *)
  let map =
    Map.open_existing Nodup ~key:Conv.string ~value:Conv.string
                      ?name env in

  let out_buf_sz = %d in
  (* A cursor is required to iter over the whole DB: *)
  Cursor.go Ro map (fun cursor ->
    Cursor.iter ~cursor ~f:(fun k v ->
      let src = Pointer.of_string k in
      let dst = Pointer.make out_buf_sz in
      let src, dst = %s src dst in
      assert (dst.Pointer.start <= dst.Pointer.stop) ;
      String.print stdout (Bytes.sub_string dst.bytes 0 dst.start) ;
      String.print stdout !kv_delim ;
      let src = Pointer.of_string v in
      let dst = Pointer.make out_buf_sz in
      let src, dst = %s src dst in
      assert (dst.Pointer.start <= dst.Pointer.stop) ;
      String.print stdout (Bytes.sub_string dst.bytes 0 dst.start) ;
      String.print stdout !eov_delim ;
      flush stdout
    ) map
  )

%s
|} out_buf_size convert_key_id convert_val_id run_main


let loader ?(out_buf_size=50_000) convert_key_id convert_val_id =
  Printf.sprintf {|
open Batteries

let main () =
  let fname = ref "/dev/stdin"
  and envname = ref ""  (* mandatory *)
  and mapname = ref ""  (* empty is OK *)
  and kv_delim = ref "\n"
  and eov_delim = ref "\n" in

  let syntax () =
    String.print stdout
      "Syntax: [(--delim|-d) DELIM] [(--kv-delim|-k) DELIM] envpath [subdb]\n" ;
    exit 1 in

  let read_whole_file fname =
    File.with_file_in ~mode:[`text] fname IO.read_all in

  let rec loop_args i =
    if i < Array.length Sys.argv then (
      let arg = Sys.argv.(i) in
      if i < Array.length Sys.argv - 1 &&
         (String.icompare arg "--delim" = 0 ||
          String.icompare arg "-d" = 0)
      then (
        eov_delim := Sys.argv.(i + 1) ;
        loop_args (i + 2)
      ) else if i < Array.length Sys.argv - 1 &&
                (String.icompare arg "--kv-delim" = 0 ||
                 String.icompare arg "-k" = 0)
      then (
        kv_delim := Sys.argv.(i + 1) ;
        loop_args (i + 2)
      ) else if i < Array.length Sys.argv - 1 &&
                (String.icompare arg "--input" = 0 ||
                 String.icompare arg "-i" = 0)
      then (
        fname := Sys.argv.(i + 1) ;
        loop_args (i + 2)
      ) else if String.length Sys.argv.(i) = 0 then (
        syntax ()
      ) else if Sys.argv.(i).[0] = '-' then (
        syntax ()
      ) else if !envname = "" then (
        envname := Sys.argv.(i) ;
        loop_args (i + 1)
      ) else if !mapname = "" then (
        mapname := Sys.argv.(i) ;
        loop_args (i + 1)
      ) else (
        failwith "Must specify only the environment path and then the map name"
      )
    ) in
  loop_args 1 ;

  if !envname = "" then
    failwith "LMDB environment name is missing" ;

  let open Lmdb in
  let env = Env.(create Rw ~flags:Flags.write_map) !envname in
  let name = if !mapname = "" then None else Some !mapname in
  (* We first serialize into a string for simplicity: *)
  let map =
    Map.create Nodup ~key:Conv.string ~value:Conv.string
                      ?name env in

  let out_buf_sz = %d in

  let split_kv =
    if !eov_delim = !kv_delim then
      fun kvs ->
        List.fold_left (fun (lst, k_opt) s ->
          match k_opt with
          | None -> lst, Some s
          | Some k -> (k, s) :: lst, None
        ) ([], None) kvs |>
        fst
    else
      fun kvs ->
        List.filter ((<>) "") kvs |>
        List.map (fun kv ->
          try String.split ~by:!kv_delim kv
          with Not_found ->
            Printf.sprintf "Cannot find delimiter %%s in %%s"
              !kv_delim kv |>
            failwith
        ) in

  read_whole_file !fname |>
  String.split_on_string ~by:!eov_delim |>
  split_kv |>
  List.iter (fun (k, v) ->
    let src = Pointer.of_string k in
    let dst = Pointer.make out_buf_sz in
    let src, dst = %s src dst in
    assert (Pointer.remSize src = 0) ;
    assert (dst.Pointer.start <= dst.Pointer.stop) ;
    let key = Bytes.sub_string dst.bytes 0 dst.start in

    let src = Pointer.of_string v in
    let dst = Pointer.make out_buf_sz in
    let src, dst = %s src dst in
    assert (Pointer.remSize src = 0) ;
    assert (dst.Pointer.start <= dst.Pointer.stop) ;
    let value = Bytes.sub_string dst.bytes 0 dst.start in

    Map.set map key value ;
  )

%s
|} out_buf_size convert_key_id convert_val_id run_main
