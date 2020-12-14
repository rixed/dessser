(* All code is printed rather than in a library so that there is no need for
 * a runtime library to compile/run dessserc generated programs. *)

let readWholeFile =
  Printf.sprintf {|
static std::string readWholeFile(std::string const fname)
{
  std::ifstream t(fname);
  std::string str(std::istreambuf_iterator<char>(t),
                  (std::istreambuf_iterator<char>()));
  return str;
}

|}

let converter ?(out_buf_size=50_000) entry_point =
  readWholeFile ^
  Printf.sprintf {|
int main(int numArgs, char **args)
{
  char const *fname = "/dev/stdin";
  char delim = '\n';  // added after each output
  char const *single_input = nullptr;

  for (int a = 1; a < numArgs; a++) {
    if (
      a < numArgs - 1 && (
        0 == strcasecmp(args[a], "--delim") ||
        0 == strcasecmp(args[a], "-d")
      )
    ) {
      delim = args[++a][0];
    } else if (
      a < numArgs - 1 && (
        0 == strcasecmp(args[a], "--input") ||
        0 == strcasecmp(args[a], "-i")
      )
    ) {
      fname = args[++a];
    } else {
      single_input = args[a];
    }
  }

  std::string input =
    single_input ?
      single_input :
      readWholeFile(fname);
  Pointer src(input);

  while (src.rem() > 0) {
    Pointer dst { %d };

    Pair<Pointer, Pointer> ptrs = dessser_gen::%s(src, dst);

    // Print serialized:
    assert(ptrs.v2.offset < ptrs.v2.size-1);
    if (ptrs.v2.buffer) {
      fwrite(ptrs.v2.buffer.get(), 1, ptrs.v2.offset, stdout);
      if (delim != '\0') fwrite(&delim, sizeof(delim), 1, stdout);
    } // else it's a heap value

    src = ptrs.v1;

    if (single_input && src.rem() > 0) {
      std::cerr << src.rem() << " bytes left of input" << std::endl;
      return 1;
    }
  }

  return 0;
}
|} out_buf_size entry_point

let dumper ?(out_buf_size=50_000) _convert_key_name _convert_val_name =
  ignore out_buf_size ;
  DessserTools.todo "dumper for C++"

let loader ?(out_buf_size=50_000) _convert_key_name _convert_val_name =
  ignore out_buf_size ;
  DessserTools.todo "loader for C++"

let aggregator ?(out_buf_size=50_000) _state_name input_name output_name =
  readWholeFile ^
  Printf.sprintf {|
int main(int numArgs, char **args)
{
  char const *fname = "/dev/stdin";
  char delim = '\n';  // added after each output

  for (int a = 1; a < numArgs; a++) {
    if (
      a < numArgs - 1 && (
        0 == strcasecmp(args[a], "--delim") ||
        0 == strcasecmp(args[a], "-d")
      )
    ) {
      delim = args[++a][0];
    } else if (
      a < numArgs - 1 && (
        0 == strcasecmp(args[a], "--input") ||
        0 == strcasecmp(args[a], "-i")
      )
    ) {
      fname = args[++a];
    } else {
      std::cout << "Syntax: [(--delim|-d) DELIM] [(--input|-i) FILE]" << std::endl;
      exit(1);
    }
  }

  std::string input { readWholeFile(fname) };
  Pointer src(input);

  while (src.rem() > 0) {
    /* Accumulate that input into the global state: */
    src = dessser_gen::%s(src);
  }

  /* Output the finalized state: */
  Pointer dst { %d };
  dst = dessser_gen::%s(dst);
  assert(dst.offset < dst.size-1);

  if (dst.buffer) {
    fwrite(dst.buffer.get(), 1, dst.offset, stdout);
    if (delim != '\0') fwrite(&delim, sizeof(delim), 1, stdout);
  } // else it's a heap value

  return 0;
}
|} input_name out_buf_size output_name
