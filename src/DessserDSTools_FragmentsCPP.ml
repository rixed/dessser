let converter ?(out_buf_size=50_000) entry_point =
  Printf.sprintf {|
static std::string readWholeFile(std::string const fname)
{
  std::ifstream t(fname);
  std::string str(std::istreambuf_iterator<char>(t),
                  (std::istreambuf_iterator<char>()));
  return str;
}

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
      delim = args[a+1][0];
    } else if (
      a < numArgs - 1 && (
        0 == strcasecmp(args[0], "--input") ||
        0 == strcasecmp(args[0], "-i")
      )
    ) {
      fname = args[a+1];
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
    Size outSz(%d);
    Pointer dst(outSz);

    Pair<Pointer, Pointer> ptrs = %s(src, dst);

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
