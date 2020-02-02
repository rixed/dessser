#include <cassert>
#include <string>
#include <iostream>
#include "dessser/runtime.h"

void test_i128(std::string const s)
{
  int128_t v = i128_of_string(s);
  std::string const got_s(string_of_i128(v));
  assert(got_s == s);
}

int main()
{
  test_i128("1");
  test_i128("-1");
  test_i128("-1234");
  test_i128("-14274278215051572572471087");
  test_i128("-142");
  test_i128("-142742782150515725724710874243663988457");
}
