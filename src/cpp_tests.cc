#include <cassert>
#include <string>
#include <iostream>
#include "dessser/runtime.h"

void test_i128(std::string const s)
{
  std::optional<int128_t> const v { i128_of_string(s) };
  std::string const got_s { v ? string_of_i128(*v) : "NULL" };
  assert(got_s == s);

  char const *str = s.c_str();
  int128_t v2;
  (void)i128_from_chars(str, str+s.size(), &v2);
  assert(v2 == v);
}

int main()
{
  test_i128("1");
  test_i128("-1");
  test_i128("-1234");
  test_i128("-14274278215051572572471087");
  test_i128("-142");
  test_i128("-142742782150515725724710874243663988457");
  test_i128("-124783461433994629716699467879838724110");
}
