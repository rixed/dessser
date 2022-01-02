#ifndef IP_H_210101
#define IP_H_210101
#include "dessser/typedefs.h"
#if __has_include(<arpa/inet.h>)

extern "C" {
# include <arpa/inet.h>
}

namespace dessser {

inline std::string string_of_ipv4(uint32_t const ip)
{
  char str[INET_ADDRSTRLEN];
  return inet_ntop(AF_INET, &ip, str, sizeof(str));
}

inline std::string string_of_ipv6(uint128_t const ip)
{
  char str[INET6_ADDRSTRLEN];
  return inet_ntop(AF_INET6, &ip, str, sizeof(str));
}

}  // namespace dessser

#else  // no <arpa/inet.h>

# ifdef _WIN32

#include <string>

namespace dessser {

inline std::string string_of_ipv4(uint32_t const ip)
{
  return
    std::to_string((ip >> 24U) & 255) + '.' +
    std::to_string((ip >> 16U) & 255) + '.' +
    std::to_string((ip >> 8U) & 255) + '.' +
    std::to_string((ip >> 0U) & 255);
}

inline char char_of_digit(uint8_t const d)
{
  if (d < 10) return '0' + d; else return 'a' + (d - 10);
}

inline std::string string_of_ipv6_word(uint16_t const w)
{
  return
    std::string(1, char_of_digit((w >> 12U) & 0xf)) +
    char_of_digit((w >> 8U) & 0xf) +
    char_of_digit((w >> 4U) & 0xf) +
    char_of_digit((w >> 0U) & 0xf);
}

inline std::string string_of_ipv6(uint128_t const ip)
{
  std::string s;
  for (unsigned i = 0; i < 8; i++) {
    if (i) s += ':';
    s += string_of_ipv6_word((ip >> (16U*i)) & 0xffff);
  }
  return s;
}

}  // namespace dessser

# else
#   error "No <arpa/inet.h> and not WIN32?"
# endif  // _WIN32

#endif  // has_include(<arpa/inet.h>)

#endif
