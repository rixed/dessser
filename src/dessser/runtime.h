#ifndef RUNTIME_H_191025
#define RUNTIME_H_191025
#include <iostream> // for dump
#include <limits>   // for std::numeric_limits
#include <optional> // for nullable types
#include <sstream>
#include <string>
#include <utility>  // for pair
#include "dessser/typedefs.h"
#include "dessser/Bytes.h"
#include "dessser/Pointer.h"
#include "dessser/Vec.h"
#include "dessser/List.h"

inline uint64_t qwordOfFloat(double v)
{
  uint64_t res;
  static_assert(sizeof(v) == sizeof(res));
  memcpy(&res, &v, sizeof(v));
  return res;
}

inline double floatOfQword(uint64_t v)
{
  double res;
  static_assert(sizeof(v) == sizeof(res));
  memcpy(&res, &v, sizeof(v));
  return res;
}

inline std::string hexStringOfFloat(double f)
{
  std::ostringstream so;
  so << std::hexfloat << f;
  return so.str();
}

/* Conversion from 128 bits wide integers to strings.
 * Adapted from Jonathan Leffler's answer at
 * https://stackoverflow.com/questions/11656241/how-to-print-uint128-t-number-using-gcc */

/*      UINT64_MAX 18446744073709551615ULL */
#define P10_UINT64 10000000000000000000ULL   /* 19 zeroes */
#define E10_UINT64 19

inline std::string string_of_u128(uint128_t const u128)
{
  if (u128 <= UINT64_MAX) {
    return std::to_string((uint64_t)u128);
  } else {
    uint128_t const hi(u128 / P10_UINT64);
    uint64_t const lo(u128 % P10_UINT64);
    std::string const ss(std::to_string(lo));
    size_t num_zeros(E10_UINT64 - ss.length());
    return string_of_u128(hi) + std::string(num_zeros, '0') + ss;
  }
}

inline uint128_t u128_of_string(std::string const &s)
{
  size_t const len = s.length();
  if (len <= E10_UINT64) return std::stoull(s);
  size_t const hi_len(len - E10_UINT64);
  uint128_t const hi(u128_of_string(s.substr(0, hi_len)));
  uint128_t const lo(u128_of_string(s.substr(hi_len, E10_UINT64)));
  return hi * P10_UINT64 + lo;
}

/*      INT64_MAX 9223372036854775807LL */
#define P10_INT64 1000000000000000000LL   /* 18 zeroes */
#define E10_INT64 18

inline std::string string_of_i128(int128_t const i128)
{
  if (INT64_MIN <= i128 && i128 <= INT64_MAX) {
    return std::to_string((int64_t)i128);
  } else {
    int128_t const hi(i128 / P10_INT64);
    int64_t const lo(i128 % P10_INT64);
    std::string const ss(std::to_string(std::abs(lo)));
    size_t num_zeros(E10_INT64 - ss.length());
    return string_of_i128(hi) + std::string(num_zeros, '0') + ss;
  }
}

inline int128_t i128_of_string(std::string const &s)
{
  size_t const len = s.length();
  if (len <= E10_INT64) return std::stoll(s);
  size_t const hi_len(len - E10_INT64);
  int128_t const hi(i128_of_string(s.substr(0, hi_len)));
  int128_t const lo(i128_of_string(s.substr(hi_len, E10_INT64)));
  return hi * P10_INT64 + lo;
}

/* Pretty printers for everything so that Dump can rely on the '<<' operator: */

#include <iostream>

template <typename T1, typename T2>
std::ostream &operator<<(std::ostream &os, std::pair<T1,T2> const &p)
{
  os << '<' << p.first << ", " << p.second << ">";
  return os;
}

#endif
