#ifndef RUNTIME_H_191025
#define RUNTIME_H_191025
#include <exception>
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
#include "dessser/Arr.h"
#include "dessser/Set.h"
#include "dessser/SimpleSet.h"
#include "dessser/SlidingWindow.h"
#include "dessser/TumblingWindow.h"
#include "dessser/Sampling.h"
#include "dessser/HashTable.h"
#include "dessser/Heap.h"
#include "dessser/SList.h"
#include "dessser/Mask.h"

#define _STRIZE(arg) #arg
#define STRIZE(x)  _STRIZE(x)

namespace dessser_gen {

inline uint64_t qword_of_float(double v)
{
  uint64_t res;
  static_assert(sizeof(v) == sizeof(res));
  memcpy(&res, &v, sizeof(v));
  return res;
}

inline double float_of_qword(uint64_t v)
{
  double res;
  static_assert(sizeof(v) == sizeof(res));
  memcpy(&res, &v, sizeof(v));
  return res;
}

inline std::string hex_string_of_float(double f)
{
  std::ostringstream so;
  so << std::hexfloat << f;
  return so.str();
}

inline std::size_t clamp_to_string_length(long p, std::size_t const l)
{
  if (0 == l) return 0;
  if (p >= 0) return std::min((std::size_t)p, l);
  while (p < 0) p += l;
  return p;
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
    std::size_t num_zeros(E10_UINT64 - ss.length());
    return string_of_u128(hi) + std::string(num_zeros, '0') + ss;
  }
}

inline uint128_t u128_of_string(std::string const &s)
{
  std::size_t const len = s.length();
  assert(len > 0);
  if (len <= E10_UINT64) return std::stoull(s);
  std::size_t const hi_len(len - E10_UINT64);
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
    std::size_t num_zeros(E10_INT64 - ss.length());
    return string_of_i128(hi) + std::string(num_zeros, '0') + ss;
  }
}

inline bool is_sign(char const x) { return x == '-' || x == '+'; }
inline bool is_digit(char const x) { return x >= '0' && x <= '9'; }

inline std::optional<int128_t> i128_of_string(std::string const &s)
{
  // FIXME: do not split just after the leading minus sign!
  std::size_t const len = s.length();
  assert(len > 0);
  std::size_t const max_len(E10_INT64 + (is_sign(s[0]) ? 1:0));
  if (len <= max_len) {
    std::size_t pos;
    try {
      int128_t const v_ { std::stoll(s, &pos) };
      if (pos == s.length()) return v_;
      else return std::nullopt;
    } catch (const std::exception&) {
      return std::nullopt;
    }
  }
  std::size_t const hi_len(len - E10_INT64);
  std::optional<int128_t> const hi { i128_of_string(s.substr(0, hi_len)) };
  if (! hi) return std::nullopt;
  std::optional<int128_t> const lo { i128_of_string(s.substr(hi_len, E10_INT64)) };
  if (! lo) return std::nullopt;
  return
    *hi >= 0 ? *hi * P10_INT64 + *lo : *hi * P10_INT64 - *lo;
}

inline std::size_t i128_from_chars(char const *start, char const *stop, int128_t *res)
{
  assert(stop > start);
  std::size_t count = is_sign(*start) ? 1 : 0;
  for (; start + count < stop && is_digit(start[count]); count++) ;
  assert(count > 0);
  std::string const s { start, count };
  std::optional<int128_t> v { i128_of_string(s) };
  *res = v ? *v : 0;
  return count;
}

inline std::size_t u128_from_chars(char const *start, char const *stop, uint128_t *res)
{
  assert(stop > start);
  std::size_t count = is_sign(*start) ? 1 : 0;
  for (count = 0 ; start + count < stop && is_digit(start[count]); count++) ;
  assert(count > 0);
  std::string const s { start, count };
  std::optional<int128_t> v { i128_of_string(s) };
  *res = v ? (uint128_t)*v : 0;
  return count;
}

inline Arr<std::string> string_split(std::string sep, std::string str)
{
  Arr<std::string> res;
  std::size_t const sep_len { sep.length() };
  if (0 == sep_len) return res;  /* TODO: rather a list of single chars? */
  std::size_t last { 0 };
  for (std::size_t p = str.find(sep, last); p != std::string::npos; last = p + sep_len) {
    res.push_back(str.substr(last, p - last));
  }
  res.push_back(str.substr(last));
  return res;
}

inline std::string string_join(std::string sep, Arr<std::string> strs)
{
  std::string res;
  for (std::string const &s : strs) {
    if (res.empty()) res += sep;
    res += s;
  }
  return res;
}

template <typename T>
std::ostream &operator<<(std::ostream &os, std::optional<T> const &o)
{
  if (o.has_value()) os << o.value();
  else os << "??";
  return os;
}

std::ostream &operator<<(std::ostream &os, uint128_t n)
{
  os << string_of_u128(n);
  return os;
}

std::ostream &operator<<(std::ostream &os, int128_t n)
{
  os << string_of_i128(n);
  return os;
}

/* In general tuples a distinct type is created with a distinct output
 * operator. This is required, since types that are distinct for dessser might
 * not be distinct for C++ (for instance, both Size and U32 being mapped into
 * uint32_t), and so we would risk redefining several times the same
 * operator<< function, which is an error.
 * But because many runtime functions (such as Pointer.reader) return a
 * pair, and those types cannot conveniently be given a name both the runtime
 * and the generated code agree on, then it's better to use non distinct
 * types for those, and then also define operator<< for pairs.
 * We could do this for longer tuples while we are at it, if it's beneficial. */

template <typename T1, typename T2>
std::ostream &operator<<(std::ostream &os, std::tuple<T1,T2> const &t)
{
  os << '<' << std::get<0>(t) << ", " << std::get<1>(t) << ">";
  return os;
}

};

#endif
