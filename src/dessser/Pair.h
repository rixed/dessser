#ifndef PAIR_H_200209
#define PAIR_H_200209

/* No std::pair because we need a constructor with non-const initializers */

template<class T1, class T2>
struct Pair {
  T1 v1;
  T2 v2;

  Pair (T1 i1, T2 i2) : v1(i1), v2(i2) {}

  Pair (Pair<T1, T2> const &other)
    : v1(other.v1), v2(other.v2) {}

  Pair () = default;
};

/* Pretty printers for everything so that Dump can rely on the '<<' operator: */

#include <iostream>

template <typename T1, typename T2>
std::ostream &operator<<(std::ostream &os, Pair<T1,T2> const &p)
{
  os << '<' << p.v1 << ", " << p.v2 << ">";
  return os;
}

#endif
