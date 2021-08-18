#ifndef VEC_H_191031
#define VEC_H_191031
/* A pre-scaled version of std::vector */
#include <functional>
#include <initializer_list>
#include <iostream>
#include <vector>

namespace dessser {

template<unsigned DIM, class T>
struct Vec : public std::vector<T> {
  Vec()
  {
    this->reserve(DIM);
  }

  Vec(std::initializer_list<T> lst)
  {
    this->reserve(DIM);
    this->insert(this->begin(), lst);
  }

  // Mapped from another Vec:
  template<class TInit, class T2>
  Vec(TInit init, std::function<T(TInit, T2)> f, Vec<DIM, T2> that)
  {
    this->reserve(DIM);
    for (T2 const &x2 : that) {
      this->push_back(f(init, x2));
    }
  }
};

template<unsigned DIM, class T>
inline std::ostream &operator<<(std::ostream &os, Vec<DIM, T> const &v)
{
  os << '[';
  bool sep = false;
  for (T const &i : v) {
    if (sep) os << ", ";
    sep = true;
    os << i;
  }
  os << ']';
  return os;
}

};

#endif
