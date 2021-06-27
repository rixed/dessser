#ifndef VEC_H_191031
#define VEC_H_191031
/* A pre-scaled version of std::vector */
#include <functional>
#include <initializer_list>
#include <iostream>
#include <vector>

namespace dessser_gen {

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
  Vec(TInit const init, std::function<T(TInit, T2)> f, Vec<DIM, T2> const that)
  {
    // TODO
  }
};

template<unsigned DIM, class T>
std::ostream &operator<<(std::ostream &os, Vec<DIM, T> const &v)
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
