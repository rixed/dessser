#ifndef VEC_H_191031
#define VEC_H_191031
/* A pre-scaled version of std::vector */
#include <functional>
#include <initializer_list>
#include <iostream>
#include <memory>
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

  // Truncation of another vector:
  template<unsigned DIM2>
  Vec(Vec<DIM2, T> other, size_t offset, size_t len)
  {
    this->reserve(DIM);
    for (size_t i = 0; i < len; i++) {
      this->push_back(other[offset + i]);
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

/* Objects composed of pointer to subcomponents must compare equal by value: */

template<unsigned DIM, class T>
inline bool operator==(Vec<DIM, std::shared_ptr<T>> const &a, Vec<DIM, std::shared_ptr<T>> const &b)
{
  for (size_t i = 0; i < DIM; i ++) {
    if (*a[i] != *b[i]) return false;
  }
  return true;
}

template<unsigned DIM, class T>
inline bool operator!=(Vec<DIM, std::shared_ptr<T>> const &a, Vec<DIM, std::shared_ptr<T>> const &b)
{
  return !operator==(a, b);
}

template<unsigned DIM, class T>
inline bool operator==(Vec<DIM, T*> const &a, Vec<DIM, T*> const &b)
{
  for (size_t i = 0; i < DIM; i ++) {
    if (*a[i] != *b[i]) return false;
  }
  return true;
}

template<unsigned DIM, class T>
inline bool operator!=(Vec<DIM, T*> const &a, Vec<DIM, T*> const &b)
{
  return !operator==(a, b);
}

}

#endif
