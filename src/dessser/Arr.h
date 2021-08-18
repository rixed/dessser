#ifndef ARR_H_191223
#define ARR_H_191223
/* Like Vec, but for when the size is unknown statically */
#include <functional>
#include <initializer_list>
#include <iostream>
#include <vector>
#include "dessser/Vec.h"
#include "dessser/Set.h"

namespace dessser {
template<class T>
struct Arr : public std::vector<T> {
  Arr()
  {
    this->reserve(10);
  }

  Arr(std::initializer_list<T> lst)
  {
    this->reserve(10);
    this->insert(this->begin(), lst);
  }

  // Used by AllocVec
  Arr(std::size_t n, T const &x) : std::vector<T>(n, x)
  {
  }

  // Used by chopBegin/chopEnd:
  Arr(typename std::vector<T>::const_iterator first,
      typename std::vector<T>::const_iterator last)
    : std::vector<T>(first, last)
  {
  }

  template<unsigned DIM>
  Arr(Vec<DIM, T> vec) : std::vector<T>(vec)
  {
  }

  Arr(Set<T> *set)
  {
    this->reserve(set->size());
    set->iter([this](T &x) {
      this->push_back(x);
    });
  }

  // Mapped from another Arr:
  template<class TInit, class T2>
  Arr(TInit init, std::function<T(TInit, T2)> f, Arr<T2> that)
  {
    this->reserve(that.size());
    for (T2 const &x2 : that) {
      this->push_back(f(init, x2));
    }
  }

  /* Overwrite size to return an uint32_t as specified in E.type_of */
  uint32_t size() const
  {
    return std::vector<T>::size();
  }

  Arr<T> chopBegin(size_t n)
  {
    if (n >= size()) return Arr<T>();
    return Arr(this->cbegin() + n, this->cend());
  }

  Arr<T> chopEnd(size_t n)
  {
    if (n >= size()) return Arr<T>();
    return Arr(this->cbegin(), this->cend() - n);
  }
};

template<class T>
inline std::ostream &operator<<(std::ostream &os, Arr<T> const &v)
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
