#ifndef LIST_H_191223
#define LIST_H_191223
/* Like Vec, but for when the size is unknown statically */
#include <functional>
#include <initializer_list>
#include <vector>
#include "dessser/Vec.h"
#include "dessser/Set.h"

template<class T>
struct Lst : public std::vector<T> {
  Lst()
  {
    this->reserve(10);
  }

  Lst(std::initializer_list<T> lst)
  {
    this->reserve(10);
    this->insert(this->begin(), lst);
  }

  template<unsigned DIM>
  Lst(Vec<DIM, T> vec) : std::vector<T>(vec)
  {
  }

  Lst(Set<T> *set)
  {
    this->reserve(set->size());
    set->iter([this](T &x) {
      this->push_back(x);
    });
  }

  // Mapped from another Lst:
  template<class TInit, class T2>
  Lst(TInit const init, std::function<T(TInit, T2)> f, Lst<T2> const that)
  {
    // TODO
  }

  /* Overwrite size to return an uint32_t as specified in E.type_of */
  uint32_t size()
  {
    return std::vector<T>::size();
  }

  Lst<T> chopBegin(size_t n)
  {
    if (n >= size()) return Lst<T>();
    return Lst(this->cbegin() + n, this->cend());
  }

  Lst<T> chopEnd(size_t n)
  {
    if (n >= size()) return Lst<T>();
    return Lst(this->cbegin(), this->cend() - n);
  }
};

#endif

