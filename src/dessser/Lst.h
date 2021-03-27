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
  template<class T2>
  Lst(Lst<T2> const that, std::function<T(T2)> f)
  {
    // TODO
  }

  /* Overwrite size to return an uint32_t as specified in E.type_of */
  uint32_t size()
  {
    return std::vector<T>::size();
  }
};

#endif

