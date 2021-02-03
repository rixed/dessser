#ifndef LIST_H_191223
#define LIST_H_191223
/* Like Vec, but for when the size is unknown statically */
#include <functional>
#include <initializer_list>
#include <vector>

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

