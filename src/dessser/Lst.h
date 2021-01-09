#ifndef LIST_H_191223
#define LIST_H_191223
/* Like Vec, but for when the size is unknown statically */
#include <initializer_list>
#include <vector>

template<class T>
struct List : public std::vector<T> {
  List()
  {
    this->reserve(10);
  }

  List(std::initializer_list<T> lst)
  {
    this->reserve(10);
    this->insert(this->begin(), lst);
  }

  /* Overwrite size to return an uint32_t as specified in E.type_of */
  uint32_t size()
  {
    return std::vector<T>::size();
  }
};

#endif

