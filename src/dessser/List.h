#ifndef LIST_H_191223
#define LIST_H_191223
/* Like Vec, but for when the size is unknown statically */
#include <vector>

template<class T>
struct List : public std::vector<T> {
  List() {
    this->reserve(10);
  }
};

#endif

