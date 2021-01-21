#ifndef VEC_H_191031
#define VEC_H_191031
/* A pre-scaled version of std::vector */
#include <functional>
#include <vector>
#include <initializer_list>

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
  template<class T2>
  Vec(Vec<DIM, T2> const that, std::function<T(T2)> f)
  {
    // TODO
  }
};

#endif
