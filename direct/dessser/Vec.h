#ifndef VEC_H_191031
#define VEC_H_191031
/* A pre-scaled version of std::vector */
#include <vector>

template<unsigned DIM, class T>
class Vec : public std::vector<T> {
  Vec() {
    this->reserve(DIM);
  }
};

#endif
