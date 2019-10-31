#ifndef VEC_H_191031
#define VEC_H_191031

/* A pre-scaled version of std::vector */
template<class T, unsigned DIM>
class Vec {
  std::vector<T> v;

  Vec() {
    v.reserve(DIM);
  }
};

#endif
