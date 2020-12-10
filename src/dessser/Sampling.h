#ifndef SIMPLESET_H_201202
#define SIMPLESET_H_201202
#include "dessser/Set.h"

template<class T>
struct Sampling : public Set<T> {
  std::list<T> l;

  Sampling() {}
  ~Sampling() {}

  void insert(T const &x) override {
    l.push_back(x);
  }

  std::pair<T, std::list<T>> lastUpdate() const override {
    return std::pair<T, std::list<T>>(
      l.back(), std::list<T>());
  }

  size_t size() const override {
    return l.size();
  }
};

#endif
