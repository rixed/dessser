#ifndef SAMPLING_H_201202
#define SAMPLING_H_201202
#include "dessser/Set.h"

template<class T>
struct Sampling : public Set<T> {
  /* From oldest to youngest: */
  std::list<T> l;

  Sampling() {}
  Sampling(Sampling const &other) : l(other.l) {}
  ~Sampling() {}

  void insert(T const &x) override {
    l.push_back(x);
  }

  std::pair<T, std::list<T>> lastUpdate() const override {
    return std::pair<T, std::list<T>>(
      l.back(), std::list<T>());
  }

  uint32_t size() const override {
    return l.size();
  }

  void iter(std::function<void(T &)> f) override {
    for (T &x : l) {
      f(x);
    }
  }
};

#endif
