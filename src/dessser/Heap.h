#ifndef HEAP_H_20210409
#define HEAP_H_20210409
#include <functional>
#include <set>
#include <vector>
#include "dessser/Set.h"

namespace dessser {

template<class T>
struct Heap : public Set<T> {
  /* From oldest to youngest: */
  std::multiset<T> h;
  size_t length;

  Heap(std::function<int(T, T)> cmp)
    : h([&cmp](T const &a, T const &b) -> bool {
        int const c { cmp(a, b) };
        if (c < 0) return true;
        return false;
      }),
      length(0)
  {}

  Heap(Heap const &other)
    : h(other.h),
      length(other.length)
  {}

  ~Heap() {}

  void insert(T const &x) override {
    h.insert(x);
    std::pair<typename std::multiset<T>::const_iterator, bool> ret = h.insert(x);
  }

  std::pair<T, std::list<T>> lastUpdate() const override {
    throw "Not implemented: Heap::lastUpdate";
  }

  uint32_t size() const override {
    return length;
  }

  bool member(T const &x) override {
    return h.cend() != h.find(x);
  }

  void iter(std::function<void(T &)> f) override {
    for (T &x : h) {
      f(x);
    }
  }

  T const &getMin() override {
    return &(*h.cbegin());
  }
};

}

#endif
