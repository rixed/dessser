#ifndef SIMPLESET_H_201202
#define SIMPLESET_H_201202
#include "dessser/Set.h"

namespace dessser {

template<class T>
struct SimpleSet : public Set<T> {
  /* From oldest to youngest: */
  std::list<T> l;

  SimpleSet() {}
  SimpleSet(SimpleSet const &other) : l(other.l) {}
  ~SimpleSet() {}

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

  bool member(T const &x) override {
    return l.cend() != std::find(l.cbegin(), l.cend(), x);
  }

  void iter(std::function<void(T &)> f) override {
    for (T &x : l) {
      f(x);
    }
  }
};

template<class T>
inline bool operator==(SimpleSet<T> lhs, SimpleSet<T> rhs)
{
  if (lhs.size() != rhs.size()) return false;

  auto i1 { lhs.cbegin() };
  auto i2 { rhs.cbegin() };
  while (i1 != lhs.cend()) {
    if (*i1++ != *i2++) return false;
  }

  return true;
}

template<class T>
inline bool operator!=(SimpleSet<T> lhs, SimpleSet<T> rhs)
{
  return !operator==(lhs, rhs);
}

}

#endif
