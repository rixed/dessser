#ifndef HASHTABLE_H_20210327
#define HASHTABLE_H_20210327
#include <unordered_set>
#include "dessser/Set.h"

namespace dessser_gen {

template<class T>
struct HashTable : public Set<T> {
  /* From oldest to youngest: */
  std::unordered_set<T> h;
  typename std::unordered_set<T>::const_iterator last_added;

  HashTable() {}
  HashTable(HashTable const &other) : h(other.h) {}
  ~HashTable() {}

  void insert(T const &x) override {
    std::pair<typename std::unordered_set<T>::const_iterator, bool> ret = h.insert(x);
    last_added = ret.first;
  }

  std::pair<T, std::list<T>> lastUpdate() const override {
    return std::pair<T, std::list<T>>(
      *last_added, std::list<T>());
  }

  uint32_t size() const override {
    return h.size();
  }

  bool member(T const &x) override {
    return h.cend() != h.find(x);
  }

  void iter(std::function<void(T &)> f) override {
    for (T &x : h) {
      f(x);
    }
  }
};

};

#endif
