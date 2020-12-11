#ifndef SIMPLESET_H_201202
#define SIMPLESET_H_201202
#include "dessser/Set.h"

template<class T>
struct TumblingWindow : public Set<T> {
  /* From oldest to youngest: */
  std::list<T> l;

  TumblingWindow() {}
  ~TumblingWindow() {}

  void insert(T const &x) override {
    l.push_back(x);
  }

  std::pair<T const &, std::list<T const &>> lastUpdate() const override {
    return std::pair<T const &, std::list<T const &>>(
      l.back(), std::list<T const &>());
  }
};

#endif
