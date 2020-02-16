#ifndef SLIST_H_200208
#define SLIST_H_200208
#include <cassert>
#include <memory>
#include "dessser/typedefs.h"
#include "dessser/List.h"

template<class T>
struct SList {
  struct Cell {
    T val;
    SList<T> next;
    Cell(T v, SList<T> n) : val(v), next(n) {}
  };

  std::shared_ptr<Cell> cells;

  // The empty list:
  SList() {};

  // From another SList (sharing cells)
  SList(SList<T> const &other)
    : cells(other.cells) {}

  // Cons:
  SList(T hd, SList<T> tl)
    : cells(std::make_shared<Cell>(hd, tl)) {}

  bool empty() const { return !cells; }

  T head() const
  {
    assert(! empty());
    return cells->val;
  }

  SList<T> tail() const
  {
    assert(! empty());
    return cells->next;
  }

  // For debugging:
  size_t length() const
  {
    if (empty()) return 0;
    else return 1 + cells->next.length();
  }

  List<T> toList() const
  {
    List<T> l;
    for (SList<T> const &sl = this; !sl.empty(); sl.cells->next) {
      l.push_back(sl.head());
    }
  }
};

#include <sstream>

template<class T>
static inline std::ostream &operator<<(std::ostream &os, SList<T> const &l)
{
  if (l.is_empty()) {
    os << "{empty}";
  } else {
    os << '{' << l.head() << ',' << l.tail() << '}';
  }
  return os;
}

#endif
