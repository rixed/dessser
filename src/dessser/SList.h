#ifndef SLST_H_200208
#define SLST_H_200208
#include <cassert>
#include <functional>
#include <memory>
#include "dessser/typedefs.h"
#include "dessser/Lst.h"
#include "dessser/SimpleSet.h"

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

  // Mapped from another SList:
  template<class T2>
  SList(SList<T2> const that, std::function<T(T2)> f)
  {
    // TODO
  }

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

  // FIXME: need to be faster so store the length in the cell:
  size_t length() const
  {
    if (empty()) return 0;
    else return 1 + cells->next.length();
  }

  List<T> toList() const
  {
    List<T> l;
    for (SList<T> const *sl = this; !sl->empty(); sl = &sl->cells->next) {
      l.push_back(sl->head());
    }
    return l;
  }

  List<T> toListRev() const
  {
    List<T> l;
    size_t i = length();
    l.resize(i);
    for (SList<T> const *sl = this; !sl->empty(); sl = &sl->cells->next) {
      l[--i] = sl->head();
    }
    return l;
  }

  /* The only set that can be serializer, and therefore that require a
   * convertion from an SList, is the SimpleSet: */
  SimpleSet<T> *toSet() const
  {
    SimpleSet<T> *s = new SimpleSet<T>();
    /* In this SList the deserialized values are stored from youngest to
     * oldest and so must be reversed here: */
    for (SList<T> const *sl = this; !sl->empty(); sl = &sl->cells->next) {
      s->l.push_front(sl->head());
    }
    return s;
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
