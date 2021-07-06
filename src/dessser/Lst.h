#ifndef LST_H_200208
#define LST_H_200208
#include <cassert>
#include <functional>
#include <memory>
#include "dessser/typedefs.h"
#include "dessser/Arr.h"
#include "dessser/SimpleSet.h"

#include <sstream>

namespace dessser_gen {

template<class T>
struct Lst {
  struct Cell {
    T val;
    Lst<T> next;
    Cell(T v, Lst<T> n) : val(v), next(n) {}
  };

  std::shared_ptr<Cell> cells;

  // The empty list:
  Lst() {};

  // From another Lst (sharing cells)
  Lst(Lst<T> const &other)
    : cells(other.cells) {}

  // Cons:
  Lst(T hd, Lst<T> tl)
    : cells(std::make_shared<Cell>(hd, tl)) {}

  // Mapped from another Lst:
  template<class TInit, class T2>
  Lst(TInit const init, std::function<T(TInit, T2)> f, Lst<T2> const that)
  {
    // TODO
  }

  bool empty() const { return !cells; }

  T head() const
  {
    assert(! empty());
    return cells->val;
  }

  Lst<T> tail() const
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

  Arr<T> toList() const
  {
    Arr<T> l;
    for (Lst<T> const *sl = this; !sl->empty(); sl = &sl->cells->next) {
      l.push_back(sl->head());
    }
    return l;
  }

  Arr<T> toListRev() const
  {
    Arr<T> l;
    size_t i = length();
    l.resize(i);
    for (Lst<T> const *sl = this; !sl->empty(); sl = &sl->cells->next) {
      l[--i] = sl->head();
    }
    return l;
  }

  /* The only set that can be serializer, and therefore that require a
   * convertion from an Lst, is the SimpleSet: */
  SimpleSet<T> *toSet() const
  {
    SimpleSet<T> *s = new SimpleSet<T>();
    /* In this Lst the deserialized values are stored from youngest to
     * oldest and so must be reversed here: */
    for (Lst<T> const *sl = this; !sl->empty(); sl = &sl->cells->next) {
      s->l.push_front(sl->head());
    }
    return s;
  }
};

template<class T>
static inline std::ostream &operator<<(std::ostream &os, Lst<T> const &l)
{
  if (l.is_empty()) {
    os << "{empty}";
  } else {
    os << '{' << l.head() << ',' << l.tail() << '}';
  }
  return os;
}

};

#endif
