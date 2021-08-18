#ifndef LST_H_200208
#define LST_H_200208
#include <cassert>
#include <functional>
#include <iterator>
#include <memory>
#include "dessser/typedefs.h"
#include "dessser/Arr.h"
#include "dessser/SimpleSet.h"

#include <sstream>

namespace dessser {

template<class T>
struct Lst {
  struct Cell {
    T val;
    Lst<T> next;
    Cell(T v, Lst<T> n = Lst<T>()) : val(v), next(n) {}
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
  Lst(TInit init, std::function<T(TInit, T2)> f, Lst<T2> that)
  {
    // last will always point to the final null_ptr cells:
    std::shared_ptr<Cell> *last { &cells };

    for (T2 const &x2 : that) {
      T x { f(init, x2) };
      // Append at last:
      *last = std::make_shared<Cell>(x);
      last = &(*last)->next.cells;
    }
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

  uint32_t size() const { return (uint32_t)length(); }  // For Cardinality

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

  /* Explicitly implicit: */
  Lst<T> &operator=(Lst<T> const &) = default;
  Lst<T> &operator=(Lst<T> &&) = default;

  // Range based loops:

  struct Iterator {
    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::size_t;
    using value_type = T;
    using pointer = T*;
    using reference = T&;

    std::shared_ptr<Cell> cells;

    Iterator(std::shared_ptr<Cell> cells_) : cells(cells_) {}

    reference operator*() const {
      return cells->val;
    }

    pointer operator->() {
      return &cells->val;
    }

    Iterator &operator++() {
      cells = cells->next.cells;
      return *this;
    }

    Iterator operator++(int) {
      Iterator tmp { cells };
      ++(*this);
      return tmp;
    }

    inline bool operator==(Iterator const &o)
    {
      return cells.get() == o.cells.get();
    }

    inline bool operator!=(Iterator const &o)
    {
      return !operator==(o);
    }
  };

  Iterator begin() { return Iterator(cells); }
  Iterator end() { return Iterator(std::shared_ptr<Cell>()); }

  T const &operator[](std::size_t i) const {
    Lst<T> const *that { this };
    while (i-- != 0) {
      that = &that->cells->next;
    }
    return that->cells->val;
  }
};

template<class T>
static inline std::ostream &operator<<(std::ostream &os, Lst<T> const &l)
{
  if (l.empty()) {
    os << "{empty}";
  } else {
    os << '{' << l.head() << ',' << l.tail() << '}';
  }
  return os;
}

template<class T>
inline bool operator==(Lst<T> const &lhs, Lst<T> const &rhs)
{
  //if (lhs.length() != rhs.length()) return false;  Once length is made O(1)
  if (lhs.cells.get() == rhs.cells.get()) return true;
  if (!lhs.cells || !rhs.cells) return false;
  return
    lhs.cells->val == rhs.cells->val &&
    lhs.cells->next == rhs.cells->next;
}

template<class T>
inline bool operator!=(Lst<T> const &lhs, Lst<T> const &rhs)
{
  return !(lhs == rhs);
}

};

#endif
