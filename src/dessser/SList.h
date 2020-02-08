#ifndef SLIST_H_200208
#define SLIST_H_200208
#include <cassert>
#include <memory>
#include <optional>
#include "dessser/typedefs.h"

template<class T>
struct SList {
  std::optional<
    std::pair<
      T,  // head
      std::shared_ptr< SList<T> > // tail
    >
  > cell;

  // The empty list:
  SList() {};

  // Cons:
  SList(T hd, SList<T> tl)
    : cell(hd, std::make_shared<SList<T>>(tl)) {}

  bool is_empty() const { return cell.has_value(); }

  T head() const {
    assert(! is_empty());
    return cell->first;
  }

  std::shared_ptr<SList<T>> tail() const {
    assert(! is_empty());
    return cell->second;
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
