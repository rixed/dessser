#ifndef MASK_H_200513
#define MASK_H_200513
#include <cassert>
#include <vector>

namespace dessser {

struct Mask {
  enum Op { COPY, SKIP, SET_NULL, RECURSE } op;
  std::vector<Mask> recurse; /* Only set when op is RECURSE */

  Mask(Op o) : op(o) {}
  Mask(std::vector<Mask> const &r) : op(RECURSE), recurse(r) {}

  Mask const get(size_t i) const
  {
    if (op == RECURSE) {
      return recurse[i];
    } else {
      return *this;
    }
  }
};

inline bool operator==(Mask const &lhs, Mask const &rhs)
{
  if (lhs.op != rhs.op) return false;
  if (lhs.op == Mask::RECURSE) {
    return lhs.recurse == rhs.recurse;
  } else {
    return true;
  }
}

inline bool operator!=(Mask const &lhs, Mask const &rhs)
{
  return !(lhs == rhs);
}

};

#endif
