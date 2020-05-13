#ifndef MASK_H_200513
#define MASK_H_200513
#include <cassert>
#include <vector>

struct MaskAction {
  enum Op { COPY, SKIP, SET_NULL, RECURSE } op;
  std::vector<MaskAction> recurse;

  MaskAction(Op o) : op(o) {}
  MaskAction(std::vector<MaskAction> const &r) : op(RECURSE), recurse(r) {}
};

inline bool operator==(const MaskAction& lhs, const MaskAction& rhs)
{
  if (lhs.op != rhs.op) return false;
  if (lhs.op == MaskAction::RECURSE) {
    return lhs.recurse == rhs.recurse;
  } else {
    return true;
  }
}

inline bool operator!=(const MaskAction& lhs, const MaskAction& rhs)
{
  return !(lhs == rhs);
}


struct Mask {
  // One or the other:
  size_t copy_all;
  std::vector<MaskAction> const *actions;

  Mask(size_t l) : copy_all(l), actions(nullptr) {}
  Mask(std::vector<MaskAction> const *a) : copy_all(0), actions(a) {}

  MaskAction const get(size_t i) const
  {
    if (actions) {
      assert(i < actions->size());
      return actions[i];
    } else {
      assert(i < copy_all);
      return MaskAction(MaskAction::COPY);
    }
  }

  /* Should be member of MaskAction but C++ does not support recursive type decls */
  static Mask const enter_action(MaskAction a, size_t l)
  {
    switch (a.op) {
      case MaskAction::COPY:
        return Mask(l);
      case MaskAction::SKIP:
        // pass
      case MaskAction::SET_NULL:
        assert(false);
        break;
      case MaskAction::RECURSE:
        return Mask(&a.recurse);
    }
    assert(false);
  }
};

#endif
