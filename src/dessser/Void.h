#ifndef VOID_H_211119
#define VOID_H_211119
#include <ostream>

namespace dessser {

struct Void {};

inline std::ostream &operator<<(std::ostream &os, Void const &)
{
  return os;
}

inline bool operator==(Void const &, Void const &)
{
  return true;
}

inline bool operator!=(Void const &, Void const &)
{
  return false;
}

}

#endif
