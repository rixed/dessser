#ifndef VOID_H_211119
#define VOID_H_211119

namespace dessser {

struct Void {};

#define VOID ::dessser::Void()

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
