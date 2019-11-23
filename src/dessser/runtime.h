#ifndef RUNTIME_H_191025
#define RUNTIME_H_191025
#include <string>
#include <optional> // for nullable types
#include <iostream> // for dump
#include <utility>  // for pair
#include <limits>   // for std::numeric_limits
#include "dessser/typedefs.h"
#include "dessser/Bytes.h"
#include "dessser/Pointer.h"
#include "dessser/Vec.h"

inline uint64_t qwordOfFloat(double v)
{
  uint64_t res;
  static_assert(sizeof(v) == sizeof(res));
  memcpy(&res, &v, sizeof(v));
  return res;
}

inline double floatOfQword(uint64_t v)
{
  double res;
  static_assert(sizeof(v) == sizeof(res));
  memcpy(&res, &v, sizeof(v));
  return res;
}

#endif
