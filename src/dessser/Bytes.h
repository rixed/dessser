#ifndef BYTES_H_191025
#define BYTES_H_191025
#include <cstring>
#include <memory>
#include <string>
#include "dessser/typedefs.h"

namespace dessser {

struct Bytes {
  /* Shared with pointers: */
  //std::shared_ptr<uint8_t[]> buffer;
  // Work around MacOS c17 issue:
  std::shared_ptr<uint8_t> buffer;
  /* total capacity of the whole buffer: */
  size_t capa;
  /* Size of this byte string: */
  size_t size;
  /* Location of this byte string within buffer: */
  size_t offset;

  // Empty constructor for let expressions:
  Bytes() :
    capa(0),
    size(0),
    offset(0)
  {}

  Bytes(std::shared_ptr<uint8_t> buffer_, size_t size_, size_t offset_) :
    buffer(buffer_),
    capa(size_),
    size(size_),
    offset(offset_)
  {}

  Bytes(std::string const s)
  {
    capa = size = s.size();
    buffer = std::shared_ptr<uint8_t>(new uint8_t[size]);
    offset = 0;
    memcpy(buffer.get(), s.c_str(), size);
  }

  Bytes(uint8_t *bytes, size_t size_) :
    buffer(new uint8_t[size_]),
    capa(size_),
    size(size_),
    offset(0)
  {
    memcpy(buffer.get(), bytes, size_);
  }

  // To be used by below constructors:
  void copyFrom(Bytes const &b)
  {
    capa = b.capa;
    buffer = b.buffer;
    offset = b.offset;
    size = b.size;
  }

  // Concatenate two other Bytes:
  Bytes(Bytes const &b1, Bytes const &b2)
  {
    if (b1.buffer == b2.buffer &&
        b1.offset + b1.size == b2.offset) { /* fast paths first */
      capa = b1.capa;
      buffer = b1.buffer;
      offset = b1.offset;
      size = b1.size + b2.size;
    } else if (b1.size == 0) {
      copyFrom(b2);
    } else if (b2.size == 0) {
      copyFrom(b1);
    } else { /* slow path */
      capa = size = b1.size + b2.size;
      buffer = std::shared_ptr<uint8_t>(new uint8_t[size]);
      offset = 0;
      memcpy(buffer.get(), b1.buffer.get()+b1.offset, b1.size);
      memcpy(buffer.get()+b1.size, b2.buffer.get()+b2.offset, b2.size);
    }
  }

  Bytes(Bytes const &b1, uint8_t b)
  {
    if (b1.offset + b1.size < b1.capa &&
        b1.buffer.get()[b1.offset + b1.size] == b) { /* fast path */
      capa = b1.capa;
      buffer = b1.buffer;
      offset = b1.offset;
      size = b1.size + 1;
    } else { /* slow path */
      capa = size = b1.size + 1;
      buffer = std::shared_ptr<uint8_t>(new uint8_t[size]);
      offset = 0;
      memcpy(buffer.get(), b1.buffer.get()+b1.offset, b1.size);
      buffer.get()[b1.size] = b;
    }
  }

  std::string toString() const
  {
    return std::string((char const *)(buffer.get() + offset), size);
  }

  size_t length() const
  {
    return size;
  }

  uint8_t operator[](std::size_t i) const
  {
    return buffer.get()[offset + i];
  }
};

static inline std::ostream &operator<<(std::ostream &os, Bytes const &b)
{
  os << b.toString();
  return os;
}

inline bool operator==(Bytes const &lhs, Bytes const &rhs)
{
  if (lhs.size != rhs.size) return false;
  if (lhs.buffer == rhs.buffer && lhs.offset == rhs.offset) return true;
  // Slow path
  return 0 == std::memcmp(lhs.buffer.get() + lhs.offset,
                          rhs.buffer.get() + rhs.offset, lhs.size);
}

inline bool operator!=(Bytes const &lhs, Bytes const &rhs)
{
  return !(lhs == rhs);
}

};

#endif
