#ifndef BYTES_H_191025
#define BYTES_H_191025
#include <memory>
#include <string>
#include <cstring>
#include "dessser/typedefs.h"

struct Bytes {
  /* Shared with pointers: */
  std::shared_ptr<Byte[]> buffer;
  /* Size of this byte string: */
  size_t size;
  /* Locating of this byte string within byffer: */
  size_t offset;

  Bytes(std::shared_ptr<Byte[]> buffer_, size_t size_, size_t offset_) :
    buffer(buffer_),
    size(size_),
    offset(offset_)
  {}

  Bytes(double v)
  {
    size = sizeof(double);
    buffer = std::shared_ptr<Byte[]>(new Byte[size]);
    offset = 0;
    memcpy(buffer.get(), &v, size);
  }

  Bytes(std::string s)
  {
    size = s.size();
    buffer = std::shared_ptr<Byte[]>(new Byte[size]);
    offset = 0;
    memcpy(buffer.get(), s.c_str(), size);
  }

  std::string toString() const
  {
    return std::string((char const *)(buffer.get() + offset), size);
  }
};

#endif
