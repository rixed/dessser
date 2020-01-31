#ifndef BYTES_H_191025
#define BYTES_H_191025
#include <memory>
#include <string>
#include <cstring>
#include "dessser/typedefs.h"

struct Bytes {
  /* Shared with pointers: */
  std::shared_ptr<Byte[]> buffer;
  /* total capacity of the whole buffer: */
  size_t capa;
  /* Size of this byte string: */
  size_t size;
  /* Location of this byte string within buffer: */
  size_t offset;

  Bytes(std::shared_ptr<Byte[]> buffer_, size_t size_, size_t offset_) :
    buffer(buffer_),
    capa(size_),
    size(size_),
    offset(offset_)
  {}

  Bytes(std::string s)
  {
    capa = size = s.size();
    buffer = std::shared_ptr<Byte[]>(new Byte[size]);
    offset = 0;
    memcpy(buffer.get(), s.c_str(), size);
  }

  // To be used by below constructors:
  void CopyFrom(Bytes const &b)
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
      CopyFrom(b2);
    } else if (b2.size == 0) {
      CopyFrom(b1);
    } else { /* slow path */
      capa = size = b1.size + b2.size;
      buffer = std::shared_ptr<Byte[]>(new Byte[size]);
      offset = 0;
      memcpy(buffer.get(), &b1.buffer[b1.offset], b1.size);
      memcpy(&buffer[b1.size], b2.buffer.get(), b2.size);
    }
  }

  Bytes(Bytes const &b1, Byte b)
  {
    if (b1.offset + b1.size < b1.capa &&
        b1.buffer[b1.offset + b1.size] == b) { /* fast path */
      capa = b1.capa;
      buffer = b1.buffer;
      offset = b1.offset;
      size = b1.size + 1;
    } else { /* slow path */
      capa = size = b1.size + 1;
      buffer = std::shared_ptr<Byte[]>(new Byte[size]);
      offset = 0;
      memcpy(buffer.get(), &b1.buffer[b1.offset], b1.size);
      buffer[b1.size] = b;
    }
  }

  std::string toString() const
  {
    return std::string((char const *)(buffer.get() + offset), size);
  }
};

#endif
