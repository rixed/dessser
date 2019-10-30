#ifndef POINTER_H_191025
#define POINTER_H_191025
#include <cassert>
#include <memory>
#include <cstring>
#include "Bytes.h"
#include "typedefs.h"

/* We have 2 types of pointers:
 * Pointers that point to a byte buffer used to (de)serialize values, and
 * pointers that point to a heap allocated value that's being build with
 * set_field or peeked at with get_field.
 * In a few cases we can use both interchangeably, although they share
 * nothing in common, including copying them by value (which makes
 * inheritance unpractical). */

/* The type of pointer used to serialize/deserialize with write/read functions: */
struct Pointer {
  // Shared with all pointers derived from this one:
  std::shared_ptr<Byte[]> buffer;
  // Total size of the buffer
  size_t size;
  // Current location of the read/write pointer inside the buffer:
  size_t offset;
  /* The type of pointer used to hold a heap allocated value that's being
   * build with BackEndCPP.set_field or deconstructed with get_field.
   * If this is set then buffer must by null, and the other way around.
   * On shared_ptr<void>, it works because BackEndCPP.alloc_value alloc
   * with new, and we build the shared_ptr when the actual type is known. */
  std::shared_ptr<void> value;

  /* Construct (with uninitialized buffer) from a size: */
  Pointer(Size const &sz) :
    buffer(new Byte[sz]),
    size(sz),
    offset(0)
  {}

  /* Construct from a string: */
  Pointer(std::string const &str) :
    buffer(new Byte[str.size()]),
    size(str.size()),
    offset(0)
  {
    memcpy(buffer.get(), str.c_str(), size);
  }

  /* Construct from another Pointer, sharing the buffer */
  Pointer(Pointer const &that) :
    buffer(that.buffer),
    size(that.size),
    offset(that.offset),
    value(that.value)
  {}

  /* Construct from any heap allocated value */
  Pointer(std::shared_ptr<void> value_) :
    size(0),
    offset(0),
    value(value_)
  {}

  Size rem() const
  {
    return (size - offset);
  }

  Pointer skip(Size const &sz) const
  {
    Pointer ptr(*this);
    ptr.offset += sz;
    return ptr;
  }

  bool getBit(size_t b) const
  {
    size_t const off = offset + b / 8;
    size_t const bit = b & 7;
    assert( off < size);
    return !!(buffer[off] & (1 << bit));
  }

  Byte peekByte(size_t at = 0) const
  {
    assert(offset + at < size);
    return buffer[offset + at];
  }

  std::pair<Byte, Pointer> readByte() const
  {
    return std::make_pair<Byte, Pointer>(
      peekByte(), skip(1));
  }

  Word peekWordLe() const
  {
    assert(offset + 2 <= size);
    return (((Word)buffer[offset + 1]) << 8) |
           buffer[offset];
  }

  std::pair<Word, Pointer> readWordLe() const
  {
    return std::make_pair<Word, Pointer>(
      peekWordLe(), skip(2));
  }

  Word peekWordBe() const
  {
    assert(offset + 2 <= size);
    return (((Word)buffer[offset]) << 8) |
           buffer[offset + 1];
  }

  std::pair<Word, Pointer> readWordBe() const
  {
    return std::make_pair<Word, Pointer>(
      peekWordBe(), skip(2));
  }

  DWord peekDWordLe() const
  {
    assert(offset + 4 <= size);
    return (((DWord)buffer[offset + 3]) << 24) |
           (((DWord)buffer[offset + 2]) << 16) |
           (((DWord)buffer[offset + 1]) << 8) |
           buffer[offset];
  }

  std::pair<DWord, Pointer> readDWordLe() const
  {
    return std::make_pair<DWord, Pointer>(
      peekDWordLe(), skip(4));
  }

  QWord peekQWordLe() const
  {
    assert(offset + 8 <= size);
    return (((QWord)buffer[offset + 7]) << 56) |
           (((QWord)buffer[offset + 6]) << 48) |
           (((QWord)buffer[offset + 5]) << 40) |
           (((QWord)buffer[offset + 4]) << 32) |
           (((QWord)buffer[offset + 3]) << 24) |
           (((QWord)buffer[offset + 2]) << 16) |
           (((QWord)buffer[offset + 1]) << 8) |
           buffer[offset];
  }

  std::pair<QWord, Pointer> readQWordLe() const
  {
    return std::make_pair<QWord, Pointer>(
      peekQWordLe(), skip(8));
  }

  OWord peekOWordLe() const
  {
    assert(offset + 16 <= size);
    return (((OWord)buffer[offset + 15]) << 120) |
           (((OWord)buffer[offset + 14]) << 112) |
           (((OWord)buffer[offset + 13]) << 104) |
           (((OWord)buffer[offset + 12]) << 96) |
           (((OWord)buffer[offset + 11]) << 88) |
           (((OWord)buffer[offset + 10]) << 80) |
           (((OWord)buffer[offset + 9]) << 72) |
           (((OWord)buffer[offset + 8]) << 64) |
           (((OWord)buffer[offset + 7]) << 56) |
           (((OWord)buffer[offset + 6]) << 48) |
           (((OWord)buffer[offset + 5]) << 40) |
           (((OWord)buffer[offset + 4]) << 32) |
           (((OWord)buffer[offset + 3]) << 24) |
           (((OWord)buffer[offset + 2]) << 16) |
           (((OWord)buffer[offset + 1]) << 8) |
           buffer[offset];
  }

  std::pair<OWord, Pointer> readOWordLe() const
  {
    assert(offset + 16 <= size);
    return std::make_pair<OWord, Pointer>(
      peekOWordLe(), skip(16));
  }

  std::pair<Bytes, Pointer> readBytes(Size const &sz) const
  {
    assert(offset + sz <= size);
    return std::make_pair<Bytes, Pointer>(
      Bytes(buffer, sz, offset),
      skip(sz));
  }

  void setBit(size_t b, bool v)
  {
    size_t const off = offset + b / 8;
    size_t const bit = b & 7;
    assert(off < size);
    Byte const mask = 1 << bit;
    buffer[off] =
      v ? buffer[off] | mask :
          buffer[off] & ~mask;
  }

  void pokeByte(Byte v)
  {
    assert(offset < size);
    buffer[offset] = v;
  }

  Pointer writeByte(Byte v)
  {
    pokeByte(v);
    return (skip(1));
  }

  Pointer writeWordLe(Word v)
  {
    assert(offset + 2 <= size);
    buffer[offset] = v;
    buffer[offset+1] = v >> 8;
    return (skip(2));
  }

  Pointer writeDWordLe(DWord v)
  {
    assert(offset + 4 <= size);
    buffer[offset] = v;
    buffer[offset+1] = v >> 8;
    buffer[offset+2] = v >> 16;
    buffer[offset+3] = v >> 24;
    return (skip(4));
  }

  Pointer writeQWordLe(QWord v)
  {
    assert(offset + 8 <= size);
    buffer[offset] = v;
    buffer[offset+1] = v >> 8;
    buffer[offset+2] = v >> 16;
    buffer[offset+3] = v >> 24;
    buffer[offset+4] = v >> 32;
    buffer[offset+5] = v >> 40;
    buffer[offset+6] = v >> 48;
    buffer[offset+7] = v >> 56;
    return (skip(8));
  }

  Pointer writeOWordLe(OWord v)
  {
    assert(offset + 16 <= size);
    buffer[offset] = v;
    buffer[offset+1] = v >> 8;
    buffer[offset+2] = v >> 16;
    buffer[offset+3] = v >> 24;
    buffer[offset+4] = v >> 32;
    buffer[offset+5] = v >> 40;
    buffer[offset+6] = v >> 48;
    buffer[offset+7] = v >> 56;
    buffer[offset+8] = v >> 64;
    buffer[offset+9] = v >> 72;
    buffer[offset+10] = v >> 80;
    buffer[offset+11] = v >> 88;
    buffer[offset+12] = v >> 96;
    buffer[offset+13] = v >> 104;
    buffer[offset+14] = v >> 112;
    buffer[offset+15] = v >> 120;
    return (skip(16));
  }

  Pointer writeBytes(Bytes const &v)
  {
    assert(offset + v.size <= size);
    memcpy(buffer.get() + offset, v.buffer.get() + v.offset, v.size);
    return skip(v.size);
  }

  Pointer blitBytes(Byte const b, Size const &sz)
  {
    assert(offset + sz <= size);
    memset(buffer.get() + offset, b, sz);
    return skip(sz);
  }

  Size operator-(Pointer const &that)
  {
    assert(buffer == that.buffer);
    assert(offset >= that.offset);
    return Size(offset - that.offset);
  }
};

#endif
