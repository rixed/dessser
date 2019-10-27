#ifndef POINTER_H_191025
#define POINTER_H_191025
#include <cassert>
#include <memory>
#include <cstring>
#include "Bytes.h"
#include "typedefs.h"

class Pointer {
  // Shared with all pointers derived from this one:
  std::shared_ptr<Byte> buffer;
  // Total size of the buffer
  size_t size;
  // Current location of the read/write pointer inside the buffer:
  size_t offset;

public:
  /* Construct (with uninitialized buffer) from a size: */
  Pointer(Size const &sz) :
    buffer(new Byte[sz]),
    size(sz),
    offset(0)
  {}

  /* construct from a string: */
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
    offset(that.offset)
  {}

  Pointer skip(Size const &sz) const
  {
    Pointer ptr(*this);
    ptr.offset += sz;
    return ptr;
  }

  bool getBit(size_t b) const
  {
    assert(offset < size);
    return !!(buffer.get()[offset] & (1 << (b & 3)));
  }

  Byte peekByte(size_t at = 0) const
  {
    assert(offset + at < size);
    return buffer.get()[offset + at];
  }

  std::pair<Byte, Pointer> readByte() const
  {
    return std::make_pair<Byte, Pointer>(
      peekByte(), skip(1));
  }

  Word peekWordLe() const
  {
    assert(offset + 2 <= size);
    return (((Word)buffer.get()[offset + 1]) << 8) |
           buffer.get()[offset];
  }

  std::pair<Word, Pointer> readWordLe() const
  {
    return std::make_pair<Word, Pointer>(
      peekWordLe(), skip(2));
  }

  Word peekWordBe() const
  {
    assert(offset + 2 <= size);
    return (((Word)buffer.get()[offset]) << 8) |
           buffer.get()[offset + 1];
  }

  std::pair<Word, Pointer> readWordBe() const
  {
    return std::make_pair<Word, Pointer>(
      peekWordBe(), skip(2));
  }

  DWord peekDWordLe() const
  {
    assert(offset + 4 <= size);
    return (((DWord)buffer.get()[offset + 3]) << 24) |
           (((DWord)buffer.get()[offset + 2]) << 16) |
           (((DWord)buffer.get()[offset + 1]) << 8) |
           buffer.get()[offset];
  }

  std::pair<DWord, Pointer> readDWordLe() const
  {
    return std::make_pair<DWord, Pointer>(
      peekDWordLe(), skip(4));
  }

  QWord peekQWordLe() const
  {
    assert(offset + 8 <= size);
    return (((QWord)buffer.get()[offset + 7]) << 56) |
           (((QWord)buffer.get()[offset + 6]) << 48) |
           (((QWord)buffer.get()[offset + 5]) << 40) |
           (((QWord)buffer.get()[offset + 4]) << 32) |
           (((QWord)buffer.get()[offset + 3]) << 24) |
           (((QWord)buffer.get()[offset + 2]) << 16) |
           (((QWord)buffer.get()[offset + 1]) << 8) |
           buffer.get()[offset];
  }

  std::pair<QWord, Pointer> readQWordLe() const
  {
    return std::make_pair<QWord, Pointer>(
      peekQWordLe(), skip(8));
  }

  OWord peekOWordLe() const
  {
    assert(offset + 16 <= size);
    return (((OWord)buffer.get()[offset + 15]) << 120) |
           (((OWord)buffer.get()[offset + 14]) << 112) |
           (((OWord)buffer.get()[offset + 13]) << 104) |
           (((OWord)buffer.get()[offset + 12]) << 96) |
           (((OWord)buffer.get()[offset + 11]) << 88) |
           (((OWord)buffer.get()[offset + 10]) << 80) |
           (((OWord)buffer.get()[offset + 9]) << 72) |
           (((OWord)buffer.get()[offset + 8]) << 64) |
           (((OWord)buffer.get()[offset + 7]) << 56) |
           (((OWord)buffer.get()[offset + 6]) << 48) |
           (((OWord)buffer.get()[offset + 5]) << 40) |
           (((OWord)buffer.get()[offset + 4]) << 32) |
           (((OWord)buffer.get()[offset + 3]) << 24) |
           (((OWord)buffer.get()[offset + 2]) << 16) |
           (((OWord)buffer.get()[offset + 1]) << 8) |
           buffer.get()[offset];
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
      Bytes(buffer, offset, sz),
      skip(sz));
  }

  void setBit(size_t b, bool v)
  {
    assert(offset < size);
    Byte const mask = 1 << b;
    buffer.get()[offset] =
      v ? buffer.get()[offset] | mask : buffer.get()[offset] & ~mask;
  }

  void pokeByte(Byte v)
  {
    assert(offset < size);
    buffer.get()[offset] = v;
  }

  Pointer writeByte(Byte v)
  {
    pokeByte(v);
    return (skip(1));
  }

  Pointer writeWordLe(Word v)
  {
    assert(offset + 2 <= size);
    buffer.get()[offset] = v;
    buffer.get()[offset+1] = v >> 8;
    return (skip(2));
  }

  Pointer writeDWordLe(DWord v)
  {
    assert(offset + 4 <= size);
    buffer.get()[offset] = v;
    buffer.get()[offset+1] = v >> 8;
    buffer.get()[offset+2] = v >> 16;
    buffer.get()[offset+3] = v >> 24;
    return (skip(4));
  }

  Pointer writeQWordLe(QWord v)
  {
    assert(offset + 8 <= size);
    buffer.get()[offset] = v;
    buffer.get()[offset+1] = v >> 8;
    buffer.get()[offset+2] = v >> 16;
    buffer.get()[offset+3] = v >> 24;
    buffer.get()[offset+4] = v >> 32;
    buffer.get()[offset+5] = v >> 40;
    buffer.get()[offset+6] = v >> 48;
    buffer.get()[offset+7] = v >> 56;
    return (skip(8));
  }

  Pointer writeOWordLe(OWord v)
  {
    assert(offset + 16 <= size);
    buffer.get()[offset] = v;
    buffer.get()[offset+1] = v >> 8;
    buffer.get()[offset+2] = v >> 16;
    buffer.get()[offset+3] = v >> 24;
    buffer.get()[offset+4] = v >> 32;
    buffer.get()[offset+5] = v >> 40;
    buffer.get()[offset+6] = v >> 48;
    buffer.get()[offset+7] = v >> 56;
    buffer.get()[offset+8] = v >> 64;
    buffer.get()[offset+9] = v >> 72;
    buffer.get()[offset+10] = v >> 80;
    buffer.get()[offset+11] = v >> 88;
    buffer.get()[offset+12] = v >> 96;
    buffer.get()[offset+13] = v >> 104;
    buffer.get()[offset+14] = v >> 112;
    buffer.get()[offset+15] = v >> 120;
    return (skip(16));
  }

  Pointer writeBytes(Bytes const &v)
  {
    assert(offset + v.size <= size);
    memcpy(buffer.get() + offset, v.buffer.get() + v.offset, v.size);
    return skip(v.size);
  }

  Size operator-(Pointer const &that)
  {
    assert(buffer == that.buffer);
    assert(offset >= that.offset);
    return Size(offset - that.offset);
  }
};

#endif
