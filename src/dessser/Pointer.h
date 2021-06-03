#ifndef POINTER_H_191025
#define POINTER_H_191025
#include <cassert>
#include <memory>
#include <cstring>
#include <stack>
#include "dessser/Bytes.h"
#include "dessser/Pair.h"
#include "dessser/typedefs.h"

/* Pointers that point to a byte buffer used to (de)serialize values. */

/* The type of pointer used to serialize/deserialize with write/read functions: */
struct Pointer {
  /* We can either point to a buffer that is created when the pointer is
   * created, or to some unmanaged memory location (data_ptr_of_address):
   * FIXME: Have a MakeBuffer and AddressOfBuffer, then do away with
   *        DataPtrOfBuffer etc? Won't change much to OCaml backend, which
   *        still must switch between the two as the buffer address won't
   *        be constant there, though.
   */
  // Shared with all pointers derived from this one:
  //std::shared_ptr<Byte[]> buffer;
  // Work around MacOS c17 issue:
  std::shared_ptr<Byte> buffer;
  // If the pointer was created from an address (to some non-managed memory):
  Byte *bytes;
  // Total size of the buffer/bytes arena (regardless of offset)
  size_t size;
  // Current location of the read/write pointer inside the buffer/bytes:
  size_t offset;
  // Stack of saved offset positions:
  std::stack<size_t> stack;

  /* The type of pointer used to hold a heap allocated value.
   * If this is set then buffer must by null, and the other way around.
   * On shared_ptr<void>, it works because BackEndCPP.alloc_value alloc
   * with new, and we build the shared_ptr when the actual type is known. */
  std::shared_ptr<void> value; // FIXME: obsolete?

  /* Construct (with uninitialized buffer) from a size: */
  Pointer(Size const &sz) :
    buffer(new Byte[sz]),
    bytes(nullptr),
    size(sz),
    offset(0)
  {}

  /* Construct from a string: */
  Pointer(std::string const &str) :
    buffer(new Byte[str.size()]),
    bytes(nullptr),
    size(str.size()),
    offset(0)
  {
    memcpy(buffer.get(), str.c_str(), size);
  }

  /* Construct from another Pointer, sharing the buffer */
  Pointer(Pointer const &that) :
    buffer(that.buffer),
    bytes(that.bytes),
    size(that.size),
    offset(that.offset),
    stack(that.stack),
    value(that.value)
  {}

  /* Construct from another Pointer, narrowing a subpart of it */
  Pointer(Pointer const &that, Size offset_, Size size_) :
    buffer(that.buffer),
    bytes(that.bytes),
    size(that.offset + size_),
    offset(that.offset + offset_),
    stack(that.stack),
    value(that.value)
  {
    assert(size <= that.size);
    assert(offset <= that.size);
  }

  /* Construct from any heap allocated value */
  Pointer(std::shared_ptr<void> value_) :
    bytes(nullptr),
    size(0),
    offset(0),
    value(value_)
  {}

  /* Default constructor for uninitialized objects: */
  Pointer() {}

  void checkOffset(size_t offs) const
  {
    if (offs > size)
      throw std::out_of_range(
        std::to_string(size - offs) + " bytes short");
  }

  Byte get(size_t offs) const
  {
    return bytes ? bytes[offs] : buffer.get()[offs];
  }

  Size rem() const
  {
    return size - offset;
  }

  Pointer skip(Size const &sz) const
  {
    Pointer ptr(*this);
    ptr.checkOffset(ptr.offset + sz);
    ptr.offset += sz;
    return ptr;
  }

  bool getBit(size_t b) const
  {
    size_t const off = offset + b / 8;
    size_t const bit = b & 7;
    checkOffset(off + 1);
    return !!(get(off) & (1 << bit));
  }

  Byte peekByte(size_t at = 0) const
  {
    checkOffset(offset + at + 1);
    return get(offset + at);
  }

  Pair<Byte, Pointer> readByte() const
  {
    return Pair<Byte, Pointer>(
      peekByte(), skip(1));
  }

  Word peekWordLe() const
  {
    checkOffset(offset + 2);
    return (((Word)get(offset + 1)) << 8) | get(offset);
  }

  Pair<Word, Pointer> readWordLe() const
  {
    return Pair<Word, Pointer>(
      peekWordLe(), skip(2));
  }

  Word peekWordBe() const
  {
    checkOffset(offset + 2);
    return (((Word)get(offset)) << 8) | get(offset + 1);
  }

  Pair<Word, Pointer> readWordBe() const
  {
    return Pair<Word, Pointer>(
      peekWordBe(), skip(2));
  }

  DWord peekDWordLe() const
  {
    checkOffset(offset + 4);
    return (((DWord)get(offset + 3)) << 24) |
           (((DWord)get(offset + 2)) << 16) |
           (((DWord)get(offset + 1)) << 8) |
           get(offset);
  }

  Pair<DWord, Pointer> readDWordLe() const
  {
    return Pair<DWord, Pointer>(
      peekDWordLe(), skip(4));
  }

  DWord peekDWordBe() const
  {
    checkOffset(offset + 4);
    return (((DWord)get(offset)) << 24) |
           (((DWord)get(offset + 1)) << 16) |
           (((DWord)get(offset + 2)) << 8) |
           get(offset + 3);
  }

  Pair<DWord, Pointer> readDWordBe() const
  {
    return Pair<DWord, Pointer>(
      peekDWordBe(), skip(4));
  }

  QWord peekQWordLe() const
  {
    checkOffset(offset + 8);
    return (((QWord)get(offset + 7)) << 56) |
           (((QWord)get(offset + 6)) << 48) |
           (((QWord)get(offset + 5)) << 40) |
           (((QWord)get(offset + 4)) << 32) |
           (((QWord)get(offset + 3)) << 24) |
           (((QWord)get(offset + 2)) << 16) |
           (((QWord)get(offset + 1)) << 8) |
           get(offset);
  }

  Pair<QWord, Pointer> readQWordLe() const
  {
    return Pair<QWord, Pointer>(
      peekQWordLe(), skip(8));
  }

  QWord peekQWordBe() const
  {
    checkOffset(offset + 8);
    return (((QWord)get(offset)) << 56) |
           (((QWord)get(offset + 1)) << 48) |
           (((QWord)get(offset + 2)) << 40) |
           (((QWord)get(offset + 3)) << 32) |
           (((QWord)get(offset + 4)) << 24) |
           (((QWord)get(offset + 5)) << 16) |
           (((QWord)get(offset + 6)) << 8) |
           get(offset + 7);
  }

  Pair<QWord, Pointer> readQWordBe() const
  {
    return Pair<QWord, Pointer>(
      peekQWordBe(), skip(8));
  }

  OWord peekOWordLe() const
  {
    checkOffset(offset + 16);
    return (((OWord)get(offset + 15)) << 120) |
           (((OWord)get(offset + 14)) << 112) |
           (((OWord)get(offset + 13)) << 104) |
           (((OWord)get(offset + 12)) << 96) |
           (((OWord)get(offset + 11)) << 88) |
           (((OWord)get(offset + 10)) << 80) |
           (((OWord)get(offset + 9)) << 72) |
           (((OWord)get(offset + 8)) << 64) |
           (((OWord)get(offset + 7)) << 56) |
           (((OWord)get(offset + 6)) << 48) |
           (((OWord)get(offset + 5)) << 40) |
           (((OWord)get(offset + 4)) << 32) |
           (((OWord)get(offset + 3)) << 24) |
           (((OWord)get(offset + 2)) << 16) |
           (((OWord)get(offset + 1)) << 8) |
           get(offset);
  }

  Pair<OWord, Pointer> readOWordLe() const
  {
    checkOffset(offset + 16);
    return Pair<OWord, Pointer>(
      peekOWordLe(), skip(16));
  }

  OWord peekOWordBe() const
  {
    checkOffset(offset + 16);
    return (((OWord)get(offset)) << 120) |
           (((OWord)get(offset + 1)) << 112) |
           (((OWord)get(offset + 2)) << 104) |
           (((OWord)get(offset + 3)) << 96) |
           (((OWord)get(offset + 4)) << 88) |
           (((OWord)get(offset + 5)) << 80) |
           (((OWord)get(offset + 6)) << 72) |
           (((OWord)get(offset + 7)) << 64) |
           (((OWord)get(offset + 8)) << 56) |
           (((OWord)get(offset + 9)) << 48) |
           (((OWord)get(offset + 10)) << 40) |
           (((OWord)get(offset + 11)) << 32) |
           (((OWord)get(offset + 12)) << 24) |
           (((OWord)get(offset + 13)) << 16) |
           (((OWord)get(offset + 14)) << 8) |
           get(offset + 15);
  }

  Pair<OWord, Pointer> readOWordBe() const
  {
    checkOffset(offset + 16);
    return Pair<OWord, Pointer>(
      peekOWordBe(), skip(16));
  }

  Pair<Bytes, Pointer> readBytes(Size const &sz) const
  {
    checkOffset(offset + sz);
    return Pair<Bytes, Pointer>(
      bytes ?
        Bytes(bytes + offset, sz) : Bytes(buffer, sz, offset),
      skip(sz));
  }

  void set(size_t offs, Byte v)
  {
    if (bytes) {
      bytes[offs] = v;
    } else {
      buffer.get()[offs] = v;
    }
  }

  void setBit(size_t b, bool v)
  {
    size_t const off = offset + b / 8;
    size_t const bit = b & 7;
    checkOffset(off + 1);
    Byte const mask = 1 << bit;
    set(off, v ? get(off) | mask : get(off) & ~mask);
  }

  void pokeByte(Byte v)
  {
    checkOffset(offset + 1);
    set(offset, v);
  }

  Pointer writeByte(Byte v)
  {
    pokeByte(v);
    return skip(1);
  }

  Pointer writeWordLe(Word v)
  {
    checkOffset(offset + 2);
    set(offset, v);
    set(offset+1, v >> 8);
    return skip(2);
  }

  Pointer writeWordBe(Word v)
  {
    checkOffset(offset + 2);
    set(offset+1, v);
    set(offset, v >> 8);
    return skip(2);
  }

  Pointer writeDWordLe(DWord v)
  {
    checkOffset(offset + 4);
    set(offset, v);
    set(offset+1, v >> 8);
    set(offset+2, v >> 16);
    set(offset+3, v >> 24);
    return skip(4);
  }

  Pointer writeDWordBe(DWord v)
  {
    checkOffset(offset + 4);
    set(offset+3, v);
    set(offset+2, v >> 8);
    set(offset+1, v >> 16);
    set(offset,  v >> 24);
    return skip(4);
  }

  Pointer writeQWordLe(QWord v)
  {
    checkOffset(offset + 8);
    set(offset, v);
    set(offset+1, v >> 8);
    set(offset+2, v >> 16);
    set(offset+3, v >> 24);
    set(offset+4, v >> 32);
    set(offset+5, v >> 40);
    set(offset+6, v >> 48);
    set(offset+7, v >> 56);
    return skip(8);
  }

  Pointer writeQWordBe(QWord v)
  {
    checkOffset(offset + 8);
    set(offset+7, v);
    set(offset+6, v >> 8);
    set(offset+5, v >> 16);
    set(offset+4, v >> 24);
    set(offset+3, v >> 32);
    set(offset+2, v >> 40);
    set(offset+1, v >> 48);
    set(offset, v >> 56);
    return skip(8);
  }

  Pointer writeOWordLe(OWord v)
  {
    checkOffset(offset + 16);
    set(offset, v);
    set(offset+1, v >> 8);
    set(offset+2, v >> 16);
    set(offset+3, v >> 24);
    set(offset+4, v >> 32);
    set(offset+5, v >> 40);
    set(offset+6, v >> 48);
    set(offset+7, v >> 56);
    set(offset+8, v >> 64);
    set(offset+9, v >> 72);
    set(offset+10, v >> 80);
    set(offset+11, v >> 88);
    set(offset+12, v >> 96);
    set(offset+13, v >> 104);
    set(offset+14, v >> 112);
    set(offset+15, v >> 120);
    return skip(16);
  }

  Pointer writeOWordBe(OWord v)
  {
    checkOffset(offset + 16);
    set(offset+15, v);
    set(offset+14, v >> 8);
    set(offset+13, v >> 16);
    set(offset+12, v >> 24);
    set(offset+11, v >> 32);
    set(offset+10, v >> 40);
    set(offset+9, v >> 48);
    set(offset+8, v >> 56);
    set(offset+7, v >> 64);
    set(offset+6, v >> 72);
    set(offset+5, v >> 80);
    set(offset+4, v >> 88);
    set(offset+3, v >> 96);
    set(offset+2, v >> 104);
    set(offset+1, v >> 112);
    set(offset, v >> 120);
    return skip(16);
  }

  Pointer writeBytes(Bytes const &v)
  {
    checkOffset(offset + v.size);
    Byte *dest = bytes ? bytes : buffer.get();
    memcpy(dest + offset, v.buffer.get() + v.offset, v.size);
    return skip(v.size);
  }

  Pointer blitBytes(Byte const b, Size const &sz)
  {
    checkOffset(offset + sz);
    Byte *dest = bytes ? bytes : buffer.get();
    memset(dest + offset, b, sz);
    return skip(sz);
  }

  Size operator-(Pointer const &that)
  {
    assert(buffer == that.buffer);
    assert(bytes == that.bytes);
    assert(offset >= that.offset);
    return Size(offset - that.offset);
  }

  Pointer push()
  {
    Pointer ptr(*this);
    ptr.stack.push(ptr.offset);
    return ptr;
  }

  Pointer pop()
  {
    Pointer ptr(*this);
    assert(! ptr.stack.empty());
    ptr.offset = ptr.stack.top();
    ptr.stack.pop();
    return ptr;
  }

  Size remSize() const
  {
    assert(size >= offset);
    return (Size)(size - offset);
  }

  Size getOffset() const
  {
    return (Size)offset;
  }
};

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cctype>

static inline std::string printable_string_of_byte(Byte const b)
{
  std::stringstream stream;
  if (isprint(b)) stream << (char)b;
  else stream << "\\x" << std::hex << std::setw(2) << std::setfill('0') << (int)b;
  return stream.str();
}

static inline std::ostream &operator<<(std::ostream &os, Pointer const &p)
{
  Byte *dest = p.bytes ? p.bytes : p.buffer.get();

  if (dest) {
    os << '"';
    for (unsigned i = 0; i < p.offset; i++)
      os << printable_string_of_byte(dest[i]);
    os << '|';
    for (unsigned i = p.offset ; i < p.size; i++)
      os << printable_string_of_byte(dest[i]);
    os << '"' << " (offset=" << p.offset << ")";
  } else {
    os << "<empty>";
  }
  return os;
}

#endif
