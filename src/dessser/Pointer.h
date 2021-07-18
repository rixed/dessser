#ifndef POINTER_H_191025
#define POINTER_H_191025
#include <cassert>
#include <cstring>
#include <memory>
#include <tuple>
#include "dessser/Bytes.h"
#include "dessser/typedefs.h"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <cctype>

namespace dessser_gen {

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
  //std::shared_ptr<uint8_t[]> buffer;
  // Work around MacOS c17 issue:
  std::shared_ptr<uint8_t> buffer;
  // If the pointer was created from an address (to some non-managed memory):
  uint8_t *bytes;
  // Total size of the buffer/bytes arena (regardless of offset)
  size_t size;
  // Current location of the read/write pointer inside the buffer/bytes:
  size_t offset;

  /* The type of pointer used to hold a heap allocated value.
   * If this is set then buffer must by null, and the other way around.
   * On shared_ptr<void>, it works because BackEndCPP.alloc_value alloc
   * with new, and we build the shared_ptr when the actual type is known. */
  std::shared_ptr<void> value; // FIXME: obsolete?

  /* Construct (with uninitialized buffer) from a size: */
  Pointer(Size const &sz) :
    buffer(new uint8_t[sz]),
    bytes(nullptr),
    size(sz),
    offset(0)
  {}

  /* Construct from a string: */
  Pointer(std::string const &str) :
    buffer(new uint8_t[str.size()]),
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
    value(that.value)
  {}

  /* Construct from another Pointer, narrowing a subpart of it */
  Pointer(Pointer const &that, Size offset_, Size size_) :
    buffer(that.buffer),
    bytes(that.bytes),
    size(that.offset + size_),
    offset(that.offset + offset_),
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

  uint8_t get(size_t offs) const
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

  uint8_t peekU8(size_t at = 0) const
  {
    checkOffset(offset + at + 1);
    return get(offset + at);
  }

  std::tuple<uint8_t, Pointer> readU8() const
  {
    return std::tuple<uint8_t, Pointer>(
      peekU8(), skip(1));
  }

  uint16_t peekU16Le(size_t at = 0) const
  {
    checkOffset(offset + at + 2);
    return (((uint16_t)get(offset + at + 1)) << 8) | get(offset + at);
  }

  std::tuple<uint16_t, Pointer> readU16Le() const
  {
    return std::tuple<uint16_t, Pointer>(
      peekU16Le(), skip(2));
  }

  uint16_t peekU16Be(size_t at = 0) const
  {
    checkOffset(offset + at + 2);
    return (((uint16_t)get(offset + at)) << 8) | get(offset + at + 1);
  }

  std::tuple<uint16_t, Pointer> readU16Be() const
  {
    return std::tuple<uint16_t, Pointer>(
      peekU16Be(), skip(2));
  }

  uint32_t peekU32Le(size_t at = 0) const
  {
    checkOffset(offset + 4);
    return (((uint32_t)get(offset + at + 3)) << 24) |
           (((uint32_t)get(offset + at + 2)) << 16) |
           (((uint32_t)get(offset + at + 1)) << 8) |
           get(offset + at);
  }

  std::tuple<uint32_t, Pointer> readU32Le() const
  {
    return std::tuple<uint32_t, Pointer>(
      peekU32Le(), skip(4));
  }

  uint32_t peekU32Be(size_t at = 0) const
  {
    checkOffset(offset + 4);
    return (((uint32_t)get(offset + at)) << 24) |
           (((uint32_t)get(offset + at + 1)) << 16) |
           (((uint32_t)get(offset + at + 2)) << 8) |
           get(offset + at + 3);
  }

  std::tuple<uint32_t, Pointer> readU32Be() const
  {
    return std::tuple<uint32_t, Pointer>(
      peekU32Be(), skip(4));
  }

  uint64_t peekU64Le(size_t at = 0) const
  {
    checkOffset(offset + 8);
    return (((uint64_t)get(offset + at + 7)) << 56) |
           (((uint64_t)get(offset + at + 6)) << 48) |
           (((uint64_t)get(offset + at + 5)) << 40) |
           (((uint64_t)get(offset + at + 4)) << 32) |
           (((uint64_t)get(offset + at + 3)) << 24) |
           (((uint64_t)get(offset + at + 2)) << 16) |
           (((uint64_t)get(offset + at + 1)) << 8) |
           get(offset + at);
  }

  std::tuple<uint64_t, Pointer> readU64Le() const
  {
    return std::tuple<uint64_t, Pointer>(
      peekU64Le(), skip(8));
  }

  uint64_t peekU64Be(size_t at = 0) const
  {
    checkOffset(offset + 8);
    return (((uint64_t)get(offset + at)) << 56) |
           (((uint64_t)get(offset + at + 1)) << 48) |
           (((uint64_t)get(offset + at + 2)) << 40) |
           (((uint64_t)get(offset + at + 3)) << 32) |
           (((uint64_t)get(offset + at + 4)) << 24) |
           (((uint64_t)get(offset + at + 5)) << 16) |
           (((uint64_t)get(offset + at + 6)) << 8) |
           get(offset + at + 7);
  }

  std::tuple<uint64_t, Pointer> readU64Be() const
  {
    return std::tuple<uint64_t, Pointer>(
      peekU64Be(), skip(8));
  }

  uint128_t peekU128Le(size_t at = 0) const
  {
    checkOffset(offset + 16);
    return (((uint128_t)get(offset + at + 15)) << 120) |
           (((uint128_t)get(offset + at + 14)) << 112) |
           (((uint128_t)get(offset + at + 13)) << 104) |
           (((uint128_t)get(offset + at + 12)) << 96) |
           (((uint128_t)get(offset + at + 11)) << 88) |
           (((uint128_t)get(offset + at + 10)) << 80) |
           (((uint128_t)get(offset + at + 9)) << 72) |
           (((uint128_t)get(offset + at + 8)) << 64) |
           (((uint128_t)get(offset + at + 7)) << 56) |
           (((uint128_t)get(offset + at + 6)) << 48) |
           (((uint128_t)get(offset + at + 5)) << 40) |
           (((uint128_t)get(offset + at + 4)) << 32) |
           (((uint128_t)get(offset + at + 3)) << 24) |
           (((uint128_t)get(offset + at + 2)) << 16) |
           (((uint128_t)get(offset + at + 1)) << 8) |
           get(offset + at);
  }

  std::tuple<uint128_t, Pointer> readU128Le() const
  {
    checkOffset(offset + 16);
    return std::tuple<uint128_t, Pointer>(
      peekU128Le(), skip(16));
  }

  uint128_t peekU128Be(size_t at = 0) const
  {
    checkOffset(offset + 16);
    return (((uint128_t)get(offset + at)) << 120) |
           (((uint128_t)get(offset + at + 1)) << 112) |
           (((uint128_t)get(offset + at + 2)) << 104) |
           (((uint128_t)get(offset + at + 3)) << 96) |
           (((uint128_t)get(offset + at + 4)) << 88) |
           (((uint128_t)get(offset + at + 5)) << 80) |
           (((uint128_t)get(offset + at + 6)) << 72) |
           (((uint128_t)get(offset + at + 7)) << 64) |
           (((uint128_t)get(offset + at + 8)) << 56) |
           (((uint128_t)get(offset + at + 9)) << 48) |
           (((uint128_t)get(offset + at + 10)) << 40) |
           (((uint128_t)get(offset + at + 11)) << 32) |
           (((uint128_t)get(offset + at + 12)) << 24) |
           (((uint128_t)get(offset + at + 13)) << 16) |
           (((uint128_t)get(offset + at + 14)) << 8) |
           get(offset + at + 15);
  }

  std::tuple<uint128_t, Pointer> readU128Be() const
  {
    checkOffset(offset + 16);
    return std::tuple<uint128_t, Pointer>(
      peekU128Be(), skip(16));
  }

  std::tuple<Bytes, Pointer> readBytes(Size const &sz) const
  {
    checkOffset(offset + sz);
    return std::tuple<Bytes, Pointer>(
      bytes ?
        Bytes(bytes + offset, sz) : Bytes(buffer, sz, offset),
      skip(sz));
  }

  void set(size_t offs, uint8_t v)
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
    uint8_t const mask = 1 << bit;
    set(off, v ? get(off) | mask : get(off) & ~mask);
  }

  void pokeU8(uint8_t v)
  {
    checkOffset(offset + 1);
    set(offset, v);
  }

  Pointer writeU8(uint8_t v)
  {
    pokeU8(v);
    return skip(1);
  }

  Pointer writeU16Le(uint16_t v)
  {
    checkOffset(offset + 2);
    set(offset, v);
    set(offset+1, v >> 8);
    return skip(2);
  }

  Pointer writeU16Be(uint16_t v)
  {
    checkOffset(offset + 2);
    set(offset+1, v);
    set(offset, v >> 8);
    return skip(2);
  }

  Pointer writeU32Le(uint32_t v)
  {
    checkOffset(offset + 4);
    set(offset, v);
    set(offset+1, v >> 8);
    set(offset+2, v >> 16);
    set(offset+3, v >> 24);
    return skip(4);
  }

  Pointer writeU32Be(uint32_t v)
  {
    checkOffset(offset + 4);
    set(offset+3, v);
    set(offset+2, v >> 8);
    set(offset+1, v >> 16);
    set(offset,  v >> 24);
    return skip(4);
  }

  Pointer writeU64Le(uint64_t v)
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

  Pointer writeU64Be(uint64_t v)
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

  Pointer writeU128Le(uint128_t v)
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

  Pointer writeU128Be(uint128_t v)
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
    uint8_t *dest = bytes ? bytes : buffer.get();
    memcpy(dest + offset, v.buffer.get() + v.offset, v.size);
    return skip(v.size);
  }

  Pointer blitBytes(uint8_t const b, Size const &sz)
  {
    checkOffset(offset + sz);
    uint8_t *dest = bytes ? bytes : buffer.get();
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

inline bool operator==(const Pointer& lhs, const Pointer& rhs)
{
  return
      lhs.buffer == rhs.buffer &&
      lhs.bytes == rhs.bytes &&
      lhs.size == rhs.size &&
      lhs.offset == rhs.offset;
}

inline bool operator!=(const Pointer& lhs, const Pointer& rhs)
{
  return !(lhs == rhs);
}

static inline std::string printable_string_of_byte(uint8_t const b)
{
  std::stringstream stream;
  if (isprint(b)) stream << (char)b;
  else stream << "\\x" << std::hex << std::setw(2) << std::setfill('0') << (int)b;
  return stream.str();
}

static inline std::ostream &operator<<(std::ostream &os, Pointer const &p)
{
  uint8_t *dest = p.bytes ? p.bytes : p.buffer.get();

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

};

#endif
