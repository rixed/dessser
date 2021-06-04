// vim: ft=c bs=2 ts=2 sts=2 sw=2 expandtab
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

// From stdint:
#include <uint8.h>
#include <uint16.h>
#include <uint32.h>
#include <uint64.h>
#include <uint128.h>

static bool debug = false;

/* type ExtPointer.t.
 * User data is a pointer and size: */

static intnat ext_pointer_hash(value);

struct ext_pointer_user_data {
  unsigned char *data;
  size_t len;
};

#define ExtPointerUserData_val(v) ((struct ext_pointer_user_data *)Data_custom_val(v))
#define ExtPointerData_val(v) ExtPointerUserData_val(v)->data
#define ExtPointerLen_val(v) ExtPointerUserData_val(v)->len

static intnat ext_pointer_hash(value v)
{
  CAMLparam1(v);
  CAMLreturn((intnat)ExtPointerData_val(v));
}

/* Allocate a NULL pointer: */
value ext_pointer_new(value data_, value len_)
{
  CAMLparam2(data_, len_);
  void *data = (void *)Uint64_val(data_);
  size_t len = Int_val(len_);
  CAMLlocal1(res);

  static struct custom_operations ext_pointer_ops = {
    "org.happyleptic.dessser.ext_pointer",
    custom_finalize_default,
    custom_compare_default,
    ext_pointer_hash,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
  };

  res = caml_alloc_custom(&ext_pointer_ops, sizeof(struct ext_pointer_user_data), 0, 1);
  ExtPointerData_val(res) = data;
  ExtPointerLen_val(res) = len;

  if (debug)
    fprintf(stderr, "%s: new ext_pointer @%p, len:%zu\n",
            __func__, data, len);

  CAMLreturn(res);
}

CAMLprim value ext_pointer_size(value v1_)
{
  CAMLparam1(v1_);
  CAMLreturn(ExtPointerLen_val(v1_));
}

CAMLprim value ext_pointer_peek1(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  CAMLreturn(Val_uint8(ExtPointerData_val(v)[offset]));
}

CAMLprim value ext_pointer_peek2_le(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 1 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(Val_uint16(
    (uint16_t)data[0] |
    ((uint16_t)data[1] << 8U)));
}

CAMLprim value ext_pointer_peek2_be(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 1 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(Val_uint16(
    (uint16_t)data[1] |
    ((uint16_t)data[0] << 8U)));
}

CAMLprim value ext_pointer_peek4_le(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 3 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(copy_uint32(
    (uint32_t)data[0] |
    ((uint32_t)data[1] << 8U) |
    ((uint32_t)data[2] << 16U) |
    ((uint32_t)data[3] << 24U)));
}

CAMLprim value ext_pointer_peek4_be(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 3 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(copy_uint32(
    (uint32_t)data[3] |
    ((uint32_t)data[2] << 8U) |
    ((uint32_t)data[1] << 16U) |
    ((uint32_t)data[0] << 24U)));
}

CAMLprim value ext_pointer_peek8_le(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 7 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(copy_uint64(
    (uint64_t)data[0] |
    ((uint64_t)data[1] << 8U) |
    ((uint64_t)data[2] << 16U) |
    ((uint64_t)data[3] << 24U) |
    ((uint64_t)data[4] << 32U) |
    ((uint64_t)data[5] << 40U) |
    ((uint64_t)data[6] << 48U) |
    ((uint64_t)data[7] << 56U)));
}

CAMLprim value ext_pointer_peek8_be(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 7 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(copy_uint64(
    (uint64_t)data[7] |
    ((uint64_t)data[6] << 8U) |
    ((uint64_t)data[5] << 16U) |
    ((uint64_t)data[4] << 24U) |
    ((uint64_t)data[3] << 32U) |
    ((uint64_t)data[2] << 40U) |
    ((uint64_t)data[1] << 48U) |
    ((uint64_t)data[0] << 56U)));
}

CAMLprim value ext_pointer_peek16_le(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 15 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(copy_uint128(
# ifdef HAVE_UINT128
    (__uint128_t)data[0] |
    ((__uint128_t)data[1] << 8U) |
    ((__uint128_t)data[2] << 16U) |
    ((__uint128_t)data[3] << 24U) |
    ((__uint128_t)data[4] << 32U) |
    ((__uint128_t)data[5] << 40U) |
    ((__uint128_t)data[6] << 48U) |
    ((__uint128_t)data[7] << 56U) |
    ((__uint128_t)data[8] << 64U) |
    ((__uint128_t)data[9] << 72U) |
    ((__uint128_t)data[10] << 80U) |
    ((__uint128_t)data[11] << 88U) |
    ((__uint128_t)data[12] << 96U) |
    ((__uint128_t)data[13] << 104U) |
    ((__uint128_t)data[14] << 112U) |
    ((__uint128_t)data[15] << 120U)
# else
#   error "Not implemented: ext_pointer_peek16_le whithout native uint128"
# endif
  ));
}

CAMLprim value ext_pointer_peek16_be(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset + 15 < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  CAMLreturn(copy_uint128(
# ifdef HAVE_UINT128
    (__uint128_t)data[15] |
    ((__uint128_t)data[14] << 8U) |
    ((__uint128_t)data[13] << 16U) |
    ((__uint128_t)data[12] << 24U) |
    ((__uint128_t)data[11] << 32U) |
    ((__uint128_t)data[10] << 40U) |
    ((__uint128_t)data[9] << 48U) |
    ((__uint128_t)data[8] << 56U) |
    ((__uint128_t)data[7] << 64U) |
    ((__uint128_t)data[6] << 72U) |
    ((__uint128_t)data[5] << 80U) |
    ((__uint128_t)data[4] << 88U) |
    ((__uint128_t)data[3] << 96U) |
    ((__uint128_t)data[2] << 104U) |
    ((__uint128_t)data[1] << 112U) |
    ((__uint128_t)data[0] << 120U)
# else
#   error "Not implemented: ext_pointer_peek16_be whithout native uint128"
# endif
  ));
}

/* Returns a slice, which is a record with bytes, offset and length.
 * Since we'll have to copy the bytes we return just the requested bytes: */
CAMLprim value ext_pointer_peekn(value v, value offset_, value len_)
{
  CAMLparam3(v, offset_, len_);
  CAMLlocal2(slice, bytes);
  size_t offset = Int_val(offset_);
  size_t len = Int_val(len_);
  assert(offset + len <= ExtPointerLen_val(v));

  bytes = caml_alloc_string(len);
  if (! bytes) caml_failwith("Cannot malloc bytes for peekn");
  slice = caml_alloc_tuple(3);
  if (! slice) caml_failwith("Cannot malloc slice for peekn");

  memcpy(String_val(bytes), ExtPointerData_val(v) + offset, len);
  Store_field(slice, 0, bytes);
  Store_field(slice, 1, Val_int(0));
  Store_field(slice, 2, len_);

  CAMLreturn(slice);
}

CAMLprim value ext_pointer_poke1(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  ExtPointerData_val(v)[offset] = Uint8_val(x_);

  if (debug)
    fprintf(stderr, "%s: poke @%p+%zu, val:0x%x\n",
            __func__, ExtPointerData_val(v), offset, (unsigned)Uint8_val(x_));

  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke2_le(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  uint16_t x = Uint16_val(x_);
  data[0] = x;
  data[1] = x >> 8U;
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke2_be(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  uint16_t x = Uint16_val(x_);
  data[1] = x;
  data[0] = x >> 8U;
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke4_le(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  uint32_t x = Uint32_val(x_);
  data[0] = x;
  data[1] = x >> 8U;
  data[2] = x >> 16U;
  data[3] = x >> 24U;
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke4_be(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  uint32_t x = Uint32_val(x_);
  data[3] = x;
  data[2] = x >> 8U;
  data[1] = x >> 16U;
  data[0] = x >> 24U;
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke8_le(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  uint64_t x = Uint64_val(x_);
  data[0] = x;
  data[1] = x >> 8U;
  data[2] = x >> 16U;
  data[3] = x >> 24U;
  data[4] = x >> 32U;
  data[5] = x >> 40U;
  data[6] = x >> 48U;
  data[7] = x >> 56U;
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke8_be(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
  uint64_t x = Uint64_val(x_);
  data[7] = x;
  data[6] = x >> 8U;
  data[5] = x >> 16U;
  data[4] = x >> 24U;
  data[3] = x >> 32U;
  data[2] = x >> 40U;
  data[1] = x >> 48U;
  data[0] = x >> 56U;
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke16_le(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
# ifdef HAVE_UINT128
  __uint128_t x = Uint128_val(x_);
  data[0] = x;
  data[1] = x >> 8U;
  data[2] = x >> 16U;
  data[3] = x >> 24U;
  data[4] = x >> 32U;
  data[5] = x >> 40U;
  data[6] = x >> 48U;
  data[7] = x >> 56U;
  data[8] = x >> 64U;
  data[9] = x >> 72U;
  data[10] = x >> 80U;
  data[11] = x >> 88U;
  data[12] = x >> 96U;
  data[13] = x >> 104U;
  data[14] = x >> 112U;
  data[15] = x >> 120U;
# else
#   error "Not implemented: ext_pointer_poke16_le whithout native uint128"
# endif
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poke16_be(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  unsigned char *data = ExtPointerData_val(v) + offset;
# ifdef HAVE_UINT128
  __uint128_t x = Uint128_val(x_);
  data[15] = x;
  data[14] = x >> 8U;
  data[13] = x >> 16U;
  data[12] = x >> 24U;
  data[11] = x >> 32U;
  data[10] = x >> 40U;
  data[9] = x >> 48U;
  data[8] = x >> 56U;
  data[7] = x >> 64U;
  data[6] = x >> 72U;
  data[5] = x >> 80U;
  data[4] = x >> 88U;
  data[3] = x >> 96U;
  data[2] = x >> 104U;
  data[1] = x >> 112U;
  data[0] = x >> 120U;
# else
#   error "Not implemented: ext_pointer_poke16_le whithout native uint128"
# endif
  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_poken(value v, value offset_, value slice)
{
  CAMLparam3(v, offset_, slice);
  size_t offset = Int_val(offset_);
  assert(Is_block(slice));
  size_t slice_offset = Int_val(Field(slice, 1));
  size_t len = Int_val(Field(slice, 2));
  assert(offset + len <= ExtPointerLen_val(v));
  memcpy(
    ExtPointerData_val(v) + offset,
    Bytes_val(Field(slice, 0)) + slice_offset,
    len);

  if (debug)
    fprintf(stderr, "%s: poken @%p+%zu, len:%zu\n",
            __func__, ExtPointerData_val(v), offset, len);

  CAMLreturn(Val_unit);
}

CAMLprim value ext_pointer_to_string(value v)
{
  CAMLparam1(v);
  size_t len = ExtPointerLen_val(v);
  /* This is only used for Int.of_substring so no need for more than 40 digits: */
  if (len > 40) len = 40;
  CAMLreturn(caml_alloc_initialized_string(len, (char *)ExtPointerData_val(v)));
}
