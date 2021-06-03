// vim: ft=c bs=2 ts=2 sts=2 sw=2 expandtab
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <uint64.h> // from stdint

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

  CAMLreturn(res);
}

CAMLprim value ext_pointer_size(value v1_)
{
  CAMLparam1(v1_);
  CAMLreturn(ExtPointerLen_val(v1_));
}

CAMLprim value ext_pointer_peek(value v, value offset_)
{
  CAMLparam2(v, offset_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  CAMLreturn(Val_int(ExtPointerData_val(v)[offset]));
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

CAMLprim value ext_pointer_poke(value v, value offset_, value x_)
{
  CAMLparam3(v, offset_, x_);
  size_t offset = Int_val(offset_);
  assert(offset < ExtPointerLen_val(v));
  ExtPointerData_val(v)[offset] = Int_val(x_);
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
