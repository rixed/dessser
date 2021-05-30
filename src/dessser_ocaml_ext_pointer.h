// vim: ft=c bs=2 ts=2 sts=2 sw=2 expandtab
/* External pointers for OCaml backend */
#ifndef EXT_POINTER_H_20210531
#define EXT_POINTER_H_20210531y

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

value ext_pointer_new(void *data, size_t len);

#endif
