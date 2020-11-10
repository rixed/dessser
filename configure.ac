AC_INIT(dessser, 2.0.0)
m4_include([m4/ocaml.m4])

AC_ARG_VAR(OCAMLOPTFLAGS, [Additional flags to pass to the ocaml native compiler])
AC_ARG_VAR(OCAMLCFLAGS, [Additional flags to pass to the ocaml bytecode compiler])

AC_PROG_OCAML
AS_IF(
  [test "$OCAMLC" = no],
  AC_MSG_ERROR([You must install the OCaml compiler.]))

AC_PROG_FINDLIB
AS_IF(
  [test "$OCAMLFIND" = no],
  AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command).]))

AC_CHECK_OCAML_PKG(batteries)
AC_CHECK_OCAML_PKG(parsercombinator)
AC_CHECK_OCAML_PKG(stdint)
AC_CHECK_OCAML_PKG(cmdliner)
AC_CHECK_OCAML_PKG(parsercombinator)
AC_CHECK_OCAML_PKG(qcheck)

AC_CONFIG_FILES([
  Makefile
  opam
])
AC_OUTPUT
