#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(libchop, 0.0, ludovic.courtes@laas.fr)
AM_INIT_AUTOMAKE(libchop, 0.0)

AC_CONFIG_SRCDIR([src/chop.h])
AC_CONFIG_HEADER([src/chop-config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
AC_CHECK_PROG([has_compile_et], [compile_et], [yes], [no])
if test "x$compile_et" = "xno"; then
  AC_MSG_ERROR([Could not find `compile_et'.  Please, install `libcom_err'])
fi

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC

AC_CONFIG_FILES([Makefile
                 src/Makefile
		 utils/Makefile])

AC_OUTPUT
