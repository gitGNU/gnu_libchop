#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(libchop, 0.0, ludovic.courtes@laas.fr)
AM_INIT_AUTOMAKE(libchop, 0.0)

AC_CONFIG_SRCDIR(src/chop.c)
AC_CONFIG_HEADER(include/chop/chop-config.h)

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

# Checkpoint the cache
AC_CACHE_SAVE

AC_CHECK_PROG([has_compile_et], [compile_et], [yes], [no])
if test "x$compile_et" = "xno"; then
  AC_MSG_ERROR([Could not find `compile_et'.  Please, install `libcom_err'])
fi

# Checks for libraries.
AC_CHECK_LIB([c], [vasprintf], [],
  [AC_MSG_ERROR([`vasprintf' not found.  Are you using the GNU libc?])])
AC_CHECK_LIB([gcrypt], [gcry_md_hash_buffer], [],
  [AC_MSG_ERROR([`libgcrypt' not found.  Please, install it first.])])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h obstack.h gdbm.h gcrypt.h com_err.h])

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
