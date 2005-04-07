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
AC_CHECK_LIB([com_err], [com_err], [],
  [AC_MSG_ERROR([`libcom_err' not found.  Please, install it first, ok?!])])
AC_CHECK_LIB([gdbm], [gdbm_open], [],
  [AC_MSG_ERROR([`libgdbm' not found.  Come on, install it first.])])
AC_CHECK_LIB([z], [deflate], [],
  [AC_MSG_ERROR([`libz' not found.  Too bad.])])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h obstack.h argp.h gdbm.h gcrypt.h com_err.h zlib.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC

# Checkpoint the cache
AC_CACHE_SAVE

# Look for GNU Gperf (used for O(1) class lookup).
AC_PATH_PROG([gperf], [gperf], [not-found])
AM_CONDITIONAL(HAVE_GPERF, test "x$gperf" != "xnot-found")
if test "x$gperf" != "xnot-found"; then
  AC_SUBST(GPERF, $gperf)
  AC_DEFINE([HAVE_GPERF], 1, [Tells whether GNU Gperf is available.])   
else
  AC_MSG_WARN([Class lookup by name will not be compiled in])
fi

# Check for Guile and G-Wrap
AC_PATH_PROG([guile], [guile],
  AC_MSG_WARN([Guile not found.  Not building Guile bindings.]))

if test "x$guile" != "x"; then
#   AC_CHECK_LIB([guile], [scm_u8vector_elements], [],
#     AC_MSG_ERROR([You version of Guile doesn't support SRFI-4 vectors.  Upgrade to 1.7.2 or later.]))

   AC_PATH_PROG([g_wrap_config], [g-wrap-config],
     AC_MSG_ERROR([Could not find `g-wrap-config'.  You mush has G-Wrap to produce Guile bindings.]))

   AC_MSG_CHECKING([whether G-Wrap's version is 1.9.5])
   if $g_wrap_config --version | grep 1.9.5 > /dev/null; then
     AC_MSG_RESULT([yes])
   else
     AC_MSG_RESULT([no])
     AC_MSG_ERROR([Please, use the right G-Wrap version.])
   fi

   AC_SUBST(GUILE, $guile)
   AC_SUBST(G_WRAP_CONFIG, $g_wrap_config)
fi

AM_CONDITIONAL(HAVE_GUILE, test "x$guile" != "x")

AC_CONFIG_FILES([Makefile
                 include/Makefile
                 src/Makefile
		 utils/Makefile
		 tests/Makefile
		 tests/interfaces/Makefile
		 tests/features/Makefile
		 guile/Makefile])

AC_OUTPUT
