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

# `rpcgen' is actually not a strict requirement.
AC_PATH_PROG([rpcgen], [rpcgen],
  [AC_MSG_ERROR([`rpcgen' not found.  This is normally part of the GNU libc.])])
AC_SUBST([RPCGEN], [$rpcgen])

# Checkpoint the cache
AC_CACHE_SAVE

AC_CHECK_PROG([has_compile_et], [compile_et], [yes], [no])
if test "x$compile_et" = "xno"; then
  AC_MSG_ERROR([Could not find `compile_et'.  Please, install `libcom_err'])
fi

# Checks for libraries.
AM_PATH_LIBGCRYPT([1.2.0], [has_libgcrypt=yes], [has_libgcrypt=no])
if test "x$has_libgcrypt" != "xyes"; then
   AC_MSG_ERROR([`libgcrypt' 1.2+ not found.  Please, install it first.])
else
   CPPFLAGS="$CPPFLAGS $LIBGCRYPT_CFLAGS"
   LIBS="$LIBS $LIBGCRYPT_LIBS"
fi

AC_CHECK_LIB([c], [vasprintf], [],
  [AC_MSG_ERROR([`vasprintf' not found.  Are you using the GNU libc?])])
AC_CHECK_LIB([com_err], [com_err], [],
  [AC_MSG_ERROR([`libcom_err' not found.  Please, install it first, ok?!])])
AC_CHECK_LIB([gdbm], [gdbm_open], [],
  [AC_MSG_ERROR([`libgdbm' not found.  Come on, install it first.])])
AC_CHECK_LIB([z], [deflate], [],
  [AC_MSG_ERROR([`libz' not found.  Too bad.])])

# Optional libraries
AC_CHECK_LIB([tdb], [tdb_open], [have_libtdb=yes], [have_libtdb=no])
if test "x$have_libtdb" = "xyes"; then
   AC_CHECK_HEADER([tdb.h], [], [have_libtdb=no])
fi
AM_CONDITIONAL([HAVE_TDB], test "x$have_libtdb" = "xyes")
if test "x$have_libtdb" = "xyes"; then
   AC_DEFINE([HAVE_TDB], 1, [Tells whether libtdb is available.])
   LIBS="$LIBS -ltdb"
else
   AC_MSG_WARN([`libtdb' not found, won't be used])
fi

# QDBM, http://qdbm.sf.net/
if pkg-config qdbm; then
   # Looks like it's available.
   CPPFLAGS="$CPPFLAGS `pkg-config qdbm --cflags`"
   LIBS="$LIBS `pkg-config qdbm --libs`"

   AC_CHECK_LIB([qdbm], [dpopen], [have_libqdbm=yes], [have_libqdbm=no])
   if test "x$have_libqdbm" = "xyes"; then
      AC_CHECK_HEADER([depot.h], [], [have_libqdbm=no])
      AC_CHECK_HEADER([hovel.h], [], [have_libqdbm=no])
   fi
else
   have_libqdbm=no
fi

AM_CONDITIONAL([HAVE_QDBM], test "x$have_libqdbm" = "xyes")
if test "x$have_libqdbm" = "xyes"; then
   AC_DEFINE([HAVE_QDBM], 1, [Tells whether libqdbm is available.])
else
   AC_MSG_WARN([`libqdbm' not found, won't be used])
fi
   

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h obstack.h argp.h gdbm.h gcrypt.h com_err.h zlib.h netinet/in.h lightning.h])

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
   GUILE_PROGS
   GUILE_FLAGS

#   AC_CHECK_LIB([guile], [scm_u8vector_elements], [],
#     AC_MSG_ERROR([You version of Guile doesn't support SRFI-4 vectors.  Upgrade to 1.7.2 or later.]))

   AC_PATH_PROG([g_wrap_config], [g-wrap-config],
     AC_MSG_ERROR([Could not find `g-wrap-config'.  You mush has G-Wrap to produce Guile bindings.]))

   AC_MSG_CHECKING([whether G-Wrap's version is 1.9.5+])
   if $g_wrap_config --version | grep ['1.9.[5-9]'] > /dev/null; then
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
