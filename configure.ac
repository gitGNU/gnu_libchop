#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(libchop, 0.0, ludovic.courtes@laas.fr)
AM_INIT_AUTOMAKE(libchop, 0.0)

AC_CONFIG_SRCDIR(src/chop.c)
AC_CONFIG_HEADER(include/chop/chop-config.h)

AC_ARG_ENABLE([guile-bindings],
	AS_HELP_STRING([--enable-guile-bindings],
	  [produce Guile bindings (requires Guile and G-Wrap)]),
	[produce_guile_bindings="$enableval"],
	[produce_guile_bindings="no"])


# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

# `rpcgen' (recommended)
AC_PATH_PROG([rpcgen], [rpcgen],
  [AC_MSG_ERROR([`rpcgen' not found.  This is normally part of the GNU libc.])])
AC_SUBST([RPCGEN], [$rpcgen])

# Checkpoint the cache
AC_CACHE_SAVE

# libcom_err2.
AC_PATH_PROG([compile_et], [compile_et],
  [AC_MSG_ERROR([Could not find `compile_et'.  Please, install `libcom_err'])])
AC_SUBST([COMPILE_ET], [$compile_et])
PKG_CHECK_EXISTS([com_err],
  [CPPFLAGS="$CPPFLAGS `$PKG_CONFIG com_err --cflags`"
   LIBS="$LIBS `$PKG_CONFIG com_err --libs`"],
  [AC_MSG_ERROR([`libcom_err' is not available.  Please, install it first.])])

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

# SleepyCat's Berkeley Data Base (BDB), version 4.4
AC_CHECK_LIB([db-4.4], [db_create], [have_libbdb=yes], [have_libbdb=no])
if test "x$have_libbdb" = "xyes"; then
   AC_CHECK_HEADER([db.h], [], [have_libbdb=no])
fi
AM_CONDITIONAL([HAVE_BDB], test "x$have_libbdb" = "xyes")
if test "x$have_libbdb" = "xyes"; then
   AC_DEFINE([HAVE_BDB], 1, [Tells whether SleepyCat's BDB is available.])
   LIBS="$LIBS -ldb-4.4"
else
   AC_MSG_WARN([BDB not found, won't be used])
fi


# QDBM, http://qdbm.sf.net/
PKG_CHECK_EXISTS([qdbm], [
   # Looks like it's available.
   CPPFLAGS="$CPPFLAGS `$PKG_CONFIG qdbm --cflags`"
   LIBS="$LIBS `$PKG_CONFIG qdbm --libs`"

   AC_CHECK_LIB([qdbm], [dpopen], [have_libqdbm=yes], [have_libqdbm=no])
   if test "x$have_libqdbm" = "xyes"; then
      AC_CHECK_HEADER([depot.h], [], [have_libqdbm=no])
      AC_CHECK_HEADER([hovel.h], [], [have_libqdbm=no])
   fi],
   [have_libqdbm=no])

AM_CONDITIONAL([HAVE_QDBM], test "x$have_libqdbm" = "xyes")
if test "x$have_libqdbm" = "xyes"; then
   AC_DEFINE([HAVE_QDBM], 1, [Tells whether libqdbm is available.])
else
   AC_MSG_WARN([`libqdbm' not found, won't be used])
fi

# GNUtls (recommended).
AM_PATH_LIBGNUTLS([1.4.1], [have_gnutls=yes], [have_gnutls=no])
AM_CONDITIONAL([HAVE_GNUTLS], test "x$have_gnutls" = "xyes")
if test "x$have_gnutls" = "xyes"; then
   AC_DEFINE([HAVE_GNUTLS], 1, [Tells whether GNUtls is available.])
fi

# DBus (optional and experimental!).
AC_MSG_CHECKING([whether DBus 1 is available])
PKG_CHECK_EXISTS([dbus-1], [
   CPPFLAGS="$CPPFLAGS `$PKG_CONFIG dbus-1 --cflags`"
   LIBS="$LIBS `$PKG_CONFIG dbus-1 --libs`"
   have_dbus_1="yes"],
   [have_dbus_1="no"])

if test "x$have_dbus_1" = "xyes"; then
   AC_DEFINE([HAVE_DBUS], 1, [Tells whether DBus is available.])
fi
AM_CONDITIONAL([HAVE_DBUS], test "x$have_dbus_1" = "xyes")
AC_MSG_RESULT([$have_dbus_1])

# Avahi.
AC_MSG_CHECKING([whether Avahi is available])
PKG_CHECK_EXISTS([avahi-client], [
   AVAHI_CPPFLAGS="$CPPFLAGS `$PKG_CONFIG avahi-client --cflags`"
   AVAHI_LIBS="$LIBS `$PKG_CONFIG avahi-client --libs`"
   have_avahi="yes"],
   [have_avahi="no"])
if test "x$have_avahi" = "xyes"; then
   AC_DEFINE([HAVE_AVAHI], 1, [Tells whether Avahi is available.])
   AC_SUBST([AVAHI_CPPFLAGS])
   AC_SUBST([AVAHI_LIBS])
fi
AM_CONDITIONAL([HAVE_AVAHI], test "x$have_avahi" = "xyes")
AC_MSG_RESULT([$have_avahi])

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([stdlib.h obstack.h argp.h gdbm.h gcrypt.h com_err.h zlib.h netinet/in.h lightning.h pthread.h])

# libuuid (optional)
AC_CHECK_HEADER([uuid/uuid.h], [have_libuuid=yes], [have_libuuid=no])
if test "x$have_libuuid" = "xyes"; then
   AC_CHECK_LIB([uuid], [uuid_generate], [have_libuuid=yes],
     [have_libuuid=no])
fi
if test "x$have_libuuid" = "xyes"; then
   LIBS="$LIBS -luuid"
   AC_DEFINE([HAVE_LIBUUID], [1], [Define to 1 if libuuid is available.])
fi

AM_CONDITIONAL([HAVE_LIBUUID], [test "x$have_libuuid" = "xyes"])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([cuserid])

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

if test "x$produce_guile_bindings" = "xyes"; then
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
fi

AM_CONDITIONAL(HAVE_GUILE, test "x$guile" != "x")

AC_CONFIG_FILES([Makefile
                 include/Makefile
                 src/Makefile
		 utils/Makefile
		 tests/Makefile
		 tests/include/Makefile
		 tests/interfaces/Makefile
		 tests/features/Makefile
		 guile/Makefile
		 doc/Makefile])

AC_OUTPUT
