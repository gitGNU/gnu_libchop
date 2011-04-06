#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([libchop],
        m4_esyscmd([build-aux/git-version-gen .tarball-version]),
        [libchop-devel@nongnu.org])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.11 gnu silent-rules std-options subdir-objects \
 color-tests parallel-tests])

AC_CONFIG_SRCDIR(src/chop.c)
AC_CONFIG_HEADER(include/chop/chop-config.h)
AC_CONFIG_MACRO_DIR(m4)

AC_ARG_ENABLE([guile-bindings],
	AS_HELP_STRING([--enable-guile-bindings],
	  [produce GNU Guile 1.8 bindings (requires Guile 1.8 and G-Wrap)]),
	[produce_guile_bindings="$enableval"],
	[produce_guile_bindings="no"])

AC_ARG_WITH([dbus],
	AS_HELP_STRING([--with-dbus],
	  [Use D-BUS client/servers (experimental)]),
	[use_dbus="$withval"],
	[use_dbus="no"])

AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])
   

dnl Checks for programs.
AC_PROG_CC_C99
gl_EARLY
AM_PROG_CC_C_O

LT_INIT([disable-static])

# Gnulib
gl_INIT

# GCC specifics.
if test "x$GCC" = "xyes"; then
  # We absolutely need `-fno-strict-aliasing' because of the casts
  # used to deal with polymorphism.
  CFLAGS="-fno-strict-aliasing $CFLAGS"
  CFLAGS="-Wall -Wmissing-prototypes -Wpointer-arith $CFLAGS"
  CFLAGS="-Wdeclaration-after-statement -Wcast-align $CFLAGS"
fi

dnl ONC RPC's `rpcgen'.
AC_PATH_PROG([RPCGEN], [rpcgen])
if test "x$RPCGEN" = "x"; then
   AC_MSG_ERROR([`rpcgen' not found.  It is part of GNU C Library.])
fi
AC_SUBST([RPCGEN])

dnl Checkpoint the cache
AC_CACHE_SAVE

dnl Checks for libraries.
AM_PATH_LIBGCRYPT([1.2.0], [has_libgcrypt=yes], [has_libgcrypt=no])
if test "x$has_libgcrypt" != "xyes"; then
   AC_MSG_ERROR([GNU libgcrypt 1.2+ not found.  Please, install it first.])
else
   CPPFLAGS="$CPPFLAGS $LIBGCRYPT_CFLAGS"
   LIBS="$LIBS $LIBGCRYPT_LIBS"
fi

AC_CHECK_LIB([gdbm], [gdbm_open], [],
  [AC_MSG_ERROR([`libgdbm' not found.  Come on, install it first.])])
AC_CHECK_LIB([z], [deflate], [],
  [AC_MSG_ERROR([`libz' not found.  Too bad.])])


#
# Optional libraries
#

# libbz2 (bzip2 compression)
AC_CHECK_LIB([bz2], [BZ2_bzCompress], [have_libbz2=yes], [have_libbz2=no])
if test "x$have_libbz2" = "xyes"; then
  AC_CHECK_HEADER([bzlib.h], [], [have_libbz2=no])
fi
AM_CONDITIONAL([HAVE_LIBBZ2], test "x$have_libbz2" = "xyes")
if test "x$have_libbz2" = "xyes"; then
   AC_DEFINE([HAVE_LIBBZ2], 1, [Tells whether `libbz2' is available.])
   LIBS="$LIBS -lbz2"
else
   AC_MSG_WARN([`libbz2' not found, won't be used.])
fi


dnl LZO (GPL'd, lightweight, fast compression)
AC_CHECK_LIB([lzo2], [lzo1x_decompress], [have_lzo=yes], [have_lzo=no])
if test "x$have_lzo" = "xyes"; then
   AC_CHECK_HEADER([lzo/lzo1x.h], [], [have_lzo=no])
fi
AM_CONDITIONAL([HAVE_LZO], test "x$have_lzo" = "xyes")
if test "x$have_lzo" = "xyes"; then
   AC_DEFINE([HAVE_LZO], 1, [Tells whether the LZO library is available.])
   LIBS="$LIBS -llzo2"
else
   AC_MSG_WARN([`liblzo2' not found, won't be used.])
fi


# TDB, the Trivial Database (part of Samba)
# XXX: We could use `pkg-config' but `tdb.pc' was not available in TDB 1.0.x.
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

# SleepyCat/Oracle Berkeley Data Base (BDB), version 4.x.
AC_CHECK_LIB([db-4], [db_create], [have_libbdb=yes], [have_libbdb=no])
if test "x$have_libbdb" = "xyes"; then
   AC_CHECK_HEADER([db.h], [], [have_libbdb=no])
fi
AM_CONDITIONAL([HAVE_BDB], test "x$have_libbdb" = "xyes")
if test "x$have_libbdb" = "xyes"; then
   AC_DEFINE([HAVE_BDB], 1, [Tells whether SleepyCat's BDB is available.])
   LIBS="$LIBS -ldb-4"
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

dnl GnuTLS (recommended).
PKG_CHECK_MODULES([LIBGNUTLS], [gnutls], [have_gnutls=yes], [have_gnutls=no])
PKG_CHECK_MODULES([LIBGNUTLS_EXTRA], [gnutls-extra], [have_gnutls_extra=yes], [have_gnutls_extra=no])
AM_CONDITIONAL([HAVE_GNUTLS],
  test "x$have_gnutls" = "xyes" && test "x$have_gnutls_extra" = "xyes")
if test "x$have_gnutls" = "xyes" && test "x$have_gnutls_extra" = "xyes"; then
   AC_DEFINE([HAVE_GNUTLS], 1, [Tells whether GnuTLS is available.])
fi

# DBus (optional and experimental!).
if test "x$use_dbus" = "xyes"; then
   AC_MSG_CHECKING([whether DBus 1 is available])
   PKG_CHECK_EXISTS([dbus-1], [
      CPPFLAGS="$CPPFLAGS `$PKG_CONFIG dbus-1 --cflags`"
      LIBS="$LIBS `$PKG_CONFIG dbus-1 --libs`"
      have_dbus_1="yes"],
      [have_dbus_1="no"])

   if test "x$have_dbus_1" = "xyes"; then
      AC_DEFINE([HAVE_DBUS], 1, [Tells whether DBus is available.])
   fi
   AC_MSG_RESULT([$have_dbus_1])
fi
AM_CONDITIONAL([HAVE_DBUS],
  test "x$use_dbus" = "xyes" && test "x$have_dbus_1" = "xyes")


# Avahi.
AC_MSG_CHECKING([whether Avahi is available])
PKG_CHECK_MODULES([AVAHI], [avahi-client], [have_avahi="yes"], [have_avahi="no"])
if test "x$have_avahi" = "xyes"; then
   AC_DEFINE([HAVE_AVAHI], 1, [Tells whether Avahi is available.])
fi
AM_CONDITIONAL([HAVE_AVAHI], test "x$have_avahi" = "xyes")

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([stdlib.h stdarg.h argp.h gdbm.h gcrypt.h zlib.h \
  netinet/in.h lightning.h pthread.h valgrind/memcheck.h])

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
AC_C_INLINE
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([cuserid])

# Checkpoint the cache
AC_CACHE_SAVE

dnl Look for GNU Gperf (used for O(1) class lookup).
AC_PATH_PROG([GPERF], [gperf])
if test "x$ac_cv_path_GPERF" = "x"; then
   AC_MSG_ERROR([GNU Gperf not found.  Please install it.])
fi
AC_SUBST([GPERF])

if test "x$produce_guile_bindings" = "xyes"; then
  # Check for Guile and G-Wrap
  AC_PATH_PROG([guile], [guile])

  if test "x$guile" != "x"; then
     GUILE_PROGS
     GUILE_FLAGS
     GUILE_SITE_DIR

  #   AC_CHECK_LIB([guile], [scm_u8vector_elements], [],
  #     AC_MSG_ERROR([You version of Guile doesn't support SRFI-4 vectors.  Upgrade to 1.7.2 or later.]))

     AC_PATH_PROG([g_wrap_config], [g-wrap-config])
     if test "x$g_wrap_config" = "x"; then
       AC_MSG_ERROR([Could not find `g-wrap-config'.  You must have G-Wrap to produce Guile bindings.])
     else
       AC_MSG_CHECKING([whether G-Wrap's version is 1.9.7+])
       if "$g_wrap_config" --version | grep ['1.9.[7-9]'] > /dev/null || \
	  "$g_wrap_config" --version | grep ['1.9.[1-9][0-9]'] > /dev/null ; then
	 AC_MSG_RESULT([yes])
       else
	 AC_MSG_RESULT([no])
	 AC_MSG_ERROR([Please, use the right G-Wrap version.])
       fi

       # Make sure G-Wrap is usable, i.e., its modules are in
       # `$GUILE_LOAD_PATH' or some such.
       GUILE_MODULE_AVAILABLE([have_g_wrap_module], [(g-wrap)])
       if test "x$have_g_wrap_module" = "xno"; then
	  AC_MSG_ERROR([Module `(g-wrap)' is not in Guile's load path.])
       fi

       AC_SUBST(GUILE, $guile)
       AC_SUBST(G_WRAP_CONFIG, $g_wrap_config)
     fi
  else
    AC_MSG_WARN([GNU Guile not found.  Not building Guile bindings.])
  fi
fi

if test "x$guile" != "x"; then
   have_guile="yes"
else
   have_guile="no"
fi
AM_CONDITIONAL([HAVE_GUILE], [test "x$guile" != "x"])

#
# GNU Guile 2.x.
#

guile2=""
PKG_CHECK_EXISTS([guile-2.0],
  [if test "x`$PKG_CONFIG guile-2.0 --variable bindir`" = "x"; then
     AC_PATH_PROG([GUILE], [guile])
   else
     GUILE="`$PKG_CONFIG guile-2.0 --variable bindir`"
   fi
   AC_SUBST([GUILE])
   guile2="$GUILE"])

if test "x$guile2" = "x"; then
   have_guile2="no"
else
   have_guile2="yes"
fi
AM_CONDITIONAL([HAVE_GUILE2], [test "x$have_guile2" = "xyes"])

if test "x$guilemoduledir" = "x"; then
  guilemoduledir="$datarootdir/guile/site/2.0"
fi
AC_SUBST([guilemoduledir])

if test "x$produce_guile_bindings" = "xyes"; then
  if test "$guilemoduledir" != "$GUILE_SITE"; then
     # Guile won't be able to locate the module "out of the box", so
     # warn the user.
     AC_MSG_WARN([`guilemoduledir' ($guilemoduledir) is different from `GUILE_SITE' ($GUILE_SITE).])
     AC_MSG_WARN([Make sure to adjust the `GUILE_LOAD_PATH' environment variable accordingly,])
     AC_MSG_WARN([or re-run `configure' with `--with-guilemoduledir=$GUILE_SITE'.])
  fi
fi

dnl Summary.
AC_MSG_NOTICE([Storage])
AC_MSG_NOTICE([  GNU dbm ........................ yes])
AC_MSG_NOTICE([  tdb ............................ $have_libtdb])
AC_MSG_NOTICE([  bdb ............................ $have_libbdb])
AC_MSG_NOTICE([  qdbm ........................... $have_libqdbm])
AC_MSG_NOTICE([  libuuid ........................ $have_libuuid])
AC_MSG_NOTICE([Compression])
AC_MSG_NOTICE([  zlib ........................... yes])
AC_MSG_NOTICE([  libbz2 ......................... $have_libbz2])
AC_MSG_NOTICE([  lzo ............................ $have_lzo])
AC_MSG_NOTICE([Networking])
AC_MSG_NOTICE([  GnuTLS ......................... $have_gnutls])
AC_MSG_NOTICE([  Avahi .......................... $have_avahi])
AC_MSG_NOTICE([Languages])
AC_MSG_NOTICE([  GNU Guile 1.8 .................. $have_guile])
AC_MSG_NOTICE([  GNU Guile 2.x .................. $have_guile2])

AC_CONFIG_FILES([Makefile
		 guile/pre-inst-guile
		 lib/Makefile
                 include/Makefile
                 src/Makefile
		 utils/Makefile
		 utils/chop-vcs
		 tests/Makefile
		 guile/Makefile
		 guile2/Makefile
		 doc/Makefile])

AC_CONFIG_COMMANDS([pre-inst-guile-exec], [chmod +x guile/pre-inst-guile])
AC_CONFIG_COMMANDS([chop-vcs-exec], [chmod +x utils/chop-vcs])

# The `config.h' file is needed by Gnulib modules.
AC_CONFIG_LINKS(lib/config.h:include/chop/chop-config.h)

AC_OUTPUT
