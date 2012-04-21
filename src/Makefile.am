# libchop -- a utility library for distributed storage and data backup
# Copyright (C) 2008, 2010, 2011, 2012  Ludovic Courtès <ludo@gnu.org>
# Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)
#
# Libchop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Libchop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with libchop.  If not, see <http://www.gnu.org/licenses/>.

EXTRA_DIST = filter-zip-push-pull.c store-generic-db.c	\
             extract-classes.sh gcrypt-enum-mapping.h	\
	     filter-lzo-common.c

lib_LTLIBRARIES = libchop.la libchop-block-server.la \
                  libchop-store-browsers.la

libchop_la_LDFLAGS =				\
  -version-info 0:0:0				\
  $(LTLIBINTL) $(LIBTIRPC_LIBS)

libchop_block_server_la_LDFLAGS =		\
  -version-info 0:0:0				\
  $(LIBTIRPC_LIBS)

libchop_store_browsers_la_LDFLAGS = -version-info 0:0:0

libchop_la_SOURCES = chop.c logs.c				\
		     chopper-fixed-size.c			\
                     chopper-anchor-based.c			\
		     chopper-whole-stream.c			\
		     streams.c stores.c				\
		     cipher.c hash.c buffers.c			\
		     store-dummy.c				\
		     store-gdbm.c				\
		     store-fs.c					\
		     store-sunrpc.c				\
		     store-filtered.c				\
		     store-smart.c				\
		     store-stat.c				\
		     block-indexers.c				\
		     block-indexer-hash.c block-indexer-chk.c	\
		     block-indexer-integer.c			\
		     indexers.c indexer-tree.c			\
		     filters.c					\
		     filter-zlib-zip.c filter-zlib-unzip.c	\
		     stream-file.c stream-mem.c			\
		     stream-filtered.c				\
		     base32.c

libchop_la_CFLAGS = $(AM_CFLAGS) $(LIBTIRPC_CFLAGS)

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include	\
	      -I$(top_srcdir)/lib     -I$(top_builddir)/lib	\
	      -DGCRYPT_NO_MPI_MACROS -DGCRYPT_NO_DEPRECATED

# Gnulib
libchop_la_LIBADD = $(top_builddir)/lib/libgnu.la

if HAVE_LIBBZ2
libchop_la_SOURCES += filter-bzip2-zip.c filter-bzip2-unzip.c
else
EXTRA_DIST += filter-bzip2-zip.c filter-bzip2-unzip.c
endif

if HAVE_LZO
libchop_la_SOURCES += filter-lzo-zip.c filter-lzo-unzip.c
else
EXTRA_DIST += filter-lzo-zip.c filter-lzo-unzip.c filter-lzo-common.c
endif

if HAVE_TDB
libchop_la_SOURCES += store-tdb.c
else
EXTRA_DIST += store-tdb.c
endif

if HAVE_BDB
libchop_la_SOURCES += store-bdb.c
else
EXTRA_DIST += store-bdb.c
endif

if HAVE_QDBM
libchop_la_SOURCES += store-qdbm.c
else
EXTRA_DIST += store-qdbm.c
endif

if HAVE_LIBUUID
libchop_la_SOURCES += block-indexer-uuid.c
else
EXTRA_DIST += block-indexer-uuid.c
endif

if HAVE_GNUTLS
libchop_la_SOURCES += sunrpc-tls.c
libchop_la_CFLAGS  += $(LIBGNUTLS_CFLAGS) $(LIBGNUTLS_EXTRA_CFLAGS)
libchop_la_LDFLAGS += $(LIBGNUTLS_LIBS) $(LIBGNUTLS_EXTRA_LIBS)
else
EXTRA_DIST += sunrpc-tls.c
endif

if HAVE_DBUS
libchop_la_SOURCES += store-dbus.c
endif

# RPC stubs (for the remote block store)
rpc_interface    = $(top_srcdir)/rpc/block_rstore.x
rpc_xdr          = block_rstore_xdr.c
rpc_client_stubs = block_rstore_clnt.c
rpc_server_stubs = block_rstore_svc.c
rpc_headers      = block_rstore.h \
  $(top_builddir)/include/chop/block_rstore.h

nodist_libchop_la_SOURCES = $(rpc_client_stubs) $(rpc_xdr)
BUILT_SOURCES  = $(rpc_headers) $(rpc_server_stubs)

# The convenience library for block store RPC servers.
libchop_block_server_la_SOURCES   = block-server.c $(rpc_xdr)
libchop_block_server_la_LIBADD    = libchop.la $(AM_LIBS)
libchop_block_server_la_CPPFLAGS  = $(AM_CPPFLAGS)
libchop_block_server_la_CFLAGS    = $(LIBTIRPC_CFLAGS)


if HAVE_AVAHI
libchop_block_server_la_SOURCES  += store-publisher-avahi.c
libchop_block_server_la_LIBADD   += $(AVAHI_LIBS)
libchop_block_server_la_CFLAGS   += $(AVAHI_CFLAGS) $(AM_CFLAGS)
else
EXTRA_DIST += store-publisher-avahi.c
endif

if HAVE_AVAHI
# Convenience library for service discovery.
libchop_store_browsers_la_SOURCES = store-browsers.c		\
				    store-browser-avahi.c
libchop_store_browsers_la_LIBADD   = $(AVAHI_LIBS) libchop.la $(AM_LIBS)
libchop_store_browsers_la_CPPFLAGS = $(AM_CPPFLAGS)
libchop_store_browsers_la_CFLAGS   = $(AVAHI_CFLAGS) $(AM_CFLAGS)
endif

if HAVE_GNUTLS
AM_CFLAGS += $(LIBGNUTLS_CFLAGS) $(LIBGNUTLS_EXTRA_CFLAGS)
endif

BUILT_SOURCES += errors.c

errors.c: $(top_srcdir)/include/chop/errors.h
	( echo "/* Automatically generated, do not edit.  */" ;			\
	  echo "static const char *const error_table[] = {" ;			\
	  cat "$^" | grep '^#define [A-Z_]\+.*-.*$$' |				\
	  sed -e's|#define \([A-Z_]\+\).*/\* \(.*\) \*/$$|  [-\1] = "\2",|g' ;	\
	  echo "  \"Unknown error code\"" ;					\
	  echo "};" )								\
	> "$@.tmp"
	mv "$@.tmp" "$@"

block_rstore_clnt.c: block_rstore.h
	echo "#include \"block_rstore.h\"" > $@ &&	\
	$(RPCGEN) -l < $(rpc_interface) >> $@

block_rstore_xdr.c:  block_rstore.h
	echo "#include \"block_rstore.h\"" > $@ &&	\
	$(RPCGEN) -c < $(rpc_interface) >> $@

block_rstore_svc.c:  block_rstore.h
	echo "#include \"block_rstore.h\"" > $@ &&	\
	$(RPCGEN) -m < $(rpc_interface) >> $@

$(top_builddir)/include/chop/block_rstore.h: block_rstore.h
	cp $< $@

block_rstore.h: $(rpc_interface)
	$(RPCGEN) -h < $^ > $@


BUILT_SOURCES += class-lookup.c

class-lookup.c: $(libchop_la_SOURCES)
	$(srcdir)/extract-classes.sh $^ | \
	$(GPERF) -t -C -N chop_lookup_class_entry > $@

CLEANFILES = $(BUILT_SOURCES) $(nodist_libchop_la_SOURCES)
