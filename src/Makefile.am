EXTRA_DIST = filter-zip-push-pull.c store-generic-db.c	\
             extract-classes.sh gcrypt-enum-mapping.h	\
	     chop-errors.et filter-lzo-common.c

lib_LTLIBRARIES = libchop.la libchop-block-server.la \
                  libchop-store-browsers.la

libchop_la_LDFLAGS = -version-info 0:0:0
libchop_block_server_la_LDFLAGS = -version-info 0:0:0
libchop_store_browsers_la_LDFLAGS = -version-info 0:0:0

libchop_la_SOURCES = chop.c chop-errors.c logs.c		\
		     chopper-fixed-size.c			\
                     chopper-anchor-based.c			\
		     chopper-whole-stream.c			\
		     streams.c stores.c				\
		     cipher.c hash.c buffers.c			\
		     store-dummy.c				\
		     store-gdbm.c				\
		     store-sunrpc.c				\
		     store-filtered.c				\
		     store-smart.c				\
		     store-stat.c				\
		     block-indexers.c				\
		     block-indexer-hash.c block-indexer-chk.c	\
		     indexers.c indexer-tree.c			\
		     filters.c					\
		     filter-zlib-zip.c filter-zlib-unzip.c	\
		     stream-file.c stream-mem.c			\
		     stream-filtered.c

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include	\
	      -I$(top_srcdir)/lib     -I$(top_builddir)/lib	\
	      -D_GNU_SOURCE=1
AM_CFLAGS   = -Winline -Wall -fno-strict-aliasing

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

libchop_la_SOURCES += $(rpc_client_stubs) $(rpc_xdr)
BUILT_SOURCES  = $(rpc_client_stubs) $(rpc_server_stubs)	\
		 $(rpc_xdr) $(rpc_headers)

# The convenience library for block store RPC servers.
libchop_block_server_la_SOURCES   = block-server.c $(rpc_xdr)
libchop_block_server_la_LIBADD    = libchop.la $(AM_LIBS)
libchop_block_server_la_CPPFLAGS  = $(AM_CPPFLAGS)

if HAVE_AVAHI
libchop_block_server_la_SOURCES  += store-publisher-avahi.c
libchop_block_server_la_LIBADD   += $(AVAHI_LIBS)
libchop_block_server_la_CPPFLAGS += $(AVAHI_CPPFLAGS)
else
EXTRA_DIST += store-publisher-avahi.c
endif

if HAVE_AVAHI
# Convenience library for service discovery.
libchop_store_browsers_la_SOURCES = store-browsers.c		\
				    store-browser-avahi.c
libchop_store_browsers_la_LIBADD = $(AVAHI_LIBS) libchop.la $(AM_LIBS)
libchop_store_browsers_la_CPPFLAGS = $(AVAHI_CPPFLAGS) $(AM_CPPFLAGS)
endif

if HAVE_GNUTLS
AM_CFLAGS += $(LIBGNUTLS_CFLAGS) $(LIBGNUTLS_EXTRA_CFLAGS)
endif



chop-errors.c chop-errors.h: chop-errors.et
	$(COMPILE_ET) $^ &&				\
	touch chop-errors.[ch] &&			\
	mv chop-errors.h $(top_builddir)/include/chop

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

if HAVE_GPERF

BUILT_SOURCES += class-lookup.c

class-lookup.c: $(libchop_la_SOURCES)
	$(srcdir)/extract-classes.sh $^ | \
	$(GPERF) -t -C -N chop_lookup_class_entry > $@

endif

CLEANFILES = $(BUILT_SOURCES)
