BUILT_SOURCES = chop-errors.c
EXTRA_DIST = filter-zip-push-pull.c store-generic-db.c \
             extract-classes.sh gcrypt-enum-mapping.h

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
		     stream-filtered.c  # stream-ext2.c 

if HAVE_TDB
libchop_la_SOURCES += store-tdb.c
endif

if HAVE_BDB
libchop_la_SOURCES += store-bdb.c
endif

if HAVE_QDBM
libchop_la_SOURCES += store-qdbm.c
endif

if HAVE_LIBUUID
libchop_la_SOURCES += block-indexer-uuid.c
endif

if HAVE_DBUS
libchop_la_SOURCES += store-dbus.c
endif

# RPC stubs (for the remote block store)
rpc_xdr =  block_rstore_xdr.c
rpc_client_stubs = block_rstore_clnt.c
rpc_server_stubs = block_rstore_svc.c
rpc_headers = $(top_srcdir)/include/chop/block_rstore.h	\
              $(top_srcdir)/rpc/block_rstore.h
libchop_la_SOURCES += $(rpc_client_stubs) $(rpc_xdr)
BUILT_SOURCES += $(rpc_client_stubs) $(rpc_server_stubs)	\
		 $(rpc_xdr) $(rpc_headers)

# The convenience library for block store RPC servers.
libchop_block_server_la_SOURCES = store-server.c $(rpc_xdr)
libchop_block_server_la_LIBS = -lchop

# Convenience library for service discovery.
libchop_store_browsers_la_SOURCES = store-browsers.c		\
				    store-browser-avahi.c
libchop_store_browsers_la_LIBADD = $(AVAHI_LIBS) -lchop $(AM_LIBS)
libchop_store_browsers_la_CPPFLAGS = $(AVAHI_CPPFLAGS) $(AM_CPPFLAGS)

AM_CPPFLAGS = -I../include -D_GNU_SOURCE=1
AM_CFLAGS = -O0 -g -Winline -Wall -fno-strict-aliasing



chop-errors.c chop-errors.h: chop-errors.et
	$(COMPILE_ET) $^ && mv chop-errors.h $(top_srcdir)/include/chop

block_rstore_clnt.c: $(top_srcdir)/rpc/block_rstore.x \
		     $(top_srcdir)/rpc/block_rstore.h
	$(RPCGEN) -l $< > $@

block_rstore_xdr.c:  $(top_srcdir)/rpc/block_rstore.x \
		     $(top_srcdir)/rpc/block_rstore.h
	$(RPCGEN) -c $< > $@

block_rstore_svc.c:  $(top_srcdir)/rpc/block_rstore.x \
		     $(top_srcdir)/rpc/block_rstore.h
	$(RPCGEN) -m $< > $@

$(top_srcdir)/include/chop/block_rstore.h: $(top_srcdir)/rpc/block_rstore.x
	$(RPCGEN) -h $^ > $@

# We need this one too cause the stub includes it.
$(top_srcdir)/rpc/block_rstore.h: $(top_srcdir)/rpc/block_rstore.x
	$(RPCGEN) -h $^ > $@

if HAVE_GPERF

BUILT_SOURCES += class-lookup.c

class-lookup.c: $(libchop_la_SOURCES)
	./extract-classes.sh $^ | gperf -t -C -N chop_lookup_class_entry > $@

endif
