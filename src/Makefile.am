BUILT_SOURCES = chop-errors.c
EXTRA_DIST = filter-zip-push-pull.c store-generic-db.c \
             extract-classes.sh gcrypt-enum-mapping.h

lib_LTLIBRARIES = libchop.la libchop-block-server.la

libchop_la_SOURCES = chop.c chop-errors.c logs.c		\
		     chopper-fixed-size.c			\
                     chopper-anchor-based.c			\
		     stores.c					\
		     store-dummy.c				\
		     store-gdbm.c				\
		     store-remote.c				\
		     store-filtered.c				\
		     cipher.c                                   \
		     hash.c					\
		     buffers.c					\
		     indexer-hash-tree.c			\
		     filters.c					\
		     filter-zlib-zip.c filter-zlib-unzip.c	\
		     stream-file.c stream-mem.c # stream-ext2.c 

if HAVE_TDB
libchop_la_SOURCES += store-tdb.c
endif

# RPC client-side stubs (for the remote block store)
rpc_client_stubs = block_rstore_clnt.c block_rstore_xdr.c
libchop_la_SOURCES += $(rpc_client_stubs)
BUILT_SOURCES += $(rpc_client_stubs)

# The convenience library for block store RPC servers.
libchop_block_server_la_SOURCES = store-server.c
libchop_block_server_la_LIBS = -lchop

CPPFLAGS = -I../include -Wall -fno-strict-aliasing -D_GNU_SOURCE=1
CFLAGS = -O0 -g

COMPILE_ET = compile_et
RPCGEN = rpcgen

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
