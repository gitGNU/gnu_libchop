BUILT_SOURCES = chop-errors.c
#extra_HEADERS = chop-errors.h

lib_LTLIBRARIES = libchop.la

libchop_la_SOURCES = chop.c chop-errors.c logs.c		\
		     chopper-fixed-size.c			\
                     chopper-anchor-based.c			\
		     store-dummy.c				\
		     store-gdbm.c				\
		     store-remote.c				\
		     store-filtered.c				\
		     hash.c					\
		     buffers.c					\
		     indexer-hash-tree.c			\
		     filters.c					\
		     filter-zlib-zip.c filter-zlib-unzip.c	\
		     stream-file.c # stream-ext2.c 

# RPC client-side stubs (for the remote block store)
libchop_la_SOURCES += block_rstore_clnt.c block_rstore_xdr.c

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

$(top_srcdir)/rpc/block_rstore.h: $(top_srcdir)/rpc/block_rstore.x
	$(RPCGEN) -h $^ > $@
