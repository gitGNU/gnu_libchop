BUILT_SOURCES = chop-errors.c
#extra_HEADERS = chop-errors.h

lib_LTLIBRARIES = libchop.la

libchop_la_SOURCES = chop.c chop-errors.c \
		     chopper-fixed-size.c \
		     store-dummy.c        \
		     store-gdbm.c         \
		     hash.c		  \
		     buffers.c            \
		     indexer-hash-tree.c  \
		     stream-file.c # stream-ext2.c 

CPPFLAGS = -I../include -Wall -fno-strict-aliasing

COMPILE_ET = compile_et

chop-errors.c, chop-errors.h: chop-errors.et
	$(COMPILE_ET) $^ && mv chop-errors.h $(top_srcdir)/include/chop
