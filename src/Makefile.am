BUILT_SOURCES = chop-errors.c
#extra_HEADERS = chop-errors.h

lib_LTLIBRARIES = libchop.la

libchop_la_SOURCES = chop.c chop-errors.c \
		     chopper-fixed-size.c \
		     store-gdbm.c         \
		     store-hash-tree.c    \
		     buffers.c            \
		     stream-file.c # stream-ext2.c 

CPPFLAGS = -I../include -Wall

COMPILE_ET = compile_et

chop-errors.c, chop-errors.h: chop-errors.et
	$(COMPILE_ET) $^
