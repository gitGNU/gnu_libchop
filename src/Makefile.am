BUILT_SOURCES = chop-errors.c
#extra_HEADERS = chop-errors.h

lib_LTLIBRARIES = libchop.la

libchop_la_SOURCES = chop.c chop-errors.c \
		     stream-file.c # stream-ext2.c 

COMPILE_ET = compile_et

chop-errors.c: chop-errors.et
	$(COMPILE_ET) $^
