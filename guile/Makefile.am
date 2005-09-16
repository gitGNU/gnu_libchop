GUILE = @GUILE@
G_WRAP_CONFIG = @G_WRAP_CONFIG@

SETGWPATH = "(set! %load-path (cons \"/usr/local/share/guile/site\" %load-path))"
lib_LTLIBRARIES = libguile-chop.la


binding_specs = choppers-spec.scm cipher-spec.scm core-spec.scm		\
		hash-spec.scm indexers-spec.scm stores-spec.scm		\
		streams-spec.scm logs-spec.scm store-stats-spec.scm	\
		filters-spec.scm

built_c_files = $(binding_specs:%-spec.scm=%.c)
built_scm_files = $(binding_specs:%-spec.scm=%.scm)

libguile_chop_la_SOURCES = core-support.c

# Crap.  We can't use $(built_c_files) here because Automake will
# fail to compute the right $(libguile_chop_la_OBJECTS) if we do so.
libguile_chop_la_SOURCES += choppers.c cipher.c core.c hash.c	\
		            indexers.c stores.c streams.c	\
			    logs.c store-stats.c filters.c



EXTRA_DIST = $(binding_specs) README
CLEANFILES = $(built_c_files) $(built_scm_files)


AM_CPPFLAGS = -I../include $(shell $(G_WRAP_CONFIG) --c-compile-args guile)
AM_CFLAGS = -O0 -Wall -g
LIBS = $(shell $(G_WRAP_CONFIG) --c-link-args guile) -L../src -lchop

## FIXME:  This fixes a g-wrap-config deficiency here.
AM_CPPFLAGS += -I/usr/local/include

%.c %.scm: %-spec.scm
	$(GUILE) $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
          (set! %load-path (cons \".\" %load-path)) \
	  (use-modules (g-wrap)) \
	  (use-modules ($(<:%.scm=%))) \
	  (generate-wrapset 'guile '$(<:%-spec.scm=%) \"$(<:%-spec.scm=%)\")"
