GUILE = @GUILE@
G_WRAP_CONFIG = @G_WRAP_CONFIG@

SETGWPATH = "(set! %load-path (cons \"/usr/local/share/guile/site\" %load-path))"
lib_LTLIBRARIES = libguile-chop.la

libguile_chop_la_SOURCES = core.c hash.c cipher.c streams.c	\
                           stores.c choppers.c indexers.c

EXTRA_DIST = choppers-spec.scm cipher-spec.scm core-spec.scm	\
             hash-spec.scm indexers-spec.scm stores-spec.scm	\
             streams-spec.scm


CPPFLAGS += -I../include $(shell $(G_WRAP_CONFIG) --c-compile-args guile)
LIBS += $(shell $(G_WRAP_CONFIG) --c-link-args guile) -L../src -lchop

## FIXME:  This fixes a g-wrap-config deficiency here.
CPPFLAGS += -I/usr/local/include

%.c %.scm: %-spec.scm
	$(GUILE) $(GUILE_FLAGS) -c  \
	  ${SETGWPATH}" \
          (set! %load-path (cons \".\" %load-path)) \
	  (use-modules (g-wrap)) \
	  (use-modules ($(<:%.scm=%))) \
	  (generate-wrapset 'guile '$(<:%-spec.scm=%) \"$(<:%-spec.scm=%)\")"
