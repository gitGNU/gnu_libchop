GUILE = @GUILE@
G_WRAP_CONFIG = @G_WRAP_CONFIG@

lib_LTLIBRARIES = libguile-chop.la libguile-chop-store-browsers.la

# Suffix of the Guile module directory.
module_suffix = modules/chop

# Installation directory for Guile modules.
chop_moduledir = $(GUILE_SITE)/chop

binding_specs = choppers-spec.scm cipher-spec.scm core-spec.scm		\
		hash-spec.scm indexers-spec.scm stores-spec.scm		\
		streams-spec.scm logs-spec.scm store-stats-spec.scm	\
		filters-spec.scm block-indexers-spec.scm		\
		store-browsers-spec.scm

built_c_files = $(binding_specs:%-spec.scm=%.c)
built_scm_files =							\
  $(addprefix $(module_suffix)/, $(binding_specs:%-spec.scm=%.scm))

nodist_chop_module_DATA = $(built_scm_files)

libguile_chop_la_SOURCES = core-support.c

# Crap.  We can't use $(built_c_files) here because Automake will
# fail to compute the right $(libguile_chop_la_OBJECTS) if we do so.
libguile_chop_la_SOURCES += choppers.c cipher.c core.c hash.c	\
		            indexers.c stores.c streams.c	\
			    logs.c store-stats.c filters.c	\
			    block-indexers.c

libguile_chop_la_LIBADD = -L$(top_builddir)/src -lchop

libguile_chop_store_browsers_la_SOURCES = store-browsers.c
libguile_chop_store_browsers_la_LIBADD =		\
    -L$(top_builddir)/src -lchop-store-browsers -lguile-chop

support_c_files = block-indexers-support.c	\
		  choppers-support.c		\
		  cipher-support.c		\
		  filters-support.c		\
		  hash-support.c		\
		  indexers-support.c		\
		  logs-support.c		\
		  store-browsers-support.c	\
		  store-stats-support.c		\
		  stores-support.c		\
		  streams-support.c


EXTRA_DIST = $(binding_specs) $(support_c_files) README $(TESTS)
CLEANFILES = $(built_c_files) $(built_scm_files)


AM_CPPFLAGS = -I../include $(shell $(G_WRAP_CONFIG) --c-compile-args guile)
AM_CFLAGS = -O0 -Wall -g
LIBS = $(shell $(G_WRAP_CONFIG) --c-link-args guile) -L../src -lchop

## FIXME:  This fixes a g-wrap-config deficiency here.
AM_CPPFLAGS += -I/usr/local/include

%.c $(module_suffix)/%.scm: %-spec.scm
	spec_name="$(basename $(<:%-spec.scm=%))" &&			\
	echo "* generating bindings for \`$$spec_name'..." &&		\
	$(GUILE) $(GUILE_FLAGS) -L $(srcdir) -c				\
	  "(use-modules (g-wrap))					\
	   (use-modules ($$spec_name-spec))				\
	   (generate-wrapset 'guile '$$spec_name \"$$spec_name\")"	\
	&& mkdir -p $(module_suffix)					\
	&& mv "$$spec_name.scm" $(module_suffix)


#
# Test suite.
#

TESTS = testsuite.scm
TESTS_ENVIRONMENT = GUILE=$(builddir)/pre-inst-guile

check_SCRIPTS = testsuite.scm
