# libchop -- a utility library for distributed storage and data backup
# Copyright (C) 2008, 2010, 2011  Ludovic Courtès <ludo@gnu.org>
# Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)
#
# Libchop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Libchop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with libchop.  If not, see <http://www.gnu.org/licenses/>.

noinst_HEADERS = include/testsuite.h

check_PROGRAMS =				\
  interfaces/filters				\
  interfaces/stores				\
  interfaces/choppers				\
  interfaces/cipher				\
  interfaces/block-indexers			\
  interfaces/indexers				\
  interfaces/deserialize			\
  interfaces/class-lookup

check_PROGRAMS +=				\
  features/filter-zip				\
  features/stream-filtered			\
  features/chopper-anchor-based			\
  features/stream-indexing			\
  features/base32				\
  features/block-indexer-integrity

check_SCRIPTS =					\
  utils/archiver				\
  utils/archiver-fd				\
  utils/block-server

if HAVE_GUILE2

check_SCRIPTS += utils/backup

endif

EXTRA_DIST =					\
  $(check_SCRIPTS)				\
  utils/lib.sh					\
  utils/tls-dh-params				\
  utils/tls-rsa-params				\
  utils/test-key.dsa.pub			\
  utils/test-key.dsa.sec			\
  utils/test-key.rsa.pub			\
  utils/test-key.rsa.sec

TESTS = $(check_PROGRAMS) $(check_SCRIPTS)

# The test scripts need this environment.
TESTS_ENVIRONMENT =						\
  PATH="$(top_builddir)/utils:$$PATH"				\
  GUILE_LOAD_PATH="$(abs_top_srcdir)/guile2:$(abs_top_builddir)/guile2:$$GUILE_LOAD_PATH" \
  libchop_libdir="$(abs_top_builddir)/src"			\
  top_srcdir="$(abs_top_srcdir)" srcdir="$(abs_srcdir)/utils"

AM_CFLAGS = $(GCC_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include	\
	      -I$(top_srcdir)/lib     -I$(top_builddir)/lib	\
	      -I$(top_srcdir)/tests/include

LIBS = $(top_builddir)/src/libchop.la		\
       $(top_builddir)/lib/libgnu.la
