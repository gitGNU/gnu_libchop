# libchop -- a utility library for distributed storage and data backup
# Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
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

check_PROGRAMS = filters stores choppers cipher block-indexers indexers

if HAVE_GPERF
check_PROGRAMS += class-lookup
endif

TESTS = $(check_PROGRAMS)

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include	\
	      -I$(top_srcdir)/lib     -I$(top_builddir)/lib	\
	      -I$(top_srcdir)/tests/include

LIBS = $(top_builddir)/src/libchop.la		\
       $(top_builddir)/lib/libgnu.la

distclean-local:
	rm -f ,,t-*.db
