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

ACLOCAL_AMFLAGS  = -I m4

SUBDIRS = po lib src utils include doc

if HAVE_GUILE2
SUBDIRS += guile2
endif

SUBDIRS += tests

EXTRA_DIST = rpc/block_rstore.x

dist-hook: gen-ChangeLog gen-tarball-version

.PHONY: gen-ChangeLog
gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    --since=2004 > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

EXTRA_DIST += $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo $(VERSION) > $@-t && mv $@-t $@
gen-tarball-version:
	echo $(VERSION) > $(distdir)/.tarball-version
.PHONY: gen-tarball-version
