#!/bin/sh
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

# Write a list of the built-in classes' names, looking at the given source
# files, in a way that is suitable as a `gperf' input.

echo '%{'
echo '#include <chop/chop.h>'
echo '#include <chop/objects.h>'
echo '#include <chop/indexers.h>'
echo '#include <chop/filters.h>'
echo '%}'
echo
echo 'struct chop_class_entry { const char *name; const void *class; };'
echo '%%'

cat $@ | \
grep '^CHOP_DEFINE_RT_CLASS' | \
sed -es'/^CHOP_DEFINE_RT_CLASS\(_WITH_METACLASS\)\? *(\([a-zA-Z0-9_]\+\),.*$/\2, \&chop_\2_class/g'
