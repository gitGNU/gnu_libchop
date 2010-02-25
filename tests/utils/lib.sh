# libchop -- a utility library for distributed storage and data backup
# Copyright (C) 2010  Ludovic Courtès <ludo@gnu.org>
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

# This is an Autotest-like interface.  It's more lightweight though,
# perhaps slightly less portable but the goal is to use only POSIX
# shell constructs.

if test "x$BASH" != "x"
then
    set -o posix
fi

set -x

chop_fail_if()
{
    if eval "$@"
    then
	echo "failure condition \`$@' is true"
	chop_cleanup
	exit 1
    fi
}

chop_cleanup()
{
    rm -f $chop_CLEANFILES
    if test "x$chop_CLEANUP_HOOK" != "x"
    then
	eval "$chop_CLEANUP_HOOK"
    fi
}
