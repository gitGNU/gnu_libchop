#!/bin/sh
# Bootstrap the GNU Build System.

if [ ! -f build-aux/git-version-gen ]
then
    echo "Please run \`gnulib-tool --update' first." >&2
    echo "See <http://www.gnu.org/software/gnulib/> for more info about Gnulib." >&2
    exit 1
fi

exec autoreconf -vfi
