#!/bin/sh
# Bootstrap the GNU autotools mess.

aclocal && \
libtoolize && \
autoheader && \
automake -a -c --foreign && \
autoconf

