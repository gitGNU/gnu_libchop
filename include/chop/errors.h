/* libchop -- a utility library for distributed storage
   Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
   Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)

   Libchop is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Libchop is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with libchop.  If not, see <http://www.gnu.org/licenses/>.  */

/* Error codes.  */

#ifndef CHOP_ERRORS_H
#define CHOP_ERRORS_H

/* POSIX says that error codes in <errno.h> should be strictly positive.
   Thus negative numbers are used for libchop-specific errors, and positive
   `chop_error_t' values are interpreted as `errno'.  */
typedef int chop_error_t;

#define CHOP_OK                       0 /* Success */
#define CHOP_ERR_UNKNOWN_STREAM      -1 /* Unknown stream type */
#define CHOP_ERR_UNKNOWN_STORE       -2 /* Unknown store type */
#define CHOP_ERR_NOT_FOUND           -3 /* Item not found */
#define CHOP_ERR_NOT_IMPL            -4 /* Operation not implemented */
#define CHOP_INVALID_ARG             -5 /* Invalid argument */
#define CHOP_OUT_OF_RANGE_ARG        -6 /* Argument out of range */
#define CHOP_STREAM_END              -7 /* End of stream */
#define CHOP_FILTER_FULL             -8 /* Filter is full */
#define CHOP_FILTER_EMPTY            -9 /* Filter is empty */
#define CHOP_FILTER_UNHANDLED_FAULT -10 /* Filter fault not handled */
#define CHOP_FILTER_ERROR           -11 /* Generic filter error */
#define CHOP_STORE_ERROR            -12 /* Block store generic error */
#define CHOP_STORE_END              -13 /* End of block store */
#define CHOP_BLOCK_INDEXER_ERROR    -14 /* Block indexer generic error */
#define CHOP_BLOCK_FETCHER_ERROR    -15 /* Block fetcher generic error */
#define CHOP_INDEXER_ERROR          -16 /* Stream indexer generic error */
#define CHOP_INDEXER_EMPTY_SOURCE   -17 /* The indexer's input chopper produced zero bytes */
#define CHOP_STORE_BLOCK_UNAVAIL    -18 /* Block is unavailable in the underlying block store */
#define CHOP_DESERIAL_TOO_SHORT     -19 /* Deserialization buffer is too short */
#define CHOP_DESERIAL_CORRUPT_INPUT -20 /* Deserialization input buffer corrupted */
#define CHOP_CIPHER_ERROR           -21 /* Generic cipher error */
#define CHOP_CIPHER_WEAK_KEY        -22 /* Weak encryption key detected */

#endif
