;;; Copyright (C) 2011  Ludovic Courtès <ludo@gnu.org>
;;;
;;; Libchop is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Libchop is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with libchop.  If not, see <http://www.gnu.org/licenses/>.

(define-module (chop filters)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop objects)
  #:use-module (chop internal)
  #:export (filter?

            make-zlib-zip-filter
            make-bzip2-zip-filter
            make-lzo-zip-filter
            make-zip-filter
            %default-compression-level

            make-zlib-unzip-filter
            make-bzip2-unzip-filter
            make-lzo-unzip-filter
            make-unzip-filter

            error/filter-full
            error/filter-empty
            error/filter-unhandled-fault
            error/filter-error))

(define-libchop-type filter "filter"
  filter?
  wrap-filter unwrap-filter)

(define-error-code error/filter-full "CHOP_FILTER_FULL")
(define-error-code error/filter-empty "CHOP_FILTER_EMPTY")
(define-error-code error/filter-unhandled-fault "CHOP_FILTER_UNHANDLED_FAULT")
(define-error-code error/filter-error "CHOP_FILTER_ERROR")


;;;
;;; Constructors.
;;;

(define make-zlib-zip-filter
  (let ((f (libchop-type-constructor "zlib_zip_filter_init"
                                     (int size_t)
                                     "zlib_zip_filter"
                                     wrap-filter)))
    (lambda* (#:optional (compression-level -1) (input-size 0))
      "Return a zlib-based compression filter with compression level
COMPRESSION-LEVEL (an integer between 0 and 9) with an input buffer of
INPUT-SIZE bytes.  If COMPRESSION-LEVEL is -1 or omitted, then zlib's default
compression level is used.  If INPUT-SIZE is zero or omitted, then a default
size is used."
      (f compression-level input-size))))

(define make-bzip2-zip-filter
  (and (lookup-class "bzip2_zip_filter")
       (let ((f (libchop-type-constructor "bzip2_zip_filter_init"
                                          (size_t size_t size_t)
                                          "bzip2_zip_filter"
                                          wrap-filter)))
         (lambda* (#:optional (block-count-100k 0) (work-factor 0)
                              (input-size 0))
           "Return a bzip2-based compression filter using BLOCK-COUNT-100K
blocks for compression internally (the higher, the better), and using
WORK-FACTOR to determine how compression behaves when presented the worst
case repetitive input (see the `libbzip2' manual for details).  The returned
filter will internally use a buffer of INPUT-SIZE bytes."
           (f block-count-100k work-factor input-size)))))

(define make-lzo-zip-filter
  (and (lookup-class "lzo_zip_filter")
       (let ((f (libchop-type-constructor "lzo_zip_filter_init"
                                          (size_t)
                                          "lzo_zip_filter"
                                          wrap-filter)))
         (lambda* (#:optional (input-size 0))
           "Return an LZO compression filter, using a input buffer of INPUT-SIZE
bytes.  In practice, since LZO is state-less, this means that input data will
be compressed by blocks of INPUT-SIZE bytes.  Thus, it is necessary to use a
reasonably large buffer size to obtain reasonable compression.  If INPUT-SIZE
is zero, a reasonable default is used."
           (f input-size)))))

(define make-zlib-unzip-filter
  (let ((f (libchop-type-constructor "zlib_unzip_filter_init"
                                     (size_t)
                                     "zlib_unzip_filter"
                                     wrap-filter)))
    (lambda* (#:optional (input-size 0))
      "Return a zlib-based decompression filter with an input buffer of
INPUT-SIZE bytes.  If INPUT-SIZE is zero or omitted, then a default size is
used."
      (f input-size))))

(define make-bzip2-unzip-filter
  (and (lookup-class "bzip2_unzip_filter")
       (let ((f (libchop-type-constructor "bzip2_unzip_filter_init"
                                          (int size_t)
                                          "bzip2_unzip_filter"
                                          wrap-filter)))
         (lambda* (#:optional (small? #f) (input-size 0))
           "Return a bzip2 decompression filter with an input buffer of
INPUT-SIZE bytes.  If INPUT-SIZE is zero or omitted, then a default size is
used.  If SMALL? is true, then `libbzip2' will use an alternate decompression
algorithm that is slower but uses less memory."
           (f (if small? 1 0) input-size)))))

(define make-lzo-unzip-filter
  (and (lookup-class "lzo_unzip_filter")
       (let ((f (libchop-type-constructor "lzo_unzip_filter_init"
                                          (size_t)
                                          "lzo_unzip_filter"
                                          wrap-filter)))
         (lambda* (#:optional (input-size 0))
           "Return a lzo decompression filter with an input buffer of
INPUT-SIZE bytes.  In practice, the input buffer will be grown as needed
anyway."
           (f input-size)))))


;;;
;;; Generic constructors.
;;;

(define %default-compression-level
  (c-integer-value "CHOP_ZIP_FILTER_DEFAULT_COMPRESSION"
                   "#include <chop/filters.h>"))

(define* (make-zip-filter class
                          #:optional
                          (compression-level %default-compression-level)
                          (input-size 8192))
  "Return a zip filter of type CLASS with the given parameters."
  (and (object-is-a? class (lookup-class "zip_filter_class"))
       (let* ((z (gc-malloc (class-instance-size class)))
              (p (libchop-slot-ref "zip_filter_class" "generic_open" '*
                                   (unwrap-class class)
                                   "#include <chop/filters.h>"))
              (f (pointer->procedure chop-error-t p `(,int ,size_t *)))
              (e (f compression-level input-size z)))
         (if (= e 0)
             (register-libchop-object! (wrap-filter z))
             (raise-chop-error e)))))

(define* (make-unzip-filter class #:optional (input-size 8192))
  "Return an unzip filter of type CLASS with the given parameters."
  (and (object-is-a? class (lookup-class "unzip_filter_class"))
       (let* ((z (gc-malloc (class-instance-size class)))
              (p (libchop-slot-ref "unzip_filter_class" "generic_open" '*
                                   (unwrap-class class)
                                   "#include <chop/filters.h>"))
              (f (pointer->procedure chop-error-t p `(,size_t *)))
              (e (f input-size z)))
         (if (= e 0)
             (register-libchop-object! (wrap-filter z))
             (raise-chop-error e)))))
