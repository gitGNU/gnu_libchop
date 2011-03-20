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

(define-module (chop hash)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop objects)
  #:use-module (chop internal)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (hash-method?
            hash-size
            hash-method-name
            lookup-hash-method
            bytevector-hash

            hash-method/sha1
            hash-method/rmd160
            hash-method/md5
            hash-method/md4
            hash-method/md2
            hash-method/tiger
            hash-method/haval
            hash-method/sha256
            hash-method/sha384
            hash-method/sha512))

(define-record-type <hash-method>
  ;; Disjoint type for hash methods.
  (make-hash-method value)
  hash-method?
  (value hash-method-value))

(set-record-type-printer! <hash-method>
                          (lambda (h p)
                            (format p "#<hash-method ~a>"
                                    (hash-method-name h))))

(define %hash-method-value hash-method-value)

(define %hash-methods
  ;; int -> hash-method mapping.
  (make-hash-table))

(define-syntax define-hash-method
  (syntax-rules ()
    ((_ name c-name)
     (begin
       (define name
         (let* ((v (c-integer-value c-name "#include <chop/hash.h>"))
                (m (make-hash-method v)))
           (hashv-set! %hash-methods v m)
           m))))))

(define-hash-method hash-method/sha1 "CHOP_HASH_SHA1")
(define-hash-method hash-method/rmd160 "CHOP_HASH_RMD160")
(define-hash-method hash-method/md5 "CHOP_HASH_MD5")
(define-hash-method hash-method/md4 "CHOP_HASH_MD4")
(define-hash-method hash-method/md2 "CHOP_HASH_MD2")
(define-hash-method hash-method/tiger "CHOP_HASH_TIGER")
(define-hash-method hash-method/haval "CHOP_HASH_HAVAL")
(define-hash-method hash-method/sha256 "CHOP_HASH_SHA256")
(define-hash-method hash-method/sha384 "CHOP_HASH_SHA384")
(define-hash-method hash-method/sha512 "CHOP_HASH_SHA512")


;;;
;;; Method.
;;;

(define hash-size
  (let ((f (libchop-function size_t "hash_size" (int))))
    (lambda (hash-method)
      "Return the size of a hash produced by HASH-METHOD."
      (f (hash-method-value hash-method)))))

(define hash-method-name
  (let ((f (libchop-function '* "hash_method_name" (int))))
    (lambda (hash-method)
      "Return the name of HASH-METHOD."
      (pointer->string (f (hash-method-value hash-method))))))

(define lookup-hash-method
  (let ((f (libchop-function "hash_method_lookup" ('* '*))))
    (lambda (name)
      "Return the hash method named NAME; raise an exception if not found."
      (let ((out (make-bytevector (sizeof int))))
        (f (string->pointer name) (bytevector->pointer out))
        (hashv-ref %hash-methods (bytevector-uint-ref out 0
                                                      (native-endianness)
                                                      (sizeof int)))))))
(define bytevector-hash
  (let ((f (libchop-function void "hash_buffer"
                             (int '* size_t '*))))
    (lambda (hash-method bv)
      "Return the hash of BV using HASH-METHOD."
      (let* ((size (hash-size hash-method))
             (out  (make-bytevector size)))
        (f (hash-method-value hash-method)
           (bytevector->pointer bv)
           (bytevector-length bv)
           (bytevector->pointer out))
        out))))
