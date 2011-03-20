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

(define-module (chop cipher)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop objects)
  #:use-module (chop internal)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (cipher-algorithm?
            cipher-algorithm-key-size
            cipher-algorithm-block-size
            cipher-algorithm-name
            lookup-cipher-algorithm

            cipher-algorithm/idea
            cipher-algorithm/3des
            cipher-algorithm/cast5
            cipher-algorithm/blowfish
            cipher-algorithm/safer-sk128
            cipher-algorithm/des-sk
            cipher-algorithm/aes
            cipher-algorithm/aes192
            cipher-algorithm/aes256
            cipher-algorithm/twofish
            cipher-algorithm/twofish128
            cipher-algorithm/arcfour
            cipher-algorithm/des

            cipher-mode?
            cipher-mode-name
            lookup-cipher-mode

            cipher-mode/ecb
            cipher-mode/cfb
            cipher-mode/cbc
            cipher-mode/stream
            cipher-mode/ofb

            make-cipher
            cipher?
            cipher-algorithm
            cipher-mode
            set-cipher-key!))


;;;
;;; Algorithms.
;;;

(define-record-type <cipher-algorithm>
  ;; Disjoint type for cipher algorithms.
  (make-cipher-algorithm value)
  cipher-algorithm?
  (value cipher-algorithm-value))

(set-record-type-printer! <cipher-algorithm>
                          (lambda (h p)
                            (format p "#<cipher-algorithm ~a>"
                                    (cipher-algorithm-name h))))

(define %cipher-algorithm-value cipher-algorithm-value)

(define %cipher-algorithms
  ;; int -> cipher-algorithm mapping.
  (make-hash-table))

(define-syntax define-cipher-algorithm
  (syntax-rules ()
    ((_ name c-name)
     (begin
       (define name
         (let* ((v (c-integer-value c-name "#include <chop/cipher.h>"))
                (m (make-cipher-algorithm v)))
           (hashv-set! %cipher-algorithms v m)
           m))))))

(define-cipher-algorithm cipher-algorithm/idea "CHOP_CIPHER_IDEA")
(define-cipher-algorithm cipher-algorithm/3des "CHOP_CIPHER_3DES")
(define-cipher-algorithm cipher-algorithm/cast5 "CHOP_CIPHER_CAST5")
(define-cipher-algorithm cipher-algorithm/blowfish "CHOP_CIPHER_BLOWFISH")
(define-cipher-algorithm cipher-algorithm/safer-sk128 "CHOP_CIPHER_SAFER_SK128")
(define-cipher-algorithm cipher-algorithm/des-sk "CHOP_CIPHER_DES_SK")
(define-cipher-algorithm cipher-algorithm/aes "CHOP_CIPHER_AES")
(define-cipher-algorithm cipher-algorithm/aes192 "CHOP_CIPHER_AES192")
(define-cipher-algorithm cipher-algorithm/aes256 "CHOP_CIPHER_AES256")
(define-cipher-algorithm cipher-algorithm/twofish "CHOP_CIPHER_TWOFISH")
(define-cipher-algorithm cipher-algorithm/twofish128 "CHOP_CIPHER_TWOFISH128")
(define-cipher-algorithm cipher-algorithm/arcfour "CHOP_CIPHER_ARCFOUR")
(define-cipher-algorithm cipher-algorithm/des "CHOP_CIPHER_DES")


;;;
;;; Modes.
;;;

(define-record-type <cipher-mode>
  ;; Disjoint type for cipher modes.
  (make-cipher-mode value)
  cipher-mode?
  (value cipher-mode-value))

(set-record-type-printer! <cipher-mode>
                          (lambda (h p)
                            (format p "#<cipher-mode ~a>"
                                    (cipher-mode-name h))))

(define %cipher-mode-value cipher-mode-value)

(define %cipher-modes
  ;; int -> cipher-mode mapping.
  (make-hash-table))

(define-syntax define-cipher-mode
  (syntax-rules ()
    ((_ name c-name)
     (begin
       (define name
         (let* ((v (c-integer-value c-name "#include <chop/cipher.h>"))
                (m (make-cipher-mode v)))
           (hashv-set! %cipher-modes v m)
           m))))))

(define-cipher-mode cipher-mode/ecb "CHOP_CIPHER_MODE_ECB")
(define-cipher-mode cipher-mode/cfb "CHOP_CIPHER_MODE_CFB")
(define-cipher-mode cipher-mode/cbc "CHOP_CIPHER_MODE_CBC")
(define-cipher-mode cipher-mode/stream "CHOP_CIPHER_MODE_STREAM")
(define-cipher-mode cipher-mode/ofb "CHOP_CIPHER_MODE_OFB")


;;;
;;; Accessors.
;;;

(define cipher-algorithm-key-size
  (let ((f (libchop-function size_t "cipher_algo_key_size" (int))))
    (lambda (cipher-algorithm)
      "Return the size of keys consumed by CIPHER-ALGORITHM."
      (f (cipher-algorithm-value cipher-algorithm)))))

(define cipher-algorithm-block-size
  (let ((f (libchop-function size_t "cipher_algo_block_size" (int))))
    (lambda (cipher-algorithm)
      "Return the size of blocks for CIPHER-ALGORITHM."
      (f (cipher-algorithm-value cipher-algorithm)))))

(define cipher-algorithm-name
  (let ((f (libchop-function '* "cipher_algo_name" (int))))
    (lambda (cipher-algorithm)
      "Return the name of CIPHER-ALGORITHM."
      (pointer->string (f (cipher-algorithm-value cipher-algorithm))))))

(define lookup-cipher-algorithm
  (let ((f (libchop-function "cipher_algo_lookup" ('* '*))))
    (lambda (name)
      "Return the cipher algorithm named NAME; raise an exception if not found."
      (let ((out (make-bytevector (sizeof int))))
        (f (string->pointer name) (bytevector->pointer out))
        (hashv-ref %cipher-algorithms (bytevector-uint-ref out 0
                                                           (native-endianness)
                                                           (sizeof int)))))))


(define cipher-mode-name
  (let ((f (libchop-function '* "cipher_mode_name" (int))))
    (lambda (cipher-mode)
      "Return the name of CIPHER-MODE."
      (pointer->string (f (cipher-mode-value cipher-mode))))))

(define lookup-cipher-mode
  (let ((f (libchop-function "cipher_mode_lookup" ('* '*))))
    (lambda (name)
      "Return the cipher mode named NAME; raise an exception if not found."
      (let ((out (make-bytevector (sizeof int))))
        (f (string->pointer name) (bytevector->pointer out))
        (hashv-ref %cipher-modes (bytevector-uint-ref out 0
                                                      (native-endianness)
                                                      (sizeof int)))))))


;;;
;;; Methods.
;;;

(define-wrapped-pointer-type <cipher>
  cipher?
  wrap-cipher unwrap-cipher
  (lambda (c p)
    (format p "#<cipher ~x (~x) ~a ~a>"
            (object-address c)
            (pointer-address (unwrap-cipher c))
            (cipher-algorithm-name (cipher-algorithm c))
            (cipher-mode-name (cipher-mode c)))))

(define %unwrap-cipher unwrap-cipher)

(define %close-cipher
  (dynamic-func "chop_cipher_close" libchop))

(define make-cipher
  (let ((f (libchop-function '* "cipher_open" (int int))))
    (lambda (algorithm mode)
      "Return a new cipher handle for ALGORITHM and MODE."
      (let ((ptr (f (cipher-algorithm-value algorithm)
                    (cipher-mode-value mode))))
        (if (null-pointer? ptr)
            #f
            (begin
              (set-pointer-finalizer! ptr %close-cipher)
              (wrap-cipher ptr)))))))

(define cipher-algorithm
  (let ((f (libchop-function int "cipher_algorithm" ('*))))
    (lambda (cipher)
      "Return the algorithm of CIPHER."
      (hashv-ref %cipher-algorithms
                 (f (unwrap-cipher cipher))))))

(define cipher-mode
  (let ((f (libchop-function int "cipher_mode" ('*))))
    (lambda (cipher)
      "Return the mode of CIPHER."
      (hashv-ref %cipher-modes
                 (f (unwrap-cipher cipher))))))

(define set-cipher-key!
  (let ((f (libchop-function "cipher_set_key" ('* '* size_t))))
    (lambda (cipher key)
      "Use KEY, a bytevector, as the key for CIPHER.  An exception is raised
if the size of KEY is invalid for CIPHER's algorithm."
      (f (unwrap-cipher cipher)
         (bytevector->pointer key)
         (bytevector-length key)))))
