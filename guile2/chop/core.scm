;;; Copyright (C) 2010  Ludovic Courtès <ludo@gnu.org>
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

(define-module (chop core)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (chop internal)
  #:export (error-message
            bytevector->hex-string
            bytevector->base32-string
            hex-string->bytevector
            base32-string->bytevector

            error/ok
            error/not-found
            error/not-impl
            error/invalid-arg
            error/out-of-range-arg
            error/deserial-too-short
            error/deserial-corrupt-input))


;;;
;;; Utility functions.
;;;

(define error-message
  (let ((f (libchop-function '* "error_message" (chop-error-t))))
    (lambda (err)
      (pointer->string (f err)))))

(define bytevector->hex-string
  (let ((f (libchop-function void "buffer_to_hex_string"
                             ('* size_t '*))))
    (lambda (bv)
      (let* ((len (bytevector-length bv))
             (in  (bytevector->pointer bv))
             (out (bytevector->pointer (make-bytevector (+ 1 (* len 2)) 7))))
        (f in len out)
        (substring (pointer->string out) 0 (* 2 len))))))

(define bytevector->base32-string
  (let ((f (libchop-function void "buffer_to_base32_string"
                             ('* size_t '*))))
    (lambda (bv)
      (let* ((len (bytevector-length bv))
             (in  (bytevector->pointer bv))
             (out (bytevector->pointer (make-bytevector (+ 1 (* len 2)) 7))))
        (f in len out)
        (pointer->string out)))))

(define hex-string->bytevector
  (let ((f (libchop-function void "hex_string_to_buffer"
                             ('* size_t '* '*))))
    (lambda (s)
      (let* ((len (string-length s))
             (in  (string->pointer s))
             (out (make-bytevector (quotient len 2)))
             (end (bytevector->pointer (make-bytevector (sizeof '*)))))
        (f in len (bytevector->pointer out) end)
        (let ((consumed (- (pointer-address (dereference-pointer end))
                           (pointer-address in))))
          (values out consumed))))))

(define (round-up x y)
  (if (= 0 (modulo x y))
      x
      (+ x (- y (modulo x y)))))

(define (sub-bytevector bv size)
  (let ((sub (make-bytevector size)))
    (bytevector-copy! bv 0 sub 0 size)
    sub))

(define base32-string->bytevector
  (let ((f (libchop-function size_t "base32_string_to_buffer"
                             ('* size_t '* '*))))
    (lambda (s)
      (let* ((len (string-length s))
             (in  (string->pointer s))
             (out (make-bytevector (* (round-up len 8) 5/8)))
             (end (bytevector->pointer (make-bytevector (sizeof '*)))))
        (let ((size     (f in len (bytevector->pointer out) end))
              (consumed (- (pointer-address (dereference-pointer end))
                           (pointer-address in))))
          (values (sub-bytevector out size) consumed))))))


;;;
;;; Error codes of <chop/errors.h>.
;;;

;; TODO: Some of the variables below will move to the right module.

(define-error-code error/ok "CHOP_OK")
(define-error-code error/unknown-store "CHOP_ERR_UNKNOWN_STORE")
(define-error-code error/not-found "CHOP_ERR_NOT_FOUND")
(define-error-code error/not-impl "CHOP_ERR_NOT_IMPL")
(define-error-code error/invalid-arg "CHOP_INVALID_ARG")
(define-error-code error/out-of-range-arg "CHOP_OUT_OF_RANGE_ARG")
(define-error-code error/filter-full "CHOP_FILTER_FULL")
(define-error-code error/filter-empty "CHOP_FILTER_EMPTY")
(define-error-code error/filter-unhandled-fault "CHOP_FILTER_UNHANDLED_FAULT")
(define-error-code error/filter-error "CHOP_FILTER_ERROR")
(define-error-code error/store-error "CHOP_STORE_ERROR")
(define-error-code error/store-end "CHOP_STORE_END")
(define-error-code error/block-indexer-error "CHOP_BLOCK_INDEXER_ERROR")
(define-error-code error/block-fetcher-error "CHOP_BLOCK_FETCHER_ERROR")
(define-error-code error/indexer-error "CHOP_INDEXER_ERROR")
(define-error-code error/indexer-empty-source "CHOP_INDEXER_EMPTY_SOURCE")
(define-error-code error/store-block-unavailable "CHOP_STORE_BLOCK_UNAVAIL")
(define-error-code error/deserial-too-short "CHOP_DESERIAL_TOO_SHORT")
(define-error-code error/deserial-corrupt-input "CHOP_DESERIAL_CORRUPT_INPUT")
(define-error-code error/cipher-error "CHOP_CIPHER_ERROR")
(define-error-code error/cipher-weak-key "CHOP_CIPHER_WEAK_KEY")
