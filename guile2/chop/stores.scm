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

(define-module (chop stores)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop objects)
  #:use-module (chop internal)
  #:export (store?
            dummy-block-store-open
            dummy-proxy-block-store-open
            file-based-block-store-open
            store-read-block
            store-write-block
            store-close))

(define-libchop-type store "block_store"
  store?
  wrap-store unwrap-store)


;;;
;;; Constructors.
;;;

(define dummy-block-store-open
  (let ((f (libchop-type-constructor void "dummy_block_store_open"
                                     ('*)
                                     "block_store" wrap-store)))
    (lambda (name)
      "Return a dummy block store named NAME."
      (f (string->pointer name)))))

(define dummy-proxy-block-store-open
  (let ((f (libchop-type-constructor "dummy_proxy_block_store_open"
                                     ('* '*)
                                     "block_store" wrap-store)))
    (lambda (name back-end)
      "Return a dummy block store that proxies BACK-END."
      (f (string->pointer name) (unwrap-store back-end)))))

;; FIXME: dummy-block-store-log

(define file-based-block-store-open
  (let ((f (libchop-type-constructor "file_based_store_open"
                                     ('* '* int mode_t)
                                     "block_store" wrap-store)))
    (lambda (class file open-flags mode)
      "Open the file-based store of type CLASS stored at FILE with the given
OPEN-FLAGS and MODE."
      (and (object-is-a? class (lookup-class "file_based_store_class"))
           (f (unwrap-class class) (string->pointer file) open-flags mode)))))

;; FIXME: sunrpc-block-store-open
;; FIXME: sunrpc/tls-block-store-simple-open
;; FIXME: filtered-store-open

;; FIXME: make-block-store


;;;
;;; Methods.
;;;

(define %key-layout
  ;; Layout of `struct chop_block_key'.
  (list '*     ;; key
        size_t ;; size
        '*     ;; dispose
        '*     ;; owner
        ))

(define bytevector->key
  (let ((size (compile-time-value
               (c-size-of "struct chop_block_key"
                          "#include <chop/stores.h>"
                          %libchop-libs
                          %libchop-cc
                          %libchop-cppflags)))
        (refs (make-weak-key-hash-table)))
    (lambda (bv)
      (let ((key (make-c-struct %key-layout
                                (list (bytevector->pointer bv)
                                      (bytevector-length bv)
                                      %null-pointer
                                      %null-pointer))))
        (hashq-set! refs key bv)
        key))))

(define (store-read-block store key)
  "Read the block stored under KEY in STORE and return it."
  (let ((buf   (make-empty-buffer))
        (size* (bytevector->pointer (make-bytevector (sizeof size_t))))
        (m     (libchop-method (unwrap-store store) "block_store" "read_block"
                               ('* '* '* '*)
                               (includes "#include <chop/stores.h>"))))
    (m (unwrap-store store) (bytevector->key key) buf
       size*)
    ;;; XXX: SIZE* is ignored.
    (buffer->bytevector buf)))

(define (store-write-block store key buffer)
  "Write BUFFER under KEY in STORE."
  (let ((m (libchop-method (unwrap-store store) "block_store" "write_block"
                           ('* '* '* size_t)
                           (includes "#include <chop/stores.h>"))))
    (m (unwrap-store store)
       (bytevector->key key)
       (bytevector->pointer buffer)
       (bytevector-length buffer))))

(define (store-close store)
  "Close STORE."
  (let ((m (libchop-method (unwrap-store store) "block_store" "close"
                           ('*)
                           (includes "#include <chop/stores.h>"))))
    (m (unwrap-store store))))