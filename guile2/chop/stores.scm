;;; Copyright (C) 2011, 2012  Ludovic Courtès <ludo@gnu.org>
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
            filtered-block-store-open

            store-read-block
            store-write-block
            store-delete-block
            store-close

            error/unknown-store
            error/store-block-unavailable
            error/store-error
            error/store-end))

(define-libchop-type store "block_store"
  store?
  wrap-store unwrap-store)

(define-error-code error/unknown-store "CHOP_ERR_UNKNOWN_STORE")
(define-error-code error/store-block-unavailable "CHOP_STORE_BLOCK_UNAVAIL")
(define-error-code error/store-error "CHOP_STORE_ERROR")
(define-error-code error/store-end "CHOP_STORE_END")


;;;
;;; Constructors.
;;;

(define dummy-block-store-open
  (let ((f (libchop-type-constructor void "dummy_block_store_open"
                                     ('*)
                                     "dummy_block_store" wrap-store)))
    (lambda (name)
      "Return a dummy block store named NAME."
      (f (string->pointer name)))))

(define dummy-proxy-block-store-open
  (let ((f (libchop-type-constructor "dummy_proxy_block_store_open"
                                     ('* '*)
                                     "dummy_block_store" wrap-store)))
    (lambda (name back-end)
      "Return a dummy block store that proxies BACK-END."
      (f (string->pointer name) (unwrap-store back-end)))))

;; FIXME: dummy-block-store-log

(define (file-based-block-store-open class file open-flags mode)
  "Open the file-based store of type CLASS stored at FILE with the given
OPEN-FLAGS and MODE."
  (and (object-is-a? class (lookup-class "file_based_store_class"))
       (let* ((s (gc-malloc (class-instance-size class)))
              (p (libchop-slot-ref "file_based_store_class"
                                   "generic_open" '*
                                   (unwrap-class class)
                                   "#include <chop/stores.h>"))
              (f (pointer->procedure chop-error-t p
                                     `(* * ,int ,mode_t *)))
              (e (f (unwrap-class class) (string->pointer file)
                    open-flags mode s)))
         (if (= e 0)
             (register-libchop-object! (wrap-store s))
             (raise-chop-error e)))))

(define %filter-class
  (lookup-class "filter"))

(define filtered-block-store-open
  (let ((f (libchop-type-constructor "filtered_store_open"
                                     ('* '* '* int)
                                     "filtered_block_store" wrap-store)))
    (lambda* (input-filter output-filter backend #:optional close-backend?)
      "Return a filtered block store which uses INPUT-FILTER to filter the
contents of blocks that are written to it, OUTPUT-FILTER to filter the
contents of blocks as they are read from it, and uses BACKEND as the
underlying block store.  If CLOSE-BACKEND? is true, then BACKEND will be
closed when the returned store is closed."
      (f (unwrap-object %filter-class input-filter)
         (unwrap-object %filter-class output-filter)
         (unwrap-store backend)
         (if close-backend?
             proxy/eventually-close
             proxy/leave-as-is)))))

;; FIXME: sunrpc-block-store-open
;; FIXME: sunrpc/tls-block-store-simple-open

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
        (size* (make-size_t-pointer))
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

(define (store-delete-block store key)
  "Delete from STORE the block pointed to by KEY.  Return KEY."
  (let ((m (libchop-method (unwrap-store store) "block_store" "delete_block"
                           ('* '*)
                           (includes "#include <chop/stores.h>"))))
    (m (unwrap-store store) (bytevector->key key))
    key))

(define (store-close store)
  "Close STORE."
  (let ((m (libchop-method (unwrap-store store) "block_store" "close"
                           ('*)
                           (includes "#include <chop/stores.h>"))))
    (m (unwrap-store store))))
