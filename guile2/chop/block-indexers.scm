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

(define-module (chop block-indexers)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop hash)
  #:use-module (chop cipher)
  #:use-module (chop objects)
  #:use-module (chop internal)
  #:export (block-indexer?

            hash-block-indexer-open
            chk-block-indexer-open

            index-handle?

            block-indexer-index-handle-class
            block-indexer-fetcher-class
            block-indexer-index
            block-indexer-fetcher

            block-fetcher?

            block-fetcher-fetch
            block-fetcher-exists?

            error/block-indexer-error
            error/block-fetcher-error))

(define-libchop-type block-indexer "block_indexer"
  block-indexer?
  wrap-block-indexer unwrap-block-indexer)

(define-libchop-type index-handle "index_handle"
  index-handle?
  wrap-index-handle unwrap-index-handle)

(define-libchop-type block-fetcher "block_fetcher"
  block-fetcher?
  wrap-block-fetcher unwrap-block-fetcher)

(define-error-code error/block-indexer-error "CHOP_BLOCK_INDEXER_ERROR")
(define-error-code error/block-fetcher-error "CHOP_BLOCK_FETCHER_ERROR")

;; Hack to allow access to our friends.

(define %hash-method-value (@@ (chop hash) %hash-method-value))
(define %cipher-mode-value (@@ (chop cipher) %cipher-mode-value))
(define %cipher-algorithm-value (@@ (chop cipher) %cipher-algorithm-value))
(define %unwrap-cipher (@@ (chop cipher) %unwrap-cipher))


;;;
;;; Constructors.
;;;

(define hash-block-indexer-open
  (let ((f (libchop-type-constructor "hash_block_indexer_open"
                                     (int)
                                     "hash_block_indexer"
                                     wrap-block-indexer)))
    (lambda (hash-method)
      "Return a hash block indexer that uses HASH-METHOD."
      (f (%hash-method-value hash-method)))))

(define chk-block-indexer-open
  (let ((f (libchop-type-constructor "chk_block_indexer_open"
                                     ('* int int int)
                                     "chk_block_indexer"
                                     wrap-block-indexer)))
    (lambda (cipher key-hash-method block-id-hash-method)
      "Return a content-hash key block indexer that encrypts blocks using the
KEY-HASH-METHOD hash of the block, and indexing them under the
BLOCK-ID-HASH-METHOD hash of the cipher text."
      (let ((bi (f (%unwrap-cipher cipher)
                   0 ;; let the GC free CIPHER
                   (%hash-method-value key-hash-method)
                   (%hash-method-value block-id-hash-method))))
        (register-weak-reference bi cipher)
        bi))))


;;;
;;; Methods.
;;;

(define (block-indexer-index-handle-class bi)
  "Return the class of index handles associated with BI."
  (let ((c (libchop-slot-ref "block_indexer" "index_handle_class" '*
                             (unwrap-block-indexer bi)
                             "#include <chop/block-indexers.h>")))
    (wrap-object (lookup-class "class") c)))

(define (block-indexer-fetcher-class bi)
  "Return the class of fetcher associated with BI."
  (let ((c (libchop-slot-ref "block_indexer" "block_fetcher_class" '*
                             (unwrap-block-indexer bi)
                             "#include <chop/block-indexers.h>")))
    (wrap-object (lookup-class "class") c)))

(define %store-class
  (lookup-class "block_store"))

(define (block-indexer-index bi store bv)
  "Index buffer BV into STORE using block-indexer BI."
  (let* ((m  (libchop-method (unwrap-block-indexer bi)
                             "block_indexer" "index_block"
                             ('* '* '* size_t '*)
                             (includes "#include <chop/block-indexers.h>")))
         (ic (block-indexer-index-handle-class bi))
         (i  (gc-malloc (class-instance-size ic))))
    (m (unwrap-block-indexer bi)
       (unwrap-object %store-class store)
       (bytevector->pointer bv)
       (bytevector-length bv)
       i)
    (register-libchop-object! (wrap-index-handle i))))

(define (block-indexer-fetcher bi)
  "Return a new fetcher that is the dual of BI."
  (let ((p (gc-malloc
            (class-instance-size (block-indexer-fetcher-class bi))))
        (m (libchop-method (unwrap-block-indexer bi)
                           "block_indexer" "init_fetcher"
                           ('* '*)
                           (includes "#include <chop/block-indexers.h>"))))
    (m (unwrap-block-indexer bi) p)
    (register-libchop-object! (wrap-block-fetcher p))))

(define (block-fetcher-fetch bf index store)
  "Fetch the block designated by INDEX from STORE using BF; return the block
contents as a bytevector."
  (let ((m (libchop-method (unwrap-block-fetcher bf)
                           "block_fetcher" "fetch_block"
                           ('* '* '* '* '*)
                           (includes "#include <chop/block-indexers.h>")))
        (b (make-empty-buffer))
        (r (make-size_t-pointer)))
    (m (unwrap-block-fetcher bf) (unwrap-index-handle index)
       (unwrap-object %store-class store)
       b r)
    (values (buffer->bytevector b)
            (dereference-size_t r))))

(define (block-fetcher-exists? bf index store)
  "Return true if the data pointed to by INDEX exists in STORE, using block
fetcher BF."
  (let ((m (libchop-method (unwrap-block-fetcher bf)
                           "block_fetcher" "block_exists"
                           ('* '* '* '*)
                           (includes "#include <chop/block-indexers.h>")))
        (r (make-int-pointer)))
    (m (unwrap-block-fetcher bf) (unwrap-index-handle index)
       (unwrap-object %store-class store)
       r)
    (not (= 0 (dereference-int r)))))
