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
            block-indexer-index))

(define-libchop-type block-indexer "block_indexer"
  block-indexer?
  wrap-block-indexer unwrap-block-indexer)

(define-libchop-type index-handle "index_handle"
  index-handle?
  wrap-index-handle unwrap-index-handle)

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
         (i  (bytevector->pointer
              (make-bytevector (class-instance-size ic)))))
    (m (unwrap-block-indexer bi)
       (unwrap-object %store-class store)
       (bytevector->pointer bv)
       (bytevector-length bv)
       i)
    (register-libchop-object! (wrap-index-handle i))))
