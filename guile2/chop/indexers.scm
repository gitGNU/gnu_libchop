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

(define-module (chop indexers)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (chop block-indexers)
  #:use-module (chop objects)
  #:use-module (chop internal)
  #:export (indexer?
            tree-indexer-open
            indexer-stream-class
            indexer-index-blocks
            indexer-fetch-stream

            error/indexer-error
            error/indexer-empty-source))

(define-libchop-type indexer "indexer"
  indexer?
  wrap-indexer unwrap-indexer)

(define-error-code error/indexer-error "CHOP_INDEXER_ERROR")
(define-error-code error/indexer-empty-source "CHOP_INDEXER_EMPTY_SOURCE")


;;;
;;; Constructors.
;;;

(define tree-indexer-open
  (let ((f (libchop-type-constructor "tree_indexer_open"
                                     (size_t)
                                     "tree_indexer"
                                     wrap-indexer)))
    (lambda* (#:optional (indices-per-block 100))
      "Return a tree indexer that will populate each intermediary meta-data
block with INDICES-PER-BLOCK index handles."
      (f indices-per-block))))


;;;
;;; Methods.
;;;

(define %store-class
  (lookup-class "block_store"))

(define %block-indexer-class
  (lookup-class "block_indexer"))

(define %block-fetcher-class
  (lookup-class "block_fetcher"))

(define %index-handle-class
  (lookup-class "index_handle"))

(define %stream-class
  (lookup-class "stream"))

(define %chopper-class
  (lookup-class "chopper"))


(define (indexer-stream-class i)
  "Return the class of streams associated with I."
  (let ((c (libchop-slot-ref "indexer" "stream_class" '*
                             (unwrap-indexer i))))
    (wrap-object (lookup-class "class") c)))

(define (indexer-index-blocks indexer chopper bi data-store meta-data-store)
  "Index all the blocks returned by CHOPPER using INDEXER and BI; data blocks
are written to DATA-STORE and meta-data blocks produced by INDEXER are
written to META-DATA-STORE.  Return the index handle of the root meta-data
block."
  (let* ((m  (libchop-method (unwrap-indexer indexer)
                             "indexer" "index_blocks"
                             ('* '* '* '* '* '*)))
         (ic (block-indexer-index-handle-class bi))
         (i  (gc-malloc-pointerless (class-instance-size ic))))
    (m (unwrap-indexer indexer)
       (unwrap-object %chopper-class chopper)
       (unwrap-object %block-indexer-class bi)
       (unwrap-object %store-class data-store)
       (unwrap-object %store-class meta-data-store)
       i)
    (register-libchop-object! (wrap-object %index-handle-class i))))

(define (indexer-fetch-stream indexer index bf data-store meta-data-store)
  "Return a stream object from which the data designated by INDEX can be
fetched from DATA-STORE and META-DATA-STORE, according to BF and INDEXER."
  (let* ((m  (libchop-method (unwrap-indexer indexer)
                             "indexer" "fetch_stream"
                             ('* '* '* '* '* '*)))
         (sc (indexer-stream-class indexer))
         (p  (gc-malloc-pointerless (class-instance-size sc))))
    (m (unwrap-indexer indexer)
       (unwrap-object %index-handle-class index)
       (unwrap-object %block-fetcher-class bf)
       (unwrap-object %store-class data-store)
       (unwrap-object %store-class meta-data-store)
       p)
    (let ((s (register-libchop-object! (wrap-object %stream-class p))))
      (register-weak-reference s bf)
      (register-weak-reference s data-store)
      (register-weak-reference s meta-data-store)
      s)))
