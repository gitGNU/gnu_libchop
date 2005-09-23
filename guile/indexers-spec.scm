;;;; Copyright (C) 2005 Ludovic Courtès
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this software; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;; MA 02139, USA.
;;;;

(define-module (indexers-spec)
  #:use-module (core-spec)
  #:use-module (hash-spec)
  #:use-module (cipher-spec)
  #:use-module (streams-spec)
  #:use-module (stores-spec)
  #:use-module (choppers-spec)
  #:use-module (logs-spec)

  #:use-module (oop goops)
  #:use-module (srfi srfi-1)

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-indexer-wrapset>))


;; the wrapset itself.

(define-class <chop-indexer-wrapset> (<gw-guile-wrapset>)
  #:id 'indexers
  #:dependencies '(standard core hash cipher streams stores choppers logs))


(define-method (global-declarations-cg (ws <chop-indexer-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/indexers.h>\n"
	"#include \"core-support.h\"\n"
	"#include \"indexers-support.c\"\n\n"))




(define-method (initialize (ws <chop-indexer-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop indexers)) initargs))

  (wrap-constant! ws
		  #:name 'indexer-error/generic
		  #:type 'long
		  #:value "CHOP_INDEXER_ERROR"
		  #:description "Generic indexer error")

  (wrap-as-chop-object! ws
			#:name '<indexer>
			#:c-type-name "chop_indexer_t *"
			#:c-const-type-name "const chop_indexer_t *")

  (wrap-as-chop-object! ws
			#:name '<index-handle>
			#:c-type-name "chop_index_handle_t *"
			#:c-const-type-name "const chop_index_handle_t *")

  ;; constructors

  (wrap-function! ws
		  #:name 'hash-tree-indexer-open
		  #:c-name "chop_hash_tree_indexer_open_alloc"
		  #:returns '<errcode>
		  #:arguments '((hash-method content-hash-method)
				(hash-method key-hash-method)
				((<cipher-handle> aggregated) cipher)
				(int keys-per-block (default 100))
				((<indexer> out) indexer)))

  ;; methods

  (wrap-function! ws
		  #:name 'indexer-index-blocks
		  #:c-name "chop_indexer_index_blocks_alloc"
		  #:returns '<errcode>
		  #:arguments '((<indexer> indexer)
				(<chopper> input)
				(<store> data-store)
				(<store> meta-data-store)
				((<index-handle> out) handle)))

  (wrap-function! ws
		  #:name 'indexer-fetch-stream
		  #:c-name "chop_indexer_fetch_stream_alloc"
		  #:returns '<errcode>
		  #:arguments '((<indexer> indexer)
				(<index-handle> handle)
				(<store> data-store)
				(<store> meta-data-store)
				((<stream> out) stream)))

  (wrap-function! ws
		  #:name 'index-handle-ascii-serialize
		  ;; FIXME:  There is currently a leak here:  the returned
		  ;; string is never freed.  That's a G-Wrap problem.
		  #:returns '<errcode>
		  #:c-name "chop_index_handle_ascii_serialize"
		  #:arguments '((<index-handle> handle)
				((mchars out caller-owned) serialized)))

  (wrap-function! ws
		  #:name 'index-handle-ascii-deserialize
		  #:returns '<errcode>
		  #:c-name "chop_index_handle_ascii_deserialize"
		  #:arguments '((<indexer> indexer)
				((mchars caller-owned) ascii-handle)
				((<index-handle> out) handle)))

  (wrap-function! ws
		  #:name 'hash-tree-indexer-log
		  #:returns '<log>
		  #:c-name "chop_hash_tree_indexer_log"
		  #:arguments '((<indexer> indexer)))

)

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End: