;;; libchop -- a utility library for distributed storage and data backup
;;; Copyright (C) 2008, 2010  Ludovic Courtès <ludo@gnu.org>
;;; Copyright (C) 2005, 2006, 2007  Centre National de la Recherche Scientifique (LAAS-CNRS)
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

(define-module (store-stats-spec)
  #:use-module (core-spec)
  #:use-module (stores-spec)

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

  #:export (<chop-store-stats-wrapset>))


;; the wrapset itself.

(define-class <chop-store-stats-wrapset> (<gw-guile-wrapset>)
  #:id 'store-stats
  #:dependencies '(standard core stores))



(define-method (global-declarations-cg (ws <chop-store-stats-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/stores.h>\n"
	"#include <chop/store-stats.h>\n\n"
	"#include \"core-support.h\"\n"
	"#include \"store-stats-support.c\"\n\n"))




(define-method (initialize (ws <chop-store-stats-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop store-stats)) initargs))

  (wrap-as-chop-object! ws
			#:name '<block-store-stats>
			#:c-type-name "chop_block_store_stats_t *"
			#:c-const-type-name "const chop_block_store_stats_t *")

  ;; constructors

  (wrap-function! ws
		  #:name 'stat-block-store-open
		  #:c-name "chop_stat_block_store_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) name)
				((<store> aggregated) backend)
				(bool close-backend? (default #f))
				((<store> out) store))
		  #:description "Return a statistic-gathering block store
that acts as a proxy to store @var{backend}.")

  (wrap-function! ws
		  #:name 'stat-block-store-stats
		  #:c-name "chop_stat_block_store_stats_alloc"
		  #:returns '<block-store-stats>
		  #:arguments '((<store> stat-store))
		  #:description "If @var{stat-store} is a statistic-gathering
block store, return a copy of the statistics it has gathered so far (a
@code{block-store-stats} object), otherwise, return @code{#f}.")


  ;; accessors

  (wrap-function! ws
		  #:name 'block-store-stats:blocks-written
		  #:c-name "chop_block_store_stats_blocks_written"
		  #:returns 'int
		  #:arguments '((<block-store-stats> stats)))

  (wrap-function! ws
		  #:name 'block-store-stats:bytes-written
		  #:c-name "chop_block_store_stats_bytes_written"
		  #:returns 'int
		  #:arguments '((<block-store-stats> stats)))

  (wrap-function! ws
		  #:name 'block-store-stats:virgin-blocks
		  #:c-name "chop_block_store_stats_virgin_blocks"
		  #:returns 'int
		  #:arguments '((<block-store-stats> stats)))

  (wrap-function! ws
		  #:name 'block-store-stats:virgin-bytes
		  #:c-name "chop_block_store_stats_virgin_bytes"
		  #:returns 'int
		  #:arguments '((<block-store-stats> stats)))

  (wrap-function! ws
		  #:name 'block-store-stats:average-block-size
		  #:c-name "chop_block_store_stats_average_block_size"
		  #:returns 'float
		  #:arguments '((<block-store-stats> stats)))

  (wrap-function! ws
		  #:name 'block-store-stats:min-block-size
		  #:c-name "chop_block_store_stats_min_block_size"
		  #:returns 'int
		  #:arguments '((<block-store-stats> stats)))

  (wrap-function! ws
		  #:name 'block-store-stats:max-block-size
		  #:c-name "chop_block_store_stats_max_block_size"
		  #:returns 'int
		  #:arguments '((<block-store-stats> stats)))

)

;; arch-tag: f152b05f-d0d5-4f07-bd95-8962d1611267

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End: