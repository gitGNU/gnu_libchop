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

(define-module (choppers-spec)
  #:use-module (core-spec)
  #:use-module (streams-spec)

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

  #:export (<chop-chopper-wrapset>))


;; the wrapset itself.

(define-class <chop-chopper-wrapset> (<gw-guile-wrapset>)
  #:id 'choppers
  #:dependencies '(standard core streams))


(define-method (global-declarations-cg (ws <chop-chopper-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/choppers.h>\n\n"
	"#include \"core-support.h\"\n"
	"#include \"choppers-support.c\"\n\n"))




(define-method (initialize (ws <chop-chopper-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop choppers)) initargs))

  (wrap-as-chop-object! ws
			#:name '<chopper>
			#:c-type-name "chop_chopper_t *"
			#:c-const-type-name "const chop_chopper_t *")

  ;; constructors

  (wrap-function! ws
		  #:name 'fixed-size-chopper-open
		  #:c-name "chop_fixed_size_chopper_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((<stream> aggregated) input)
				(int block-size)
				(bool pad-blocks? (default #f))
				((<chopper> out) chopper)))

  (wrap-function! ws
		  #:name 'anchor-based-chopper-open
		  #:c-name "chop_anchor_based_chopper_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((<stream> aggregated) input)
				(int window-size (default 10))
				(long window-fpr-mask (default 8191))
				((<chopper> out) chopper)))

  (wrap-function! ws
		  #:name 'chopper-generic-open
		  #:c-name "chop_chopper_generic_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) class-name)
				((<stream> aggregated) input)
				(int typical-block-size (default 8192))
				((<chopper> out) chopper)))

  ;; methods

  (wrap-function! ws
		  #:name 'chopper-read-block
		  #:c-name "chop_chopper_read_block_alloc_u8vector"
		  #:returns '<errcode>
		  #:arguments '((<chopper> chopper)
				((scm out) buffer)))

  (wrap-function! ws
		  #:name 'chopper-set-stream!
		  #:c-name "chop_chopper_set_stream"
		  #:returns 'void
		  #:arguments '((<chopper> chopper)
				(<stream> stream)))


)

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End: