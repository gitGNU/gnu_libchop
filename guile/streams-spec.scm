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

(define-module (streams-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-stream-wrapset>))


;; the wrapset itself.

(define-class <chop-stream-wrapset> (<gw-guile-wrapset>)
  #:id 'streams
  #:dependencies '(standard core))


;; types

;; A wrapped C pointer.
(define-class <chop-stream-type> (<gw-wct>))



(define-method (global-declarations-cg (ws <chop-stream-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/streams.h>\n\n"
	"#include \"stream-ctors-dtors.c\"\n\n"))

(define-method (check-typespec-options (type <chop-stream-type>)
				       (options <list>))
  (format #t "check-typespec-options: ~a~%" type)
  (next-method)
  #t)



(define-method (initialize (ws <chop-stream-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop streams)) initargs))

  (wrap-constant! ws
		  #:name 'stream/end
		  #:type 'long
		  #:value "CHOP_STREAM_END"
		  #:description "End of stream")

  (wrap-as-wct! ws
		#:name '<stream>
		#:c-type-name "chop_stream_t *"
		#:c-const-type-name "const chop_stream_t *"
		#:destroy-value-function-name "chop_stream_close_dealloc")

;   (wrap-function! ws
; 		  #:name 'file-stream-open
; 		  #:returns '<errcode>
; 		  #:c-name "chop_file_stream_open_alloc"
; 		  #:arguments '(((mchars caller-owned) path)
; 				((<stream> out) stream)))

  (wrap-function! ws
		  #:name 'stream-read!
		  #:returns '<errcode>
		  #:c-name "chop_stream_read"
		  #:arguments '(((<stream> caller-owned) stream)
				(<writable-input-buffer> buffer)
				((int out) read))
		  #:description "Read from @var{stream}.")

)

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End: