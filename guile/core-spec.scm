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

(define-module (core-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-13) ;; strings

  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-core-wrapset>

	    <chop-errcode-type>
	    <chop-input-buffer-type>
	    <chop-writable-input-buffer-type>

	    <chop-output-buffer>
	    <chop-output-buffer-with-out-size>

	    wrap-as-chop-object!))

(debug-enable 'backtrace)


;; The wrapper itself.
(define-class <chop-core-wrapset> (<gw-wrapset>)
  #:dependencies '(standard))



;;
;; The error code type `errcode_t' mapped to Guile exceptions.
;;

(define-class <chop-errcode-type> (<gw-type>))

(define-method (c-type-name (type <chop-errcode-type>))
  "long")

(define-method (call-cg (type <chop-errcode-type>) (result <gw-value>)
                        func-call-code error-var)
  (let ((result-var (gensym "result")))
    (list
     "{"
     "  " (c-type-name type) " " result-var " = " func-call-code ";"
     "  if (" result-var " != 0)"
     "    {"
     "      "(scm-var result)" = SCM_BOOL_F;"
     "      scm_throw (scm_from_locale_symbol (\"chop-error\"), "
     "                 scm_list_1 (scm_from_long (" result-var ")));"
     "    }"
     "}")))

(define-method (post-call-result-cg (type <chop-errcode-type>)
                                    (result <gw-value>)
                                    status-var)
  '())


;;
;; Input buffers as SRFI-4 vectors
;;

(define-class <chop-input-buffer-type> (<gw-type>))
(define-class <chop-writable-input-buffer-type> (<chop-input-buffer-type>))

(define-method (check-typespec-options (type <chop-input-buffer-type>)
				       (options <list>))
  #t)

(define-method (c-type-name (type <chop-input-buffer-type>))
  "const char *")

(define-method (c-type-name (type <chop-writable-input-buffer-type>))
  "char *")

; (define-method (c-type-name (type <chop-input-buffer-type>)
; 			    (typespec <gw-typespec>))
;   (c-type-name type))


(define-method (wrap-value-cg (type <chop-input-buffer-type>)
			      (value <gw-value>)
			      error-var)
  (list (scm-var value) " = scm_make_u8vector ();\n"))  ;; FIXME: Do it!

(define-method (pre-call-arg-cg (type <chop-input-buffer-type>)
				(param <gw-value>) error-var)
  (let ((handle-var (string-append (var param) "_handle"))
	(size-var (string-append (var param) "_size"))
	(increment-var (string-append (var param) "_inc")))
    ;; Declare the variables that will hold the necessary information
    (list (format #f "\n/* pre-call-arg-cg ~a */\n" type)
	  "scm_t_array_handle " handle-var "; "
	  "size_t " size-var " = 0; "
	  "ssize_t " increment-var " = 0;\n\n"

	  ;; call (indirectly) `unwrap-value-cg'.
	  (next-method))))

(define-method (unwrap-value-cg (type <chop-input-buffer-type>)
				(value <gw-value>)
				error-var
				(writable-buffer? <boolean>))
  ;; This method is actually called by `pre-call-arg-cg'.
  (let ((handle-var (string-append (var value) "_handle"))
	(size-var (string-append (var value) "_size"))
	(increment-var (string-append (var value) "_inc")))
    (list "if (SCM_FALSEP (scm_u8vector_p (" (scm-var value) ")))"
	  `(gw:error ,error-var type ,(wrapped-var value))
	  "else { "
	  (var value) " = (char *)"
	  (if writable-buffer?
	      "scm_u8vector_writable_elements ("
	      "scm_u8vector_elements (")
	  (scm-var value)
	  ", &" handle-var ", &" size-var ", &" increment-var ");\n"
	  "if (" increment-var " != 1)\n{\n/* Non-contiguous vector! */\n"
	  `(gw:error ,error-var type ,(wrapped-var value))
	  "\n}\n"
	  "}\n")))

(define-method (unwrap-value-cg (type <chop-input-buffer-type>)
				(value <gw-value>) error-var)
  (unwrap-value-cg type value error-var #f))

(define-method (unwrap-value-cg (type <chop-writable-input-buffer-type>)
				(value <gw-value>) error-var)
  (unwrap-value-cg type value error-var #t))

(define-method (post-call-arg-cg (type <chop-input-buffer-type>)
				 (param <gw-value>) error-var)
  (let ((handle-var (string-append (var param) "_handle")))
    (list "\n/* post-call-arg-cg/input-buffer */\n"
	  "if (scm_u8vector_p (" (scm-var param) ") == SCM_BOOL_T)\n{\n"
	  "scm_array_handle_release (&" handle-var ");\n"
	  "scm_remember_upto_here (" (scm-var param) ");\n}\n")))

(define-method (call-arg-cg (type <chop-input-buffer-type>)
			    (value <gw-value>))
  ;; XXX: This is somewhat hackish: stealthily pass an addionaly argument to
  ;; the wrapped C function.
  (let ((size-var (string-append (var value) "_size")))
    (list (next-method) ", " size-var)))



;;
;; Growing `chop_buffer_t' buffers used as output buffers and mapped to
;; SRFI-4 u8vectors.
;;
;; FIXME: This is unused 'cause I couldn't make it work.  Instead, I found it
;; much easier to use `scm' and do part of the work by myself...
;;

(define-class <chop-output-buffer> (<gw-type>))

;; Sometimes, as in `chop_store_read_block ()', an output SIZE parameter is
;; passed along with the `chop_buffer_t' object.
(define-class <chop-output-buffer-with-out-size> (<chop-output-buffer>))

(define-method (make-typespec (type <chop-output-buffer>) (options <list>))
  ;; Automatically make it an `out' argument.
  (next-method type
	       (if (memq 'out options) options (cons 'out options))))

(define-method (check-typespec-options (type <chop-output-buffer>)
				       (options <list>))
  #t)

(define-method (c-type-name (type <chop-output-buffer>)
			    (typespec <gw-typespec>))
  (let ((out? (memq 'out (typespec-options typespec))))
    (if out? "char **" "char **")))

(define-method (c-type-name (type <chop-output-buffer>))
  "char **")

(define-method (unwrap-value-cg (type <chop-output-buffer>)
				(value <gw-value>) error-var)
  ;; XXX:  The ultimate hack: always initialize the buffer.
  (list "chop_buffer_init (&" (var value) ", 0);\n"))


(define-method (wrap-value-cg (type <chop-output-buffer>)
			      (value <gw-value>) error-var)
  (let ((content-var (string-append (var value) "_buf"))
	(size-var (string-append (var value) "_size")))
    (list "\n{\n"
	  "char *" content-var "; "
	  "size_t " size-var "; "
	  size-var " = chop_buffer_size (&" (var value) ");\n"
	  content-var " = malloc (" size-var ");\n"
	  "if (" content-var " == NULL)\n{\n"
	  `(gw:error ,error-var memory)
	  "\n}\nelse\n{\n"
	  "memcpy (" content-var ", chop_buffer_content (&" (var value) "), "
	  size-var ");\n"
	  "chop_buffer_return (&" (var value) ");\n"
	  (scm-var value) " = scm_take_u8vector (" content-var ", "
	  size-var ");\n}\n}\n")))

(define-method (global-definitions-cg (ws <gw-guile-wrapset>)
				      (type <gw-type>))
;  (format #t "gdc: ~a~%" type)
  (next-method))

; (define-method (global-definitions-cg (ws <gw-guile-wrapset>)
; 				      (function <gw-item>))
;   (if (is-a? function <gw-function>)
;       (begin
; 	(format #t "global-definitions-cg~%")
; 	(append (next-method)
; 		(if (string=? (c-name function) "chop_store_read_block")
; 		    (list "\n/* blurps */\n")
; 		    '())))
;       (begin
; 	(format #t "gdc: ~a~%" (class-name (class-of function)))
; 	(next-method))))

(define-method (pre-call-arg-cg (type <chop-output-buffer-with-out-size>)
				(value <gw-value>) error-var)
  (let ((size-var (string-append (var value) "_unused_size")))
;    (format #t "pre-call-arg-cg: ~a~%" type)
    (list "size_t " size-var "; /* the unused out size */\n"
	  (next-method))))

(define-method (call-arg-cg (type <chop-output-buffer-with-out-size>)
			    (value <gw-value>))
  (let ((size-var (string-append (var value) "_unused_size")))
    (list (next-method) ", & /* the unvisible arg */" size-var)))


;;;
;;; Wrapping of chop classes that inherit `chop_object_t'.
;;;

(define-public (wrap-as-chop-object! ws . args)
  (apply wrap-as-wct! ws
	 #:allowed-options '(caller-owned callee-owned out aggregated)
	 #:wcp-equal-predicate "gwrap_chop_object_equal"
	 #:wcp-free-function "gwrap_chop_object_cleanup"
	 #:wcp-mark-function "gwrap_chop_object_mark"
	 args))


;;
;; XXX  Hack:  Allow <gw-wct> to be used as `out' parameters.
;;

(define-method (check-typespec-options (type <gw-wct>)
				       (options <list>))
  #t)



;;
;; The guts of the core, waow.
;;

(define-method (initializations-cg (ws <chop-core-wrapset>) error-var)
  (list (next-method)
	"\nchop_init_with_allocator (chop_scm_malloc,"
        "\n                          chop_scm_realloc, chop_scm_free);"
        "\n"))

(define-class <chop-core-wrapset> (<gw-guile-wrapset>)
  #:id 'core
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <chop-core-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n"
	"#include <chop/objects.h>\n\n"
	"#include <g-wrap/core-runtime.h>\n"
	"#include <stdio.h>\n\n"
	"#include \"core-support.h\"\n\n"))

(define-method (initialize (ws <chop-core-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop core)) initargs))

  ;; error codes

  (add-type! ws (make <chop-errcode-type>
		      #:name '<errcode>
		      #:needs-result-var? #f))

  (let ((c->scm-enum (lambda (c-name)
		       (string-map (lambda (chr)
				     (if (char=? chr #\_)
					 #\- chr))
				   (string-downcase
				    (symbol->string c-name))))))

    ;; Wrap the error values defined in `src/chop-errors.et' using a
    ;; Scheme-friendly naming Scheme.  Error names all look like
    ;; `error/invalid-arg', `error/unknown-stream', etc.
    ;;
    ;; Note that specific errors (like store-related errors, etc.) are
    ;; wrapped in the corresponding specification.

    (wrap-enum! ws
		#:name 'errcode
		#:c-type-name "errcode_t"
		#:values (append

			  (map (lambda (name)
				 (let ((sym-name
					(string-append "error/"
						       (c->scm-enum name)))
				       (enum-value
					(string-append "CHOP_ERR_"
						       (symbol->string name))))
				   `(,(string->symbol sym-name)
				     . ,enum-value)))
			       '(UNKNOWN_STREAM UNKNOWN_STORE NOT_FOUND
			         NOT_IMPL))

			  (map (lambda (name)
				 (let ((sym-name
					(string-append "error/"
						       (c->scm-enum name)))
				       (enum-value
					(string-append "CHOP_"
						       (symbol->string
							name))))
				   `(,(string->symbol sym-name)
				     . ,enum-value)))
			       '(INVALID_ARG OUT_OF_RANGE_ARG
				 DESERIAL_TOO_SHORT
				 DESERIAL_CORRUPT_INPUT)))))

  (wrap-function! ws
		  #:name 'error-message
		  #:returns '(mchars callee-owned)
		  #:c-name "error_message"
		  #:arguments '((long code)))

  (add-type! ws (make <chop-input-buffer-type>
		  #:name '<input-buffer>))

  (add-type! ws (make <chop-writable-input-buffer-type>
		  #:name '<writable-input-buffer>)))

