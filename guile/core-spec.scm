(define-module (core-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap util)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-core-wrapset>

	    <chop-errcode-type>
	    <chop-input-buffer-type>
	    <chop-writable-input-buffer-type>))


;; The wrapper itself.
(define-class <chop-core-wrapset> (<gw-wrapset>)
  #:dependencies '(standard))


;; The error code type `errcode_t'.
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
     "    scm_throw (scm_str2symbol (\"chop-error\"), "
     "               scm_int2num (" result-var "));"
     "}")))

(define-method (post-call-result-cg (type <chop-errcode-type>)
                                    (result <gw-value>)
                                    status-var)
  '())


;; Input buffers as SRFI-4 vectors

(define-class <chop-input-buffer-type> (<gw-type>))
(define-class <chop-writable-input-buffer-type> (<chop-input-buffer-type>))

(define-method (c-type-name (type <chop-input-buffer-type>))
  "const char *")

(define-method (c-type-name (type <chop-writable-input-buffer-type>))
  "char *")

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
    (list "\n/* pre-call-arg-cg */\n"
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
	  (var value) " = "
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
    (list "\n/* post-call-arg-cg */\n"
	  "scm_array_handle_release (&" handle-var ");\n")))

(define-method (call-arg-cg (type <chop-input-buffer-type>)
			    (value <gw-value>))
  (let ((size-var (string-append (var value) "_size")))
    (list (var value) ", "  ;; FIXME:  use SIZE_VAR
	  (string-append "scm_c_uniform_vector_length ("
			 (scm-var value) ")"))))



(define-method (initializations-cg (ws <chop-core-wrapset>) error-var)
  (list (next-method)
	"\nchop_init ();\n\n"))

(define-class <chop-core-wrapset> (<gw-guile-wrapset>)
  #:id 'core
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <chop-core-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n\n"
	"#include <stdio.h>\n"
	"int\n"
	"frob (const char *buffer, size_t size, int weather)\n"
	"{\n"
	"  fprintf (stderr, \"frob: buffer: %p, content: %02x%02x%02x, size: %u, weather: %i\\n\",\n"
	"           buffer, buffer[0], buffer[1], buffer[2], size, weather);\n"
	"  return (weather * 2);\n"
	"}\n\n"))

(define-method (initialize (ws <chop-core-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop core)) initargs))

  (add-type! ws (make <chop-errcode-type>
		      #:name '<errcode>
		      #:needs-result-var? #f))

  (wrap-function! ws
		  #:name 'error-message
		  #:returns '(mchars callee-owned)
		  #:c-name "error_message"
		  #:arguments '((long code)))

  (add-type! ws (make <chop-input-buffer-type>
		  #:name '<input-buffer>))

  (add-type! ws (make <chop-writable-input-buffer-type>
		  #:name '<writable-input-buffer>))

  (wrap-function! ws
		  #:name 'frob
		  #:returns 'int
		  #:c-name "frob"
		  #:arguments '((<input-buffer> buffer)
				(int weather))))

