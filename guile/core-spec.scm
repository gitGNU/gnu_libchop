(define-module (core-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-core-wrapset>

	    <chop-errcode-type>))


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

(define-method (c-type-name (type <chop-input-buffer-type>))
  "char *")

(define-method (wrap-value-cg (type <chop-input-buffer-type>)
			      (value <gw-value>)
			      error-var)
  (list (scm-var value) " = scm_make_u8vector ();\n"))

(define-method (unwrap-value-cg (type <chop-input-buffer-type>)
				(value <gw-value>)
				error-var)
  (list (var value) " = XXX;\n"))

(define-method (call-arg-cg (type <chop-input-buffer-type>)
			    (value <gw-value>))
  (list (var value) ", " (string-append (var value) "_size")))



(define-method (initializations-cg (ws <chop-core-wrapset>) error-var)
  (list (next-method)
	"\nchop_init ();\n\n"))

(define-class <chop-core-wrapset> (<gw-guile-wrapset>)
  #:id 'core
  #:dependencies '(standard))

(define-method (global-declarations-cg (ws <chop-core-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n\n"))

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

  (wrap-function! ws
		  #:name 'frob
		  #:returns 'int
		  #:c-name "frob"
		  #:arguments '((<input-buffer> buffer)
				(int weather))))

