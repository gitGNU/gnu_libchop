(define-module (hash-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap c-codegen)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-hash-wrapset>))



(define-class <chop-hash-wrapset> (<gw-guile-wrapset>)
  #:id 'hash
  #:dependencies '(standard core))

(define-method (global-declarations-cg (ws <chop-hash-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/hash.h>\n\n"
	"#include \"hash-support.c\"\n\n"))


(define-method (initialize (ws <chop-hash-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop hash)) initargs))

  (wrap-enum! ws
	      #:name 'hash-method
	      #:c-type-name "chop_hash_method_t"
	      #:values (map (lambda (name)
			      (let ((sym-name
				     (string-append "hash-method/"
						    (string-downcase
						     (symbol->string name))))
				    (enum-value
				     (string-append "CHOP_HASH_"
						    (symbol->string name))))
				`(,(string->symbol sym-name) . ,enum-value)))
			    '(NONE SHA1 RMD160 MD5 MD4 MD2 TIGER HAVAL
			      SHA256 SHA384 SHA512)))

  (wrap-function! ws
		  #:name 'hash-size
		  #:c-name "chop_hash_size"
		  #:returns 'int
		  #:arguments '((hash-method m)))

  (wrap-function! ws
		  #:name 'hash-method-name
		  #:c-name "chop_hash_method_name"
		  #:returns '(mchars callee-owned)
		  #:arguments '((hash-method m)))

  (wrap-function! ws
		  #:name 'hash-method-lookup
		  #:c-name "chop_hash_method_lookup"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) name)
				((hash-method out) m)))

  (wrap-function! ws
		  #:name 'hash-buffer
		  #:c-name "chop_hash_buffer_alloc"
		  #:returns 'scm
		  #:arguments '((hash-method method)
				(<input-buffer> buffer))
		  #:description "Return a newly allocated u8vector containing
the digest of @var{buffer} according to hash method @var{method}.  The length
of the returned vector is equal to the result of @code{hash-size} for
@var{method}.  If @var{method} is not a valid hash method, @code{#f} is
returned."))

