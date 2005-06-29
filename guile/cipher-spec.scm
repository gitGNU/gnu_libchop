(define-module (cipher-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (g-wrap)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap enumeration)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-cipher-wrapset>))



(define-class <chop-cipher-wrapset> (<gw-guile-wrapset>)
  #:id 'cipher
  #:dependencies '(standard core))

(define-method (global-declarations-cg (ws <chop-cipher-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/cipher.h>\n\n"
	"#include \"cipher-support.c\"\n\n"))


(define-method (initialize (ws <chop-cipher-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop cipher)) initargs))

  (wrap-enum! ws
	      #:name 'cipher-algo
	      #:c-type-name "chop_cipher_algo_t"
	      #:values (map (lambda (name)
			      (let ((sym-name
				     (string-append "cipher-algo/"
						    (string-downcase
						     (symbol->string name))))
				    (enum-value
				     (string-append "CHOP_CIPHER_"
						    (symbol->string name))))
				`(,(string->symbol sym-name) . ,enum-value)))
			    '(NONE IDEA 3DES CAST5 BLOWFISH SAFER_SK128
			      DES_SK AES AES192 AES256 TWOFISH TWOFISH128
			      ARCFOUR DES)))

  (wrap-enum! ws
	      #:name 'cipher-mode
	      #:c-type-name "chop_cipher_mode_t"
	      #:values (map (lambda (name)
			      (let ((sym-name
				     (string-append "cipher-mode/"
						    (string-downcase
						     (symbol->string name))))
				    (enum-value
				     (string-append "CHOP_CIPHER_MODE_"
						    (symbol->string name))))
				`(,(string->symbol sym-name) . ,enum-value)))
			    '(NONE ECB CFB CBC STREAM OFB)))

  (wrap-as-wct! ws
		#:name '<cipher-handle>
		#:c-type-name "chop_cipher_handle_t"
		#:c-const-type-name "const chop_cipher_handle_t"
		#:destroy-value-function-name "chop_cipher_handle_close_dealloc")

  (wrap-function! ws
		  #:name 'cipher-key-size
		  #:c-name "chop_cipher_algo_key_size"
		  #:returns 'int
		  #:arguments '((cipher-algo m)))

  (wrap-function! ws
		  #:name 'cipher-block-size
		  #:c-name "chop_cipher_algo_block_size"
		  #:returns 'int
		  #:arguments '((cipher-algo m)))

  (wrap-function! ws
		  #:name 'cipher-algo-name
		  #:c-name "chop_cipher_algo_name"
		  #:returns '(mchars callee-owned)
		  #:arguments '((cipher-algo m)))

  (wrap-function! ws
		  #:name 'cipher-algo-lookup
		  #:c-name "chop_cipher_algo_lookup"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) name)
				((cipher-algo out) m)))
  (wrap-function! ws
		  #:name 'cipher-mode-name
		  #:c-name "chop_cipher_mode_name"
		  #:returns '(mchars callee-owned)
		  #:arguments '((cipher-mode m)))

  (wrap-function! ws
		  #:name 'cipher-mode-lookup
		  #:c-name "chop_cipher_mode_lookup"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) name)
				((cipher-mode out) m)))

  (wrap-function! ws
		  #:name 'cipher-open
		  #:c-name "chop_cipher_open"
		  #:returns '<cipher-handle>
		  #:arguments '((cipher-algo a)
				(cipher-mode m)))

  (wrap-function! ws
		  #:name 'cipher-algorithm
		  #:c-name "chop_cipher_algorithm"
		  #:returns 'cipher-algo
		  #:arguments '((<cipher-handle> handle)))

  )

