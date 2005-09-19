(define-module (logs-spec)
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

  #:export (<chop-log-wrapset>))



(define-class <chop-log-wrapset> (<gw-guile-wrapset>)
  #:id 'logs
  #:dependencies '(standard core))

(define-method (global-declarations-cg (ws <chop-log-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/logs.h>\n\n"
	"#include \"logs-support.c\"\n\n"))


(define-method (initialize (ws <chop-log-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop logs)) initargs))

  ;; We don't need to destroy the underlying C object so everything is pretty
  ;; easy.
  (wrap-as-wct! ws
		#:name '<log>
		#:c-type-name "chop_log_t *"
		#:c-const-type-name "const chop_log_t *")

  (wrap-function! ws
		  #:name 'log-detach
		  #:c-name "chop_log_detach"
		  #:returns 'void
		  #:arguments '((<log> log)))

  (wrap-function! ws
		  #:name 'log-close
		  #:c-name "chop_log_close"
		  #:returns 'void
		  #:arguments '((<log> log)))

  (wrap-function! ws
		  #:name 'log-name
		  #:c-name "chop_log_name"
		  #:returns '(mchars callee-owned)
		  #:arguments '((<log> log)))

  (wrap-function! ws
		  #:name 'log-set-name!
		  #:c-name "chop_log_set_name"
		  #:returns 'void
		  #:arguments '((<log> log)
				((mchars caller-owned) name)))

  (wrap-function! ws
		  #:name 'log-attach-to-port
		  #:c-name "chop_log_attach_to_port"
		  #:returns 'void
		  #:arguments '((<log> log)
				(<raw-scheme-type> port)))

  (wrap-function! ws
		  #:name 'log-attach-to-user
		  #:c-name "chop_log_attach_to_scheme_user"
		  #:returns 'void
		  #:arguments '((<log> log)
				(<raw-scheme-type> proc)))

  )

