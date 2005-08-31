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

(define-module (stores-spec)
  #:use-module (core-spec)

  #:use-module (oop goops)
  #:use-module (srfi srfi-1)

  #:use-module (g-wrap)
  #:use-module (g-wrap rti)
  #:use-module (g-wrap c-types)
  #:use-module (g-wrap ws standard)

  ;; Guile-specific things
  #:use-module (g-wrap guile)
  #:use-module (g-wrap guile ws standard)

  #:export (<chop-store-wrapset>))


;; the wrapset itself.

(define-class <chop-store-wrapset> (<gw-guile-wrapset>)
  #:id 'stores
  #:dependencies '(standard core))


;; types

;; A wrapped C pointer.
(define-class <chop-store-type> (<gw-wct>))


;; Define block key as a variant of non-writable input buffers.
(define-class <chop-block-key-type> (<gw-type>))

(define-method (check-typespec-options (type <chop-block-key-type>)
				       (options <list>))
  #t)

(define-method (c-type-name (type <chop-block-key-type>)
			    (typespec <gw-typespec>))
  "chop_block_key_t")

(define-method (c-type-name (type <chop-block-key-type>))
  "chop_block_key_t")

(define-method (pre-call-arg-cg (type <chop-block-key-type>)
				(param <gw-value>) error-var)
  (let ((handle-var (string-append (var param) "_handle"))
	(content-var (string-append (var param) "_buf"))
	(size-var (string-append (var param) "_size"))
	(increment-var (string-append (var param) "_inc")))
    ;; Declare the variables that will hold the necessary information
    (list (format #f "\n/* pre-call-arg-cg ~a */\n" type)
	  "scm_t_array_handle " handle-var "; "
	  "const char *" content-var "; "
	  "size_t " size-var " = 0; "
	  "ssize_t " increment-var " = 0;\n\n"

	  ;; call (indirectly) `unwrap-value-cg'.
	  (next-method))))

(define-method (unwrap-value-cg (type <chop-block-key-type>)
				(value <gw-value>) error-var)
  ;; This method is actually called by `pre-call-arg-cg'.
  (let ((handle-var (string-append (var value) "_handle"))
	(size-var (string-append (var value) "_size"))
	(increment-var (string-append (var value) "_inc"))
	(content-var (string-append (var value) "_buf")))
;	(content-cpy-var (string-append (var value) "_buf_cpy")))
    (list "if (SCM_FALSEP (scm_u8vector_p (" (scm-var value) ")))"
	  `(gw:error ,error-var type ,(wrapped-var value))
	  "else { "
;;	  "char *" content-cpy-var ";\n"
	  "scm_gc_protect_object (" (scm-var value) ");\n"
	  content-var " = " "scm_u8vector_elements ("
	  (scm-var value)
	  ", &" handle-var ", &" size-var ", &" increment-var ");\n"
;;	  content-cpy-var " = scm_malloc (" size-var ");\n"
;;	  "if (!" content-cpy-var ") { "
;;	  "abort ();\n"
;;;	  `(gw:error ,error-var memory ,(wrapped-var value))
;;	  "} else memcpy (" content-cpy-var ", " content-var
;;	  ", " size-var ");\n"
	  "chop_block_key_init (&" (var value) ", (char *)" content-var
	  ", " size-var ", NULL, NULL);\n"
	  "}\n")))

(define-method (wrap-value-cg (type <chop-block-key-type>)
			      (value <gw-value>) error-var)
  (let ((key-buf (string-append (var value) "_buf")))
    (format #t "wrap-value-cg/block-key~%")
    (list "{ /* wrap-value-cg/block-key */\n"
	  "  char *" key-buf ";\n"
	  "  "key-buf" = scm_malloc (chop_block_key_size (&"(var value)"));\n"
	  "  memcpy ("key-buf", chop_block_key_buffer (&"(var value)"),\n"
	  "          chop_block_key_size (&"(var value)"));\n"
	  "  "(scm-var value)" = scm_take_u8vector ("key-buf", "
	  "chop_block_key_size (&"(var value)"));\n"
	  "}\n")))

(define-method (post-call-arg-cg (type <chop-block-key-type>)
				 (param <gw-value>) error-var)
  (if-typespec-option
   param 'out

   ;; `out' param:  call `wrap-value-cg'
   (next-method)

   ;; `in' param:  release the previously acquired handle
   (let ((handle-var (string-append (var param) "_handle")))
     (list "\n/* post-call-arg-cg/block-key */\n"
	   "scm_array_handle_release (&" handle-var ");\n"
	   "scm_gc_unprotect_object (" (scm-var param) ");\n"))))

(define-method (call-arg-cg (type <chop-block-key-type>)
			    (value <gw-value>))
  ;; Pass a pointer to the key object rather than the key itself.
  (list "& /* key! */" (var value)))


(define-method (global-declarations-cg (ws <chop-store-wrapset>))
  (list (next-method)
	"#include <chop/chop.h>\n#include <chop/stores.h>\n\n"
	"/* The following are needed for the MODE arg of `gdbm-store-open'.  */\n"
	"#include <sys/types.h>\n"
	"#include <sys/stat.h>\n"
	"#include <fcntl.h>\n\n"
	"#include \"stores-support.c\"\n\n"))




(define-method (initialize (ws <chop-store-wrapset>) initargs)
  (format #t "initializing ~a~%" ws)

  (slot-set! ws 'shlib-path "libguile-chop")

  (next-method ws (append '(#:module (chop stores)) initargs))

  (add-type! ws (make <chop-block-key-type>
		  #:name '<block-key>))

  (wrap-as-wct! ws
		#:name '<store>
		#:c-type-name "chop_block_store_t *"
		#:c-const-type-name "const chop_block_store_t *"
		#:destroy-value-function-name "chop_store_close_dealloc")

  ;; constructors

  (wrap-function! ws
		  #:name 'dummy-block-store-open
		  #:c-name "chop_dummy_block_store_open_alloc"
		  #:returns '<store>
		  #:arguments '(((mchars caller-owned) name)))

  (wrap-function! ws
		  #:name 'dummy-proxy-block-store-open
		  #:c-name "chop_dummy_proxy_block_store_open_alloc"
		  #:returns '<store>
		  #:arguments '(((mchars caller-owned) name)
				(<store> backend)))

  (wrap-function! ws
		  #:name 'smart-block-store-open
		  #:c-name "chop_smart_block_store_open_alloc"
		  #:returns '<store>
		  #:arguments '((<store> backend)))

  (wrap-function! ws
		  #:name 'file-based-block-store-open
		  #:c-name "chop_file_based_store_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) class-name)
				((mchars caller-owned) file-name)
				(int open-flags (default "O_RDWR | O_CREAT"))
				(int mode (default "S_IRUSR | S_IWUSR"))
				((<store> out) new-store)))

  ;; XXX: In the end, the `gdbm', `tdb', etc. functions below should be
  ;; superseded by `file-based-block-store-open'.  This would allow Scheme
  ;; code to use libchop even if not all those store classes are available.

  (wrap-function! ws
		  #:name 'gdbm-block-store-open
		  #:c-name "chop_gdbm_block_store_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) name)
				(int block-size (default 0))
				(int open-flags (default "O_RDWR | O_CREAT"))
				(int mode (default "S_IRUSR | S_IWUSR"))
				((<store> out) new-gdbm-store)))

  (wrap-function! ws
		  #:name 'tdb-block-store-open
		  #:c-name "chop_tdb_block_store_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) name)
				(int hash-size (default 0))
				(int open-flags (default "O_RDWR | O_CREAT"))
				(int mode (default "S_IRUSR | S_IWUSR"))
				((<store> out) new-tdb-store)))

  (wrap-function! ws
		  #:name 'remote-block-store-open
		  #:c-name "chop_remote_block_store_open_alloc"
		  #:returns '<errcode>
		  #:arguments '(((mchars caller-owned) host)
				((mchars caller-owned) protocol)
				((<store> out) new-store)))

  (wrap-function! ws
		  #:name 'make-block-store
		  #:c-name "chop_make_scheme_block_store"
		  #:returns '<store>
		  #:arguments '((<raw-scheme-type> read-block-proc)
				(<raw-scheme-type> write-block-proc)
				(<raw-scheme-type> block-exists-proc)
				(<raw-scheme-type> remove-block-proc)
				(<raw-scheme-type> first-key-proc)
				(<raw-scheme-type> next-key-proc)
				(<raw-scheme-type> sync-proc)
				(<raw-scheme-type> close-proc)))

  (wrap-function! ws
		  #:name 'store-write-block
		  #:returns '<errcode>
		  #:c-name "chop_store_write_block"
		  #:arguments '(((<store> caller-owned) store)
				(<block-key> key)
				(<input-buffer> buffer))
		  #:description "Read from @var{stream}.")

  (wrap-function! ws
		  #:name 'store-read-block
		  #:returns '<errcode>
		  #:c-name "chop_store_read_block_alloc_u8vector"
		  #:arguments '((<store> store)
				(<block-key> key)
				((<raw-scheme-type> out) buffer))
		  #:description "Read from @var{store} the block whose key
is @var{key} and return a u8vector representing its content.")

  (wrap-function! ws
		  #:name 'store-block-exists?
		  #:returns '<errcode>
		  #:c-name "chop_store_block_exists"
		  #:arguments '((<store> store)
				(<block-key> key)
				((bool out) exists?))
		  #:description "Return @code{#t} if block under key
@var{key} exists in @var{store}.")

  (wrap-function! ws
		  #:name 'store-delete-block
		  #:returns '<errcode>
		  #:c-name "chop_store_delete_block"
		  #:arguments '((<store> store)
				(<block-key> key))
		  #:description "Delete from @var{store} block with key
@var{key}.")

  (wrap-function! ws
		  #:name 'store-first-key
		  #:returns '<errcode>
		  #:c-name "chop_store_first_key"
		  #:arguments '((<store> store)
				((<block-key> out) key))
		  #:description "Return the first key under which data is
available in @var{store}.")

  (wrap-function! ws
		  #:name 'store-next-key
		  #:returns '<errcode>
		  #:c-name "chop_store_next_key"
		  #:arguments '((<store> store)
				(<block-key> key)
				((<block-key> out) next-key))
		  #:description
"Return the key right after @var{key} under which data is available in
@var{store}.")

  (wrap-function! ws
		  #:name 'store-sync
		  #:returns '<errcode>
		  #:c-name "chop_store_sync"
		  #:arguments '((<store> store)))

  (wrap-function! ws
		  #:name 'store-close
		  #:returns '<errcode>
		  #:c-name "chop_store_close"
		  #:arguments '((<store> store)))

)

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; End: