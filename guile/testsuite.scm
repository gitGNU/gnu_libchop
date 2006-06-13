#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(testsuite)) '\'main')'
exec ${GUILE-guile} --debug -L module -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#
;;;
;;; Copyright 2005, 2006  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA


(define-module (testsuite))

(use-modules (chop core)
	     (chop cipher)
	     (chop filters)
	     (chop streams)
	     (chop stores)
	     (chop store-stats)
	     (chop choppers)
	     (chop block-indexers)
	     (chop indexers))

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Runs a test-suite to verify the behaviour of various things.  The main
;;; purpose of this script is to stress the garbage collector in order to
;;; catch potential errors.
;;;
;;; Code:

(define %stress-loop-count 700)

(define (stress name+thunk)
  (format #t "running `~a'... " (car name+thunk))
  (let ((thunk (cdr name+thunk)))
    (let loop ((i 0))
      (if (>= i %stress-loop-count)
	  (begin
	    (format #t "done~%")
	    (gc)
	    #t)
	  (begin
	    (if (not (thunk))
		(begin
		  (format #t "FAILED~%")
		  (exit 1)))
	    (if (= 0 (modulo i 334)) (gc))
	    (loop (+ 1 i)))))))

;; The name of a valid file that may be used as input for testing.
(define %test-input-file #f)


;;;
;;; Individual tests.
;;;

(define (t-stream)
  (stream-close (file-stream-open %test-input-file))
  (let* ((input (make-u8vector 500))
	 (s (mem-stream-open input))
	 (buf (make-u8vector 1)))
    (let loop ((total 0)
	       (read (false-if-exception (stream-read! s buf))))
      (if (not read)
	  (begin
	    ;;(format #t "got ~a bytes " total)
	    (stream-close s)
	    (= total (u8vector-length input)))
	  (begin
	    (and (eq? (u8vector-length buf) read)
		 (= (u8vector-ref buf 0) (u8vector-ref input total))
		 (loop (+ read total)
		       (if (< (+ total read) (u8vector-length input))
			   (begin
			     (u8vector-set! buf 0
					    (u8vector-ref input (+ total read)))
			     1)
			   #f)
; 		       (false-if-exception (stream-read! s buf))
		       )))))))


(define (t-indexer+log)
  (tree-indexer-log (tree-indexer-open 100)))

(define (t-hash)
  (hash-block-indexer-open 'hash-method/sha1))

(define (t-cipher)
  (cipher-open 'cipher-algo/blowfish 'cipher-mode/ecb))

(define (t-cipher+)
  (chk-block-indexer-open
   (cipher-open 'cipher-algo/blowfish 'cipher-mode/ecb)
   'hash-method/sha1 'hash-method/sha1))

(define (t-stat-store)
  (let ((s (stat-block-store-open "closing-stat-proxy"
				  (dummy-block-store-open "dummy1")
				  #t)))
    (store-write-block s #u8(1 2 3) #u8(1 2 3 4 5 6 7 8))
    (store-write-block s #u8(2 3 4) #u8(1 2 3 4 5 6 7 8))
    (stat-block-store-stats s)))

(define (t-complex1)
  (let* ((f (file-stream-open %test-input-file))
	 (c (anchor-based-chopper-open f))
	 (bi (hash-block-indexer-open 'hash-method/sha1))
	 (s (dummy-block-store-open "dummy2"))
	 ;; 			   (s (file-based-block-store-open
	 ;; 			       "tdb" ",,testsuite.db"))
	 (sbs (stat-block-store-open
	       "non-closing-stat-proxy"
	       s #f))
	 (i (tree-indexer-open 100)))
    ;;(set! s #f)
    (let ((index (indexer-index-blocks i c bi sbs sbs)))
      (format #f "index-handle: ~a~%"
	      (index-handle-ascii-serialize index)))
    (stream-close f)
    (for-each store-close (list sbs s))
    (let ((stats (stat-block-store-stats sbs)))
      ;;(set! sbs #f)
      ;;(gc)
      (values (block-store-stats:blocks-written stats)
	      (block-store-stats:virgin-bytes stats)
	      (block-store-stats:average-block-size
	       stats)))))

(define (t-complex2)
  (let ((s (filtered-stream-open (file-stream-open %test-input-file)
				 (zlib-zip-filter-init) #f))
	(vec (make-u8vector 1000)))
    (let loop ((ret (false-if-exception
		     (stream-read! s vec))))
      (if (not ret)
	  (begin
	    ;;(format #t "closing zstream~%")
	    (stream-close s)
	    #t)
	  (begin
	    ;;(gc)
	    ;;(format #t "ret=~a~%" ret)
	    (loop (false-if-exception
		   (stream-read! s vec))))))))

(define %uz (zlib-unzip-filter-init))
(define %z  (zlib-zip-filter-init 1))

(define (t-stacked-zip-filtered-streams)
  ;; XXX: Guile's GC leaks some objects.  However, since zlib-zip-filter
  ;; objects embed 64K of state (the compression buffer), the effect of leaks
  ;; is magnified.
  (let* ((size (stat:size (stat %test-input-file)))
	 (uz (filtered-stream-open
	      (filtered-stream-open (file-stream-open %test-input-file)
				    (zlib-zip-filter-init 1) #t)
	      (zlib-unzip-filter-init) #t))
	 (buf (make-u8vector 300)))

    (let loop ((total 0)
	       (read (false-if-exception (stream-read! uz buf))))
      (if (not read)
	  (begin
	    (stream-close uz)
	    (= total size))
	  (begin
	    (loop (+ total read)
		  (false-if-exception (stream-read! uz buf))))))))



;;;
;;; Core.
;;;

(define-macro (unit-test name)
  `(cons ',name ,(symbol-append 't- name)))

(define-public (testsuite . args)
  (set! %test-input-file (string-append (getcwd) "/testsuite.scm"))
  (if (not (false-if-exception (stat %test-input-file)))
      (error "could not determine a valid input file name"
	     %test-input-file))

  (for-each stress
	    (list (unit-test stream)
 		  (unit-test indexer+log)
 		  (unit-test hash)
 		  (unit-test cipher)
 		  (unit-test cipher+)
 		  (unit-test stat-store)
 		  (unit-test complex1)
 		  (unit-test complex2)
		  (unit-test stacked-zip-filtered-streams)
		  ))

  (format #t "~%* done!~%~%~%"))

(define main testsuite)

;;; arch-tag: 1a38077e-0f90-41c9-a480-0c6906f071ce

;;; testsuite.scm ends here
