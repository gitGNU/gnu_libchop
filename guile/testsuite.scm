#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(testsuite)) '\'main')'
exec ${GUILE-guile} -L module -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#
;;;
;;; Copyright 2005  Ludovic Courtès <ludovic.courtes@laas.fr>
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
;;; Runs a test-suite to verify the behaviour of various things.
;;;
;;; Code:

(define (stress thunk)
  (format #t "stressing... ")
  (let loop ((i 0))
    (if (>= i 10000)
	(begin
	  (format #t "done~%")
	  (gc)
	  #t)
	(begin
	  (thunk)
	  (loop (+ 1 i))))))

(define-public (testsuite . args)
  (for-each stress
	    (list (lambda ()
		    (stream-close (file-stream-open "testsuite.scm")))
		  (lambda ()
		    (hash-block-indexer-open 'hash-method/sha1))
		  (lambda ()
		    (cipher-open 'cipher-algo/blowfish 'cipher-mode/ecb))
		  (lambda ()
		    (tree-indexer-log (tree-indexer-open 100)))
		  (lambda ()
		    (chk-block-indexer-open
		     (cipher-open 'cipher-algo/blowfish 'cipher-mode/ecb)
		     'hash-method/sha1 'hash-method/sha1))
		  (lambda ()
		    (stat-block-store-open "hello"
					   (dummy-block-store-open
					    "world")))
		  (lambda ()
		    (let* ((f (file-stream-open "testsuite.scm"))
			   (c (anchor-based-chopper-open f))
			   (bi (hash-block-indexer-open 'hash-method/sha1))
			   (s (dummy-block-store-open "world"))
; 			   (s (file-based-block-store-open
; 			       "tdb" ",,testsuite.db"))
			   (sbs (stat-block-store-open "hello" s))
			   (i (tree-indexer-open 100)))
		      ;;(set! s #f)
		      (let ((index (indexer-index-blocks i c bi sbs sbs)))
			(format #f "index-handle: ~a~%"
				(index-handle-ascii-serialize index)))
		      (stream-close f)
		      (for-each store-close (list sbs s))
		      (let ((stats (stat-block-store-stats sbs)))
			(values (block-store-stats:blocks-written stats)
				(block-store-stats:virgin-bytes stats)
				(block-store-stats:average-block-size stats)))))
		  ))
  (format #t "~%* done!~%~%~%"))

(define main testsuite)

;;; arch-tag: 1a38077e-0f90-41c9-a480-0c6906f071ce

;;; testsuite.scm ends here
