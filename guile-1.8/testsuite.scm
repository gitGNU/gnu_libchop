#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(testsuite)) '\'main')'
exec ${GUILE-guile} -L modules -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#
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

(define-module (testsuite))

(use-modules (chop core)
             (chop objects)
	     (chop cipher)
	     (chop filters)
	     (chop streams)
	     (chop stores)
	     (chop store-stats)
	     (chop choppers)
	     (chop block-indexers)
	     (chop indexers)

	     (srfi srfi-1)
             (srfi srfi-11))  ;; `let-values'

;;; Author:  Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Runs a test-suite to verify the behaviour of various things.  The main
;;; purpose of this script is to stress the garbage collector in order to
;;; catch potential errors.
;;;
;;; Code:

(define %stress-loop-count 500)

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

(define (t-class)
  (and (every (lambda (name)
                (equal? (class-name (class-lookup name))
                        name))
              '("fixed_size_chopper" "file_stream"
                "hash_block_indexer" "chk_block_indexer"))
       (object-is-a? (class-lookup "fixed_size_chopper")
                     (class-lookup "chopper_class"))
       (let ((input (mem-stream-open '#u8(1 2 3 4 5))))
         (every (lambda (name)
                  (let ((long-name (string-append name "_chopper")))
                    (object-is-a? (chopper-generic-open name input)
                                  (class-lookup long-name))))
                '("fixed_size" "anchor_based")))))

(define (t-serialize)
  (let ((input (mem-stream-open '#u8(1 2 3 4 5))))
    (every (lambda (obj)
             (let ((str (serialize-object/ascii obj)))
               (string? str)))
           (map block-indexer-make-fetcher
                (list (hash-block-indexer-open 'hash-method/sha1)
                      (chk-block-indexer-open (cipher-open
                                               'cipher-algo/blowfish
                                               'cipher-mode/ecb)
                                              'hash-method/sha1
                                              'hash-method/sha1))))))

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

(define (t-stream->port)
  (let* ((input (make-u8vector 7777))
         (port  (stream->port (mem-stream-open input)))
         (buf   (make-u8vector 1)))
    (let loop ((total 0))
      (if (>= total (u8vector-length input))
          (= total (u8vector-length input))
          (let ((read (uniform-vector-read! buf port)))
            (and (or (= read 0)
                     (and (= read 1)
                          (= (u8vector-ref input total)
                             (u8vector-ref buf 0))))
                 (loop (+ read total))))))))


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

(define (t-tuple-deserialization)
  (let ((tuple "tree_indexer:chk_block_fetcher:chk_index_handle:64:blowfish,ecb:b518771f907d75c54b8c604c6f25070c,0105338239a6e950db0234ff2d917dc6/112"))
    (let-values (((handle indexer fetcher bytes-read)
                  (ascii-deserialize-index-tuple tuple)))
      (if (and ;;(indexer? indexer) (block-fetcher? fetcher)
               ;;(index-handle? handle)
               (equal? bytes-read (string-length tuple)))
          #t
          (error "tuple deserialization failed")))))

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
      (and (equal? index
                   (index-handle-ascii-deserialize
                    (class-lookup "hash_index_handle")
                    (index-handle-ascii-serialize index)))
           (begin
             (stream-close f)
             (for-each store-close (list sbs s))
             (let ((stats (stat-block-store-stats sbs)))
               ;;(set! sbs #f)
               ;;(gc)
               (values (block-store-stats:blocks-written stats)
                       (block-store-stats:virgin-bytes stats)
                       (block-store-stats:average-block-size
                        stats))))))))

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


(define (t-stacked-zip-filtered-streams)
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


(define (t-aggregated-objects)
  ;; Check whether filters aggregated by a filtered stream are eventually
  ;; GC'd.  This can take some time.
  (define (query-guardian guardian expected-object-count)
    (define max-iterations (* expected-object-count 4))
    (let loop ((objects-seen 0)
               (iterations   0))
      ;;(format #t "seen ~a~%" objects-seen)
      (cond ((>= objects-seen expected-object-count)
             #t)
            ((>= iterations max-iterations)
             #f)
            (else
             (if (= 0 (modulo iterations expected-object-count))
                 (gc))
             (loop (if (guardian)
                       (+ 1 objects-seen)
                       objects-seen)
                   (+ 1 iterations))))))

  (let ((g (make-guardian)))
    (let* ((zip-filter   (zlib-zip-filter-init 1))
           (unzip-filter (zlib-unzip-filter-init))
           (uz (filtered-stream-open
                (filtered-stream-open (file-stream-open %test-input-file)
                                      zip-filter #t)
                unzip-filter #t)))
      (for-each g (list zip-filter unzip-filter uz))
      (set! zip-filter #f)
      (set! unzip-filter #f)
      (set! uz #f))

    (query-guardian g 3)))


;;;
;;; Core.
;;;

(define-macro (unit-test name)
  `(cons ',name ,(symbol-append 't- name)))

(define-public (testsuite . args)
  (set! %test-input-file
	(find file-exists?
	      (list (string-append (getcwd) "/testsuite.scm")
		    (or (getenv "GUILE") "")
		    "Makefile"
		    "/etc/fstab")))

  (format #t "test input file: `~a'~%~%" %test-input-file)
  (if (not %test-input-file)
      (error "could not determine a valid input file name"
	     %test-input-file))

  (for-each stress
	    (list (unit-test class)
                  (unit-test serialize)
                  (unit-test stream)
                  (unit-test stream->port)
 		  (unit-test indexer+log)
 		  (unit-test hash)
 		  (unit-test cipher)
 		  (unit-test cipher+)
                  (unit-test tuple-deserialization)
 		  (unit-test stat-store)
 		  (unit-test complex1)
 		  (unit-test complex2)
 		  (unit-test stacked-zip-filtered-streams)
                  (unit-test aggregated-objects)
		  ))

  (format #t "~%* done!~%"))

(define main testsuite)

;;; arch-tag: 1a38077e-0f90-41c9-a480-0c6906f071ce

;;; testsuite.scm ends here
