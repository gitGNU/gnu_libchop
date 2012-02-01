;;; libchop -- a utility library for distributed storage and data backup
;;; Copyright (C) 2012  Ludovic Courtès <ludo@gnu.org>
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

;;; Test suite support.

(define-module (chop tests)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (chop core)
  #:use-module (chop objects)
  #:use-module (chop stores)
  #:use-module (chop indexers)
  #:export (with-temporary-file
            with-temporary-store
            with-file-tree
            make-random-bytevector
            random-file-size
            index-handle->block-key))

(define (with-temporary-file proc)
  (let ((file (tmpnam)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc file))
      (lambda ()
        (delete-file file)))))

(define (with-temporary-store proc)
  (with-temporary-file
   (lambda (file)
     (let ((s (file-based-block-store-open (lookup-class "gdbm_block_store")
                                           file
                                           (logior O_RDWR O_CREAT)
                                           #o644)))
       (dynamic-wind
         (lambda () #t)
         (lambda () (proc s))
         (lambda ()
           (store-close s)))))))

(define (make-random-bytevector n)
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256))
            (loop (1+ i)))
          bv))))

(define (populate-file file size)
  (call-with-output-file file
    (lambda (p)
      (put-bytevector p (make-random-bytevector size)))))

(define (index-handle->block-key index)
  "Return the block key corresponding to INDEX as a bytevector."
  ;; XXX: This is a hack as it breaks the block-indexer interface and makes
  ;; assumptions about the serialization format.
  (cond ((object-is-a? index (lookup-class "chk_index_handle"))
         (let* ((serial (serialize-object/ascii index))
                (comma  (string-index serial #\,))
                (slash  (string-rindex serial #\/)))
           (base32-string->bytevector
            (substring serial (1+ comma) slash))))
        (else
         (error "unsupported index handle class" index))))


;;;
;;; File system testing tools, initially contributed to Guile's test suite.
;;;

(define (random-file-size)
  (define %average (* 1024 1024))                 ; 1 MiB
  (define %stddev  (* 16 1024))                   ; 16 KiB
  (inexact->exact
   (max 0 (round (+ %average (* %stddev (random:normal)))))))

(define (make-file-tree dir tree)
  "Make file system TREE at DIR."
  (let loop ((dir  dir)
             (tree tree))
    (define (scope file)
      (string-append dir "/" file))

    (match tree
      (('directory name (body ...))
       (mkdir (scope name))
       (for-each (cute loop (scope name) <>) body))
      (('directory name (? integer? mode) (body ...))
       (mkdir (scope name))
       (for-each (cute loop (scope name) <>) body)
       (chmod (scope name) mode))
      ((file)
       (populate-file (scope file) (random-file-size)))
      ((file (? integer? mode))
       (populate-file (scope file) (random-file-size))
       (chmod (scope file) mode))
      ((from '-> to)
       (symlink to (scope from))))))

(define (delete-file-tree dir tree)
  "Delete file TREE from DIR."
  (let loop ((dir  dir)
             (tree tree))
    (define (scope file)
      (string-append dir "/" file))

    (match tree
      (('directory name (body ...))
       (for-each (cute loop (scope name) <>) body)
       (rmdir (scope name)))
      (('directory name (? integer? mode) (body ...))
       (chmod (scope name) #o755)          ; make sure it can be entered
       (for-each (cute loop (scope name) <>) body)
       (rmdir (scope name)))
      ((from '-> _)
       (delete-file (scope from)))
      ((file _ ...)
       (delete-file (scope file))))))

(define-syntax-rule (with-file-tree dir tree body ...)
  (dynamic-wind
    (lambda ()
      (make-file-tree dir 'tree))
    (lambda ()
      body ...)
    (lambda ()
      (delete-file-tree dir 'tree))))
