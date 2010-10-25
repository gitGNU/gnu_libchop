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

(define-module (chop test-suite)
  #:use-module (chop)
  #:use-module (chop objects)
  #:use-module (chop streams)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "objects")

(test-equal "hex strings"
  '(#vu8(1 2 3 4 5 6 7 8 9 10) . 20)

  (call-with-values
      (lambda ()
        (hex-string->bytevector
         (bytevector->hex-string #vu8(1 2 3 4 5 6 7 8 9 10))))
    cons))

(test-equal "base32 strings"
  '(#vu8(1 2 3 4 5 6 7 8 9 10) . 16)

  (call-with-values
      (lambda ()
        (base32-string->bytevector
         (bytevector->base32-string #vu8(1 2 3 4 5 6 7 8 9 10))))
    cons))

(test-assert "lookup-class & co."
  (and (every (lambda (name)
                (equal? (class-name (lookup-class name))
                        name))
              '("fixed_size_chopper" "file_stream"
                "hash_block_indexer" "chk_block_indexer"))
       (object-is-a? (lookup-class "fixed_size_chopper")
                     (lookup-class "chopper_class"))))

(test-end)

(exit (= (test-runner-fail-count (test-runner-current)) 0))
