#!/bin/sh
# aside from this initial boilerplate, this is actually -*- scheme -*- code
main='(module-ref (resolve-module '\''(scripts PROGRAM)) '\'main')'
exec ${GUILE-guile} -l $0 -c "(apply $main (cdr (command-line)))" "$@"
!#
;;; PROGRAM --- Does something

;; 	Copyright (C) 2005 Ludovic Courtès
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;;; Author: Ludovic Courtès

;;; Commentary:

;; Usage: PROGRAM [ARGS]
;;

;;; Code:

(set! %load-path (cons "/usr/local/share/guile/site" %load-path))

(define-module (scripts PROGRAM)
  #:use-module (chop streams)
  #:export (PROGRAM))

(define (PROGRAM . args)
  #t)

(define main PROGRAM)

;;; PROGRAM ends here
