;; This is a naive implementation of the anchor selection mechanism described
;; by Udi Manber in [1].  His work is based on Rabin's fingerprints.
;;
;;
;; How to try it
;; =============
;;
;; Start Guile [2] (1.6+) and load this file.  Then try the following calls:
;;
;; guile> (show-anchors "Comment ca va?" 4)
;; ("Comm" "ca v" " va" "va")
;; guile> (show-anchors "Bonjour! Comment ca va?" 4)
;; ("onjo" "ur! " "Comm" "ca v" " va" "va")
;; guile> (show-anchors "Bonjour! Comment ca va? On fait aller." 4)
;; ("onjo" "ur! " "Comm" "ca v" " va?" "va? " "? On" "ait " "it a" "alle" "r")
;; guile> (show-anchors "Bonjour! On fait aller." 4)
;; ("onjo" "ur! " "ait " "it a" "alle" "r")
;;
;; Got the point?  Magic!  :-)
;;
;;
;; [1] Udi Manber.  Finding similar files in a large file system.
;;     In Proceedings of the Usenix Winter 1994 Conference, pages 1--10,
;;     January, 1994, http://www.cs.arizona.edu/research/reports.html.
;; [2] http://www.gnu.org/software/guile/ .
;;
;; Ludovic Courtès <ludovic.courtes@laas.fr>.

(use-modules (ice-9 optargs)
	     (ice-9 rdelim)
	     (ice-9 format))


;; Below are the three main parameters of the algorithm.  The fourth one is
;; the WINDOW-SIZE argument (see further below) which defines the size of the
;; "sliding window" that is used to compute fingerprints.
(define *prime-number* 3)
(define *modulo* (expt 2 30))

(define (anchor-fingerprint? fpr)
  "Decide whether fingerprint FPR is suitable for an anchor."
  (= 0 (logand fpr #x3f)))


(define* (compute-fingerprint chars size
			      #:key (prev-first-char #f) (prev-fpr #f))
  "Compute the fingerprint of the (at most) SIZE characters in list CHARS.
Passing PREV-FPR (the fingerprint of the SIZE characters located at the
current offset minus one) and PREV-FIRST-CHAR (the character that occured
before the first element in CHARS) allows for an optimization."
  (modulo (if (and prev-first-char prev-fpr)

	      ;; optimization
	      (+ (* prev-fpr *prime-number*)   ;; p x F1
		 (if (>= (length chars) size)  ;; t51
		     (begin
		       (format #t "last-char: ~a~%"
			       (list-ref chars (- size 1)))
		       (char->integer (list-ref chars (- size 1))))
		     0)
		 (- (* (char->integer prev-first-char) ;; - t1 x p^50
		       (expt *prime-number* size))))

	      (let loop ((chars chars)
			 (count 1)
			 (result 0))
		(if (or (null? chars) (> count size))
		    result
		    (loop (cdr chars)
			  (+ 1 count)
			  (+ result
			     (* (char->integer (car chars))
				(expt *prime-number* (- size count))))))))
	      *modulo*))

(define (find-anchors str window-size)
  "Find anchors in STR (using a `sliding window' of WINDOW-SIZE characters)
and return a list containing the positions of these anchors."
  (let loop ((chars (string->list str))
	     (offset 0)
	     (previous-fingerprint #f)
	     (previous-first-char #f)
	     (anchors '()))
    (if (null? chars)
	anchors
	(let* ((this-fpr (if previous-fingerprint
			     (compute-fingerprint
			      chars window-size
			      #:prev-fpr previous-fingerprint
			      #:prev-first-char previous-first-char)
			     (compute-fingerprint chars window-size)))
	       (is-anchor? (and (> offset 0)
				(anchor-fingerprint? this-fpr)))
	       (bytes-to-skip* (if (not is-anchor?) 1 window-size))
	       (bytes-to-skip (min (- (string-length str) offset)
				   bytes-to-skip*)))
	  (format #t "offset: ~a skip: ~a this-fpr: ~x~%"
		  offset bytes-to-skip this-fpr)
	  ;; When an anchor is found, jump till the end of the window, as
	  ;; described in [1].
	  (loop (list-tail chars bytes-to-skip)
		(+ offset bytes-to-skip)
		(if is-anchor? #f this-fpr)
		(car chars)
		(if is-anchor?
		    (append anchors (list offset))
		    anchors))))))

(define (show-anchors str window-size)
  "Return the list of anchors for STR, using a window size of WINDOW-SIZE."
  (map (lambda (anchor-pos)
	 (let ((len (string-length str))
	       (end (+ anchor-pos window-size)))
	   (substring str anchor-pos (min (- len 1) end))))
       (find-anchors str window-size)))

(define (show-blocks str window-size)
  "Cut STR into blocks around each anchor found."
  (let loop ((anchors (find-anchors str window-size))
	     (prev-anchor #f)
	     (blocks '()))
    (if (null? anchors)
	(append blocks
		(if (not prev-anchor)
		    '()
		    (list (substring str prev-anchor
				     (string-length str)))))
	(loop (cdr anchors)
	      (car anchors)
	      (append blocks
		      (list (if (not prev-anchor)
				(substring str 0 (car anchors))
				(substring str prev-anchor
					   (car anchors)))))))))

(define (file-contents file)
   "Return a string representing the contents of FILE."
   (with-input-from-file file
      (lambda ()
	 (let loop ((line (read-line))
		    (contents ""))
	    (if (eof-object? line)
		contents
		(loop (read-line)
		      (string-append contents
				     (if (string=? contents "") "" "\n")
				     line)))))))

(define* (show-file-blocks file #:optional (window-size 50))
  "Show the anchors of FILE's contents."
  (let ((blocks (show-blocks (file-contents file) window-size)))
    (for-each (lambda (block)
		(display block)
		(display "\n-----\n"))
	      blocks)))

;; Local Variables:
;; mode: scheme
;; scheme-program-name: "guile"
;; eval: (run-scheme)
;; End:
