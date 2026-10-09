(define (binary-fltotal=? x y)
  (or (eqv? x y)
      (and (flnan? x) (flnan? y)
	   (nan=? x y))))

(cond-expand
  ((library (srfi 208))
   ;; Order NaNs against non-NaN values by sign.
   (define (nan<non-nan? the-nan the-non-nan)
     ;; negative NaNs sort below all finite values.
     (flsign-negative? the-non-nan))
   ;; nan=? is already defined
   (define (nan-positive? n1) (not (nan-negative? n1)))
   (define (nan-signaling? n1) (not (nan-quiet? n1)))
   (define (nan<nan? n1 n2)
     (cond
      ((and (nan-negative? n1) (nan-positive? n2))
       #t)
      ((and (nan-positive? n1) (nan-negative? n2))
       #f)
      ((and (nan-negative? n1) (nan-negative? n2)
	    (nan-quiet? n1) (nan-signaling? n2))
       #t)
      ((and (nan-negative? n1) (nan-negative? n2)
	    (nan-signaling? n1) (nan-quiet? n2))
       #f)
      ((and (nan-positive? n1) (nan-positive? n2)
	    (nan-quiet? n1) (nan-signaling? n2))
       #f)
      ((and (nan-positive? n1) (nan-positive? n2)
	    (nan-signaling? n1) (nan-quiet? n2))
       #t)
      (else
       (< (nan-payload n1) (nan-payload n2))))))
  (else (define (nan<non-nan? the-nan the-non-nan) #t)
	(define (nan<nan? n1 n2) #f)
	(define (nan=? n1 n2) #t)))

(define (binary-fltotal<? x y)
  (cond
    ((and (flnan? x) (not (flnan? y)))
     (nan<non-nan? x y))
    ((and (flnan? y) (not (flnan? x)))
     (not (nan<non-nan? y x)))
    ((and (flnan? x) (flnan? y))
     (nan<nan? x y))
    ((and (flzero? x) (flzero? y))
     (and (flsign-negative? x)
	  (not (flsign-negative? y))))
    (else (fl<? x y))))

(define (transitive binary)
  (lambda list
    (cond
     ((null? list) #t)
     ((null? (cdr list)) #t)
     (else
      (let loop ((el1 (car list))
		 (el2 (cadr list))
		 (list (cddr list)))
	(and (binary el1 el2)
	     (or (null? list)
		 (loop el2 (car list) (cdr list)))))))))

(define (flip f)
  (lambda (x y) (f y x)))

(define (procedure-or f g)
  (lambda (x y) (or (f x y) (g x y))))

(define fltotal=? (transitive binary-fltotal=?))
(define fltotal<? (transitive binary-fltotal<?))
(define fltotal>? (transitive (flip binary-fltotal<?)))
(define fltotal<=? (transitive (procedure-or binary-fltotal=?
					     binary-fltotal<?)))
(define fltotal>=? (transitive (procedure-or binary-fltotal=?
					     (flip binary-fltotal<?))))
