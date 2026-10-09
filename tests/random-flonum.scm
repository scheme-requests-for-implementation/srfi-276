;;;; NOTE: SRFI 26 specific.
;;;;
;;;; FIXME: Many of these generators are definitely not uniform.

(define (random-flonum)
  (do ((bv (make-bytevector fl-byte-width))
       (i 0 (+ i 1)))
      ((= i fl-byte-width)
       (bytevector-flonum-native-ref bv 0))
    (bytevector-u8-set! bv i (random-integer 256))))

(define (make-random-flonum-generator)
  (gcons* 0.0 -0.0 1.0 -1.0 +inf.0 -inf.0 +nan.0 -nan.0
          random-flonum))

(define (flquantum x)
  (flmax (fl- (fladjacent x +inf.0) x)
	 (fl- x (fladjacent x -inf.0))))

(define (make-random-integer+fraction-flonum-generator)
  ;; Return a flonum whose exponent is in between
  ;; 52 and 0. This is the range of values that can have
  ;; nonzero fractional and integer parts.
  (lambda ()
    (let ((exponent (- (random-integer
			fl-precision)
		       fl-precision
		       ))
	  (sign (zero? (random-integer 2)))
	  (mantissa (random-integer (expt 2 fl-precision))))
      (flcopysign (make-flonum (flonum mantissa) 
			       exponent)
		  (if sign -1.0 1.0)))))

(define (make-random-inexact-integer-generator)
  (lambda ()
    (let ((sign (random-integer 2)))
      (* (if (zero? sign)
	     1.0
	     -1.0)
	 (random-integer (expt 2 fl-precision))))))

(define (make-random-inexact-integer+0.5-generator)
  (lambda ()
    (let ((sign (random-integer 2))
	  (i (random-integer (expt 2 (- fl-precision 2)))))
      (+ (* (if (zero? sign)
		1.0
		-1.0)
	    i)
	 0.5))))

(define (sign-generator)
  (gmap (lambda (b)
	  (if b
	      -1.0
	      1.0))
	(boolean-generator)))

(define (zero-generator)
  (gmap (lambda (b)
	  (if b -0.0 0.0))
	(boolean-generator)))

(define (make-random-flonum-below-unity-generator)
  (gcons*
   1e-30 0.1 0.5
   (fladjacent 1.0 -inf.0)
   (gremove
    flzero?
    (gmap (lambda (fl)
	    (let-values (((ipart fpart) (flinteger-fraction fl)))
	      fpart))
	  (make-random-finite-flonum-generator)))))

(define (make-random-flonum-above-unity-generator)
  (gcons*
   1.1 -1.1 2.0 -2.0
   (fladjacent 1.0 +inf.0)
   (fladjacent -1.0 -inf.0)
   (gmap (lambda (fl)
	   (if (fl>? (flabs fl) 1.0)
	       fl
	       (fl/ fl)))
	 (gfilter (lambda (x)
		    (and (not (flzero? x))
			 (not (fl=? (flabs x) 1.0))))
		  (make-random-finite-flonum-generator)))))
    
(define (make-random-finite-flonum-generator)
  (gfilter flfinite? (make-random-flonum-generator)))

(define (make-random-ordered-flonum-generator)
  (gremove flnan? (make-random-flonum-generator)))

(define (random-nan)
  (cond-expand
    ((library (srfi 208))
     (let ((payload (+ 1 
		       (random-integer #x7FFFFFFFFFF)))
	   (sign? (zero? (random-integer 2)))
	   (quiet? (zero? (random-integer 2))))
       (make-nan sign? quiet? payload (flonum 0.0))))
    (else +nan.0)))

(cond-expand
  ((not (library (srfi 252)))
   (define (boolean-generator)
     (gcons* #t #f (lambda ()
		     (zero? (random-integer 2))))))
  (else))

