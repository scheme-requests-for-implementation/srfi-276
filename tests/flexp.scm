(define (test-flexp)
  (test-group "flexp"
    (test-values (flexp 0.0) '(1.0))
    (test-values (flexp -0.0) '(1.0))
    (test-values (flexp +inf.0) '(+inf.0))
    (test-values (flexp -inf.0) '(0.0))
    (test-approximate fl-e (flexp 1.0) 1e-6)
    (test-approximate fl-1/e (flexp -1.0) 1e-6)))

(define (test-flexp-1)
  (test-group "flexp-1"
    (test-values (flexp-1 0.0) '(0.0))
    (test-values (flexp-1 -0.0) '(-0.0))
    (test-values (flexp-1 +inf.0) '(+inf.0))
    (test-values (flexp-1 -inf.0) '(-1.0))
    (test-approximate (fl- (flexp 1.0) 1.0)
		      (fl- fl-e 1.0)
		      1e-6)))

(define (test-flexp2)
  (test-group "flexp2"
    (test-values (flexp2 0.0) '(1.0))
    (test-values (flexp2 -0.0) '(1.0))
    (test-values (flexp2 +inf.0) '(+inf.0))
    (test-values (flexp2 -inf.0) '(0.0))
    (test-values (flexp2 1.0) '(2.0))
    (test-values (flexp2 -1.0) '(0.5))
    (test-values (flexp2 2.0) '(4.0))))

(define (test-flexp2-1)
  (test-group "flexp2-1"
    (test-values (flexp2-1 0.0) '(0.0))
    (test-values (flexp2-1 -0.0) '(-0.0))
    (test-values (flexp2-1 +inf.0) '(+inf.0))
    (test-values (flexp2-1 -inf.0) '(-1.0))
    (test-approximate (fl- (flexp2 1.0) 1.0)
		      1.0
		      1e-6)))

(define (test-flexp10)
  (test-group "flexp10"
    (test-values (flexp10 0.0) '(1.0))
    (test-values (flexp10 -0.0) '(1.0))
    (test-values (flexp10 +inf.0) '(+inf.0))
    (test-values (flexp10 -inf.0) '(0.0))
    (test-values (flexp10 1.0) '(10.0))
    (test-values (flexp10 -1.0) '(0.1))
    (test-values (flexp10 2.0) '(100.0))))

(define (test-flexp10-1)
  (test-group "flexp10-1"
    (test-values (flexp10-1 0.0) '(0.0))
    (test-values (flexp10-1 -0.0) '(-0.0))
    (test-values (flexp10-1 +inf.0) '(+inf.0))
    (test-values (flexp10-1 -inf.0) '(-1.0))
    (test-approximate (fl- (flexp10 1.0) 1.0)
		      9.0
		      1e-6)))

(define (test-flexpt-to-the-power-of-zero)
  (test-group "flexpt to the power of zero returns one"
    (test-property
     (lambda (fl zero)
       (fl=? (flexpt fl zero)
	     1.0))
     (list (make-random-ordered-flonum-generator)
	   (zero-generator)))))

(define (test-flexpt-0.0-negative-odd)
  (test-group "(flexpt zero odd) returns infinity"
    (test-property
     (lambda (fl zero)
       (fl=? (flexpt zero fl)
	     (flcopysign +inf.0 zero)))
     (list (gfilter (lambda (x)
		      (and (flodd? x)
			   (flnegative? x)))
		    (make-random-inexact-integer-generator))
	   (zero-generator)))))

(define (test-flexpt-0.0-negative-not-odd)
  (let ((prop
	 (lambda (fl zero)
	   (fl=? (flexpt zero fl) +inf.0)))
	(predicate?
	 (lambda (x)
	   (and (or (not (flinteger? x)) (not (flodd? x)))
		(flnegative? x)))))
    (test-group "(flexpt zero negative-even) returns positive infinity"
      (test-property
       prop
       (list
	(gfilter predicate? (make-random-inexact-integer-generator))
	(zero-generator))))
    (test-group "(flexpt zero negative-non-odd) returns positive infinity"
     (test-property
      prop
      (list
       (gfilter predicate? (make-random-finite-flonum-generator))
       (zero-generator))))))

(define (test-flexpt-finite-non-integer)
  (test-group "(flexpt negative non-integer) returns NaN"
    (test-property
     (lambda (negative-fl non-integer-fl)
       (flnan? (flexpt negative-fl non-integer-fl)))
     (list
      (gfilter flnegative? (make-random-finite-flonum-generator))
      (gremove flinteger? (make-random-finite-flonum-generator))))))

(define (test-flexpt-special-cases)
  (test-group "flexpt special cases"
    (test-values (flexpt 0.0 -inf.0)
		 '(+inf.0))
    (test-values (flexpt -0.0 -inf.0)
		 '(+inf.0))
    (test-values (flexpt 0.0 +inf.0)
		 '(+0.0))
    (test-values (flexpt -0.0 +inf.0)
		 '(+0.0))
    (test-values (flexpt -1.0 +inf.0)
		 '(1.0))
    (test-values (flexpt -1.0 -inf.0)
		 '(1.0))))

(define (test-flexpt-0.0-positive-odd)
  (test-group "(flexpt zero positive-odd) returns infinity"
    (test-property
     (lambda (zero positive-odd)
       (eqv? (flexpt zero positive-odd)
	     zero))
     (list (zero-generator)
	   (gfilter (lambda (x)
		      (and (flinteger? x)
			   (flodd? x)
			   (flpositive? x)))
		    (make-random-inexact-integer-generator))))))

(define (test-flexpt-0.0-positive-non-odd)
  (test-group "(flexpt zero positive-non-odd) returns infinity"
    (test-property
     (lambda (zero positive-non-odd)
       (eqv? (flexpt zero positive-non-odd) 0.0))
     (list (zero-generator)
	   (gfilter (lambda (x)
		      (and (flpositive? x)
			   (or
			    (not (flinteger? x))
			    (not (flodd? x)))))
		    (make-random-finite-flonum-generator))))))

(define (test-flexpt-1.0-power)
  (test-group "(flexpt 1.0 non-nan) returns 1.0"
    (test-property
     (lambda (non-nan)
       (fl=? (flexpt 1.0 non-nan) 1.0))
     (list (make-random-ordered-flonum-generator)))))

(define (test-flexpt-below-unity-to-infinity)
  (test-group "(flexpt x +inf.0) for |x| below unity returns +0.0"
    (test-property
     (lambda (x)
       (eqv? (flexpt x +inf.0) 0.0))
     (list
      (make-random-flonum-below-unity-generator)))))

(define (test-flexpt-below-unity-for-minus-infinity)
  (test-group "(flexpt x -inf.0) for |x| below unity returns +inf.0"
    (test-property
     (lambda (x)
       (fl=? (flexpt x -inf.0) +inf.0))
     (list (make-random-flonum-below-unity-generator)))))

(define (test-flexpt-above-unity-for-infinity)
  (test-group "(flexpt x +inf.0) for |x| above unity returns +inf.0"
    (test-property
     (lambda (x)
       (fl=? (flexpt x +inf.0) +inf.0))
     (list (make-random-flonum-above-unity-generator)))))

(define (test-flexpt-above-unity-for-minus-infinity)
  (test-group "(flexpt x -inf.0) for |x| above unity returns 0.0"
    (test-property
     (lambda (x)
       (eqv? (flexpt x -inf.0) 0.0))
     (list (make-random-flonum-above-unity-generator)))))

(define (test-flexpt-infinity-to-negative)
  (test-group "(flexpt +inf.0 x) for negative x returns 0.0"
    (test-property
     (lambda (x)
       (eqv? (flexpt +inf.0 x) 0.0))
     (list (gfilter flnegative? (make-random-finite-flonum-generator))))))

(define (test-flexpt-infinity-to-positive)
  (test-group "(flexpt +inf.0 x) for positive x returns +inf.0"
    (test-property
     (lambda (x)
       (fl=? (flexpt +inf.0 x) +inf.0))
     (list (gfilter flpositive? (make-random-finite-flonum-generator))))))

(define (test-flexpt-minus-infinity-to-positive-odd)
  (test-group "(flexpt -inf.0 x) for positive odd x returns -inf.0"
    (test-property
     (lambda (x)
       (fl=? (flexpt -inf.0 x) -inf.0))
     (list (gfilter (lambda (x)
		      (and (flpositive? x)
			   (flodd? x)))
		    (make-random-inexact-integer-generator))))))

(define (test-flexpt-minus-infinity-to-negative-odd)
  (test-group "(flexpt -inf.0 x) for negative odd x returns -0.0"
    (test-property
     (lambda (x)
       (eqv? (flexpt -inf.0 x) -0.0))
     (list (gfilter (lambda (x)
		      (and (flnegative? x)
			   (flodd? x)))
		    (make-random-inexact-integer-generator))))))

(define (test-flexpt-minus-infinity-to-positive-non-odd)
  (test-group "(flexpt -inf.0 x) for positive non-odd x returns +inf.0"
    (test-property
     (lambda (x)
       (eqv? (flexpt -inf.0 x) +inf.0))
     (list (gfilter (lambda (x)
		      (and (flpositive? x)
			   (or (not (flinteger? x))
			       (not (flodd? x)))))
		    (make-random-finite-flonum-generator))))))

(define (test-flexpt-minus-infinity-to-negative-non-odd)
  (test-group "(flexpt -inf.0 x) for negative non-odd x returns +0.0"
    (test-property
     (lambda (x)
       (eqv? (flexpt -inf.0 x) 0.0))
     (list (gfilter (lambda (x)
		      (and (flnegative? x)
			   (or (not (flinteger? x))
			       (not (flodd? x)))))
		    (make-random-finite-flonum-generator))))))

(define (test-flexpt-nan-rules)
  (define (prop x)
    (or (flnan? x) (fl=? x 1.0)))
  (test-group "flexpt NaN rules"
    (test-assert "(flexpt nan +0.0)"
		 (prop (flexpt +nan.0 +0.0)))
    (test-assert "(flexpt nan -0.0)"
		 (prop (flexpt +nan.0 -0.0)))
    (test-assert "(flexpt 1.0 nan)"
		 (prop (flexpt 1.0 +nan.0)))))