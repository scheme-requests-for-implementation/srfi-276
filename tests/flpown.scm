(define (test-flpown-simple)
  (test-group "flpown"
    (test-values (flpown 2.0 4) '(16.0))
    (test-values (flpown 1.0 100) '(1.0))
    (test-values (flpown 2.0 -3) '(0.125))))

(define (test-flpown-to-0)
  (test-group "(flpown x 0.0) = 1"
    (test-property
     (lambda (fl zero)
       (fl=? (flpown fl zero) 1.0))
     (list (make-random-flonum-generator)
	   (zero-generator)))))

(define (test-flpown-0-to-odd-negative-n)
  (test-group "(flpown zero n) for odd negative n is infinity"
    (test-property
     (lambda (zero n)
       (true-and-raises 'divide-by-zero
         (fl=? (flpown zero n)
	       (flcopysign +inf.0 zero))))
     (list (zero-generator)
	   (gfilter negative? (make-random-odd-integer-generator))))))

(define (test-flpown-0-to-even-negative-n)
  (test-group "(flpown zero n) for even negative n is positive infinity"
    (test-property
     (lambda (zero n)
       (true-and-raises 'divide-by-zero
         (fl=? (flpown zero n) +inf.0)))
     (list (zero-generator)
	   (gfilter negative? (make-random-even-integer-generator))))))

(define (test-flpown-0-to-even-positive-n)
  (test-group "(flpown zero n) for even positive n is positive zero"
    (test-property
     (lambda (zero n)
       (eqv? (flpown zero n) +0.0))
     (list (zero-generator)
	   (gfilter positive? (make-random-even-integer-generator))))))

(define (test-flpown-0-to-odd-positive-n)
  (test-group "(flpown zero n) for odd positive n is zero"
    (test-property
     (lambda (zero n)
       (eqv? (flpown zero n) zero))
     (list (zero-generator)
	   (gfilter positive? (make-random-odd-integer-generator))))))

(define (test-flpown-infinity-n)
  (test-group "(flpown +inf.0 n) is +inf.0"
    (test-property
     (lambda (n)
       (fl=? (flpown +inf.0 n) +inf.0))
     (list (gfilter positive? (integer-generator))))))

(define (test-flpown-negative-infinity-odd-positive-n)
  (test-group "(flpown -inf.0 n) for odd positive n is -inf.0"
    (test-property
     (lambda (n)
       (fl=? (flpown -inf.0 n) -inf.0))
     (list (gfilter (lambda (x) (and (odd? x) (positive? x)))
		    (integer-generator))))))

(define (test-flpown-negative-infinity-even-positive-n)
  (test-group "(flpown -inf.0 n) for even positive n is +inf.0"
    (test-property
     (lambda (n)
       (fl=? (flpown -inf.0 n) +inf.0))
     (list (gfilter (lambda (x) (and (even? x) (positive? x)))
		    (integer-generator))))))

(define (test-flpown-positive-infinity-to-negative-n)
  (test-group "(flpown +inf.0 n) for negative n is +0.0"
    (test-property
     (lambda (n)
       (eqv? (flpown +inf.0 n) +0.0))
     (list (gfilter negative? (integer-generator))))))

(define (test-flpown-negative-infinity-to-odd-negative-n)
  (test-group "(flpown -inf.0 n) for odd negative n is negative 0"
    (test-property
     (lambda (n)
       (eqv? (flpown -inf.0 n) -0.0))
     (list (gfilter (lambda (x) (and (odd? x) (negative? x)))
		    (integer-generator))))))

(define (test-flpown-negative-infinity-to-even-negative-n)
  (test-group "(flpown -inf.0 n) for even negative n is 0"
    (test-property
     (lambda (n)
       (eqv? (flpown -inf.0 n) 0.0))
     (list (gfilter (lambda (x) (and (even? x) (negative? x)))
		    (integer-generator))))))
