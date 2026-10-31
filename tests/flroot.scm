; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-flroot-simple)
  (test-group "flroot"
    (test-equal 2.0 (flroot 65536.0 16))
    (test-equal 0.5 (flroot 65536.0 -16))
    ;; FIXME: Double64
    (test-approximate "(flroot 15.0 4)"
      1.9679896712654303
      (flroot 15.0 4)
      0.0000000000001)
    (test-approximate "(flroot 15.0 -4)"
      .5081327481546147
      (flroot 15.0 -4)
      0.0000000000001)
    (test-approximate "(flroot 17.0 4)"
      2.0305431848689306
      (flroot 17.0 4)
      0.0000000000001)
    (test-approximate "(flroot 17.0 -4)"
      .4924790605054523
      (flroot 17.0 -4)
      .00000000000001)))

(define (test-flroot-0-to-odd-negative-n)
  (test-group "(flroot zero n) for odd negative n is infinity"
    (test-property
     (lambda (zero n)
       (true-and-raises 'divide-by-zero
         (fl=? (flroot zero n)
	       (flcopysign +inf.0 zero))))
     (list (zero-generator)
	   (gfilter (lambda (x) (and (odd? x) (negative? x)))
		    (integer-generator))))))

(define (test-flroot-0-to-even-negative-n)
  (test-group "(flroot zero n) for even negative n is positive infinity"
    (test-property
     (lambda (zero n)
       (true-and-raises 'divide-by-zero
         (fl=? (flroot zero n) +inf.0)))
     (list (zero-generator)
	   (gfilter (lambda (x) (and (even? x) (negative? x)))
		    (integer-generator))))))

(define (test-flroot-0-to-even-positive-n)
  (test-group "(flroot zero n) for even positive n is positive zero"
    (test-property
     (lambda (zero n)
       (eqv? (flroot zero n) +0.0))
     (list (zero-generator)
	   (gfilter (lambda (x) (and (even? x) (positive? x)))
		    (integer-generator))))))

(define (test-flroot-0-to-odd-positive-n)
  (test-group "(flroot zero n) for odd positive n is zero"
    (test-property
     (lambda (zero n)
       (eqv? (flroot zero n) zero))
     (list (zero-generator)
	   (gfilter (lambda (x) (and (odd? x) (positive? x)))
		    (integer-generator))))))

(define (test-flroot-infinity-n)
  (test-group "(flroot +inf.0 n) is +inf.0"
    (test-property
     (lambda (n)
       (fl=? (flroot +inf.0 n) +inf.0))
     (list (gfilter positive? (integer-generator))))))

(define (test-flroot-negative-infinity-odd-positive-n)
  (test-group "(flroot -inf.0 n) for odd positive n is -inf.0"
    (test-property
     (lambda (n)
       (fl=? (flroot -inf.0 n) -inf.0))
     (list (gfilter (lambda (x) (and (odd? x) (positive? x)))
		    (integer-generator))))))

(define (test-flroot-negative-infinity-even-positive-n)
  (test-group "(flroot -inf.0 n) for even positive n is NaN"
    (test-property
     (lambda (n)
       (true-and-raises 'invalid-operation
         (flnan? (flroot -inf.0 n))))
     (list (gfilter (lambda (x) (and (even? x) (positive? x)))
		    (integer-generator))))))

(define (test-flroot-positive-infinity-to-negative-n)
  (test-group "(flroot +inf.0 n) for negative n is -0.0"
    (test-property
     (lambda (n)
       (eqv? (flroot +inf.0 n) -0.0))
     (list (gfilter negative? (integer-generator))))))

(define (test-flroot-negative-infinity-to-odd-negative-n)
  (test-group "(flroot -inf.0 n) for odd negative n is negative 0"
    (test-property
     (lambda (n)
       (eqv? (flroot -inf.0 n) -0.0))
     (list (gfilter (lambda (x) (and (odd? x) (negative? x)))
		    (integer-generator))))))

(define (test-flroot-negative-infinity-to-even-negative-n)
  (test-group "(flroot -inf.0 n) for even negative n is NaN"
    (test-property
     (lambda (n)
       (true-and-raises 'invalid-operation
         (flnan? (flroot -inf.0 n))))
     (list (gfilter (lambda (x) (and (even? x) (negative? x)))
		    (integer-generator))))))

(define (test-flroot-to-zero)
  (test-group "(flroot x 0) is NaN"
    (test-property
     (lambda (fl)
       (true-and-raises 'invalid-operation
         (flnan? (flroot fl 0))))
     (list (make-random-flonum-generator)))))

(define (test-flroot-negative-x-to-negative-n)
  (test-group "(flroot x n) for negative x, even n is NaN"
    (test-property
     (lambda (fl n)
       (true-and-raises 'invalid-operation
         (flnan? (flroot fl n))))
     (list (gfilter negative? (make-random-flonum-generator))
	   (make-random-even-integer-generator)))))
