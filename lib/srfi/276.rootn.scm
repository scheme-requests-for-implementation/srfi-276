; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (flroot fl n)
  (unless (integer? n)
    (error "Not an integer" n))
  (cond
    ((zero? n)
     (maybe-raise-flexceptions! 'invalid-operation)
     +nan.0)
    ((and (flnegative? fl) (even? n))
     (maybe-raise-flexceptions! 'invalid-operation)
     +nan.0)
    ((and (flzero? fl) (negative? n) (odd? n))
     (maybe-raise-flexceptions! 'divide-by-zero)
     (flcopysign +inf.0 fl))
    ((and (flzero? fl) (negative? n) (even? n))
     (maybe-raise-flexceptions! 'divide-by-zero)
     +inf.0)
    ((and (flzero? fl) (positive? n) (even? n))
     +0.0)
    ((and (flzero? fl) (positive? n) (odd? n))
     fl)
    ((and (fl=? fl +inf.0) (positive? n))
     +inf.0)
    ((and (fl=? fl +inf.0) (negative? n))
     -0.0)
    ((and (fl=? fl -inf.0) (positive? n) (odd? n))
     -inf.0)
    ((and (fl=? fl -inf.0) (positive? n) (even? n))
     (maybe-raise-flexceptions! 'invalid-operation)
     +nan.0)
    ((and (fl=? fl -inf.0) (negative? n) (odd? n))
     -0.0)
    ((and (fl=? fl -inf.0) (negative? n) (even? n))
     (maybe-raise-flexceptions! 'invalid-operation)
     +nan.0)
    ((flnan? fl) fl)
    ((= n 1) (fl/ fl))
    ((= n 2) (flsqrt fl))
    ((= n -2) (flrsqrt fl))
    ((= n 3) (flcbrt fl))
    ((positive? n)
     (flcopysign (%flroot-positive (flabs fl) (exact n)) fl))
    (else
     (flcopysign
      (%flroot-negative (flabs fl) (exact n))
      fl))))

(define (fast-flpown fl n)
  ;; flpown but without double-double arithmetic.
  (if (negative? n)
      (fl/ (fast-flpown fl (- n)))
      (let loop ((acc 1.0)
		 (i (- (integer-length n) 1)))
	(if (negative? i)
	    acc
	    (let ((acc (fl* acc acc))
		  (hit? (not (zero?
			      (bitwise-and 1 (arithmetic-shift n (- i)))))))
	      (if hit?
		  (loop (* acc fl) (- i 1))
		  (loop acc (- i 1))))))))

(define (%flroot-negative fl n)
  #|
See %flroot-positive for a more detailed explanation.

In this, n is positive.

fl = x

x = s*b^e
x^(-1/n) = (s*b^e)^(-1/n)

E + g/n = e/n
e = En + g

x^(-1/n) = (s*b^g)^/(-1/n) * b^-E

Newton-Raphson iteration:
f(y) = y^-n - x
f'(y) = -ny^{n-1}

y_{n+1} = y_n - f(y_n)/f'(y_n)
        = y_n - (y_n^-n - x)/(-ny_n^{-n-1})
        = y_n + (y_n - xy_n^{n+1})/n
        = ((n+1)y_n - xy_n^{n+1})/n
  |#
  (let*-values (((s e) (flnormalized-fraction-exponent fl))
		((s) (fl* s (flonum fl-radix)))
		((e) (- e 1))
		((E g) (truncate/ e n))
		;; E is negative, g is positive
		((S) (make-flonum s g)))
    (if (fl=? S 1.0)
	(make-flonum S E)
	(let ((y
	       ;; This is different from the positive case, because
	       ;; the function goes from 0.5 to 1.0, and is strict
	       ;; monotonically decreasing instead of increasing.
	       (let bisection ((i 0)
			       (lower 0.5)
			       (upper 1.0))
		 (let ((guess (fl+ (fl/ (fl- upper lower) 2.0) lower)))
		   (if (= i 4)
		       guess
		       (let ((value (fl- (fast-flpown guess n) S)))
			 (if (flnegative? value)
			     (bisection (+ i 1)
					lower
					guess)
			     (bisection (+ i 1)
					guess
					upper))))))))
	  (do ((fl-1/n (fl/ (flonum (abs n))))
	       (fl:n+1 (flonum (+ (abs n) 1)))
	       (n+1 (+ (abs n) 1))
	       (y y (fl* fl-1/n
			 (fl- (fl* fl:n+1 y)
			      (fl* S (fast-flpown y n+1)))))
	       (i 0 (+ i 1)))
	      ((= i 6) (make-flonum y E))
           )))))

(define (%flroot-positive fl n)
  #|
    Let fl = x.

    x = s*b^e = s*b^(nm + r) , |r| < e
    x^(1/n)   = (s*b^r)^(1/n) * b^m
    S = s*b^r, S < b^n

    So the function reduces to calculating S^(1/n).

Calculate x^1/n

f(y) = y^n - x
f'(y) = ny^{n-1}

y_{n+1} = y_n - f(y_n)/f'(y_n)
        = y_n - (y^n-x)/(ny^{n-1})
        = y_n - (y_n - x/y_n^{n-1})/n
        = ((n-1)y_n - x/y_n^{n-1})/n

Unlike flcbrt or flrsqrt, we cannot compute a linear regression
beforehand, because n is arbitrary. Instead, we do a few iterations
of a bisection for an initial guess, and then do Newton-Raphson.
This should hopefully generalize to high roots.
  |#
  (let*-values (((s e) (flnormalized-fraction-exponent fl))
		((s) (fl* s (flonum fl-radix)))
		((e) (- e 1))
		((E g) (floor/ e n))
		((S) (make-flonum s g)))
    (if (fl=? S 1.0)
	(make-flonum 1.0 E)     ; Simplest case
	(let* ((y
		;; Calculate an initial guess using bisection.
		(let loop ((i 0)
			   (lower 1.0)
			   (upper 2.0))
		  (let ((guess (fl+ (fl/ (fl- upper lower) 2.0) lower)))
		    ;; FIXME: The amount of iterations required is probably
		    ;; dependent on the size of N.
		    (if (= i 4)
			guess
			(let ((value (fl- (fast-flpown guess n)
					  S)))
			  (if (flnegative? value)
			      (loop (+ i 1)
				    guess
				    upper)
			      (loop (+ i 1)
				   lower
				   guess))))))))
	  ;; Then, calculate using Newton-Raphson.
	  (let ((fl-1/n (fl/ (flonum n)))
		(n-1 (- n 1))
		(fl:n-1 (flonum (- n 1))))
	    (do ((y y (fl* (fl+ (fl* fl:n-1 y)
				(fl/ S (fast-flpown y n-1)))
			   fl-1/n))
		 (i 0 (+ i 1)))
		;; FIXME: This is a guess. The actual rate of convergence
		;; might be slower for higher N.
		((= i 6) (make-flonum y E))
            ))))))
