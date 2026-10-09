(define (test-flsinpi)
  (test-group "flsinpi"
    (test-values (flsinpi 0.0) '(0.0))
    (test-values (flsinpi -0.0) '(-0.0))
    (test-predicate (flnan? (flsinpi +inf.0)))
    (test-predicate (flnan? (flsinpi -inf.0)))
    (test-many-approximate flsinpi
			   1e-6
			   (fl-1/sqrt-2 0.25)
			   (1.0 0.50)
			   (fl-1/sqrt-2 0.75)
			   (0.0 1.0)
			   ((fl- fl-1/sqrt-2) 1.25)
			   (-1.0 1.50)
			   ((fl- fl-1/sqrt-2) 1.75)
			   (0.0 2.0))))

(define (test-flsinpi-odd)
  (test-group "flsinpi is odd"
    (test-property
     (lambda (fl)
       (fl=? (flsinpi fl) (fl- (flsinpi (fl- fl)))))
     (list (gcons*
	    0.25 0.50 0.75 1.25 1.50 1.75 2.0
	    (make-random-finite-flonum-generator))))))

(define (test-flsinpi-integer-property)
  (test-group "flsinpi on integers"
    (test-property
     (lambda (fl)
       (if (flsign-negative? fl)
	   (eqv? (flsinpi fl) -0.0)
	   (eqv? (flsinpi fl) +0.0)))
     (list (make-random-inexact-integer-generator)))))

(define (test-flcospi)
  (test-group "flcospi"
    (test-values (flcospi 0.0) '(1.0))
    (test-values (flcospi -0.0) '(1.0))
    (test-predicate (flnan? (flcospi +inf.0)))
    (test-predicate (flnan? (flcospi -inf.0)))
    (test-many-approximate flcospi
			   1e-6
			   (fl-1/sqrt-2 0.25)
			   (0.0 0.50)
			   ((fl- fl-1/sqrt-2) 0.75)
			   (-1.0 1.00)
			   ((fl- fl-1/sqrt-2) 1.25)
			   (0.0 1.50)
			   (fl-1/sqrt-2 1.75)
			   (1.0 2.0))))

(define (test-flcospi-even)
  (test-group "flcospi is even"
    (test-property
     (lambda (fl)
       (fl=? (flcospi fl) (flcospi (fl- fl))))
     (list (gcons*
	    0.25 0.50 0.75 1.25 1.50 1.75 2.0
	    (make-random-finite-flonum-generator))))))

(define (test-flcospi-half-integer)
  (test-group "flcospi is zero on half integers"
    (test-property
     (lambda (fl)
       (eqv? (flcospi fl) 0.0))
     (list
      (gcons*
       -1.5 -0.5 0.5 1.5 2.5
       (make-random-inexact-integer+0.5-generator))))))

(define (test-fltanpi)
  (test-group "fltanpi"
    (test-values (fltanpi 0.0) '(0.0))
    (test-values (fltanpi -0.0) '(-0.0))
    (test-values (fltanpi 0.25) '(1.0))
    (test-predicate (flnan? (fltanpi +inf.0)))
    (test-predicate (flnan? (fltanpi -inf.0)))
    (test-values (fltanpi 0.5) '(+inf.0))
    (test-values (fltanpi -0.5) '(-inf.0))
    (test-values (fltanpi 1.5) '(-inf.0))
    (test-values (fltanpi -1.5) '(+inf.0))))

(define (test-fltanpi-odd)
  (test-group "fltanpi is odd"
    (test-property
     (lambda (fl)
       (fl=? (fltanpi fl) (fl- (fltanpi (fl- fl)))))
     (list (make-random-finite-flonum-generator)))))

(define (test-fltanpi-positive-even-integers)
  (test-group "fltanpi on positive, even integers returns positive zero"
    (test-property
     (lambda (ifl)
       (eqv? (fltanpi ifl) +0.0))
     (list
      (gcons* 0.0 2.0 4.0
	      (gfilter (lambda (fl)
			 (and (flpositive? fl)
			      (fleven? fl)))
		       (make-random-inexact-integer-generator)))))))

(define (test-fltanpi-on-negative-odd-integers)
  (test-group "fltanpi on negative, odd integers returns positive zero"
    (test-property
     (lambda (ifl)
       (eqv? (fltanpi ifl) +0.0))
     (list
      (gcons* -1.0 -3.0
	      (gfilter (lambda (fl)
			 (and (flnegative? fl)
			      (flodd? fl)))
		       (make-random-inexact-integer-generator)))))))

(define (test-fltanpi-on-positive-odd-integers)
  (test-group "fltanpi on positive, odd integers returns negative zero"
    (test-property
     (lambda (ifl)
       (eqv? (fltanpi ifl) -0.0))
     (list
      (gcons* 1.0 3.0
	      (gfilter (lambda (fl)
			 (and (flpositive? fl)
			      (flodd? fl)))
		       (make-random-inexact-integer-generator)))))))

(define (test-fltanpi-on-negative-even-integers)
  (test-group "fltanpi on negative, even integers returns negative zero"
    (test-property
     (lambda (ifl)
       (eqv? (fltanpi ifl) -0.0))
     (list
      (gcons* -2.0 -4.0
	      (gfilter (lambda (fl)
			 (and (flnegative? fl)
			      (fleven? fl)))
		       (make-random-inexact-integer-generator)))))))

(define (test-fltanpi-on-positive-even-integer+0.5)
  (test-group "fltanpi on positive, even integers + 0.5 returns positive infinity"
    (test-property
     (lambda (fl)
       (fl=? (fltanpi fl) +inf.0))
     (list
      (gcons* 0.5 2.5
	      (gfilter (lambda (fl)
			 (let ((fl (fltruncate fl)))
			   (and (flpositive? fl)
				(fleven? fl))))
		       (make-random-inexact-integer+0.5-generator)))))))

(define (test-fltanpi-on-negative-odd-integer-0.5)
  (test-group "fltanpi on negative, odd integers - 0.5 returns positive infinity"
    (test-property
     (lambda (fl)
       (fl=? (fltanpi fl) +inf.0))
     (list
      (gcons* -1.5 -3.5
	      (gfilter (lambda (fl)
			 (let ((fl (fltruncate fl)))
			   (and (flnegative? fl)
				(flodd? fl))))
		       (make-random-inexact-integer+0.5-generator)))))))

(define (test-fltanpi-on-positive-odd-integer+0.5)
  (test-group "fltanpi on positive, odd integers + 0.5 returns negative infinity"
    (test-property
     (lambda (fl)
       (fl=? (fltanpi fl) -inf.0))
     (list
      (gcons* 1.5 3.5
	      (gfilter (lambda (fl)
			 (let ((fl (fltruncate fl)))
			   (and (flpositive? fl)
				(flodd? fl))))
		       (make-random-inexact-integer+0.5-generator)))))))

(define (test-fltanpi-on-negative-even-integer-0.5)
  (test-group "fltanpi on negative, even integers - 0.5 returns negative infinity"
    (test-property
     (lambda (fl)
       (fl=? (fltanpi fl) -inf.0))
     (list
      (gcons* -0.5 -2.5
	      (gfilter (lambda (fl)
			 (let ((fl (fltruncate fl)))
			   (and (flnegative? fl)
				(fleven? fl))))
		       (make-random-inexact-integer+0.5-generator)))))))
