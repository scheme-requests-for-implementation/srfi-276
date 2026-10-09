(define (test-flerf)
  (test-group "flerf"
    (test-values (flerf 0.0) '(0.0))
    (test-values (flerf -0.0) '(-0.0))
    (test-values (flerf +inf.0) '(+1.0))
    (test-values (flerf -inf.0) '(-1.0))))

(define (test-flerf-odd)
  (test-group "flerf is odd"
    (test-property
     (lambda (fl)
       (fl=? (flerf fl) (fl- (flerf (fl- fl)))))
     (list (make-random-finite-flonum-generator)))))

(define (test-flerfc)
  (test-group "flerfc"
    (test-values (flerfc 0.0) '(1.0))
    (test-values (flerfc +inf.0) '(0.0))
    (test-values (flerfc -inf.0) '(2.0))))

(define (test-flgamma)
  (test-group "flgamma"
    (test-values (flgamma +0.0) '(+inf.0))
    (test-values (flgamma -0.0) '(-inf.0))
    (test-values (flgamma +inf.0) '(+inf.0))
    (test-values (flgamma 1.0) '(1.0))
    (test-values (flgamma 2.0) '(1.0))
    (test-predicate (flnan? (flgamma -inf.0)))))

(define (test-flgamma-on-negative-integers)
  (test-group "flgamma on negative integers"
    (test-property
     (lambda (fl) (flnan? (flgamma fl)))
     (list (gfilter flnegative?
		    (make-random-inexact-integer-generator))))))

(define (test-flloggamma)
  (test-group "flloggamma"
    (test-values (flloggamma 1.0) '(0.0 1.0))
    (test-values (flloggamma 2.0) '(0.0 1.0))
    (test-values (flloggamma +inf.0) '(+inf.0 1.0))
    (call-with-values (lambda () (flloggamma +nan.0))
      (lambda (f e)
        (test-predicate (flnan? f))
        (test-values (flabs e) '(1.0))))
    (call-with-values (lambda () (flloggamma -inf.0))
      (lambda (f e)
        (test-assert (fl=? f +inf.0))
        (test-assert (fl=? (flabs e) 1.0))))))

(define (test-flloggamma-on-negative-integers)
  (test-group "flloggamma on negative integers"
    (test-property
     (lambda (fl)
       (let-values (((res sgn) (flloggamma fl)))
	 (and (fl=? res +inf.0)
	      (fl=? (flabs sgn) 1.0))))
     (list (gfilter flnegative?
		    (make-random-inexact-integer-generator))))))
