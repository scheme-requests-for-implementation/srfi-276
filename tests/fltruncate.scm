(define (test-fltruncate)
  (test-group "fltruncate"
    (test-values (fltruncate 0.0) '(0.0))
    (test-values (fltruncate -0.0) '(-0.0))
    (test-values (fltruncate 0.1) '(0.0))
    (test-values (fltruncate -0.1) '(-0.0))
    (test-values (fltruncate +inf.0) '(+inf.0))
    (test-values (fltruncate -inf.0) '(-inf.0))
    (test-predicate (flnan? (fltruncate +nan.0)))))

(define (test-fltruncate-is-integer)
  (test-group "fltruncate is integer"
    (test-property
     (lambda (fl) (flinteger? (fltruncate fl)))
     (list (make-random-integer+fraction-flonum-generator)))))

(define (test-fltruncate-closer-to-zero)
  (test-group "fltruncate closer to zero"
    (test-property
     (lambda (fl)
       (let ((i (fltruncate fl)))
	 (cond
	   ((flpositive? fl)
	    (fl<=? i fl))
	   ((flnegative? fl)
	    (fl<=? fl i))
	   (else (fl=? fl i)))))
     (list (make-random-integer+fraction-flonum-generator)))))