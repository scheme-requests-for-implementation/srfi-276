(define (test-flround-away)
  (test-group "flround-away"
    (test-values (flround-away 0.0) '(0.0))
    (test-values (flround-away -0.0) '(-0.0))
    (test-values (flround-away 0.1) '(0.0))
    (test-values (flround-away -0.1) '(-0.0))
    (test-values (flround-away +inf.0) '(+inf.0))
    (test-values (flround-away -inf.0) '(-inf.0))
    (test-predicate (flnan? (flround-away +nan.0)))))

(define (test-flround-away-is-integer)
  (test-group "flround-away is integer"
    (test-property
     (lambda (fl) (flinteger? (flround-away fl)))
     (list (make-random-integer+fraction-flonum-generator)))))

(define (test-flround-away-property)
  (test-group "flround-away even property"
    (test-property
     (lambda (fl)
       (let-values (((ignored frac) (flinteger-fraction (flabs fl))))
	 (if (fl<? frac 0.5)
	     (fl<=? (flabs (flround-away fl)) (flabs fl))
	     (fl>? (flabs (flround-away fl)) (flabs fl)))))
     (list (make-random-integer+fraction-flonum-generator)))))
