(define (test-flround)
  (test-group "flround"
    (test-values (flround 0.0) '(0.0))
    (test-values (flround -0.0) '(-0.0))
    (test-values (flround 0.1) '(0.0))
    (test-values (flround -0.1) '(-0.0))
    (test-values (flround +inf.0) '(+inf.0))
    (test-values (flround -inf.0) '(-inf.0))
    (test-predicate (flnan? (flround +nan.0)))))

(define (test-flround-is-integer)
  (test-group "flround is integer"
    (test-property
     (lambda (fl) (flinteger? (flround fl)))
     (list (make-random-integer+fraction-flonum-generator)))))

(define (test-flround-property)
  (test-group "flround even property"
    (test-property
     (lambda (fl)
       (let* ((i (flround fl))
	      (diff (flabs (fl- fl i))))
	 (cond
	   ((fl=? diff 0.5)
	    (fleven? i))
	   (else (fl<? diff 0.5)))))
     (list (make-random-integer+fraction-flonum-generator)))))
