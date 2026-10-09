(define (test-flposdiff)
  (test-group "flposdiff"
    (test-values (flposdiff -0.0 0.0) '(0.0))
    (test-values (flposdiff 1.0 2.0) '(0.0))
    (test-values (flposdiff 2.0 1.0) '(1.0))
    (test-values (flposdiff +inf.0 +inf.0) '(0.0))
    (test-values (flposdiff +inf.0 -inf.0) '(+inf.0))))

(define (test-flposdiff-defining-property)
  (test-group "flposdiff defining property"
    (test-property
     (lambda (fl1 fl2)
       (let ((value (flposdiff fl1 fl2)))
	 (cond
	  ((flunordered? fl1 fl2)
	   (flnan? value))
	  ((fl<=? fl1 fl2)
	   (eqv? value 0.0))
	  (else (fl=? value (fl- fl1 fl2))))))
     (list (make-random-flonum-generator)
	   (make-random-flonum-generator)))))