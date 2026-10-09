(define (test-flabs)
  (test-group "flabs"
    (test-values (flabs 0.0) '(0.0))
    (test-values (flabs -0.0) '(0.0))
    (test-values (flabs +inf.0) '(+inf.0))
    (test-values (flabs -inf.0) '(+inf.0))
    (test-values (flabs (fl- fl-greatest))
		 `(,fl-greatest))
    (test-values (flabs -1.0) '(1.0))))

(define (test-flabs-sign-positive)
  (test-group "flabs sign-positive"
    (test-property
     (lambda (fl) (not (flsign-negative? (flabs fl))))
     (list (make-random-finite-flonum-generator)))))


