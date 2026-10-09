(define (test-fl+*)
  (test-group "fl+*"
    (test-values (fl+* 0.0 0.0 0.0) '(0.0))
    (test-values (fl+* 0.0 -0.0 -0.0) '(-0.0))
    (test-values (fl+* -0.0 0.0 -0.0) '(-0.0))
    (test-values (fl+* 0.0 0.0 -0.0) '(0.0))
    (test-values (fl+* -0.0 0.0 0.0) '(0.0))
    (test-values (fl+* 2.0 4.0 1.0) '(9.0))
    (test-values (fl+* +inf.0 -inf.0 -inf.0) '(-inf.0))))