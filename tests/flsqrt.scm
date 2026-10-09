(define (test-flsqrt)
  (test-group "flsqrt"
    (test-values (flsqrt 0.0) '(0.0))
    (test-values (flsqrt -0.0) '(-0.0))
    (test-values (flsqrt +inf.0) '(+inf.0))
    (test-assert (flnan? (flsqrt (fladjacent 0.0 -inf.0))))
    (test-values (flsqrt 4.0) '(2.0))
    (test-values (flsqrt 9.0) '(3.0))
    (test-values (flsqrt 16.0) '(4.0))))