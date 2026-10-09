(define (test-flrsqrt)
  (test-group "flrsqrt"
    (test-values (flrsqrt 1.0) '(1.0))
    (test-values (flrsqrt 0.0) '(+inf.0))
    (test-values (flrsqrt -0.0) '(-inf.0))
    (test-values (flrsqrt +inf.0) '(0.0))
    (test-assert (flnan? (flrsqrt (fladjacent 0.0 -inf.0))))
    (test-values (flrsqrt 4.0) '(0.5))
    (test-values (flrsqrt 16.0) '(0.25))))