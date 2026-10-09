(define (test-flcbrt)
  (test-group "flcbrt"
    (test-values (flcbrt -0.0) '(-0.0))
    (test-values (flcbrt +0.0) '(+0.0))
    (test-values (flcbrt +inf.0) '(+inf.0))
    (test-values (flcbrt -inf.0) '(-inf.0))
    (test-values (flcbrt 8.0) '(2.0))
    (test-values (flcbrt 1000.0) '(10.0))))