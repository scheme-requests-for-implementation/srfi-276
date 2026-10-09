;;;; FIXME: These constants are derived from binary64 implementations.
;;;; You should adapt these tests to your specific floating-point
;;;; format.

(define (test-fladjacent)
  (test-group "fladjacent"
    (test-values (fladjacent 1.0 1.0) '(1.0))
    (test-values (fladjacent 1.0 1.1) '(1.0000000000000002))
    (test-values (fladjacent 1.0 +inf.0) '(1.0000000000000002))
    (test-values (fladjacent 1.0 -inf.0) '(0.999999999999999888978))
    (test-values (fladjacent 1.0 0.9) '(0.999999999999999888978))
    (test-predicate (flnan? (fladjacent 1.0 +nan.0)))
    (test-predicate (flnan? (fladjacent +nan.0 1.0)))
    (test-values (fladjacent -0.0 +0.0) '(+0.0))
    (test-values (fladjacent +0.0 -0.0) '(-0.0))
    (test-values (fladjacent fl-greatest +inf.0) '(+inf.0))
    (test-values (fladjacent +inf.0 -inf.0) `(,fl-greatest))
    (test-values (fladjacent 0.0 +inf.0) `(,fl-least))))
