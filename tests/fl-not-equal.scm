(define (test-fl!=?)
  (test-group "fl!=?"
    (test-assert (fl!=? 1.0 2.0))
    (test-assert (not (fl!=? -0.0 0.0)))
    (test-assert (not (fl!=? +nan.0 1.0)))
    (let ((x +nan.0))
      (test-assert (not (fl!=? x x))))
    (test-assert (fl!=? 1.0 1.0 1.0 2.0))))