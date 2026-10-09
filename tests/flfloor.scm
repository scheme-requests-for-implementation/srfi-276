(define (test-flfloor)
  (test-group "flfloor"
    (test-values (flfloor 0.0) '(0.0))
    (test-values (flfloor -0.0) '(-0.0))
    (test-values (flfloor 0.1) '(0.0))
    (test-values (flfloor -0.1) '(-1.0))
    (test-values (flfloor +inf.0) '(+inf.0))
    (test-values (flfloor -inf.0) '(-inf.0))
    (test-predicate (flnan? (flfloor +nan.0)))))

(define (test-flfloor-is-integer)
  (test-group "flfloor is integer"
    (test-property
     (lambda (fl) (flinteger? (flfloor fl)))
     (list (make-random-integer+fraction-flonum-generator)))))

(define (test-flfloor-not-larger)
  (test-group "flfloor is not greater than input"
    (test-property
     (lambda (fl)
       (fl<=? (flfloor fl) fl))
     (list (make-random-integer+fraction-flonum-generator)))))