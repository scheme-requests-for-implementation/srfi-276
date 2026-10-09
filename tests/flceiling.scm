(define (test-flceiling)
  (test-group "flceiling"
    (test-values (flceiling 0.0) '(0.0))
    (test-values (flceiling -0.0) '(-0.0))
    (test-values (flceiling 0.1) '(1.0))
    (test-values (flceiling -0.1) '(-0.0))
    (test-values (flceiling +inf.0) '(+inf.0))
    (test-values (flceiling -inf.0) '(-inf.0))
    (test-predicate (flnan? (flceiling +nan.0)))))

(define (test-flceiling-is-integer)
  (test-group "flceiling is integer"
    (test-property
     (lambda (fl) (flinteger? (flceiling fl)))
     (list (make-random-integer+fraction-flonum-generator)))))

(define (test-flceiling-not-smaller)
  (test-group "flceiling is not less than input"
    (test-property
     (lambda (fl)
       (fl>=? (flceiling fl) fl))
     (list (make-random-integer+fraction-flonum-generator)))))