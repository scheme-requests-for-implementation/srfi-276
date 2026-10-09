(define (test-flonum)
  (test-group "flonum"
    (test-predicate (flonum? (flonum 1)))
    (test-predicate (flonum? (flonum 1.0)))
    (test-predicate (flonum? (flonum +inf.0)))
    (test-predicate (flonum? (flonum -inf.0)))
    (test-predicate (flonum? (flonum -inf.0)))
    (test-predicate (flsign-negative? (flonum -inf.0)))
    (test-not-predicate (flsign-negative? (flonum +inf.0)))
    (test-predicate (flsign-negative? (flonum -0.0)))
    (test-not-predicate (flsign-negative? (flonum +0.0)))
    (test-predicate (flnan? (flonum +nan.0)))
    (test-assert (let ((v (flonum (+ 1 (exact fl-greatest)))))
                   (or (flinfinite? v) (fl=? v fl-greatest))))
    (test-predicate (flnan? (flonum 1+2i)))))

(define (test-flonum-property)
  (test-group "random generation of flonums"
    (test-property
     flonum?
     (list (make-random-flonum-generator)))))

