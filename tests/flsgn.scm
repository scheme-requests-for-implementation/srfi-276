(define (test-flsgn-defining-property)
  (test-group "flsgn equivalent to flcopysign"
    (test-property
     (lambda (fl)
       (fl=? (flsgn fl) (flcopysign 1.0 fl)))
     (list (make-random-flonum-generator)))))