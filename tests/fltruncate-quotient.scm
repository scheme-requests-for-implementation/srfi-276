(define (test-fltruncate-quotient-for-infinite-denominator)
  (test-group "fltruncate-quotient returns signed zero for infinite denominator"
    (test-property
     (lambda (fl sign)
       (let ((q (fltruncate-quotient fl (if sign
					    -inf.0
					    +inf.0))))
	 (fl=? q (flcopysign 0.0 fl))))
     (list (make-random-finite-flonum-generator)
	   (boolean-generator)))))

(define (test-fltruncate-quotient-finite-sign)
  (test-group "fltruncate-quotient gives correct sign given numerator and denominator"
    (test-property
      (lambda (fl1 fl2)
	(let ((q (fltruncate-quotient fl1 fl2)))
	  (or (and (flsign-positive? fl1) (flsign-positive? fl2)
		   (flsign-positive? q))
	      (and (flsign-negative? fl1) (flsign-negative? fl2)
		   (flsign-positive? q))
	      (and (not (boolean=? (flsign-positive? fl1)
				   (flsign-positive? fl2)))
		   (flsign-negative? q)))))
      (list (make-random-ordered-flonum-generator)
	    (gremove flzero? (make-random-ordered-flonum-generator))))))
