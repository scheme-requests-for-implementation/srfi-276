(define (test-nans-are-unordered)
  (test-group "NaNs are unordered"
    (test-property
     (lambda (nan fl)
       (and (flunordered? nan fl)
	    (not (flordered? nan fl))))
     (list random-nan (make-random-ordered-flonum-generator)))))

(define (test-non-nans-are-ordered)
  (test-group "non-NaNs are ordered"
    (test-property
     (lambda (fl1 fl2)
       (and (not (flunordered? fl1 fl2))
	    (flordered? fl1 fl2)))
     (list (make-random-ordered-flonum-generator)
	   (make-random-ordered-flonum-generator)))))