(define (test-flabsdiff)
  (test-group "flabsdiff"
    (test-values (flabsdiff -inf.0 +inf.0) '(+inf.0))
    (test-values (flabsdiff -0.0 -0.0) '(0.0))
    (test-values (flabsdiff 4.0 5.0) '(1.0))))

(define (test-flabsdiff-defining-property)
  (test-group "flabsdiff defining property"
    (test-property
     (lambda (fl1 fl2)
       (let ((value (flabsdiff fl1 fl2))
	     (expr (flabs (fl- fl1 fl2))))
	 (cond
	  ((flunordered? fl1 fl2) (flnan? value))
	  ((flnan? expr) (flnan? value))
	  (else (fl=? expr value)))))
     (list (make-random-flonum-generator)
	   (make-random-flonum-generator)))))
