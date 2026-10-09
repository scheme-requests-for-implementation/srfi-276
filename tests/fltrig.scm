(define (test-flsin)
  (test-group "flsin"
    (test-values (flsin 0.0) '(0.0))
    (test-values (flsin -0.0) '(-0.0))
    (test-predicate (flnan? (flsin +inf.0)))
    (test-predicate (flnan? (flsin -inf.0)))
    (test-many-approximate flsin
			   1e-6
			   (fl-1/sqrt-2 fl-pi/4)
			   (1.0 fl-pi/2)
			   (fl-1/sqrt-2 (fl+ fl-pi/2 fl-pi/4))
			   (0.0 fl-pi)
			   ((fl- fl-1/sqrt-2)
			    (fl+ fl-pi fl-pi/4))
			   (-1.0 (fl+ fl-pi fl-pi/2))
			   ((fl- fl-1/sqrt-2)
			    (fl+ fl-pi fl-pi/2 fl-pi/4))
			   (0.0 fl-2pi))))

(define (test-flsin-odd-property)
  (test-group "flsin is odd"
   (test-property
    (lambda (fl)
      (fl=? (flsin fl) (fl- (flsin (fl- fl)))))
    (list (gcons*
	   fl-pi/4 fl-pi/2 fl-pi fl-2pi
	   (make-random-finite-flonum-generator))))))

(define (test-flcos)
  (test-group "flcos"
    (test-values (flcos 0.0) '(1.0))
    (test-values (flcos -0.0) '(1.0))
    (test-predicate (flnan? (flcos +inf.0)))
    (test-predicate (flnan? (flcos -inf.0)))
    (test-many-approximate flcos
			   1e-6
			   (fl-1/sqrt-2 fl-pi/4)
			   (0.0 fl-pi/2)
			   ((fl- fl-1/sqrt-2)
			    (fl+ fl-pi/2 fl-pi/4))
			   (-1.0 fl-pi)
			   ((fl- fl-1/sqrt-2)
			    (fl+ fl-pi fl-pi/4))
			   (0.0 (fl+ fl-pi fl-pi/2))
			   (fl-1/sqrt-2 (fl+ fl-pi fl-pi/2 fl-pi/4))
			   (1.0 fl-2pi))))

(define (test-flcos-even-property)
  (test-group "flcos is even"
    (test-property
     (lambda (fl)
       (fl=? (flcos fl) (flcos (fl- fl))))
     (list (gcons*
	    fl-pi/4 fl-pi/2 fl-pi fl-2pi
	    (make-random-finite-flonum-generator))))))

(define (test-fltan)
  (test-group "fltan"
    (test-values (fltan 0.0) '(0.0))
    (test-values (fltan -0.0) '(-0.0))
    (test-predicate (flnan? (fltan +inf.0)))
    (test-predicate (flnan? (fltan -inf.0)))
    (test-many-approximate fltan
			   1e-6
			   (0.0 fl-pi)
			   (0.0 fl-2pi)
			   (1.0 fl-pi/4))))

(define (test-fltan-odd-property)
  (test-group "fltan odd property"
    (test-property
     (lambda (fl)
       (fl=? (flcos fl) (flcos (fl- fl))))
     (list (gcons*
	    fl-pi/4 fl-pi fl-2pi
	    (make-random-finite-flonum-generator))))))