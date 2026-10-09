(define (test-fl<?-implies-fltotal<?)
  (test-group "fl<? implies fltotal<?"
    (test-property
     (lambda (fl1 fl2)
       (if (fl<? fl1 fl2)
	   (fltotal<? fl1 fl2)
	   #t))
     (list (make-random-flonum-generator)
	   random-flonum
    ))))

(define (test-eqv?-implies-fltotal=?)
  (test-group "eqv? implies fltotal=?"
    (test-property
     (lambda (fl) (fltotal=? fl fl))
     (list (make-random-flonum-generator)))))

(define (test-trichotomy-of-total-order)
  (test-group "trichotomy of total order"
    (test-property
     (lambda (fl1 fl2)
       (let ((< (fltotal<? fl1 fl2))
	     (= (fltotal=? fl1 fl2))
	     (> (fltotal>? fl1 fl2)))
	 (or (and > (not =) (not <))
	     (and = (not <) (not >))
	     (and < (not =) (not >)))))
     (list (make-random-flonum-generator)
	   (make-random-flonum-generator)))))

(define (test-weak-total-order)
  (test-group "weak total order"
    (test-property
     (lambda (fl1 fl2)
       (let ((= (fltotal=? fl1 fl2))
	     (<= (fltotal<=? fl1 fl2))
	     (>= (fltotal>=? fl1 fl2)))
	 (or (and >= <= =)
	     (and >= (not <=) (not =))
	     (and <= (not >=) (not =)))))
     (list (make-random-flonum-generator)
	   (make-random-flonum-generator)))))

(define (test-nans-are-ordered-beyond-finite-values)
  (test-group "NaNs are ordered beyond finite values"
    (test-property
     (lambda (nan)
       (or (fltotal<? nan -inf.0)
	   (fltotal<? +inf.0 nan)))
     (list random-nan))))

(define (test-total-order-special-cases)
  (test-group "total order special cases"
    (test-assert (fltotal<? -0.0 +0.0))
    (test-assert (fltotal>? +0.0 -0.0))
    (test-assert (not (fltotal=? -0.0 +0.0)))
    (test-assert (fltotal<? -inf.0 +inf.0))))
