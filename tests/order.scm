(define (test-flonum-order)
  (test-group "flonum order"
    (test-assert (fl=? 0.0 0.0))
    (test-assert (fl=? -0.0 -0.0))
    (test-assert (fl=? 0.0 -0.0))
    (test-assert (fl<? 0.0 1.0))
    (test-assert (fl<? -1.0 0.0))
    (test-assert (fl<? -inf.0 +inf.0))
    (test-assert (fl<? -2.0 -1.0 0.0 1.0 2.0))
    (test-assert (fl!=? 0.0 1.0))
    (test-assert (fl<=? 1.0 1.0))
    (test-assert (not (fl=? +nan.0 +nan.0)))
    (test-assert (not (fl!=? +nan.0 +nan.0)))))

(define (test-negative-infinity-less-than-all-ordered)
  (test-group "-inf.0 is less than all ordered numbers"
    (test-property
     (lambda (fl) (fl<=? -inf.0 fl))
     (list (make-random-ordered-flonum-generator)))))

(define (test-fl-greatest-is-greater-than-all-finite)
  (test-group "fl-greatest is greater than all finite numbers"
    (test-property
     (lambda (fl) (fl<=? fl fl-greatest))
     (list (make-random-finite-flonum-generator)))))

(define (test-positive-infinity-is-greater-than-all-ordered)
  (test-group "-inf.0 is less than all ordered numbers"
    (test-property
     (lambda (fl) (fl>=? +inf.0 fl))
     (list (make-random-ordered-flonum-generator)))))

(define (test-trichotomy-of-order)
  (test-group "trichotomy of order"
    (test-property
     (lambda (fl1 fl2)
       (or (fl=? fl1 fl2)
	   (fl<? fl1 fl2)
	   (fl>? fl1 fl2)))
     (list (make-random-ordered-flonum-generator)
	   (make-random-ordered-flonum-generator)))))

(define (test-weak-order)
  (test-group "weak order"
    (test-property
     (lambda (fl1 fl2)
       (cond
	((and (fl<=? fl1 fl2)
	      (fl>=? fl1 fl2))
	 (fl=? fl1 fl2))
	((fl<=? fl1 fl2)
	 (not (fl>=? fl1 fl2)))
	((fl>=? fl1 fl2)
	 (not (fl<=? fl1 fl2)))
	(else #f)))
     (list (make-random-ordered-flonum-generator)
	   (make-random-ordered-flonum-generator)))))


