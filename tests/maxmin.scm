(define (test-flmax)
  (test-group "flmax"
    (test-values (flmax)
		 '(-inf.0))
    (test-values (flmax 1.0)
		 '(1.0))
    (test-values (flmax 1.0 2.0 3.0)
		 '(3.0))
    (test-values (flmax 1.0 2.0 3.0 -2.0 -3.0)
		 '(3.0))
    (test-predicate (flnan? (flmax 1.0 2.0 3.0 +nan.0)))))

(define (test-flmax-filter-nans)
  (test-group "flmax-filter-nans"
    (test-values (flmax-filter-nans)
		 '(-inf.0))
    (test-predicate (flnan? (flmax-filter-nans +nan.0)))
    (test-values (flmax-filter-nans 1.0 2.0 +nan.0 3.0 -2.0 +nan.0)
		 '(3.0))
  (test-group "flmax-filter-nans and flmax agree when all values are non-NaN"
    (test-property
     (lambda (list-of-flonums)
       (fl=? (apply flmax list-of-flonums)
	     (apply flmax-filter-nans list-of-flonums)))
     (list (list-generator-of (make-random-ordered-flonum-generator)))))))

(define (test-flmin)
  (test-group "flmin"
    (test-values (flmin)
		 '(+inf.0))
    (test-values (flmin 1.0)
		 '(1.0))
    (test-values (flmin 1.0 2.0 3.0)
		 '(1.0))
    (test-values (flmin 1.0 2.0 -3.0 -2.0 3.0)
		 '(-3.0))
    (test-predicate (flnan? (flmin 1.0 2.0 3.0 +nan.0)))))

(define (test-flmin-filter-nans)
  (test-group "flmin-filter-nans"
    (test-values (flmin-filter-nans)
		 '(+inf.0))
    (test-predicate (flnan? (flmin-filter-nans +nan.0)))
    (test-values (flmin-filter-nans 1.0 2.0 +nan.0 -3.0 2.0 1.0)
		 '(-3.0)))
  (test-group "flmin-filter-nans and flmin agree when all values are non-NaN"
    (test-property
     (lambda (list-of-flonums)
       (fl=? (apply flmin list-of-flonums)
	     (apply flmin-filter-nans list-of-flonums)))
     (list (list-generator-of (make-random-finite-flonum-generator))))))

(define (test-flmax-abs)
  (test-group "flmax-abs"
    (test-values (flmax-abs)
		 '(+0.0))
    (test-values (flmax-abs 1.0)
		 '(1.0))
    (test-values (flmax-abs 1.0 2.0 3.0)
		 '(3.0))
    (test-values (flmax-abs 1.0 2.0 3.0 -4.0 -3.0)
		 '(-4.0))
    (test-predicate (flnan? (flmax-abs 1.0 2.0 3.0 -4.0 -3.0 +nan.0)))))

(define (test-flmax-abs-filter-nans)
  (test-group "flmax-abs-filter-nans"
    (test-values (flmax-abs-filter-nans)
		 '(+0.0))
    (test-predicate (flnan? (flmax-abs-filter-nans +nan.0)))
    (test-values (flmax-abs-filter-nans 1.0 2.0 +nan.0 3.0 -4.0 -3.0 -2.0 +nan.0)
		 '(-4.0))
  (test-group "flmax-abs-filter-nans and flmax-abs agree when all values are non-NaN"
    (test-property
     (lambda (list-of-flonums)
       (fl=? (apply flmax-abs list-of-flonums)
	     (apply flmax-abs-filter-nans list-of-flonums)))
     (list (list-generator-of (make-random-finite-flonum-generator)))))))

(define (test-flmin-abs)
  (test-group "flmin-abs"
    (test-values (flmin-abs)
		 '(0.0))
    (test-values (flmin-abs 1.0)
		 '(1.0))
    (test-values (flmin-abs 1.0 2.0 3.0)
		 '(1.0))
    (test-values (flmin-abs 1.0 2.0 -3.0 0.1 -2.0 3.0)
		 '(0.1))
    (test-predicate (flnan? (flmin-abs 1.0 2.0 3.0 +nan.0)))))

(define (test-flmin-abs-filter-nans)
  (test-group "flmin-abs-filter-nans"
    (test-values (flmin-abs-filter-nans)
		 '(+0.0))
    (test-predicate (flnan? (flmin-abs-filter-nans +nan.0)))
    (test-values (flmin-abs-filter-nans 1.0 2.0 +nan.0 -3.0 2.0 1.0)
		 '(1.0)))
  (test-group "flmin-abs-filter-nans and flmin-abs agree when all values are non-NaN"
    (test-property
     (lambda (list-of-flonums)
       (fl=? (apply flmin-abs list-of-flonums)
	     (apply flmin-abs-filter-nans list-of-flonums)))
     (list (list-generator-of (make-random-finite-flonum-generator))))))
