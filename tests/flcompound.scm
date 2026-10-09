(define (test-flcompound-to-zero)
  (test-group "flcompound to zero"
    (test-property
     (lambda (fl)
       (fl=? (flcompound fl 0.0) 1.0))
     (list (gfilter (lambda (x)
		      (fl>=? x -1.0))
		    (gcons*
		     -1.0
		     (fladjacent -1.0 +inf.0)
		     -0.5
		     (make-random-ordered-flonum-generator)))))))

(define (test-flcompound-below-zero)
  (test-group "flcompound below zero"
    (test-property
     (lambda (fl n)
       (flnan? (flcompound fl n)))
     (list (gfilter (lambda (x)
		      (fl<? x -1.0))
		    (gcons*
		     (fladjacent -1.0 -inf.0)
		     (make-random-ordered-flonum-generator)))
	   (make-random-integer-generator 0 #e1e6)))))

(define (test-flcompound-minus-1-to-negative)
  (test-group "flcompound -1.0 negative"
    (test-property
     (lambda (n)
       (fl=? (flcompound -1.0 n) +inf.0))
     (list (gfilter negative? (make-random-integer-generator #e-1e6 #e1e6))))))

(define (test-flcompound-minus-1-to-positive)
  (test-group "flcompound -1.0 positive"
    (test-property
     (lambda (n)
       (eqv? (flcompound -1.0 n) +0.0))
     (list (gfilter positive? (make-random-integer-generator #e-1e6 #e1e6))))))

(define (test-flcompound-infinite-to-positive)
  (test-group "flcompound +inf.0 positive"
    (test-property
     (lambda (n)
       (eqv? (flcompound +inf.0 n) +inf.0))
     (list (gfilter positive? (make-random-integer-generator #e-1e6 #e1e6))))))

(define (test-flcompound-infinite-to-negative)
  (test-group "flcompund +inf.0 negative"
    (test-property
     (lambda (n)
       (eqv? (flcompound +inf.0 n) +0.0))
     (list (gfilter negative? (make-random-integer-generator #e-1e6 #e1e6))))))

(define (test-flcompound-nan)
  (define (prop z)
    (or (eqv? (flcompound +nan.0 z) z)
	(flnan? (flcompound +nan.0 z))))
  (test-group "flcompound nan"
    (test-assert "(flcompound +nan.0 +0.0)" (prop +0.0))
    (test-assert "(flcompound +nan.0 -0.0)" (prop -0.0))))