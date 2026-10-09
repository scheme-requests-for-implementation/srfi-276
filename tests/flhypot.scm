(define (test-flhypot)
  (test-group "flhypot"
    (test-values (flhypot 3.0 4.0) '(5.0))
    (test-values (flhypot 5.0 12.0) '(13.0))))

(define (test-flhypot-0.0)
  (test-group "(flhypot 0.0 x) equals (flabs x)"
    (test-property
     (lambda (fl sign)
       (fl=? (flhypot (flcopysign +0.0 sign)
		      fl)
	     (flabs fl)))
     (list (make-random-ordered-flonum-generator)
	   (sign-generator)))))

(define (test-flhypot-0.0-symmetric)
  (test-group "(flhypot 0.0 x) equals (flabs x), symmetric"
    (test-property
     (lambda (fl sign)
       (fl=? (flhypot fl
		      (flcopysign +0.0 sign))
	     (flabs fl)))
     (list (make-random-ordered-flonum-generator)
	   (sign-generator)))))

(define (test-flhypot-inf.0)
  (test-group "(flhypot inf.0 x) equals inf.0"
    (test-property
     (lambda (fl sign)
       (fl=? (flhypot (flcopysign +inf.0 sign) fl)
	     +inf.0))
     (list (make-random-ordered-flonum-generator)
	   (sign-generator)))))

(define (test-flhypot-inf.0-symmetric)
  (test-group "(flhypot inf.0 x) equals inf.0, symmetric"
    (test-property
     (lambda (fl sign)
       (fl=? (flhypot fl (flcopysign +inf.0 sign))
	     +inf.0))
     (list (make-random-ordered-flonum-generator)
	   (sign-generator)))))