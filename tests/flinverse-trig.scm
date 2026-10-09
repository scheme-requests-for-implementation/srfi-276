(define (test-flasin)
  (test-group "flasin"
    (test-values (flasin 0.0) '(0.0))
    (test-values (flasin -0.0) '(-0.0))
    (test-predicate (flnan? (flasin 1.1)))))

(define (test-flasin-is-odd)
  (test-group "flasin is odd"
    (test-property
     (lambda (fl)
       (fl=? (flasin fl) (fl- (flasin (fl- fl)))))
     (list (make-random-flonum-below-unity-generator)))))

(define (test-flasin-inversion)
  (test-group "flasin is inverse to flsin"
    (test-property
     (lambda (fl)
       (let ((result (flsin (flasin fl))))
	 (fl<=? (flabs (fl/ (fl- fl result) fl)) 1e-6)))
     (list (make-random-flonum-below-unity-generator)))))

(define (test-flacos)
  (test-group "flacos"
    (test-values (flacos 1.0) '(0.0))
    (test-predicate (flnan? (flacos (fladjacent 1.0 +inf.0))))))

(define (test-flacos-inversion)
  (test-group "flacos is inverse to flcos"
    (test-property
     (lambda (fl)
       (let ((result (flcos (flacos fl))))
	 (fl<=? (flabs (fl/ (fl- fl result) fl)) 1e-6)))
     (list (gfilter (lambda (fl)
		      ;; Loss of precision occurs because around 0,
		      ;; we add by pi/2
		      (fl>=? fl 1e-6))
		    ;; SRFI 27 generator
		    random-real)))))

(define (test-flatan)
  (test-group "flatan single argument case"
    (test-values (flatan 0.0) '(0.0))
    (test-values (flatan -0.0) '(-0.0))
    (test-values (flatan +inf.0) (list fl-pi/2))
    (test-values (flatan -inf.0) (list (fl- fl-pi/2))))
  (test-group "flatan two argument case"
    (test-values (flatan 0.0 0.0) '(0.0))
    (test-values (flatan -0.0 0.0) '(-0.0))
    (test-approximate fl-pi
		      (flatan 0.0 -0.0)
		      1e-10)
    (test-approximate (fl- fl-pi)
		      (flatan -0.0 -0.0)
		      1e-10)
    (test-approximate (fl+ fl-pi/2 fl-pi/4)
		      (flatan +inf.0 -inf.0)
		      1e-10)
    (test-approximate (fl- (fl+ fl-pi/2 fl-pi/4))
		      (flatan -inf.0 -inf.0)
		      1e-10)
    (test-approximate fl-pi/4
		      (flatan +inf.0 +inf.0)
		      1e-10)
    (test-approximate (fl- fl-pi/4)
		      (flatan -inf.0 +inf.0)
		      1e-10)))

(define (test-flatan-odd)
  (test-group "flatan is odd"
    (test-property
     (lambda (fl)
       (fl=? (flatan fl) (fl- (flatan (fl- fl)))))
     (list (make-random-ordered-flonum-generator)))))

(define (test-flatan-zero-y)
  (test-group "(flatan 0.0 x) returns the zero for x > 0"
    (test-property
     (lambda (zero x)
       (eqv? (flatan zero x) zero))
     (list (gmap
	    (lambda (fl)
	      (flcopysign 0.0 fl))
	    (sign-generator))
	   (gfilter
	    flpositive?
	    (make-random-ordered-flonum-generator))))))

(define (test-flatan-first-quadrant)
  (test-group "flatan in first quadrant"
    (test-property
     (lambda (fl1 fl2)
       (fl<=? 0.0 (flatan fl1 fl2) fl-pi/2))
     (list (gfilter flpositive?
		    (make-random-ordered-flonum-generator))
	   (gfilter flpositive?
		    (make-random-ordered-flonum-generator))))))

(define (test-flatan-positive-y-zero-x)
  (test-group "(flatan y 0.0) is pi/2"
    (test-property
     (lambda (y sign)
       (fl=? (flatan y (flcopysign 0.0 sign))
	     (flcopysign fl-pi/2 y)))
     (list (gremove flzero? (make-random-ordered-flonum-generator))
	   (sign-generator)))))

(define (test-flatan-positive-y-negative-x)
  (test-group "(flatan y x), y>0, x<0"
    (test-property
     (lambda (y x)
       (fl<=? fl-pi/2 (flatan y x) fl-pi))
     (list (gfilter flpositive? (make-random-ordered-flonum-generator))
	   (gfilter flnegative? (make-random-ordered-flonum-generator))))))

(define (test-flatan-zero-y-negative-x)
  (test-group "(flatan 0 x) is pi for x<0"
    (test-property
     (lambda (sign x)
       (fl=? (flatan (flcopysign 0.0 sign) x)
	     (flcopysign fl-pi sign)))
     (list (sign-generator)
	   (gfilter flnegative? (make-random-ordered-flonum-generator))))))

(define (test-flatan-negative-y-negative-x)
  (test-group "(flatan y x) for y<0, x<0"
    (test-property
     (lambda (y x)
       (fl<=? (fl- fl-pi)
	      (flatan y x)
	      (fl- fl-pi/2)))
     (list (gfilter flnegative? (make-random-ordered-flonum-generator))
	   (gfilter flnegative? (make-random-ordered-flonum-generator))))))

(define (test-flatan-negative-y-zero-x)
  (test-group "(flatan y 0.0) for y<0 is -pi/2"
    (test-property
     (lambda (y sign)
       (fl=? (flatan y (flcopysign 0.0 sign))
	     (fl- fl-pi/2)))
     (list (gfilter flnegative? (make-random-ordered-flonum-generator))
	   (sign-generator)))))

(define (test-flatan-negative-y-positive-x)
  (test-group "(flatan y x) for y<0, x>0"
    (test-property
     (lambda (y x)
       (fl<=? (fl- fl-pi/2) (flatan y x) 0.0))
     (list (gfilter flnegative? (make-random-ordered-flonum-generator))
	   (gfilter flpositive? (make-random-ordered-flonum-generator))))))

(define (test-flatan-x=-inf.0)
  (test-group "(flatan y -inf.0) for finite nonzero y"
    (test-property
     (lambda (y)
       (fl=? (flatan y -inf.0) (flcopysign fl-pi y)))
     (list (gremove flzero? (make-random-finite-flonum-generator))))))

(define (test-flatan-x=+inf.0)
  (test-group "(flatan y +inf.0) for finite nonzero y"
    (test-property
     (lambda (y)
       (fl=? (flatan y +inf.0) (flcopysign 0.0 y)))
     (list (gremove flzero? (make-random-finite-flonum-generator))))))

(define (test-flatan-y-infinite-x-finite)
  (test-group "flatan with y infinite and x finite"
    (test-property
     (lambda (sign x)
       (fl=? (flatan (flcopysign +inf.0 sign)
		       x)
	     (flcopysign fl-pi/2 sign)))
     (list (sign-generator)
	   (make-random-finite-flonum-generator)))))
