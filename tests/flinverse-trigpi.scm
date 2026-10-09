(define (test-flasinpi)
  (test-group "flasinpi"
    (test-values (flasinpi 0.0) '(0.0))
    (test-values (flasinpi -0.0) '(-0.0))
    (test-predicate (flnan? (flasinpi 1.1)))))

(define (test-flasinpi-is-odd)
  (test-group "flasinpi is odd"
    (test-property
     (lambda (fl)
       (fl=? (flasinpi fl) (fl- (flasinpi (fl- fl)))))
     (list (make-random-flonum-below-unity-generator)))))

(define (test-flasinpi-inversion)
  (test-group "flasinpi is inverse to flsinpi"
    (test-property
     (lambda (fl)
       (let ((result (flsinpi (flasinpi fl))))
	 (fl<=? (flabs (fl/ (fl- fl result) fl)) 1e-6)))
     (list (make-random-flonum-below-unity-generator)))))

(define (test-flacospi)
  (test-group "flacospi"
    (test-values (flacospi 1.0) '(0.0))
    (test-predicate (flnan? (flacospi (fladjacent 1.0 +inf.0))))))

(define (test-flacospi-inversion)
  (test-group "flacospi is inverse to flcospi"
    (test-property
     (lambda (fl)
       (let ((result (flcospi (flacospi fl))))
	 (fl<=? (flabs (fl/ (fl- fl result) fl)) 1e-6)))
     (list (gfilter (lambda (fl)
		      ;; Loss of precision occurs because around 0,
		      ;; we add by 1/2
		      (fl>=? fl 1e-6))
		    ;; SRFI 27 generator
		    random-real)))))

(define (test-flatanpi)
  (test-group "flatanpi single argument case"
    (test-values (flatanpi 0.0) '(0.0))
    (test-values (flatanpi -0.0) '(-0.0))
    (test-values (flatanpi +inf.0) '(0.5))
    (test-values (flatanpi -inf.0) '(-0.5)))
  (test-group "flatanpi two argument case"
    (test-values (flatanpi 0.0 0.0) '(0.0))
    (test-values (flatanpi -0.0 0.0) '(-0.0))
    (test-values (flatanpi 0.0 -0.0) '(1.0))
    (test-values (flatanpi -0.0 -0.0) '(-1.0))
    (test-values (flatanpi +inf.0 -inf.0) '(0.75))
    (test-values (flatanpi -inf.0 -inf.0) '(-0.75))
    (test-values (flatanpi +inf.0 +inf.0) '(0.25))
    (test-values (flatanpi -inf.0 +inf.0) '(-0.25))))

(define (test-flatanpi-odd)
  (test-group "flatanpi is odd"
    (test-property
     (lambda (fl)
       (fl=? (flatanpi fl) (fl- (flatanpi (fl- fl)))))
     (list (make-random-ordered-flonum-generator)))))

(define (test-flatanpi-zero-y)
  (test-group "(flatanpi 0.0 x) returns the zero for x > 0"
    (test-property
     (lambda (zero x)
       (eqv? (flatanpi zero x) zero))
     (list (gmap
	    (lambda (fl)
	      (flcopysign 0.0 fl))
	    (sign-generator))
	   (gfilter
	    flpositive?
	    (make-random-ordered-flonum-generator))))))

(define (test-flatanpi-first-quadrant)
  (test-group "flatanpi in first quadrant"
    (test-property
     (lambda (fl1 fl2)
       (fl<=? 0.0 (flatanpi fl1 fl2) 0.5))
     (list (gfilter flpositive?
		    (make-random-ordered-flonum-generator))
	   (gfilter flpositive?
		    (make-random-ordered-flonum-generator))))))

(define (test-flatanpi-positive-y-zero-x)
  (test-group "(flatanpi y 0.0) is 0.5"
    (test-property
     (lambda (y sign)
       (fl=? (flatanpi y (flcopysign 0.0 sign))
	     (flcopysign 0.5 y)))
     (list (gremove flzero? (make-random-ordered-flonum-generator))
	   (sign-generator)))))

(define (test-flatanpi-positive-y-negative-x)
  (test-group "(flatanpi y x), y>0, x<0"
    (test-property
     (lambda (y x)
       (fl<=? 0.5 (flatanpi y x) 1.0))
     (list (gfilter flpositive? (make-random-ordered-flonum-generator))
	   (gfilter flnegative? (make-random-ordered-flonum-generator))))))

(define (test-flatanpi-zero-y-negative-x)
  (test-group "(flatanpi 0 x) is 1.0 for x<0"
    (test-property
     (lambda (sign x)
       (fl=? (flatanpi (flcopysign 0.0 sign) x)
	     (flcopysign 1.0 sign)))
     (list (sign-generator)
	   (gfilter flnegative? (make-random-ordered-flonum-generator))))))

(define (test-flatanpi-negative-y-negative-x)
  (test-group "(flatanpi y x) for y<0, x<0"
    (test-property
     (lambda (y x)
       (fl<=? (fl- fl-pi)
	      (flatanpi y x)
	      -0.5))
     (list (gfilter flnegative? (make-random-ordered-flonum-generator))
	   (gfilter flnegative? (make-random-ordered-flonum-generator))))))

(define (test-flatanpi-negative-y-zero-x)
  (test-group "(flatanpi y 0.0) for y<0 is -0.5"
    (test-property
     (lambda (y sign)
       (fl=? (flatanpi y (flcopysign 0.0 sign))
	     -0.5))
     (list (gfilter flnegative? (make-random-ordered-flonum-generator))
	   (sign-generator)))))

(define (test-flatanpi-negative-y-positive-x)
  (test-group "(flatanpi y x) for y<0, x>0"
    (test-property
     (lambda (y x)
       (fl<=? -0.5 (flatanpi y x) 0.0))
     (list (gfilter flnegative? (make-random-ordered-flonum-generator))
	   (gfilter flpositive? (make-random-ordered-flonum-generator))))))

(define (test-flatanpi-x=-inf.0)
  (test-group "(flatanpi y -inf.0) for finite nonzero y"
    (test-property
     (lambda (y)
       (fl=? (flatanpi y -inf.0) (flcopysign 1.0 y)))
     (list (gremove flzero? (make-random-finite-flonum-generator))))))

(define (test-flatanpi-x=+inf.0)
  (test-group "(flatanpi y +inf.0) for finite nonzero y"
    (test-property
     (lambda (y)
       (fl=? (flatanpi y +inf.0) (flcopysign 0.0 y)))
     (list (gremove flzero? (make-random-finite-flonum-generator))))))

(define (test-flatanpi-y-infinite-x-finite)
  (test-group "flatanpi with y infinite and x finite"
    (test-property
     (lambda (sign x)
       (fl=? (flatanpi (flcopysign +inf.0 sign)
		       x)
	     (flcopysign 0.5 sign)))
     (list (sign-generator)
	   (make-random-finite-flonum-generator)))))