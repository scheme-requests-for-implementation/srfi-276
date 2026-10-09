(define (test-flsinh)
  (test-group "flsinh"
    (test-values (flsinh 0.0) '(0.0))
    (test-values (flsinh -0.0) '(-0.0))
    (test-values (flsinh +inf.0) '(+inf.0))
    (test-values (flsinh -inf.0) '(-inf.0))))

(define (test-flsinh-odd)
  (test-group "flsinh is odd"
    (test-property
     (lambda (fl)
       (fl=? (flsinh fl) (fl- (flsinh (fl- fl)))))
     (list (make-random-ordered-flonum-generator)))))

(define (test-flcosh)
  (test-group "flcosh"
    (test-values (flcosh 0.0) '(1.0))
    (test-values (flcosh -0.0) '(1.0))
    (test-values (flcosh +inf.0) '(+inf.0))
    (test-values (flcosh -inf.0) '(+inf.0))))

(define (test-flcosh-even)
  (test-group "flcosh is even"
    (test-property
     (lambda (fl)
       (fl=? (flcosh fl) (flcosh (fl- fl))))
     (list (make-random-ordered-flonum-generator)))))

(define (test-fltanh)
  (test-group "fltanh"
    (test-values (fltanh 0.0) '(0.0))
    (test-values (fltanh -0.0) '(-0.0))
    (test-values (fltanh +inf.0) '(1.0))
    (test-values (fltanh -inf.0) '(-1.0))))

(define (test-fltanh-odd)
  (test-group "fltanh is odd"
    (test-property
     (lambda (fl)
       (fl=? (fltanh fl) (fl- (fltanh (fl- fl)))))
     (list (make-random-ordered-flonum-generator)))))

(define (test-flasinh)
  (test-group "flasinh"
    (test-values (flasinh 0.0) '(0.0))
    (test-values (flasinh -0.0) '(-0.0))
    (test-values (flasinh +inf.0) '(+inf.0))
    (test-values (flasinh -inf.0) '(-inf.0))))

(define (test-flasinh-odd)
  (test-group "flasinh is odd"
    (test-property
     (lambda (fl)
       (fl=? (flsinh fl) (fl- (flsinh (fl- fl)))))
     (list (make-random-ordered-flonum-generator)))))

(define (test-flacosh)
  (test-group "flacosh"
    (test-values (flacosh 1.0) '(0.0))
    (test-values (flacosh +inf.0) '(+inf.0))))

(define (test-flacosh-below-one)
  (test-group "flacosh below one is NaN"
    (test-property
     (lambda (fl) (flnan? (flacosh fl)))
     (list random-real))))

(define (test-flatanh)
  (test-group "flatanh"
    (test-values (flatanh 0.0) '(0.0))
    (test-values (flatanh -0.0) '(-0.0))
    (test-values (flatanh 1.0) '(+inf.0))
    (test-values (flatanh -1.0) '(-inf.0))))

(define (test-flatanh-nan)
  (test-group "flatanh is not defined outside of [-1,1]"
    (test-property
     (lambda (fl) (flnan? (flatanh fl)))
     (list (gmap fl/ random-real)))))

(define (test-flatanh-odd)
  (test-group "flatanh is odd"
    (test-property
     (lambda (fl) (fl=? (flatanh fl) (fl- (flatanh (fl- fl)))))
     (list random-real))))
