(define (test-flinteger?)
  (test-group "flinteger?"
    (test-assert (flinteger? -0.0))
    (test-assert (flinteger? +0.0))
    (test-assert (flinteger? 1.0))
    (test-assert (not (flinteger? 1.5)))
    (test-assert (not (flinteger? +inf.0)))))

(define (test-flzero?)
  (test-group "flzero?"
    (test-assert (flzero? -0.0))
    (test-assert (flzero? +0.0))
    (test-assert (not (flzero? +nan.0)))))

(define (test-flpositive?)
  (test-group "flpositive?"
    (test-assert (flpositive? 1.0))
    (test-assert (flpositive? +inf.0))
    (test-assert (not (flpositive? +nan.0)))
    (test-assert (not (flpositive? +0.0)))
    (test-assert (not (flpositive? -0.0)))))

(define (test-flnegative?)
  (test-group "flnegative?"
    (test-assert (flnegative? -1.0))
    (test-assert (not (flnegative? 1.0)))
    (test-assert (flnegative? -inf.0))
    (test-assert (not (flnegative? +nan.0)))
    (test-assert (not (flnegative? -0.0)))))

(define (test-flodd?)
  (test-group "flodd?"
    (test-assert (flodd? 1.0))
    (test-assert (not (flodd? 0.0)))))

(define (test-fleven?)
  (test-group "fleven?"
    (test-assert (fleven? 2.0))
    (test-assert (not (fleven? 1.0)))
    (test-assert (fleven? 0.0))))

(define (test-flfinite?)
  (test-group "flfinite?"
    (test-assert (flfinite? fl-greatest))
    (test-assert (flfinite? fl-least))
    (test-assert (flfinite? 0.0))
    (test-assert (not (flfinite? +inf.0)))
    (test-assert (not (flfinite? +nan.0)))))

(define (test-flinfinite?)
  (test-group "flinfinite?"
    (test-assert (flinfinite? +inf.0))
    (test-assert (flinfinite? -inf.0))
    (test-assert (not (flinfinite? +nan.0)))
    (test-assert (not (flinfinite? 1.0)))))

(define (test-property-nans)
  (test-group "generation of NaNs"
    (test-property
     flnan?
     (list random-nan))))

(define (test-flnormal?)
  (test-group "flnormal?"
    (test-assert (flnormal? 1.0))
    (test-assert (flnormal? fl-least-normal))
    (test-assert (flnormal? fl-greatest))
    (test-assert (not (flnormal? 0.0)))
    (test-assert (not (flnormal? +inf.0)))
    (test-assert (not (flnormal? +nan.0)))))

(define (test-flsubnormal?)
  (test-group "flsubnormal?"
    (test-assert (flsubnormal? fl-least))
    (test-assert (flsubnormal? (fladjacent fl-least-normal 0.0)))))
