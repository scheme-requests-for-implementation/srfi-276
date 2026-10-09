(define (flonum z)
  (cond
    ((not (real? z)) (srfi-144:flonum +nan.0))
    (else (srfi-144:flonum z))))
