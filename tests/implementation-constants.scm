(define (test-fl-radix)
  (test-group "fl-radix"
    (test-predicate (exact-integer? fl-radix))
    (test-predicate (>= fl-radix 2))))

(define (test-fl-precision)
  (test-group "fl-precision"
    (test-predicate (exact-integer? fl-precision))
    (test-predicate (positive? fl-precision))))

(define (test-fl-maximum-exponent)
  (test-group "fl-maximum-exponent"
    (test-predicate (exact-integer? fl-maximum-exponent))))

(define (test-fl-minimum-exponent)
  (test-group "fl-minimum-exponent"
    (test-predicate (exact-integer? fl-minimum-exponent))))

(define (test-fl-minimum-normalized-exponent)
  (test-group "fl-minimum-normalized-exponent"
  (test-predicate (exact-integer? fl-minimum-normalized-exponent))))

(define (test-fl-limit-constants)
  (test-group "fl-limit-constants"
    (test-predicate (flonum? fl-greatest))
    (test-predicate (flonum? fl-least))
    (test-predicate (flonum? fl-least-normal))
    (test-predicate "fl-least is consistent with fl-adjacent"
                    (fl=? (fladjacent 0.0 +inf.0)
                          fl-least))
    (test-predicate (flnormal? fl-least-normal))
    (test-values "fl-least-normal is the least normal value"
                 (flnormal? (fladjacent fl-least-normal 0.0))
                 '(#f))
    (test-predicate "expected order relationship"
                    (fl<=? fl-least fl-least-normal fl-greatest))))

(define (test-fl-epsilon)
  (test-group "fl-epsilon"
    (test-predicate (flonum? fl-epsilon))
    (test-predicate "matches normal definition of C machine epsilon"
                    (fl=? (fl- (fladjacent (flonum 1.0) +inf.0)
                               (flonum 1.0))
                          fl-epsilon))))

(define (test-fl-byte-width)
  (test-group "fl-byte-width"
    (test-predicate (exact-integer? fl-byte-width))
    (test-predicate (positive? fl-byte-width))))
