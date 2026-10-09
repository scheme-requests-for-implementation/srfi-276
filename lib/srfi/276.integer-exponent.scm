;; RATIONALE:
;;
;; The largest IEEE format number that has a wikipedia article is
;; binary256, which has a 19 bit exponent. Fixnums are required to
;; be at least 24 bits long, so use a negative fixnum.

(define fl-integer-exponent-zero
  large-negative-integer)

(define fl-integer-exponent-nan
  fl-integer-exponent-zero)
(define (flinteger-exponent fl)
  (cond
    ((flnan? fl) fl-integer-exponent-nan)
    ((flzero? fl) fl-integer-exponent-zero)
    ((flinfinite? fl) large-positive-integer)
    (else (srfi-144:flinteger-exponent fl))))
