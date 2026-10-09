;;; FIXME: These could be defined to be much better.

(define (fllog2+1 x)
  ;; ln_2(1+x) = ln(1+x)/ln(2)
  (fl* fl-log2-e (fllog+1 x)))

(define (fllog10+1 x)
  (fl* fl-log10-e (fllog+1 x)))