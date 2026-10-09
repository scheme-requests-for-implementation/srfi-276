(define (test-fllog-like fllog name)
  (test-group name
    (test-values (fllog +inf.0) '(+inf.0))
    (test-values (fllog +0.0) '(-inf.0))
    (test-values (fllog -0.0) '(-inf.0))
    (test-assert (flnan? (fllog -0.1)))))

(define (test-fllog)
  (test-fllog-like fllog "fllog"))
(define (test-fllog2)
  (test-fllog-like fllog2 "fllog2"))
(define (test-fllog10)
  (test-fllog-like fllog10 "fllog10"))

(define (test-fllog+1-like fllog+1 name)
  (test-group name
    (test-values (fllog+1 +inf.0) '(+inf.0))
    (test-values (fllog+1 -1.0) '(-inf.0))
    (test-assert (flnan? (fllog -1.1)))))

(define (test-fllog+1)
  (test-fllog+1-like fllog+1 "fllog+1"))
(define (test-fllog2+1)
  (test-fllog+1-like fllog2+1 "fllog2+1"))
(define (test-fllog10+1)
  (test-fllog+1-like fllog10+1 "fllog10+1"))
