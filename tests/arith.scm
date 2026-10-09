(define (test-fl+)
  (test-group "finite fl+"
    (test-values (fl+) '(0.0))
    (test-values (fl+ 0.0) '(0.0))
    (test-values (fl+ -0.0) '(-0.0))
    (test-values (fl+ 0.0 0.0) '(0.0))
    (test-values (fl+ 0.0 -0.0) '(0.0))
    (test-values (fl+ -0.0 0.0) '(0.0))
    (test-values (fl+ -0.0 -0.0) '(-0.0))
    (test-values (fl+ 1.0 0.0) '(1.0))
    (test-values (fl+ 1.0 -0.0) '(1.0))
    (test-values (fl+ 1.0 1.0) '(2.0))
    (test-values (fl+ 1.0 -1.0) '(0.0))
    (test-values (fl+ -1.0 1.0) '(0.0))
    (test-values (fl+ 1.0 2.0 3.0 4.0) '(10.0))
    (test-values (fl+ fl-least 0.0) `(,fl-least))
    (test-values (fl+ fl-least -0.0) `(,fl-least))
    (test-values (fl+ fl-greatest 0.0) `(,fl-greatest))
    (test-values (fl+ fl-greatest -0.0) `(,fl-greatest)))

  (test-group "fl+ on infinities"
    (test-values (fl+ +inf.0 +inf.0) '(+inf.0))
    (test-values (fl+ -inf.0 -inf.0) '(-inf.0))
    (test-predicate (flnan? (fl+ +inf.0 -inf.0)))
    (test-predicate (flnan? (fl+ -inf.0 +inf.0)))
    (test-values (fl+ +inf.0 (fl- fl-greatest)) '(+inf.0))
    (test-values (fl+ -inf.0 fl-greatest) '(-inf.0)))

  (test-group "fl+ on NaN"
    (test-predicate (flnan? (fl+ 0.0 +nan.0)))))

(define (test-fl-)
  (test-group "finite fl-"
    (test-values (fl- 0.0) '(-0.0))
    (test-values (fl- -0.0) '(+0.0))
    (test-values (fl- 0.0 0.0) '(0.0))
    (test-values (fl- 0.0 -0.0) '(0.0))
    (test-values (fl- -0.0 0.0) '(-0.0))
    (test-values (fl- -0.0 -0.0) '(0.0))
    (test-values (fl- 1.0 0.0) '(1.0))
    (test-values (fl- 1.0 -0.0) '(1.0))
    (test-values (fl- 10.0 4.0 3.0 2.0 1.0) '(0.0))
    (test-values (fl- 1.0 1.0) '(0.0))
    (test-values (fl- 1.0 -1.0) '(2.0))
    (test-values (fl- -1.0 1.0) '(-2.0)))

  (test-group "fl- on infinities"
    (test-values (fl- +inf.0 -inf.0) '(+inf.0))
    (test-values (fl- -inf.0 +inf.0) '(-inf.0))
    (test-predicate (flnan? (fl- +inf.0 +inf.0)))
    (test-predicate (flnan? (fl- -inf.0 -inf.0))))

  (test-group "fl- on NaNs"
    (test-predicate (flnan? (fl- +nan.0)))
    (test-predicate (flnan? (fl- 1.0 +nan.0)))))

(define (test-fl*)
  (test-group "finite fl*"
    (test-values (fl*) '(1.0))
    (test-values (fl* 0.0) '(0.0))
    (test-values (fl* -0.0) '(-0.0))
    (test-values (fl* 2.0 4.0 8.0) '(64.0))
    (test-values (fl* -0.0 0.0) '(-0.0))
    (test-values (fl* -0.0 -0.0) '(0.0)))
  (test-group "fl* on infinities"
    (test-values (fl* +inf.0 +inf.0) '(+inf.0))
    (test-values (fl* -inf.0 +inf.0) '(-inf.0))
    (test-predicate (flnan? (fl* +inf.0 0.0))))
  (test-group "fl* on NaNs"
    (test-predicate (flnan? (fl* +nan.0)))
    (test-predicate (flnan? (fl* 1.0 +nan.0)))))

(define (test-fl/)
  (test-group "finite fl/"
    (test-values (fl/ 1.0) '(1.0))
    (test-values (fl/ 0.5) '(2.0))
    (test-values (fl/ 0.5 1.0) '(0.5))
    (test-values (fl/ 1.0 0.5) '(2.0))
    (test-values (fl/ 0.0 1.0) '(0.0))
    (test-values (fl/ 64.0 8.0 4.0 2.0) '(1.0))
    (test-values (fl/ -0.0 1.0) '(-0.0))
    (test-values (fl/ 0.0 -1.0) '(-0.0)))
  (test-group "fl/ on infinities"
    (test-values (fl/ 0.0) '(+inf.0))
    (test-values (fl/ -0.0) '(-inf.0))
    (test-values (fl/ 1.0 0.0) '(+inf.0))
    (test-values (fl/ -1.0 -0.0) '(+inf.0))
    (test-values (fl/ 1.0 -0.0) '(-inf.0)))
  (test-group "fl/ of NaNs"
    (test-predicate (flnan? (fl/ +inf.0 +inf.0)))
    (test-predicate (flnan? (fl/ -inf.0 -inf.0)))
    (test-predicate (flnan? (fl/ 0.0 0.0)))))