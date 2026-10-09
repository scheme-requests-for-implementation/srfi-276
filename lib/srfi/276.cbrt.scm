(define (flcbrt fl)
  (cond
    ((flzero? fl) fl)
    ((flinfinite? fl) fl)
    ((flnan? fl) fl)
    (else
     ;; Note that x = s*b^e, hence
     ;; x^1/3 = s^1/3*b^(e/3)
     ;;       = (s*b^g)^1/3 * b^E
     ;;       = S^1/3 * b^E
     ;; S = s*b^g
     ;; where 3E + g = e. Select g to be non-negative. Then
     ;;  {0, 1, 2}.
     ;; This reduces to computing the cube root of S,
     ;; which is in [1, 8). The cube root is in [1, 2).
     (let*-values (((s e) (flnormalized-fraction-exponent fl))
                   ((s) (fl* s (flonum fl-radix)))
                   ((e) (- e 1))
                   ((E g) (floor/ e 3))
                   ((S) (make-flonum s g)))
       ;; Newton-Raphson iteration. Want to find the root of
       ;; f(y) = y^3 - x
       ;; then
       ;; f'(y) = 3y^2
       ;; Then given guess y_n,
       ;;
       ;; y_{n+1} = y_n - f(y_n)/f'(y_n)
       ;;         = y_n - (y_n^3 - x)/3y_n^2
       ;;         = y_n - y_n/3 + x/3y_n^2
       ;;         = 2y_n/3 + x/3y_n^2
       ;;         = (2y_n + x/y_n^2)/3
       ;;
       ;; The simplest way to do the first guess is to do the least square
       ;; approximation of x^1/3.
       ;;
       ;; Computed coefficients:
       ;; y_ 0.14492123320839337*S + -0.5878165806269391
       (do ((y (fl+ (fl* (flonum 0.14492123320839337) S)
                     (flonum 0.5878165806269391))
               (fl/ (fl+ (fl* (flonum 2.0) y)
                         (fl/ S (flsquare y)))
                    3.0))
            (i 0 (+ i 1)))
           ((= i 6) (make-flonum y E)))))))
