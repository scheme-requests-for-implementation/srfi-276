(define (flrsqrt fl)
  (cond
    ((flzero? fl) (flcopysign +inf.0 fl))
    ((fl=? fl +inf.0) 0.0)
    ((flnegative? fl) +nan.0)
    (else
     ;; fl = s*b^e
     ;; fl^-1/2 = s^-1/2 b^-e/2
     ;;         = (s*b^e)^-1/2
     ;;         = (s*b^g)^-1/2 * b^-E
     ;; -E - g/2 = -e/2
     ;; E + g/2 = e/2
     ;; 2E + g = e
     ;; Select g to be non-negative.
     ;; Then either e was even, so that g = 0, or e was odd, so g = 1.
     ;;
     ;; This reduces to finding the reciprocal square root of S=s*b^g,
     ;; which is in [1, 4), so the reciprocal square root is in
     ;; [1, 1/2).
     (let*-values (((s e) (flnormalized-fraction-exponent fl))
		   ((s) (fl* s (flonum fl-radix)))
		   ((e) (- e 1))
		   ((E g) (floor/ e 2))
		   ((E) (- E))
		   ((S) (make-flonum s g)))
       ;; Newton-Raphson iteration. The function we are trying to
       ;; compute is fl^(-1/2), which is equivalent to finding the root
       ;; of
       ;; f(y) = y^-2 - fl
       ;; To do Newton-Raphson, first take the derivative
       ;; f'(y) = -2y^-3
       ;;
       ;; Then given guess y_n,
       ;; y_{n+1} = y_n - f(y_n)/f'(y_n)
       ;;         = y_n - (y^-2 - fl)/(-2y^-3)
       ;;         = y_n + y_n/2 - fly^3/2
       ;;         = y_n(3/2 - fl y_n^2/2)
       ;;
       ;; We need a first guess, which may be approximated by a LSQ
       ;; approximation of [1,4) into [1, 1/2):
       ;; y_0=-0.14870771198964516 * fl + 1.039280327235174
       (do ((y (fl+ (fl* (flonum -0.14870771198964516)
			 S)
		    (flonum 1.039280327235174))
	       (fl* y (fl- 1.5 (fl* S/2 (flsquare y)))))
	    (S/2 (fl* S 0.5))
	    (i 0 (+ i 1)))
	   ((= i 6) (make-flonum y E)))))))
