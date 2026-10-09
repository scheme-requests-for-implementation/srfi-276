(define (create-exponential-coefficients base terms)
  (let ((numerator (log base)))
    (cons
     1.0
     (let loop ((term 1)
		(fact 1))
       (if (= term terms)
	   '()
	   (cons (/ (expt numerator term)
		    fact)
		 (loop (+ term 1)
		       (* (+ term 1) fact))))))))

(define (polynomial fl coeffs)
  (if (flzero? fl)
      (car coeffs)
      (do ((coeffs coeffs (cdr coeffs))
	   (fl^f 1.0 (fl* fl^f fl))
	   (acc 0.0 (fl+ acc (fl* fl^f (car coeffs)))))
	  ((null? coeffs) acc))))

;;; FIXME: Only for Binary64!
(define flexp2
  ;;; FIXME: coeffs is designed for 0.9999... by guess-and-check.
  (let ((coeffs (create-exponential-coefficients 2 20)))
    (lambda (fl)
      (cond
       ((fl=? fl -inf.0) 0.0)
       ((fl=? fl +inf.0) +inf.0)
       ((flzero? fl) 1.0)
       (else
	(let*-values (((ipart fpart) (flinteger-fraction fl))
		      ((big) (make-flonum 1.0 (exact ipart)))
		      ((exp2^frac) (polynomial fpart coeffs)))
	  ;; 2^x = 2^(i + f) = 2^i*2^f, |f| in [0.0,1.0)
	  ;; The integer part is simple, and on Binary64 systems
	  ;; is just an frexp.
	  (fl* exp2^frac big)))))))

(define (flexp10 fl)
  ;; FIXME: Is there a better algorithm for binary FP systems?
  (flexpt 10.0 fl))

(define (generate-exp-1 flexpB base)
  (let* ((coeffs (create-exponential-coefficients base 20))
	 ;; replace 1.0 with 0.0
	 (coeffs (cons 0.0 (cdr coeffs))))
    (lambda (fl)
      ;; The splitting method used above doesn't work. However,
      ;; 2^x > 2 for x >= 1, so there isn't much accuracy to be
      ;; gained here.
      (cond
        ((flzero? fl) fl)
	((fl<? (flabs fl) 1.0)
	 (polynomial fl coeffs))
	(else (fl- (flexpB fl) 1.0))))))

(define flexp2-1
  (generate-exp-1 flexp2 2))
(define flexp10-1
  (generate-exp-1 flexp10 10))
