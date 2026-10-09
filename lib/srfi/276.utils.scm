;;; Utilities for algorithms with increased precision.

(define (fast2sum x y)
  (let* ((s (fl+ x y))
         (z (fl- s x))
         (t (fl- y z)))
    (values s t)))

(define (twomult x y)
  (let ((p (fl* x y)))
    (values p (fl+* x y (fl- p)))))

;;; fl+*, when correct rounding is not necessary

(define flfast+*
  (if fl-fast-fl+*
      fl+*
      (lambda (x y z)
        (fl+ (fl* x y) z))))

;;; double-flonum arithmetic
;;; This is the unevaluated sum of two flonums
;;; Not the same as binary64 arithietic

(define-record-type <double-flonum>
  (df ah al)
  df?
  (ah df-hi)
  (al df-lo))

(define (f+f->df big small)
  ;; Flonum, Flonum -> Double-Flonum
  ;; where big >= small
  (let-values (((s t) (fast2sum big small)))
    (df s t)))

(define (df->f df)
  ;; Double-Flonum -> Flonum (approximation)
  (fl+ (df-hi df) (df-lo df)))

(define (df*df df1 df2)
  ;; Double-Flonum, Double-Flonum -> Double-Flonum
  ;; Computes the product of two double-flonums
  (let*-values (((t1 t2) (twomult (df-hi df1) (df-hi df2)))
                ((t3) (fl+ (fl+ (fl* (df-hi df1) (df-lo df2))
                                (fl* (df-lo df1) (df-hi df2)))
                           t2))
                ((hi lo) (fast2sum t1 t3)))
    (df hi lo)))

(define (df*d %df d)
  ;; Double-Flonum, Flonum -> Double-Flonum
  ;; Computes the product of a double-flonum and a flonum
  (let*-values (((t1 t2) (twomult (df-hi %df) d))
                ((t3) (flfast+* d (df-lo %df) t2))
                ((hi lo) (fast2sum t1 t3)))
    (df hi lo)))
