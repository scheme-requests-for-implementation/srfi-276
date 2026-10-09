;;; Stef Graillat. Accurate Floating Point Product and
;;; Exponentiation. IEEE Transactions on Computers, 2009, 58 (7),
;;; pp.994-1000. ⟨10.1109/TC.2008.215⟩. ⟨hal-00164607⟩
;;;
;;; Note that his double-double algorithms are wrong: see
;;; Mioara Joldes, Jean-Michel Muller, Valentina Popescu.
;;; Tight and rigourous error bounds for basic building blocks of double-word arithmetic.
;;; ACM Transactions on Mathematical Software, (hal-01351529v2) 2017
;;; for correct calculations.
;;;
;;; This has been modified to take the double-double repreresentation
;;; of small+large.

(define (compound-positive large small n)
  ;; Compute (small + large)^n, n positive.
  ;;
  ;; Since it might be the case that small << large, we use double-double
  ;; arithmetic to compute the compounding.
  (cond
    ((negative? n) (fl/ (compound-positive small large (- n))))
    (else
     (let ((large+small (f+f->df large small))
	   (n (exact n)))
       (let loop ((acc (df 1.0 0.0))
                  (i (- (integer-length n) 1)))
         (if (negative? i)
             (df->f acc)
             (let ((acc (df*df acc acc))
		   (hit? (not (zero? (bitwise-and 1 (arithmetic-shift n (- i)))))))
               (if hit?
                   (loop (df*df acc large+small) (- i 1))
                   (loop acc (- i 1))))))))))

(define (flcompound fl n)
  (unless (integer? n)
    (error "not an integer" n))
  (cond
    ((fl<? fl -1.0) +nan.0)
    ((and (fl>=? fl -1.0) (zero? n)) 1.0)
    ((and (fl=? fl -1.0) (negative? n)) +inf.0)
    ((and (fl=? fl -1.0) (positive? n)) +0.0)
    ((and (fl=? fl +inf.0) (positive? n)) +inf.0)
    ((and (fl=? fl +inf.0) (negative? n)) +0.0)
    ((fl>? fl 1.0) (compound-positive fl 1.0 n))
    ((fl=? fl 1.0) (flexpt 2.0 n))
    ((fl<? fl 1.0) (compound-positive 1.0 fl n))
    (else fl))) ;; unordered case
