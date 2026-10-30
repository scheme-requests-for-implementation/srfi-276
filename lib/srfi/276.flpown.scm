; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT

;;;; See coments on 276.compound.scm

(define (%flpown fl n)
  (cond
    ((negative? n) (fl/ (%flpown fl (- n))))
    (else
     (let ((large+small (f+f->df fl 0.0))
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

(define (flpown fl n)
  (unless (integer? n)
    (error "not an integer" n))
  (cond
    ((zero? n) 1.0)
    ((and (flzero? fl) (negative? n) (odd? n))
     (maybe-raise-flexceptions! 'divide-by-zero)
     (flcopysign +inf.0 fl))
    ((and (flzero? fl) (negative? n) (even? n))
     (maybe-raise-flexceptions! 'divide-by-zero)
     +inf.0)
    ((and (flzero? fl) (positive? n) (even? n))
     +0.0)
    ((and (flzero? fl) (positive? n) (odd? n))
     fl)
    ;; n -> -n
    ((and (flinfinite? fl) (odd? n) (positive? n))
     (flcopysign +inf.0 fl))
    ((and (flinfinite? fl) (even? n) (positive? n))
     +inf.0)
    ((and (flinfinite? fl) (odd? n) (negative? n))
     (flcopysign 0.0 fl))
    ((and (flinfinite? fl) (even? n) (negative? n))
     0.0)
    (else (%flpown fl (exact n)))))
