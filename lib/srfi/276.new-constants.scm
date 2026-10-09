(define fl-radix
  ;; Malcolm's algorithm. See Handbook of Floating Point
  (do ((A (flonum 1.0) (fl* (flonum 2.0) A)))
      ((not (fl=? (fl- (fl+ A (flonum 1.0)) A) (flonum 1.0)))
       (do ((B (flonum 1.0) (fl+ B (flonum 1.0))))
           ((fl=? (fl- (fl+ A B) A) B)
            (exact B))))))

(define fl-precision
  (do ((i 0 (+ i 1))
       (B (flonum fl-radix))
       (A (flonum 1.0) (fl* B A)))
      ((not (fl=? (fl- (fl+ A (flonum 1.0)) A) (flonum 1.0)))
       i)))

(define fl-maximum-exponent
  (flinteger-exponent fl-greatest))

(define fl-least-normal
  (do ((candidate fl-least (fl* B candidate))
       (B (flonum fl-radix)))
      ((flnormal? candidate) candidate)))

(define fl-minimum-normalized-exponent
  (flinteger-exponent fl-least-normal))

(define fl-minimum-exponent
  (flinteger-exponent fl-least))

(define fl-byte-width 8)    ;; FIXME: Only works for Binary64
