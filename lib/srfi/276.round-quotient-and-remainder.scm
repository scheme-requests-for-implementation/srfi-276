(define (round-quotient x y)
  (flround (fl/ x y)))

(define (round-remainder x y)
  (let-values (((ignored remainder) (flremquo x y)))
    remainder))
