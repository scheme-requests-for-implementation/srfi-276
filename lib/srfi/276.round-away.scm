(define (flround-away fl)
  (fltruncate (fl+ fl (flcopysign 0.5 fl))))