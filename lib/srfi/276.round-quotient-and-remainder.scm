; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (flround-quotient x y)
  (flround (fl/ x y)))

(define (flround-remainder x y)
  (let-values (((ignored remainder) (flremquo x y)))
    remainder))
