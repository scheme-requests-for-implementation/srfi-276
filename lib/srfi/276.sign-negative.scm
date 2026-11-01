; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (flsign-negative? fl)
  (case (srfi-144:flsign-bit fl)
    ((1) #t)
    ((0) #f)))

(define (flsign-positive? fl)
  (not (flsign-negative? fl)))