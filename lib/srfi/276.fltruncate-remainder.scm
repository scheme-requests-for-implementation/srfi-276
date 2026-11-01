; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (fltruncate-remainder x y)
  (cond
    ((flzero? y)
     (maybe-raise-flexceptions! 'invalid-operation)
     +nan.0)
    ((flinfinite? x)
     (maybe-raise-flexceptions! 'invalid-operation)
     +nan.0)
    (else (srfi-144:flremainder x y))))

(define flremainder fltruncate-remainder)
