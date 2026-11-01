; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: GPL-2.0-or-later
(define-library (srfi 276 rounding-mode)
  (import (scheme base)
          (only (mit legacy runtime)
                flo:rounding-modes
                flo:rounding-mode))
  (export current-rounding-mode rounding-mode?)
  (begin
    (define (current-rounding-mode)
      (case (flo:rounding-mode)
        ((to-nearest) 'round-to-nearest/ties-to-even)
        ((toward-zero) 'round-towards-zero)
        ((downward) 'round-towards-negative)
        ((upward) 'round-towards-positive)
        (else => values)))
    (define (rounding-mode? obj)
      (pair?
       (memq obj (append '(round-to-nearest/ties-to-even
                           round-towards-zero
                           round-towards-negative
                           round-towards-positive)
                         (flo:rounding-modes)))))))