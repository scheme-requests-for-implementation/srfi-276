; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: GPL-2.0-or-later
(define-library (srfi 276 set-rounding-mode)
  (import (scheme base)
          (only (mit legacy runtime)
                flo:set-rounding-mode!))
  (export set-rounding-mode!)
  (begin
    (define (set-rounding-mode! rounding-mode)
      (flo:set-rounding-mode!
       (cond
         ((assq rounding-mode
                '((round-to-nearest/ties-to-even . to-nearest)
                  (round-towards-zero . toward-zero)
                  (round-towards-negative . downward)
                  (round-towards-positive . upward)))
          => cdr)
         (else rounding-mode))))))