; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: GPL-2.0-or-later
(define-library (srfi 276 with-rounding-mode)
  (import (scheme base)
          (only (mit legacy runtime)
                flo:with-rounding-mode))
  (export with-rounding-mode)
  (begin
    (define-syntax with-rounding-mode
      (syntax-rules ()
        ((_ mode body ...)
         (flo:with-rounding-mode
          (let ((%mode mode))
            (cond
              ((assq %mode
                     '((round-to-nearest/ties-to-even . to-nearest)
                       (round-towards-zero . toward-zero)
                       (round-towards-negative . downward)
                       (round-towards-positive . upward)))
               => cdr)
              (else %mode)))
            (lambda () body ...)))))))