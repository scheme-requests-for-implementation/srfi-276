; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-rounding-mode)
  (test-group "rounding mode"
    (test-assert (rounding-mode? (current-rounding-mode)))
    (for-each (lambda (x)
                (test-assert (rounding-mode? x)))
              '(round-to-nearest/ties-to-even
                round-towards-positive
                round-towards-negative
                round-towards-zero))))
