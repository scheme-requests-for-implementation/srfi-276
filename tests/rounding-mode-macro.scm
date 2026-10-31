; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-rounding-mode-macro)
  (for-each (lambda (sym)
              (test-equal sym
                          (eval `(rounding-mode ,sym)
                                (environment
                                 '(srfi 276 rounding-mode-macro)))))
            '(round-to-nearest/ties-to-even
              round-towards-positive
              round-towards-negative
              round-towards-zero)))
