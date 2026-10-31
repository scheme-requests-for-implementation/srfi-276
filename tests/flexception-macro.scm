; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-flexception-macro)
  (for-each (lambda (sym)
              (test-equal sym
                          (eval `(flexception ,sym)
                                (environment
                                 '(srfi 276 flexception-macro)))))
            '(invalid-operation
              divide-by-zero
              overflow
              underflow
              inexact-result)))
