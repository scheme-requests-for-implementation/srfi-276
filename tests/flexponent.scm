; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-flexponent)
  (define-syntax test-flexponent
    (syntax-rules ()
      ((_ input expected)
       (test-values (flexponent (flonum input))
                    `(,expected)))))
    (test-flexponent +inf.0 +inf.0)
    (test-flexponent -inf.0 +inf.0)
    (test-assert "(flexponent +0.0)"
      (true-and-raises 'divide-by-zero
        (eqv? (flexponent +0.0) -inf.0)))
    (test-assert "(flexponent -0.0)"
      (true-and-raises 'divide-by-zero
        (eqv? (flexponent -0.0) -inf.0)))
    (test-flexponent 0.5 -1.0)
    (test-flexponent 1.0 0.0)
    (test-flexponent -1.0 0.0)
    (test-flexponent 2.0 1.0)
    (test-flexponent -2.0 1.0)
    (test-flexponent 3.0 1.0)
    (test-flexponent 4.0 2.0))
