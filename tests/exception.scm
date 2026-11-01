; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-flexception-name)
  (test-group "flexception-name?"
    (test-assert (flexception-name? 'invalid-operation))
    (test-assert (flexception-name? 'divide-by-zero))
    (test-assert (flexception-name? 'overflow))
    (test-assert (flexception-name? 'underflow))
    (test-assert (flexception-name? 'inexact-result))))

(define (test-flexception-behavior)
  (test-group "current-flexceptions"
    (test-assert (list? (current-flexceptions)))
    (let ((test-single
           (lambda (sym group-name)
             (test-group group-name
               (lower-flexceptions! (possible-flexceptions))
               (test-assert (not (memq sym (current-flexceptions))))
               (test-assert (not (flexceptions-raised? sym)))
               (raise-flexceptions! sym)
               (test-assert (memq sym (current-flexceptions)))
               (test-assert (flexceptions-raised? sym))))))
      (for-each test-single
                '(divide-by-zero overflow underflow
                  invalid-operation)
		'("divide by zero"
                  "overflow"
		  "underflow"
		  "invalid-operation")))))
