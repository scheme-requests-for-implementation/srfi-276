; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 276 flexception-macro)
  (import (scheme base))
  (export flexception)
  (begin
    (define-syntax flexception
      (er-macro-transformer
       (lambda (exp rename compare)
         (let ((sym (list-ref exp 1)))
           (if (member (symbol->string sym)
                        '("invalid-operation"
                          "divide-by-zero"
                          "overflow"
                          "underflow"
                          "inexact-result"))
               `(,(rename 'quote) ,sym)
               (error "unknown floating-point exception" sym))))))))
