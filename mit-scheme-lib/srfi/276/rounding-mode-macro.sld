; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: MIT

(define-library (srfi 276 rounding-mode-macro)
  (import (scheme base))
  (export rounding-mode)
  (begin
    (define-syntax rounding-mode
      (er-macro-transformer
       (lambda (exp rename compare)
         (let ((sym (list-ref exp 1)))
           (if (member (symbol->string sym)
                       '("round-to-nearest/ties-to-even"
                         "round-towards-zero"
                         "round-towards-negative"
                         "round-towards-positive"))
               `(,(rename 'quote) ,sym)
               (error "unknown rounding mode" sym))))))))
