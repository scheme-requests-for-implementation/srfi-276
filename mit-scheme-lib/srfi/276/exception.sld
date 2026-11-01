; SPDX-FileCopyrightText: 2026 Peter McGoron
;
; SPDX-License-Identifier: GPL-2.0-or-later
(define-library (srfi 276 exception)
  (import (scheme base)
          (only (mit legacy runtime)
                flo:exceptions->names
                flo:names->exceptions
                flo:test-exceptions
                flo:supported-exceptions
                flo:raise-exceptions!
                flo:clear-exceptions!)
          (only (srfi 1) delete-duplicates))
  (export flexception-name?
          current-flexceptions
          possible-flexceptions
          flexceptions-raised?
          raise-flexceptions!
          lower-flexceptions!)

  (cond-expand
    ((library (srfi 1))
     (import (srfi 1)))
    (else
     (begin
       (define (append-map f lst)
         (if (null? lst)
             '()
             (append (f (car lst))
                     (append-map f (cdr lst))))))))

  (begin
    (define (flexception-name? obj)
      (pair? (memq obj (possible-flexceptions))))

    (define (current-flexceptions)
      (flo:exceptions->names
       (flo:test-exceptions
        (flo:supported-exceptions))))

    (define (possible-flexceptions)
      (delete-duplicates
        (append '(invalid-operation
                  divide-by-zero
                  overflow
                  underflow
                  inexact-result)
                (flo:exceptions->names
                 (flo:supported-exceptions)))))

    (define (process-list exceptions)
      (append-map (lambda (x)
                    (if (pair? x)
                        x
                        (list x)))
       exceptions))

    (define (flexceptions-raised? . exceptions)
      (not
       (zero?
        (flo:test-exceptions
         (flo:names->exceptions (process-list exceptions))))))

    (define (raise-flexceptions! . exceptions)
      (flo:raise-exceptions!
       (flo:names->exceptions (process-list exceptions))))

    (define (lower-flexceptions! . exceptions)
      (flo:clear-exceptions!
       (flo:names->exceptions (process-list exceptions))))
))