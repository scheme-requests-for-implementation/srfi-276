(import (srfi 64))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ (predicate arg ...))
     (test-assert (predicate arg ...)))
    ((_ name (predicate arg ...))
     (test-assert name (predicate arg ...)))))

(define-syntax test-not-predicate
  (syntax-rules ()
    ((_ (predicate args ...))
     (test-assert (not (predicate args ...))))
    ((_ name (predicate args ...))
     (test-assert name (not (predicate args ...))))))

(define-syntax test-values
  (syntax-rules ()
    ((_ (procedure args ...) values)
     (test-values #f (procedure args ...) values))
    ((_ name (procedure args ...) expected-values)
       (test-equal name
         expected-values
         (call-with-values (lambda () (procedure args ...)) list)))))

(define-syntax test-many-approximate
  (syntax-rules ()
    ((_ function error (expect input) ...)
     (begin
       (test-approximate expect (function input) error)
       ...))))
