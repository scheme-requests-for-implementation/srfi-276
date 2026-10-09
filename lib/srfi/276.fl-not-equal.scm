(define fl!=?
  (lambda args
    (cond
      ((null? args) #f)
      ((null? (cdr args)) #f)
      (else
        (let ((x (car args))
              (y (cadr args))
              (rest (cddr args)))
          (let loop ((x x) (y y) (rest rest))
            (cond
              ((flunordered? x y) #f)
              ((not (fl=? x y)) #t)
              (else (and (pair? rest)
                         (loop y (car rest) (cdr rest)))))))))))
