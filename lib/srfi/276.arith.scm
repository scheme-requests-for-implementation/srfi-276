(define (fl+ . args)
  (cond
    ((null? args) +0.0)
    ((null? (cdr args)) (car args))
    (else (fold srfi-144:fl+ (car args) (cdr args)))))

(define (fl* . args)
  (cond
    ((null? args) 1.0)
    ((null? (cdr args)) (car args))
    (else (fold srfi-144:fl* (car args) (cdr args)))))

(define (fl- x . rest)
  (cond
    ((null? rest) (srfi-144:fl- x))
    (else
     (let loop ((x x)
		(y (car rest))
		(rest (cdr rest)))
       (if (null? rest)
	   (srfi-144:fl- x y)
	   (loop (srfi-144:fl- x y)
		 (car rest)
		 (cdr rest)))))))

(define (fl/ x . rest)
  (cond
    ((null? rest) (srfi-144:fl/ x))
    (else
     (let loop ((x x)
		(y (car rest))
		(rest (cdr rest)))
       (if (null? rest)
	   (srfi-144:fl/ x y)
	   (loop (srfi-144:fl/ x y)
		 (car rest)
		 (cdr rest)))))))
