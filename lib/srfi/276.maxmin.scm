(define (flmax-filter-nans-binary x y)
  (cond
    ((flnan? x) y)
    ((flnan? y) x)
    ((and (eqv? x -0.0)
	  (eqv? y +0.0))
     +0.0)
    ((fl<? x y) y)
    (else x)))

(define (flmax-filter-nans . args)
  (if (null? args)
      -inf.0
      (fold flmax-filter-nans-binary
	    (car args)
	    (cdr args))))

(define (flmin-filter-nans-binary x y)
  (cond
   ((flnan? x) y)
   ((flnan? y) x)
   ((and (fl=? x -0.0)
	 (fl=? y +0.0))
    -0.0)
   ((fl<? x y) x)
   (else y)))

(define (flmin-filter-nans . args)
  (if (null? args)
      +inf.0
      (fold flmin-filter-nans-binary
	    (car args)
	    (cdr args))))

(define (flmax-binary x y)
  (cond
    ((flunordered? x y) +nan.0)
    ((and (eqv? x -0.0)
	  (eqv? y +0.0))
     +0.0)
    ((fl<? x y) y)
    (else x)))

(define (fold-nan f x l)
  (cond
    ((null? l) x)
    ((flnan? x) x)
    (else (fold-nan f (f x (car l)) (cdr l)))))

(define (flmax . args)
  (if (null? args)
      -inf.0
      (fold-nan flmax-binary (car args) (cdr args))))

(define (flmin-binary x y)
  (cond
    ((flunordered? x y) +nan.0)
    ((and (eqv? x -0.0) (eqv? y +0.0))
     -0.0)
    ((fl<? x y) x)
    (else y)))

(define (flmin . args)
  (if (null? args)
      +inf.0
      (fold-nan flmin-binary (car args) (cdr args))))

;;; XXX: What should the base case of flmax-abs and flmin-abs be?
;;; The base should probably be +0.0 for both of them.

(define (flmax-abs-filter-nans-binary x y)
  (cond
    ((flnan? x) y)
    ((flnan? y) x)
    (else
     (let ((abs-x (flabs x))
	   (abs-y (flabs y)))
       (cond
	((fl<? abs-x abs-y) y)
	((fl>? abs-x abs-y) x)
	(else (flmax-filter-nans-binary x y)))))))

(define (flmax-abs-filter-nans . args)
  (if (null? args)
      +0.0
      (fold flmax-abs-filter-nans-binary
	    (car args)
	    (cdr args))))

(define (flmin-abs-filter-nans-binary x y)
  (cond
    ((flnan? x) y)
    ((flnan? y) x)
    (else
     (let ((abs-x (flabs x))
	   (abs-y (flabs y)))
       (cond
	 ((fl<? abs-x abs-y) x)
	 ((fl>? abs-x abs-y) y)
	 (else (flmin-filter-nans-binary x y)))))))

(define (flmin-abs-filter-nans . args)
  (if (null? args)
      +0.0
      (fold flmin-abs-filter-nans-binary
	    (car args)
	    (cdr args))))

(define (flmax-abs-binary x y)
  (cond
    ((flunordered? x y) +nan.0)
    (else
     (let ((abs-x (flabs x))
	   (abs-y (flabs y)))
       (cond
	 ((fl<? abs-x abs-y) y)
	 ((fl>? abs-x abs-y) x)
	 (else (flmax-binary x y)))))))

(define (flmax-abs . args)
  (if (null? args)
      +0.0
      (fold-nan flmax-abs-binary (car args) (cdr args))))

(define (flmin-abs-binary x y)
  (cond
    ((flunordered? x y) +nan.0)
    (else
     (let ((abs-x (flabs x))
	   (abs-y (flabs y)))
       (cond
	 ((fl<? abs-x abs-y) x)
	 ((fl>? abs-x abs-y) y)
	 (else (flmin-binary x y)))))))

(define (flmin-abs . args)
  (if (null? args)
      +0.0
      (fold-nan flmin-abs-binary (car args) (cdr args))))
