(define (flonum->bytevector fl)
  (let ((bv (make-bytevector fl-byte-width)))
    (bytevector-flonum-set! bv 0 fl 'big)
    bv))

(define (test-binary64-bytevector-flonum-ref)
  (test-equal (bytevector-flonum-ref
               #u8(#x3F #xF0 0 0 0 0 0 0)
	       0
	       'big)
	      1.0)
  (test-equal (bytevector-flonum-ref
	       #u8(0 0 0 0 0 0 #xF0 #x3F)
	       0
	       'little)
	      1.0)
  (test-equal (bytevector-flonum-ref
	       #u8(#x3F #xF8 0 0 0 0 0 0)
	       0
	       'big)
	      1.5)
  (test-equal (bytevector-flonum-ref
	       #u8(0 0 0 0 0 0 0 0 0)
	       0
	       'big)
	      0.0)
  (test-equal (bytevector-flonum-ref
	       #u8(#x7F #xF0 0 0 0 0 0 0)
	       0
	       'big)
	       +inf.0)
  (test-equal (bytevector-flonum-ref
	       #u8(0 0 0 0 0 0 #xF0 #x7F)
	       0
	       'little)
	      +inf.0))

(define (test-binary64-bytevector-on-basic-numbers)
  (test-group "test basic numbers"
    (test-equal #u8(#x3F #xF0 0 0 0 0 0 0)
                (flonum->bytevector 1.0))
    (test-equal #u8(#x3F #xF8 0 0 0 0 0 0)
                (flonum->bytevector 1.5))
    (test-equal #u8(0 0 0 0 0 0 0 0)
                (flonum->bytevector 0.0))
    (test-equal #u8(0 0 0 0 0 0 0 1)
                (flonum->bytevector fl-least))
    (test-equal #u8(#x80 0 0 0 0 0 0 1)
                (flonum->bytevector (fl- fl-least)))
    (test-equal #u8(#x7F #xEF #xFF #xFF #xFF #xFF #xFF #xFF)
                (flonum->bytevector fl-greatest))
    (test-equal #u8(#xFF #xEF #xFF #xFF #xFF #xFF #xFF #xFF)
                (flonum->bytevector (fl- fl-greatest)))
    (test-equal #u8(0 #x10 0 0 0 0 0 0)
                (flonum->bytevector fl-least-normal))
    (test-equal #u8(#x80 #x10 0 0 0 0 0 0)
                (flonum->bytevector (fl- fl-least-normal)))
    (test-equal #u8(#x80 0 0 0 0 0 0 0)
                (flonum->bytevector -0.0))
    (test-equal #u8(#xBF #xF0 0 0 0 0 0 0)
                (flonum->bytevector -1.0))
    (test-equal #u8(#x7F #xF0 0 0 0 0 0 0)
                (flonum->bytevector +inf.0))
    (test-equal #u8(#xFF #xF0 0 0 0 0 0 0)
                (flonum->bytevector -inf.0))))

(define (test-bytevector-flonum-native-reverse)
  (test-group "bytevector-flonum-native-set! and \
               bytevector-flonum-native-ref are inverses"
    (test-property
     (lambda (len flonum)
       (let ((bv (make-bytevector (* len fl-byte-width)))
             (i (* fl-byte-width (random-integer len))))
         (bytevector-flonum-native-set! bv i flonum)
         (let ((r (bytevector-flonum-native-ref bv i)))
           (if (flnan? flonum)
               (flnan? r)
               (eqv? r flonum)))))
     (list (lambda () (+ 1 (random-integer 20)))
           (make-random-flonum-generator)))))

(define (test-bytevector-flonum-reverse)
  (test-group "bytevector-flonum-set! and \
               bytevector-flonum-ref are inverses"
    (test-property
     (lambda (len flonum endianness)
       (let* ((bytes (* len fl-byte-width))
              (bv (make-bytevector bytes))
              (i (random-integer (- bytes fl-byte-width))))
         (bytevector-flonum-set! bv i flonum endianness)
         (let ((r (bytevector-flonum-ref bv i endianness)))
           (if (flnan? flonum)
               (flnan? r)
               (eqv? r flonum)))))
     (list (lambda () (+ 1 (random-integer 100)))
           (make-random-flonum-generator)
           (gmap (lambda (bool)
                   (if bool 'little 'big))
                 (boolean-generator))
           ))))
