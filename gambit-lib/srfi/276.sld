(define-library (srfi 276)
  (import (rename (except gambit let-values let*-values)
                  (fltruncate %fltruncate)
                  (flnumerator %flnumerator)
                  (fldenominator %fldenominator)
                  (fl+* %fl+*)))
  ;;; Constants
  (export fl-radix fl-precision fl-maximum-exponent fl-fast-fl+*
          fl-minimum-exponent fl-minimum-normalized-exponent
          fl-greatest fl-least fl-least-normal fl-epsilon fl-byte-width)
  (begin
    ;; This is a hack to get around a bug in Gambit
    (define-macro (let-values bindings . rest)
      (define (remap-formals formals)
        (cond
          ((symbol? formals) (gensym formals))
          ((null? formals) '())
          (else (cons (gensym (car formals))
                      (remap-formals (cdr formals))))))
      (define (construct-bindings formals tmps)
        (cond
          ((null? formals) '())
          ((symbol? formals) (list (list formals tmps)))
          (else (cons (list (car formals) (car tmps))
                      (construct-bindings (cdr formals) (cdr tmps))))))
      (let loop ((clauses bindings)
                 (let-bindings '()))
        (if (null? clauses)
            `(let ,let-bindings . ,rest)
            (let* ((clause (car clauses))
                   (formals (car clause))
                   (tmps (remap-formals formals))
                   (expr (cadr clause)))
              `(call-with-values (lambda () ,expr)
                 (lambda ,tmps
                   ,(loop (cdr bindings)
                          (append (construct-bindings formals tmps)
                                  let-bindings))))))))

    (define-syntax let*-values
      (syntax-rules ()
        ((let*-values () body0 body1 ...)
         (let () body0 body1 ...))
        ((let*-values (binding0 binding1 ...)
             body0 body1 ...)
         (let-values (binding0)
           (let*-values (binding1 ...)
             body0 body1 ...)))))

    (c-declare "#include <float.h>")
    (c-declare "#include <fenv.h>")
    (c-declare "#pragma STDC FENV_ACCESS ON")
    (define fl-radix
      ((c-lambda () int "___return(FLT_RADIX);")))
    (define fl-precision
      ((c-lambda () int "___return(DBL_MANT_DIG);")))
    (define fl-maximum-exponent
      ((c-lambda () int "___return(ilogb(DBL_MAX));")))
    (define fl-minimum-normalized-exponent
      ((c-lambda () int "___return(ilogb(DBL_MIN));")))
    (define fl-minimum-exponent
      ((c-lambda () int "___return(ilogb(DBL_TRUE_MIN));")))
    (define fl-greatest
      ((c-lambda () double "___return(DBL_MAX);")))
    (define fl-least
      ((c-lambda () double "___return(DBL_TRUE_MIN);")))
    (define fl-least-normal
      ((c-lambda () double "___return(DBL_MIN);")))
    (define fl-epsilon
      ((c-lambda () double "___return(DBL_EPSILON);")))
    (define fl-fast-fl+*
      ((c-lambda () bool "\
#if defined(FP_FAST_FMA)
  ___return(1);
#else
  ___return(0);
#endif
")))
    (define fl-byte-width
      ((c-lambda () int "___return(sizeof(double));"))))

  (begin
    (define fl=? fl=)
    (define fl>? fl>)
    (define fl<? fl<)
    (define fl>=? fl>=)
    (define fl<=? fl<=)
    (define fllog+1 fllog1p))
  (include "../../lib/srfi/276.utils.scm")
  (export fl-e fl-1/e fl-e^2 fl-e^pi/4
          fl-log2-e fl-log10-e
          fl-log-2 fl-1/log-2
          fl-log-3 fl-log-pi
          fl-log-10 fl-1/log-10
          fl-pi fl-1/pi
          fl-2pi fl-pi/2
          fl-2/pi fl-pi/4
          fl-2/sqrt-pi fl-sqrt-pi
          fl-pi^2 fl-degree
          fl-gamma-1/2 fl-gamma-1/3 fl-gamma-2/3
          fl-sqrt-2 fl-sqrt-3 fl-sqrt-5 fl-sqrt-10
          fl-cbrt-2 fl-cbrt-3
          fl-4thrt-2 fl-1/sqrt-2
          fl-phi fl-log-phi
          fl-1/log-phi fl-euler fl-e^euler
          fl-sin-1 fl-cos-1)
  (include "../../lib/srfi/276.constants.scm")
  ;;; Constructors
  (export flonum flonum? fladjacent flcopysign
          (rename flscalbn make-flonum))
  (begin
    (define make-flonum flscalbn)
    (define (flonum z)
      (cond
        ((not (real? z)) +nan.0)
        ((fixnum? z) (fixnum->flonum z))
        (else (inexact z))))
    (define fladjacent (c-lambda (double double) double "nextafter"))
    (define flcopysign (c-lambda (double double) double "copysign")))
  ;;; Accessors
  (export flinteger-fraction flexponent flinteger-exponent
          fl-integer-exponent-zero fl-integer-exponent-nan
          flnormalized-fraction-exponent
          flsign-negative? flsign-positive?)
  (begin
    (define flnormalized-fraction-exponent
      (let ((frexp (c-lambda (double scheme-object) int
                     "\
int exp;
___F64VECTORSET(___arg2,
                ___FIX(0),
                frexp(___arg1, &exp));
___return(exp);")))
        (lambda (fl)
          (let* ((v (make-f64vector 1))
                 (exp (frexp fl v)))
            (values (f64vector-ref v 0) exp)))))
    (define (flinteger-fraction fl)
      (cond
        ((flzero? fl) (values fl fl))
        ((flinfinite? fl) (values fl (flcopysign 0.0 fl)))
        ((flnan? fl) (values fl fl))
        (else
          ;; Work around a bug in some libc(?)s
          (let ((ipart (fltruncate fl)))
            (values (flcopysign ipart fl)
                    (flcopysign (fl- fl ipart) fl))))))
    (define flexponent (c-lambda (double) double "logb"))
    (define flinteger-exponent
      (c-lambda (double) int "ilogb"))
    (define fl-integer-exponent-zero
      ((c-lambda () int "___return(FP_ILOGB0);")))
    (define fl-integer-exponent-nan
      ((c-lambda () int "___return(FP_ILOGBNAN);")))
    (define flsign-negative?
      (c-lambda (double) bool "___return(signbit(___arg1));"))
    (define flsign-positive?
      (c-lambda (double) bool "___return(!signbit(___arg1));")))
   ;;; Order predicates
   (export (rename fl= fl=?)
           (rename fl< fl<?)
           (rename fl> fl>?)
           (rename fl<= fl<=?)
           (rename fl>= fl>=?)
           fl!=?
           fltotal=? fltotal<? fltotal>? fltotal<=? fltotal>=?
           flunordered? flordered?
           flinteger?
           flzero? flpositive? flnegative?
           flodd? fleven?
           flfinite? flinfinite?
           flnan?
           flnormal? flsubnormal?)
   (include "../../lib/srfi/276.total.scm")
   (include "../../lib/srfi/276.fl-not-equal.scm")
   (begin
     (define flunordered?
       (c-lambda (double double) bool "___return(isunordered(___arg1, ___arg2));"))
     (define flordered?
       (c-lambda (double double) bool "___return(!isunordered(___arg1, ___arg2));"))
     (define flnormal?
       (c-lambda (double) bool "___return(isnormal(___arg1));"))
     (define flsubnormal?
       (c-lambda (double) bool "___return(!isnormal(___arg1));")))
   ;;; Arithmetic
   (export flmax flmin
           flmax-filter-nans flmin-filter-nans
           flmax-abs flmin-abs
           flmax-abs-filter-nans flmin-abs-filter-nans
           fl+ fl- fl* fl/ fl+* flabs
           flabsdiff flposdiff flsgn
           flnumerator fldenominator)
   (include "../../lib/srfi/276.maxmin.scm")
   (begin
     (define fl+*
       ;; This works around a bug in musl. This should be removed when
       ;; it gets fixed.
       (if fl-fast-fl+*
           %fl+*
             (lambda (x y z)
               (let ((res (%fl+* x y z)))
                 (if (flzero? res)
                     (let ((b1 (flsign-negative? x))
                           (b2 (flsign-negative? y))
                           (b3 (flsign-negative? z)))
#| Special cases:
    0.0    ; (0.0 0.0 0.0)
    0.0    ; (0.0 0.0 -0.0)
    0.0    ; (0.0 -0.0 0.0)
    -0.0   ; (0.0 -0.0 -0.0)
    0.0    ; (-0.0 0.0 0.0)
    -0.0   ; (-0.0 0.0 -0.0)
    0.0    ; (-0.0 -0.0 0.0)
    0.0    ; (-0.0 -0.0 -0.0)
|#
                       (cond
                         ((and (not b1) b2 b3) -0.0)
                         ((and b1 (not b2) b3) -0.0)
                         (else 0.0)))
                       res)))))
     (define (flabsdiff x y)
       (flabs (fl- x y)))
     (define flposdiff
       (c-lambda (double double) double "fdim"))
     (define (flsgn x)
       (flcopysign 1.0 x))
     (define (flnumerator x)
       (cond
         ((flinfinite? x) x)
         ((flnan? x) x)
         (else (%flnumerator x))))
     (define (fldenominator x)
       (cond
         ((flinfinite? x) 1.0)
         ((flnan? x) x)
         (else (%fldenominator x)))))
   ;;; Integer rounding
   (export flfloor flceiling
           fltruncate
           flround flround-away)
   ;; roundeven is in C11, but not C99.
   (begin
     (define (fltruncate fl)
       ;; Work around a bug in musl(?)
       (flcopysign (%fltruncate fl) fl))
     (define flround-away
       (c-lambda (double) double "round")))
   ;;; Integer division
   (export flquotient flremainder
           (rename flquotient fltruncate-quotient)
           (rename flremainder fltruncate-remainder)
           flremquo
           flround-quotient
           flround-remainder)
   (include "276.fltruncate-remainder.scm")
   (begin
     (define flquotient
       (c-lambda (double double) double "\
int rounding_mode = fegetround();
double result = ___arg1/___arg2;
if (!isinf(result)) {
  fesetround(FE_TOWARDZERO);
  result = ___arg1/___arg2;
  fesetround(rounding_mode);
}
  ___return(copysign(trunc(result), result));"))
     (define (flremquo x y)
       (let* ((v (make-f64vector 1))
              (remquo (c-lambda (double double scheme-object) int "\
int q;
___F64VECTORSET(___arg3, ___FIX(0), remquo(___arg1, ___arg2, &q));
___return(q);"))
              (q (remquo x y v)))
         (values (f64vector-ref v 0) q)))
     (define (flround-quotient x y)
       (flround-away (fl/ x y)))
     (define flround-remainder
       (c-lambda (double double) double "remainder")))
   ;;; Exponents
   (export flexp flexp2 flexp10
           (rename flexpm1 flexp-1)
           flexp2-1
           flexp10-1
           flexpt)
   (include "../../lib/srfi/276.exp.scm")
   ;;; Logarithms
   (export fllog fllog2 fllog10
           (rename fllog1p fllog+1)
           fllog2+1
           fllog10+1
           make-fllog-base)
   (include "../../lib/srfi/276.fllog.scm")
   (begin
     (define fllog2
       (c-lambda (double) double "log2"))
     (define fllog10
       (c-lambda (double) double "log10"))
     (define (make-fllog-base x)
       (lambda (y)
         (fl/ (fllog y) (fllog x)))))
   ;;; Powers and roots
   (export flcbrt flcompound flhypot flrsqrt flsqrt)
   (include "../../lib/srfi/276.compound.scm")
   (include "../../lib/srfi/276.rsqrt.scm")
   (begin
     (define flcbrt
       (c-lambda (double) double "cbrt")))
   ;;; Trigonometric functions
   (export flsin flcos fltan
           flsinpi flcospi fltanpi)
   (include "../../lib/srfi/276.trigpi.scm")
   ;;;; Inverse trigonometric functions
   (export flasin flacos flatan
           flasinpi flacospi flatanpi)
   (include "../../lib/srfi/276.inverse-trigpi.scm")
   ;;; Hyperbolic functions
   (export flsinh flcosh fltanh
           flasinh flacosh flatanh)
   ;;; Special functions
   (export flerf flerfc flgamma flloggamma
           flfirst-bessel flsecond-bessel)
   (begin
     (define flerf
       (c-lambda (double) double "erf"))
     (define flerfc
       (c-lambda (double) double "erfc"))
     (define flgamma
       (c-lambda (double) double "tgamma")))
   ;; Use Will Clinger's flloggamma and bessel functions
   (begin
     (define c-functions-are-available
       ((c-lambda () bool "\
#if _SVID_SOURCE || _BSD_SOURCE || _XOPEN_SOURCE
  ___return(1);
#else
  ___return(0);
#endif")))
     (define jn
       (if c-functions-are-available
           (c-lambda (int double) double "jn")
           (lambda args (error "unavailable" args))))
     (define yn
       (if c-functions-are-available
           (c-lambda (int double) double "yn")
           (lambda args (error "unavailable" args)))))
   (include "144.special.scm")
   ;;; Serialization
   (export bytevector-flonum-ref
           bytevector-flonum-native-ref
           bytevector-flonum-set!
           bytevector-flonum-native-set!
           (rename string->number string->flonum))
   (begin
     (define %bytevector-flonum-native-ref
       (c-lambda (scheme-object int) double
              "\
double flonum;
double *p = (double*)
            ((unsigned char *)___BODY_AS(___arg1, ___tU8VECTOR)
             + ___arg2);
flonum = p[0];
___return(flonum);"))
     (define (bytevector-flonum-native-ref bv k)
       (let ((len (bytevector-length bv)))
         (unless (<= 0 k (- len fl-byte-width))
           (error "invalid index" bv k))
         (unless (zero? (modulo k fl-byte-width))
           (error "unaligned access" bv k))
         (%bytevector-flonum-native-ref bv k)))
     (define %bytevector-flonum-native-endian-ref
       (c-lambda (scheme-object int) double
         "\
double flonum;
memcpy(&flonum,
       (unsigned char *)___BODY_AS(___arg1, ___tU8VECTOR)
       + ___arg2,
       sizeof(flonum));
___return(flonum);"))
     (define %bytevector-flonum-byteswapped-ref
       (c-lambda (scheme-object int) double
         "\
double flonum;
unsigned char *tmp = (unsigned char *)&flonum;
unsigned char swap;
int i;
memcpy(tmp,
       (unsigned char *)___BODY_AS(___arg1, ___tU8VECTOR)
       + ___arg2,
       sizeof(flonum));
for (i = 0; i < sizeof(flonum)/2; i++) {
  swap = tmp[i];
  tmp[i] = tmp[sizeof(flonum)-i-1];
  tmp[sizeof(flonum)-i-1] = swap;
}
___return(flonum);"))
     (define %native-endianness
       (let ((id
              ((c-lambda () int
                 "\
#if defined(___BIG_ENDIAN)
  ___return(2);
#elif defined(___LITTLE_ENDIAN)
  ___return(1);
#else
  #error \"I don't know the native endianness\"
#endif"))))
         (case id
           ((1) 'little)
           ((2) 'big))))
     (define %bytevector-flonum-little-endian-ref
       (case %native-endianness
         ((little) %bytevector-flonum-native-endian-ref)
         ((big) %bytevector-flonum-byteswapped-ref)))
     (define %bytevector-flonum-big-endian-ref
       (case %native-endianness
         ((big) %bytevector-flonum-native-endian-ref)
         ((little) %bytevector-flonum-byteswapped-ref)))
     (define (bytevector-flonum-ref bv k endianness)
       (let ((len (bytevector-length bv)))
         (unless (<= 0 k (- len fl-byte-width))
           (error "invalid index" bv k))
         (case endianness
           ((little)
            (%bytevector-flonum-little-endian-ref bv k))
           ((big)
            (%bytevector-flonum-big-endian-ref bv k))
           (else (error "unknown endianness" endianness)))))
     (define %bytevector-flonum-native-set!
       (c-lambda (scheme-object int double) void
         "\
*(double *)((unsigned char *)___BODY_AS(___arg1, ___tU8VECTOR)
            + ___arg2)
   = ___arg3;
___return;"))
     (define (bytevector-flonum-native-set! bv k fl)
       (unless (<= 0 k (- (bytevector-length bv) fl-byte-width))
         (error "invalid index" bv k))
       (unless (zero? (modulo k fl-byte-width))
         (error "unaligned access" k))
       (%bytevector-flonum-native-set! bv k fl))
     (define %bytevector-flonum-native-endian-set!
       (c-lambda (scheme-object int double) void
         "\
memcpy((unsigned char *)___BODY_AS(___arg1, ___tU8VECTOR)
       + ___arg2,
       &___arg3,
       sizeof(double));
___return;"))
     (define %bytevector-flonum-byteswapped-set!
       (c-lambda (scheme-object int double) void
         "\
unsigned char *p = (unsigned char *)
                   ___BODY_AS(___arg1, ___tU8VECTOR)
                   + ___arg2;
int i;
unsigned char tmp;
memcpy(p, &___arg3, sizeof(double));
for (i = 0; i < sizeof(double)/2; i++) {
  tmp = p[i];
  p[i] = p[sizeof(double)-i-1];
  p[sizeof(double)-i-1] = tmp;
}
___return;"))
     (define %bytevector-flonum-little-endian-set!
       (case %native-endianness
         ((little) %bytevector-flonum-native-endian-set!)
         ((big) %bytevector-flonum-byteswapped-set!)))
     (define %bytevector-flonum-big-endian-set!
       (case %native-endianness
         ((big) %bytevector-flonum-native-endian-set!)
         ((little) %bytevector-flonum-byteswapped-set!)))
     (define (bytevector-flonum-set! bv k fl endianness)
       (unless (<= 0 k (- (bytevector-length bv) fl-byte-width))
         (error "invalid index" bv k))
       (case endianness
         ((little)
          (%bytevector-flonum-little-endian-set! bv k fl))
         ((big)
          (%bytevector-flonum-big-endian-set! bv k fl))
         (else (error "I don't know the native endianness")))))
)
