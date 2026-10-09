(define-library (srfi 276)
  (import (scheme base) (scheme inexact))
  (import (rename (except (srfi 144)
                          fl-integer-exponent-zero
                          fl-integer-exponent-nan
                          flmax flmin)
                  (fl+ srfi-144:fl+)
                  (fl- srfi-144:fl-)
                  (fl/ srfi-144:fl/)
                  (fl* srfi-144:fl*)
                  (fllog1+ fllog+1)
                  (flonum srfi-144:flonum)
                  (flsign-bit srfi-144:flsign-bit)
                  (flinteger-exponent srfi-144:flinteger-exponent)
                  (flquotient fltruncate-quotient)
                  (flremainder fltruncate-remainder)
                  (flnormalized? flnormal?)
                  (fldenormalized? flsubnormal?)))
  (export fl-radix fl-precision fl-maximum-exponent
          fl-minimum-exponent fl-minimum-normalized-exponent
          fl-greatest fl-least fl-least-normal fl-epsilon fl-byte-width)
  (export fl-e fl-1/e fl-e^2 fl-e^pi/4
          fl-log2-e fl-log10-e
          fl-log-2
          fl-log-3 fl-log-pi
          fl-log-10
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
          fl-sin-1 fl-cos-1
          fl-fast-fl+*)
  (export flonum flonum? fladjacent flcopysign make-flonum)
  (export flinteger-fraction flexponent flinteger-exponent
          fl-integer-exponent-zero fl-integer-exponent-nan
          flnormalized-fraction-exponent
          flsign-negative? flsign-positive?)
  (export fl=?
           fl<?
           fl>?
           fl<=?
           fl>=?
           fl!=?
           fltotal=? fltotal<? fltotal>? fltotal<=? fltotal>=?
           flunordered? flordered?
           flinteger?
           flzero? flpositive? flnegative?
           flodd? fleven?
           flfinite? flinfinite?
           flnan?
           flnormal? flsubnormal?)
  (export flmax flmin
           flmax-filter-nans flmin-filter-nans
           flmax-abs flmin-abs
           flmax-abs-filter-nans flmin-abs-filter-nans
           fl+ fl- fl* fl/ fl+* flabs
           flabsdiff flposdiff flsgn
           flnumerator fldenominator)
   (export flfloor flceiling
           fltruncate
           flround flround-away)
   (export flquotient flremainder
           fltruncate-quotient
           fltruncate-remainder
           flremquo
           flround-quotient
           flround-remainder)
   (export flexp flexp2 flexp10
           flexp-1
           flexp2-1
           flexp10-1
           flexpt)
   (export fllog fllog2 fllog10
           fllog+1
           fllog2+1
           fllog10+1
           make-fllog-base)
   (export flcbrt flcompound flhypot flrsqrt flsqrt)
   (export flsin flcos fltan
           flsinpi flcospi fltanpi)
   (export flasin flacos flatan
           flasinpi flacospi flatanpi)
   (export flsinh flcosh fltanh
           flasinh flacosh flatanh)
   (export flerf flerfc flgamma flloggamma
           flfirst-bessel flsecond-bessel)
   (export bytevector-flonum-ref
           bytevector-flonum-native-ref
           bytevector-flonum-set!
           bytevector-flonum-native-set!
           (rename string->number string->flonum))
  (import (srfi 143))
  (begin
    (define large-positive-integer fx-greatest)
    (define large-negative-integer fx-least))
  (cond-expand
    ((library (srfi 1))
     (import (only (srfi 1) fold)))
    (else
     (begin
       (define (fold f knil lst)
         (if (null? lst)
             knil
             (fold f (f (car lst) knil) (cdr lst)))))))
(cond-expand
  ((library (srfi 160 u8))
   (import (only (srfi 160 u8) u8vector-reverse-copy!)))
  (else
   (begin
     (define (u8vector-reverse-copy! to at from)
       (do ((i (- (bytevector-length from) 1)
            (- i 1))
            (at at (+ at 1)))
           ((negative? i))
         (bytevector-u8-set! to at (bytevector-u8-ref from i)))))))
  (cond-expand
    ((library (srfi 281))
     (import (only (srfi 281) bytevector-fill!)))
    (else
     (begin
       (define (bytevector-fill! bv u8 start end)
         (do ((i start (+ i 1)))
             ((= i end))
           (bytevector-u8-set! bv i u8))))))
  (include "276.constants.scm")
  (include "276.flonum.scm")
  (include "276.arith.scm")
  (include "276.utils.scm")
  (include "276.sign-negative.scm")
  (include "276.integer-exponent.scm")
  (include "276.new-constants.scm")
  (include "276.serialization.scm")
  (include "276.flonum.scm")
  (include "276.fl-not-equal.scm")
  (include "276.total.scm")
  (include "276.flordered.scm")
  (include "276.fl-not-equal.scm")
  (include "276.maxmin.scm")
  (include "276.round-away.scm")
  (include "276.exp.scm")
  (include "276.fllog.scm")
  (include "276.cbrt.scm")
  (include "276.compound.scm")
  (include "276.rsqrt.scm")
  (include "276.trigpi.scm")
  (include "276.inverse-trigpi.scm")
)
