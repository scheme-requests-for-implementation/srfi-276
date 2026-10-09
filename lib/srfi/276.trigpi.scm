#| The kernel for sin and cos are calculated using a minimax approximation.

   The calculations were done with Sollya:

     https://sollya.org/

   Many libms use minimax polynomials to calculate sin, cos, etc. on small
   intervals after range reduction.
   However, sinpi is not in things like musl libm. So I have to figure out
   the constants myself.

   Various argument-folding tricks from Musl, from FreeBSD, who probably
   stole it from someone eles.

   FIXME: Binary64 only!
   FIXME: I have no idea how any of this works, Sollya is a magic black box
          that spits out polynomial coeffients when I summon the spirits of
          my computer with the appropriate spells
 |#

(define-syntax even-polynomial
  ;; Note: this assumes some basic constant propagation and inlining ability
  ;; to be useful.
  (syntax-rules ()
    ((_ "loop" fma x^2 C) C)
    ((_ "loop" fma x^2 C1 C2 ...)
     (fma x^2 (even-polynomial "loop" fma x^2 C2 ...) C1))
    ((_ x^2 C ...)
     (let ((fma (if fl-fast-fl+*
		    fl+*
		    (lambda (x y z)
		      (fl+ (fl* x y) z)))))
       (even-polynomial "loop" fma x^2 C ...)))))

(define (%sinpi fl)
  #|
fl in [0, 0.25].

Sollya 8.0 script:

     f = sin(pi*x);
     I = [0; 0.25];

     // For math reasons, f/x is the thing to be minimized here.
     p = fpminimax(f/x, [|0, 2, 4, 6, 8, 10, 12|], [|double...|], I, relative);

     p = x*p;
     print("Relative error: ", dirtyinfnorm(1 - p/f, I));
     print("Absolute error: ", dirtyinfnorm(p - f, I));
     print("Polynomial:");
     print(p);

sollya output:

Display mode is hexadecimal numbers.
Relative error:  0x1.678afae35cdd0ec2b3256348ef8afbc99d98a2976p-55
Absolute error:  0x1.3ebb2d89a070ba48c600c4009120b3e3774d2ecf4p-59
Polynomial:
x * (0x1.921fb54442d18p1
     + x^0x1p1 * (-0x1.4abbce625be09p2
     + x^0x1p1 * (0x1.466bc67754fffp1
     + x^0x1p1 * (-0x1.32d2ccdfe9424p-1
     + x^0x1p1 * (0x1.50782d5f14825p-4
     + x^0x1p1 * (-0x1.e2fe76fdffd2bp-8
     + x^0x1p1 * 0x1.e357ef99eb0bbp-12))))))

Since standard Scheme does not have standard hex floats, the numbers
here are the (extremely) precise outputs from Sollya in decimal mode.
|#
  (let ((fl^2 (flsquare fl)))
    (fl* fl
	 (even-polynomial
	  fl^2
	  3.141592653589793115997963468544185161590576171875
	  -5.1677127800499045306992229598108679056167602539062
	  2.5501640398670128995206596300704404711723327636719
	  -0.59926452859202017364737002935726195573806762695312
	  8.2145859939629931045779187570587964728474617004395e-2
	  -7.3699036129249257884299417753481975523754954338074e-3
	  4.609522817371582213523406590383046932402066886425e-4))))

(define (%cospi fl)
  #|
fl in [0, 0.25].

Sollya 8.0 script:

     // display=hexadecimal;
     f = cos(pi*x);
     I = [0; 0.25];

     p = fpminimax(f, [|0, 2, 4, 6, 8, 10, 12|], [|D...|], I, relative);

     print("Relative error: ", dirtyinfnorm(1 - p/f, I));
     print("Absolute error: ", dirtyinfnorm(p - f, I));
     print("Polynomial:");
     print(p);

Sollya output:

     Display mode is hexadecimal numbers.
     Relative error:  0x1p-53
     Absolute error:  0x1p-53
     Polynomial:
     0x1.fffffffffffffp-1
     + x^0x1p1 * (-0x1.3bd3cc9be4565p2
     + x^0x1p1 * (0x1.03c1f081af262p2
     + x^0x1p1 * (-0x1.55d3c7dac6352p0
     + x^0x1p1 * (0x1.e1f4fa4f3bb22p-3
     + x^0x1p1 * (-0x1.a6c9501cb8cdap-6
     + x^0x1p1 * 0x1.f3b7b6aafa2abp-10)))))

The minimax algorithm sets C0=0.999... instead of 1.0 to make other parts
of the polynomial have smaller relative. This means that
to preserve correct behavior at +-0.0, that point must be special-cased.
|#

  (if (flzero? fl)
      1.0
      (let ((fl^2 (flsquare fl)))
	(even-polynomial
	 fl^2
	 0.9999999999999999
	 -4.9348022005445715265636863477993756532669067382812
	 4.0587121263930345804737953585572540760040283203125
	 -1.33526276675384680814318016928154975175857543945312
	 0.235330539267544713855073723607347346842288970947266
	 -2.5804832682195001647418308721171342767775058746338e-2
	 1.9062714807153529044531081737545719079207628965378e-3))))

(define (fold-arguments fl)
  ;; First thing to do: map numbers to [-1, 1].
  ;; Ranges are [1, 3]   -> [-1, 1]
  ;;            [3, 5]   -> [-1, 1]
  ;;            [-3, -1] -> [-1, 1]
  ;;            [-5, -3] -> [-1, 1]
  (let*-values (((ipart fpart) (flinteger-fraction fl)))
    (cond
     ((and (fl>=? ipart 1.0)
	   (flodd? ipart))
      (fl- fpart 1.0))
     ((and (fl>=? ipart 1.0)
	   (fleven? ipart))
      fpart)
     ((and (fl<=? ipart -1.0)
	   (flodd? ipart)
	   (flzero? fpart))
      -1.0)
     ((and (fl<=? ipart -1.0)
	   (flodd? ipart))
      (fl+ 1.0 fpart))
     ((and (fl<=? ipart -1.0)
	   (fleven? ipart))
      fpart)
     (else fpart))))

(define (flsinpi fl)
  (if (not (flfinite? fl))
      +nan.0
      (let-values (((ipart fpart) (flinteger-fraction fl)))
	(cond
	  ((and (flzero? fpart)
		(flsign-negative? ipart))
	   -0.0)
	  ((and (flzero? fpart)
		(flsign-positive? ipart))
	   0.0)
	  (else
	   (let* ((arg (fold-arguments fl))
		  (a (flabs arg))
		  (value
		   (cond
		    ((fl<=? 0.0 a 0.25) (%sinpi a))
		    ((fl<=? 0.25 a 0.50) (%cospi (fl- 0.50 a)))
		    ((fl<=? 0.50 a 0.75) (%cospi (fl- a 0.50)))
		    (else (%sinpi (fl- 1.00 a))))))
	     ;; Further reductions from [-1, 1] to [0, 0.25]:
	     (flcopysign value arg)))))))

(define (flcospi fl)
  (if (not (flfinite? fl))
      +nan.0
      (let ((a (flabs (fold-arguments fl))))
	(cond
	  ((fl<=? 0.0 a 0.25) (%cospi a))
	  ((fl<=? 0.25 a 0.50) (%sinpi (fl- 0.50 a)))
	  ((fl<=? 0.50 a 0.75)
	   (flcopysign (%sinpi (fl- a 0.50)) -1.0))
	  (else (flcopysign (%cospi (fl- 1.00 a)) -1.0))))))

(define (%tanpi fl)
  #|

tanpi on [0,0.25]

Sollya script:

     display=hexadecimal;
     f = tan(pi*x);
     I = [0; 0.25];

     // For math reasons, f/x is the thing to be minimized here.
     p = fpminimax(f/x, [|0, 2, 4, 6, 8, 10, 12,
                          14, 16, 18, 20, 22, 24, 26, 28|],
                        [|double...|], I, relative);

     p = x*p;
     print("Relative error: ", dirtyinfnorm(1 - p/f, I));
     print("Absolute error: ", dirtyinfnorm(p - f, I));
     print("Polynomial:");
     print(p);

Sollya output:

     Relative error:  0x1.678afae35cdd0ec2b3256348ef8afbc99d98a2976p-55
     Absolute error:  0x1.e990fp-57
     x * (0x1.921fb54442d18p1
          + x^0x1p1 * (0x1.4abbce625be8bp3
          + x^0x1p1 * (0x1.466bc6775cf74p5
          + x^0x1p1 * (0x1.45fff9b24305p7
          + x^0x1p1 * (0x1.45f4739a998c7p9
          + x^0x1p1 * (0x1.45f311045a4fep11
          + x^0x1p1 * (0x1.45f61b419c799p13
          + x^0x1p1 * (0x1.45be1b46fe644p15
          + x^0x1p1 * (0x1.486b2d6bc0c9cp17
          + x^0x1p1 * (0x1.31291bbc2215bp19
          + x^0x1p1 * (0x1.c1c11032726e5p21
          + x^0x1p1 * (-0x1.79e8e2fe6305p22
          + x^0x1p1 * (0x1.ba2bcad85f927p27
          + x^0x1p1 * (-0x1.11a76ad4cfa19p30
          + x^0x1p1 * 0x1.3fad0a79eac2ep32))))))))))))))
|#
  (if (flzero? fl)
      fl
      (let ((fl^2 (flsquare fl)))
	(fl* fl
	     (even-polynomial
	      fl^2
	      3.141592653589793115997963468544185161590576171875
	      10.3354255601000399877875679521821439266204833984375
	      40.802624638104049381581717170774936676025390625
	      162.99995190685376655892468988895416259765625
	      651.9097779512165971027570776641368865966796875
	      2607.5958272708803633577190339565277099609375
	      10430.763308737239640322513878345489501953125
	      41695.05327601407770998775959014892578125
	      1.68150354850862990133464336395263671875e5
	      6.24968866715471609495580196380615234375e5
	      3.6843860246323221363127231597900390625e6
	      -6.19167274842460453510284423828125e6
	      2.318249827616665065288543701171875e8
	      -1.1477879572027647495269775390625e9
	      5.3632723139170360565185546875e9)))))

(define (fltanpi fl)
  (if (not (flfinite? fl))
      +nan.0
      ;; Fold arguments into [-0.5, 0.5].
      ;; [0.5, 1.5] -> [-0.5, 0.5]
      ;; [-1.5, -0.5] -> [-0.5, 0.5]
      (let-values (((ipart fpart) (flinteger-fraction fl)))
	(cond
	  ((and (flpositive? ipart)
		(fleven? ipart)
		(flzero? fpart))
	   +0.0)
	  ((and (flnegative? ipart)
		(flodd? ipart)
		(flzero? fpart))
	   +0.0)
	  ((and (flpositive? ipart)
		(flodd? ipart)
		(flzero? fpart))
	   -0.0)
	  ((and (flnegative? ipart)
		(fleven? ipart)
		(flzero? fpart))
	   -0.0)
	  ((and (flpositive? ipart)
		(fleven? ipart)
		(fl=? fpart 0.5))
	   +inf.0)
	  ((and (flpositive? ipart)
		(flodd? ipart)
		(fl=? fpart 0.5))
	   -inf.0)
	  ((and (flnegative? ipart)
		(fleven? ipart)
		(fl=? fpart -0.5))
	   -inf.0)
	  ((and (flnegative? ipart)
		(flodd? ipart)
		(fl=? fpart -0.5))
	   +inf.0)
	  (else
	   (let* ((arg (cond
			((fl>? fpart 0.5)
			 (fl- fpart 1.0))
			((fl<? fpart -0.5)
			 (fl+ fpart 1.0))
			(else fpart)))
		  (a (flabs arg))
		  (value
		   (cond
		    ((fl<=? 0.0 a 0.25) (%tanpi a))
		    (else (fl/ (%tanpi (fl- 0.50 a)))))))
	     (flcopysign value arg)))))))
