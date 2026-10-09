 ;;;; See 276.trigpi.scm for comments on where constants come from.

(define (%asinpi fl)
#|
Sollya 8.0 script:

     display=hexadecimal;
     f = asin(x)/pi;
     I = [0; 0.5];

     // For math reasons, f/x is the thing to be minimized here.
     p = fpminimax(f/x, [|0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24|], [|double...|], I, relative);

     p = x*p;
     print("Relative error: ", dirtyinfnorm(1 - p/f, I));
     print("Absolute error: ", dirtyinfnorm(p - f, I));
     print("Polynomial:");
     print(p);

Output:

     Relative error:  0x1.1d1afdb068d78a8230d62310e78fc8b488b40de9ep-54
     Absolute error:  0x1.2fab45d5555555555555555555555555555555555p-59
     Polynomial:
     x * (0x1.45f306dc9c883p-2
     + x^0x1p1 * (0x1.b2995e7b7af0fp-5
     + x^0x1p1 * (0x1.8723a1d61d2e9p-6
     + x^0x1p1 * (0x1.d1a4529a30a69p-7
     + x^0x1p1 * (0x1.3ce53861f8f21p-7
     + x^0x1p1 * (0x1.d2b076c914dfep-8
     + x^0x1p1 * (0x1.6a2b36f9b072cp-8
     + x^0x1p1 * (0x1.21604ae2714bbp-8
     + x^0x1p1 * (0x1.ff0549b66d5cp-9
     + x^0x1p1 * (0x1.035d342db8838p-9
     + x^0x1p1 * (0x1.a7b91f7df2238p-8
     + x^0x1p1 * (-0x1.6a3fb0872a22ap-8
     + x^0x1p1 * 0x1.547a51dc1fd33p-7))))))))))))
|#
  (let ((fl^2 (flsquare fl)))
    (fl* fl
	 (even-polynomial
	  fl^2
	  0.31830988618379069121644420192751567810773849487305
	  5.3051647697286090366031174880845355801284313201904e-2
	  2.387324146589523202188765083064936334267258644104e-2
	  1.42102626163069390569093641829567786771804094314575e-2
	  9.6708798700500548523040222903546236921101808547974e-3
	  7.1211137775040161262962712385160557460039854049683e-3
	  5.5262574402961499309139270508239860646426677703857e-3
	  4.41552952037687750780348139301167975645512342453e-3
	  3.8987781983574609778742114940541796386241912841797e-3
	  1.9787908739639335775617468016207567416131496429443e-3
	  6.4655019650179876289364244712487561628222465515137e-3
	  -5.5274778343706119226874662331283616367727518081665e-3
	  1.03905582147932658981792641839092539157718420028687e-2))))

(define (flasinpi fl)
  (let ((a (flabs fl)))
    (cond
     ((flnan? fl) fl)
     ((not (flfinite? a)) +nan.0)
     ((flzero? fl) fl)
     ((fl>? a 1.0) +nan.0)
     ((fl<? a 0.5)
      (flcopysign (%asinpi a) fl))
     (else
      ;; |a| in [0.5,1.0]
      ;; In this case,
      ;; asinpi(a) = 1/2 - 2*asinpi(sqrt((1-x)/2))
      ;; FIXME: how imprecise is this?
      (let ((arg (flsqrt (fl/ (fl- 1.0 a) 2.0))))
	(flcopysign (fl- 0.5 (fl* 2.0 (%asinpi arg)))
		    fl))))))

(define (flacospi fl)
  (let ((a (flabs fl)))
    (cond
      ((flnan? fl) fl)
      ((not (flfinite? a)) +nan.0)
      ((fl>? a 1.0) +nan.0)
      ((fl<? a 0.5)
       ;; FIXME: This might cause precision loss.
       ;; Maybe instead just use Sollya to generate a polynomial?
       ;; But then I don't know of a good identity to fold the cases.
       (fl- 0.5 (%asinpi fl)))
      (else
       ;; acospi(x) = 0.5 - asinpi(x)
       ;;           = 0.5 - 0.5 + 2.0*asinpi(arg)
       ;;           = 2.0*asin(arg)
       (let ((arg (flsqrt (fl/ (fl- 1.0 a) 2.0))))
	 (fl* 2.0 (%asinpi arg)))))))

(define (%atanpi fl)
#|
Sollya 8.0 script:

     display=hexadecimal;
     f = atan(x)/pi;
     I = [0; 7/16];

     // For math reasons, f/x is the thing to be minimized here.
     p = fpminimax(f/x, [|0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20|], [|double...|], I, relative);

     p = x*p;
     print("Relative error: ", dirtyinfnorm(1 - p/f, I));
     print("Absolute error: ", dirtyinfnorm(p - f, I));
     print("Polynomial:");
     print(p);

Output:

     Display mode is hexadecimal numbers.
      Relative error:  0x1.0392366c0e65bf287121b38e8df055480daa951c2p-53
     Absolute error:  0x1.e42c0a8e085c1bfa8fcba7c3d7c73916733aea848p-57
     Polynomial:
     x * (0x1.45f306dc9c882p-2
+ x^0x1p1 * (-0x1.b2995e7b7aa1ap-4
+ x^0x1p1 * (0x1.04c26be3159d2p-4
+ x^0x1p1 * (-0x1.7483752b39794p-5
+ x^0x1p1 * (0x1.21bb83edab897p-5
+ x^0x1p1 * (-0x1.da187f0e081a6p-6
+ x^0x1p1 * (0x1.90f98d931b837p-6
+ x^0x1p1 * (-0x1.59b82d43f56fbp-6
+ x^0x1p1 * (0x1.25c7c4b2168a6p-6
+ x^0x1p1 * (-0x1.b5c60eb18ddc7p-7
+ x^0x1p1 * 0x1.945a0c31a0e57p-8))))))))))
|#
  (if (flzero? fl)
      fl
      (let ((fl^2 (flsquare fl)))
	(fl* fl
	     (even-polynomial
	      fl^2
	      0.31830988618379063570529297066968865692615509033203
	      -0.106103295394554569819334233216068241745233535766602
	      6.366197722795810531870586146396817639470100402832e-2
	      -4.5472840161706745698566578539612237364053726196289e-2
	      3.536773459687585913213681010347499977797269821167e-2
	      -2.8936504437412267909390806153169251047074794769287e-2
	      2.4473560577853183811702919570052472408860921859741e-2
	      -2.11010400815529845786056029055544058792293071746826e-2
	      1.7930929265596383392900037279105163179337978363037e-2
	      -1.3359791922183185050587717057624104199931025505066e-2
	      6.1699180383843676631072661109556065639480948448181e-3)))))

(define (flatanpi-single-argument fl)
  (cond
   ((flnan? fl) fl)
   ((fl=? fl +inf.0) 0.50)
   ((fl=? fl -inf.0) -0.50)
   ((flzero? fl) fl)
   (else
    ;; The following argument reductions are from the musl libm.
    (let* ((a (flabs fl))
	   (atanpi-0.5 0.14758361765043327417540107622474052595113452388692)
	   (atanpi-1.0 0.25)
	   (atanpi-1.5 0.31283295818900118381374725243522144766524242491163))
      (cond
        ((fl<=? a (fl/ 7.0 16.0))
	 (flcopysign (%atanpi a)
		     fl))
	((fl<=? a (fl/ 11.0 16.0))
	 (let ((v
		(fl+ atanpi-0.5
		     (%atanpi (fl/ (fl- a 0.5)
				   (fl+ 1.0 (fl/ a 2.0)))))))
	   (flcopysign v fl)))
	((fl<=? a (fl/ 19.0 16.0))
	 (let ((v
		(fl+ atanpi-1.0
		     (%atanpi (fl/ (fl- a 1.0)
				   (fl+ a 1.0))))))
	   (flcopysign v fl)))
	((fl<=? a (fl/ 39.0 16.0))
	 (let ((v
		(fl+ atanpi-1.5
		     (%atanpi (fl/ (fl- a 1.5)
				   (fl+ 1.0 (fl* 1.5 a)))))))
	   (flcopysign v fl)))
	(else
	 (let ((v
		(fl+ 0.5 (%atanpi (fl- (fl/ a))))))
	   (flcopysign v fl))))))))

(define flatanpi
  (lambda (y . maybe-other)
    (cond
      ((null? maybe-other)
       (flatanpi-single-argument y))
      ((null? (cdr maybe-other))
       (let ((x (car maybe-other)))
	 (cond
	   ((and (flzero? y)
		 (flpositive? x))
	    y)
	   ((and (flfinite? y)
		 (not (flzero? y))
		 (eqv? x -inf.0))
	    (flcopysign 1.0 y))
	   ((and (flfinite? y)
		 (not (flzero? y))
		 (eqv? x +inf.0))
	    (flcopysign 0.0 y))
	   ((and (flinfinite? y)
		 (flfinite? x))
	    (flcopysign 0.5 y))
	   ((and (flinfinite? y)
		 (eqv? x -inf.0))
	    (flcopysign 0.75 y))
	   ((and (flinfinite? y)
		 (eqv? x +inf.0))
	    (flcopysign 0.25 y))
	   ((and (flpositive? x)
		 (flpositive? y))
	    (flatanpi-single-argument (fl/ y x)))
	   ((and (flpositive? y)
		 (flzero? x))
	    0.5)
	   ((and (flpositive? y)
		 (flnegative? x))
	    (fl+ (flatanpi-single-argument (fl/ y (flabs x)))
		 0.5))
	   ((and (flzero? y)
		 (flnegative? x))
	    (flcopysign 1.0 y))
	   ((and (flnegative? y)
		 (flnegative? x))
	    (fl+ (fl- (flatanpi-single-argument (fl/ y x)))
		 -0.5))
	   ((and (flnegative? y)
		 (flzero? x))
	    -0.5)
	   ((and (flnegative? y)
		 (flpositive? x))
	    (flatanpi-single-argument (fl/ y x)))
	   ((and (eqv? y +0.0)
		 (eqv? x +0.0))
	    +0.0)
	   ((and (eqv? y -0.0)
		 (eqv? x +0.0))
	    -0.0)
	   ((and (eqv? y +0.0)
		 (eqv? x -0.0))
	    1.0)
	   ((and (eqv? y -0.0)
		 (eqv? x -0.0))
	    -1.0)
	   (else (error "internal error: case should have been handled"
			y x)))))
      (else (error "invalid number of arguments" (cons y maybe-other))))))
