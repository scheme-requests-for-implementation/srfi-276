; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-flcopysign)
  (test-group "flcopysign"
    (test-values (flcopysign (flonum 0.0) (flonum 1.0))
                 (list (flonum 0.0)))
    (test-values (flcopysign (flonum 0.0) (flonum -1.0))
		 (list (flonum -0.0)))
    (test-values (flcopysign (flonum -0.0) (flonum 1.0))
		 (list (flonum 0.0)))
    (test-values (flcopysign (flonum -0.0) (flonum -1.0))
		 (list (flonum -0.0)))

    (test-values (flcopysign (flonum 1.0) (flonum 2.0))
		 (list (flonum 1.0)))
    (test-values (flcopysign (flonum 1.0) (flonum -2.0))
		 (list (flonum -1.0)))
    (test-values (flcopysign (flonum -1.0) (flonum 2.0))
		 (list (flonum 1.0)))
    (test-values (flcopysign (flonum -1.0) (flonum -2.0))
		 (list (flonum -1.0)))

    (test-values (flcopysign (flonum +inf.0) (flonum +1.0))
		 (list (flonum +inf.0)))
    (test-values (flcopysign (flonum +inf.0) (flonum -1.0))
		 (list (flonum -inf.0)))
    (test-values (flcopysign (flonum -inf.0) (flonum +1.0))
		 (list (flonum +inf.0)))
    (test-values (flcopysign (flonum -inf.0) (flonum -1.0))
		 (list (flonum -inf.0)))

    (test-values (flabs (flcopysign (flonum 1.0) (flonum +nan.0)))
		 (list (flonum 1.0)))))


