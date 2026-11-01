; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
;;;; FIXME: Needs a lot more tests.

(define (test-with-rounding-mode)
  (test-group "round-towards-zero"
    (with-rounding-mode 'round-towards-zero
      (test-assert (fl=? (fl* fl-greatest 2.0) fl-greatest))
      (test-assert (fl=? (fl* fl-greatest -2.0)
                         (fl- fl-greatest)))))
  (test-group "round-towards-positive"
    (with-rounding-mode 'round-towards-positive
      (test-assert (fl=? (fl* fl-greatest 2.0) +inf.0))
      (test-assert (fl=? (fl* fl-greatest -2.0)
                         (fl- fl-greatest)))))
  (test-group "round-towards-negative"
    (with-rounding-mode 'round-towards-negative
      (test-assert (fl=? (fl* fl-greatest 2.0) fl-greatest))
      (test-assert (fl=? (fl* fl-greatest -2.0) -inf.0)))))
