; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (test-set-rounding-mode)
  (test-group "round-towards-zero"
    (set-rounding-mode! 'round-towards-zero)
    (test-assert (fl=? (fl* fl-greatest 2.0) fl-greatest))
    (test-assert (fl=? (fl* fl-greatest -2.0)
                       (fl- fl-greatest))))
  (test-group "round-towards-positive"
    (set-rounding-mode! 'round-towards-positive)
    (test-assert (fl=? (fl* fl-greatest 2.0) +inf.0))
    (test-assert (fl=? (fl* fl-greatest -2.0)
                       (fl- fl-greatest))))
  (test-group "round-towards-negative"
    (set-rounding-mode! 'round-towards-negative)
    (test-assert (fl=? (fl* fl-greatest 2.0) fl-greatest))
    (test-assert (fl=? (fl* fl-greatest -2.0) -inf.0))))
