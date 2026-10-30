; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
;; Enable exception handling
(flo:clear-exceptions! (flo:supported-exceptions))
(load "../mit-scheme-lib/srfi/276/exception.sld")
(load "../mit-scheme-lib/srfi/276/rounding-mode.sld")
(load "../mit-scheme-lib/srfi/276/with-rounding-mode.sld")
(load "../mit-scheme-lib/srfi/276/set-rounding-mode.sld")
,(import (srfi 276 exception))
,(import (srfi 276 rounding-mode))
,(import (srfi 276 with-rounding-mode))
,(import (srfi 276 set-rounding-mode))
(load "../lib/srfi/276.sld")
,(import (srfi 27) (srfi 276) (srfi 158))

(load "tests.mit-scheme.scm")
(define skipping? #f)
(test-verbose? #f)
(load "run.body.scm")

(display "Total tests passed: ")
(display tests-passed)
(newline)

(display "Total tests failed: ")
(display tests-failed)
(newline)

(display "Total tests: ")
(display tests-run)
(newline)

#;(exit (if (zero? tests-failed)
        0
        1))
