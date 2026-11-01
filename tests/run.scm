; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(import (scheme base) (srfi 27) (srfi 276) (srfi 64) (srfi 194) (srfi 252) (srfi 158))

(test-begin "SRFI 276")
(define skipping? #f)
(cond-expand
  ((not gambit)
   (test-skip (lambda ignored skipping?)))
  (else))
(include "tests.srfi-64.scm")
(include "run.body.scm")
(test-end "SRFI 276")
