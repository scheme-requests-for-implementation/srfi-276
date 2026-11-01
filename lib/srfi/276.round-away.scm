; SPDX-FileCopyrightText: 2026 Peter McGoron
; SPDX-License-Identifier: MIT
(define (flround-away fl)
  (fltruncate (fl+ fl (flcopysign 0.5 fl))))