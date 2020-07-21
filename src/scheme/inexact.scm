;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: CC0-1.0
(library (scheme inexact)
  (export
   acos asin atan cos exp finite? infinite? log nan? sin sqrt tan)
  (import
   (except (chezscheme) finite? infinite? nan?)
   (prefix (chezscheme) cs:))

  (define (finite? z)
    (if (complex? z)
        (and (cs:finite? (real-part z))
             (cs:finite? (imag-part z)))
        (cs:finite? z)))

  (define (infinite? z)
    (if (complex? z)
        (or (cs:infinite? (real-part z))
            (cs:infinite? (imag-part z)))
        (cs:infinite? z)))

  (define (nan? z)
    (if (complex? z)
        (or (cs:nan? (real-part z))
            (cs:nan? (imag-part z)))
        (cs:nan? z))))
