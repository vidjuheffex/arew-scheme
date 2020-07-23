;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: (MIT OR BSD-3-Clause OR LicenseRef-LICENSE)
(library (srfi srfi-14 inversion-list)
  (export
   inversion-list?
   make-empty-inversion-list
   number->inversion-list
   numbers->inversion-list
   range->inversion-list
   ranges->inversion-list

   inversion-list=?
   inversion-list-adjoin
   inversion-list-complement
   inversion-list-copy
   inversion-list-difference
   inversion-list-fold/done?
   inversion-list-hash
   inversion-list-intersection
   inversion-list-member?
   inversion-list-remove
   inversion-list-size
   inversion-list-union

   inversion-list-cursor
   inversion-list-cursor?
   inversion-list-cursor-ref
   inversion-list-cursor-next
   inversion-list-cursor-at-end?)
  (import (scheme base)
          (only (chezscheme) bitwise-and)
          (srfi private include)
          (only (chezscheme) assertion-violation))

  (include/resolve ("srfi" "srfi-14") "inversion-list.body.scm"))
