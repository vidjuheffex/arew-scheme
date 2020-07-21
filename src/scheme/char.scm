;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; Copyright © 2020 Amirouche BOUBEKKI <amirouche@hyper.dev>
;; SPDX-License-Identifier: CC0-1.0
(library (scheme char)
  (export
    char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=?
    char-ci>? char-downcase char-foldcase char-lower-case?
    char-numeric? char-upcase char-upper-case? char-whitespace?
    digit-value string-ci<=? string-ci<? string-ci=?
    string-ci>=? string-ci>? string-downcase string-foldcase
    string-upcase)
  (import (chezscheme))

;; The table can be extracted with:
;; awk -F ';' '/ZERO;Nd/ {print "#x"$1}' UnicodeData.txt
;; Up to date with Unicode 11.0.0

(define *decimal-zeroes* '#(#x0030 #x0660 #x06F0 #x07C0 #x0966 #x09E6
  #x0A66 #x0AE6 #x0B66 #x0BE6 #x0C66 #x0CE6 #x0D66 #x0DE6 #x0E50
  #x0ED0 #x0F20 #x1040 #x1090 #x17E0 #x1810 #x1946 #x19D0 #x1A80
  #x1A90 #x1B50 #x1BB0 #x1C40 #x1C50 #xA620 #xA8D0 #xA900 #xA9D0
  #xA9F0 #xAA50 #xABF0 #xFF10 #x104A0 #x10D30 #x11066 #x110F0 #x11136
  #x111D0 #x112F0 #x11450 #x114D0 #x11650 #x116C0 #x11730 #x118E0
  #x11C50 #x11D50 #x11DA0 #x16A60 #x16B50 #x1D7CE #x1D7D8 #x1D7E2
  #x1D7EC #x1D7F6 #x1E950))

(define (vector-parse-start+end vec args start-name end-name callee)
  (let ((len (vector-length vec)))
    (cond ((null? args)
           (values 0 len))
          ((null? (cdr args))
           (check-indices vec
                          (car args) start-name
                          len end-name
                          callee))
          ((null? (cddr args))
           (check-indices vec
                          (car  args) start-name
                          (cadr args) end-name
                          callee))
          (else
           (error "too many arguments"
                  `(extra args were ,(cddr args))
                  `(while calling ,callee))))))

(define-syntax let-vector-start+end
  (syntax-rules ()
    ((let-vector-start+end ?callee ?vec ?args (?start ?end)
       ?body1 ?body2 ...)
     (let ((?vec (check-type vector? ?vec ?callee)))
       (call-with-values (lambda () (vector-parse-start+end ?vec
                                                            ?args
                                                            '?start
                                                            '?end
                                                            ?callee))
         (lambda (?start ?end)
           ?body1 ?body2 ...))))))

;; TODO: avoid the let-vector-start+end macro
(define (vector-binary-search vec value cmp . maybe-start+end)
  (let ((cmp (check-type procedure? cmp vector-binary-search)))
    (let-vector-start+end vector-binary-search vec maybe-start+end
                          (start end)
      (let loop ((start start) (end end) (j #f))
        (let ((i (quotient (+ start end) 2)))
          (if (or (= start end) (and j (= i j)))
              #f
              (let ((comparison
                     (check-type integer?
                                 (cmp (vector-ref vec i) value)
                                 `(,cmp for ,vector-binary-search))))
                (cond ((zero?     comparison) i)
                      ((positive? comparison) (loop start i i))
                      (else                   (loop i end i))))))))))

(define (digit-value char)
  (define (cmp zero ch)
    (if (integer? ch)
        (- (cmp zero ch))
        (let ((i (char->integer ch)))
          (cond ((< i zero) 1)
                ((> i (+ zero 9)) -1)
                (else 0)))))
  (unless (char? char)
    (assertion-violation 'digit-value "Expected a char" char))
  (cond
    ((char<=? #\0 char #\9)             ;fast case
     (- (char->integer char) (char->integer #\0)))
    ((vector-binary-search *decimal-zeroes* char cmp)
     => (lambda (zero)
          (- (char->integer char)
             (vector-ref *decimal-zeroes* zero))))
    (else #f))))
