;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: CC0-1.0
;;
;; based on akku-r7rs
;;
(library (scheme base)
  (export
   _ pk
   ... => else
   * + - / < <= = > >= abs and append apply assoc assq assv begin
   binary-port? boolean=? boolean? bytevector bytevector-append
   bytevector-copy bytevector-copy! bytevector-length
   bytevector-u8-ref bytevector-u8-set! bytevector? caar cadr
   call-with-current-continuation call-with-port call-with-values
   call/cc car case cdar cddr cdr ceiling char->integer char-ready?
   char<=? char<? char=? char>=? char>? char? close-input-port
   close-output-port close-port complex? cond cons
   current-error-port current-input-port current-output-port define
   define-syntax define-values denominator do
   dynamic-wind eof-object eof-object? eq? equal? eqv? error
   error-object-irritants error-object-message error-object? even?
   exact exact-integer-sqrt exact-integer? exact? expt
   file-error? floor floor-quotient floor-remainder floor/
   flush-output-port for-each gcd get-output-bytevector
   get-output-string guard if include inexact inexact?
   input-port-open? input-port? integer->char integer? lambda lcm
   length let let* let*-values let-syntax let-values letrec letrec*
   letrec-syntax list list->string list->vector list-copy list-ref
   list-set! list-tail list? make-bytevector make-list make-parameter
   make-string make-vector map max member memq memv min modulo
   negative? newline not null? number->string number? numerator odd?
   open-input-bytevector open-input-string open-output-bytevector
   open-output-string or output-port-open? output-port? pair?
   parameterize peek-char peek-u8 port? positive? procedure?
   quasiquote quote quotient raise raise-continuable rational?
   rationalize read-bytevector read-bytevector! read-char read-error?
   read-line read-string read-u8 real? remainder reverse round set!
   set-car! set-cdr! square string string->list string->number
   string->symbol string->utf8 string->vector string-append
   string-copy #;string-copy! string-fill! string-for-each
   string-length string-map string-ref string-set! string<=? string<?
   string=? string>=? string>? string? substring symbol->string
   symbol=? symbol? syntax-error syntax-rules textual-port? truncate
   truncate-quotient truncate-remainder truncate/ u8-ready? unless
   unquote unquote-splicing utf8->string values vector vector->list
   vector->string vector-append vector-copy vector-copy! vector-fill!
   vector-for-each vector-length vector-map vector-ref vector-set!
   vector? when with-exception-handler write-bytevector write-char
   write-string write-u8 zero?

   (rename (my:define-record-type define-record-type))
   )

  (import (rename (chezscheme)
                  (error cs:error)
                  (bytevector-copy cs:bytevector-copy)
                  (bytevector-copy! cs:bytevector-copy!)
                  (vector->list cs:vector->list))
          (only (srfi srfi-43) vector-copy!))

  (begin

    (define (pk . args)
      (let ((port (current-output-port)))
        (display ";; " port)
        (display args port)
        (newline port)
        (car (reverse args))))

    (define (bytevector-append . bvs)
      (call-with-bytevector-output-port
       (lambda (p)
         (for-each (lambda (bv) (put-bytevector p bv)) bvs))))

    (define (error-object-irritants obj)
      (and (irritants-condition? obj)
           (condition-irritants obj)))

    (define (error-object-message obj)
      (and (message-condition? obj)
           (condition-message obj)))

    (define error-object? condition?)

    (define (exact-integer? i) (and (integer? i) (exact? i)))

    (define file-error? i/o-error?)

    ;; From division library

    (define-syntax %define-division
      (syntax-rules ()
        ((_ fix quo rem q+r)
         (begin
           (define (quo x y)
             (exact (fix (/ x y))))
           (define (rem x y)
             (- x (* (quo x y) y)))
           (define (q+r x y)
             (let ((q (quo x y)))
               (values q
                       (- x (* q y)))))))))

    (%define-division
     floor
     floor-quotient
     floor-remainder0 ;; Most implementation has native modulo
     floor/)

    (define floor-remainder modulo)

    (define (get-output-bytevector port)
      ;; R7RS says "It is an error if port was not created with
      ;; open-output-bytevector.", so we can safely assume that the port
      ;; was created by open-output-bytevector. -weinholt
      (set-port-position! port 0)
      (let ((bv (get-bytevector-all port)))
        (if (eof-object? bv)
            #vu8()
            bv)))

    (define (input-port-open? port)
      (and (not (port-closed? port)) (input-port? port)))

    (define (list-set! l k obj)
      (define (itr cur count)
        (if (= count k)
            (set-car! cur obj)
            (itr (cdr cur) (+ count 1))))
      (itr l 0))

    (define (open-input-bytevector bv) (open-bytevector-input-port bv))

    (define (open-output-bytevector)
      (let-values (((p extract) (open-bytevector-output-port)))
        (define pos 0)
        (define buf #vu8())
        (define (read! target target-start count)
          (when (zero? (- (bytevector-length buf) pos))
            (set! buf (bytevector-append buf (extract))))  ;resets p
          (let ((count (min count (- (bytevector-length buf) pos))))
            (bytevector-copy! buf
                              pos
                              target target-start count)
            (set! pos (+ pos count))
            count))
        (define (write! bv start count)
          (put-bytevector p bv start count)
          (set! pos (+ pos count))
          count)
        (define (get-position)
          pos)
        (define (set-position! new-pos)
          (set! pos new-pos))
        (define (close)
          (close-port p))
        ;; It's actually an input/output port, but only
        ;; get-output-bytevector should ever read from it. If it was just
        ;; an output port then there would be no good way for
        ;; get-output-bytevector to read the data. -weinholt
        (make-custom-binary-input/output-port
         "bytevector" read! write! get-position set-position! close)))

    (define (output-port-open? port)
      (and (not (port-closed? port)) (output-port? port)))

    (define peek-u8
      (case-lambda
        (() (peek-u8 (current-input-port)))
        ((port)
         (lookahead-u8 port))))

    (define read-bytevector
      (case-lambda
        ((len) (read-bytevector len (current-input-port)))
        ((len port) (get-bytevector-n port len))))

    (define read-bytevector!
      (case-lambda
        ((bv)
         (read-bytevector! bv (current-input-port)))
        ((bv port)
         (read-bytevector! bv port 0))
        ((bv port start)
         (read-bytevector! bv port start (bytevector-length bv)))
        ((bv port start end)
         (get-bytevector-n! port bv start (- end start)))))

    (define read-error? lexical-violation?)

    (define read-line
      (case-lambda
        (() (read-line (current-input-port)))
        ((port) (get-line port))))

    (define read-string
      (case-lambda
        ((len) (read-string len (current-input-port)))
        ((len port) (get-string-n port len))))

    (define read-u8
      (case-lambda
        (() (read-u8 (current-input-port)))
        ((port) (get-u8 port))))

    (define (square x) (* x x))

    (define (%substring1 str start) (substring str start (string-length str)))

    (define string->vector
      (case-lambda
        ((str) (list->vector (string->list str)))
        ((str start) (string->vector (%substring1 str start)))
        ((str start end) (string->vector (substring str start end)))))

    (define (string-map proc . strs)
      (list->string (apply map proc (map string->list strs))))

    (define truncate-quotient quotient)

    (define truncate-remainder remainder)

    (define (truncate/ x y)
      (values (truncate-quotient x y)
              (truncate-remainder x y)))

    (define u8-ready? input-port-ready?)

    (define (%subvector v start end)
      (define mlen (- end start))
      (define out (make-vector (- end start)))
      (define (itr r)
        (if (= r mlen)
            out
            (begin
              (vector-set! out r (vector-ref v (+ start r)))
              (itr (+ r 1)))))
      (itr 0))

    (define (%subvector1 v start) (%subvector v start (vector-length v)))

    (define vector->list
      (case-lambda
        ((v) (cs:vector->list v))
        ((v start) (cs:vector->list (%subvector1 v start)))
        ((v start end) (cs:vector->list (%subvector v start end)))))

    (define vector->string
      (case-lambda
        ((v) (list->string (vector->list v)))
        ((v start) (vector->string (%subvector1 v start)))
        ((v start end) (vector->string (%subvector v start end)))))

    (define (vector-append . lis)
      (list->vector (apply append (map cs:vector->list lis))))

    (define write-bytevector
      (case-lambda
        ((bv) (write-bytevector bv (current-output-port)))
        ((bv port) (put-bytevector port bv))
        ((bv port start) (write-bytevector (%subbytevector1 bv start) port))
        ((bv port start end)
         (write-bytevector (%subbytevector bv start end) port))))

    (define write-string
      (case-lambda
        ((str) (write-string str (current-output-port)))
        ((str port) (put-string port str))
        ((str port start) (write-string str port start (string-length str)))
        ((str port start end)
         (write-string (substring str start end) port))))

    (define write-u8
      (case-lambda
        ((obj) (write-u8 obj (current-output-port)))
        ((obj port) (put-u8 port obj))))

    (define (error message . irritants)
      (if (and (symbol? message) (pair? irritants) (string? (car irritants)))
          (apply cs:error message irritants)
          (apply cs:error #f message irritants)))

    (define bytevector-copy!
      (case-lambda
        ((to at from) (bytevector-copy! to at from 0))
        ((to at from start)
         (let ((flen (bytevector-length from))
               (tlen (bytevector-length to)))
           (let ((fmaxcopysize (- flen start))
                 (tmaxcopysize (- tlen at)))
             (bytevector-copy! to at from start (+ start
                                                   (min fmaxcopysize
                                                        tmaxcopysize))))))
        ((to at from start end)
         (cs:bytevector-copy! from start to at (- end start)))))

    (define (%subbytevector bv start end)
      (define mlen (- end start))
      (define out (make-bytevector mlen))
      (cs:bytevector-copy! bv start out 0 mlen)
      out)

    (define (%subbytevector1 bv start)
      (%subbytevector bv start (bytevector-length bv)))

    (define bytevector-copy
      (case-lambda
        ((bv) (cs:bytevector-copy bv))
        ((bv start) (%subbytevector1 bv start))
        ((bv start end) (%subbytevector bv start end))))

    (define-syntax my:define-record-type
      (lambda (stx)
        (syntax-case stx ()
          ((_ type (constructor constructor-tag ...)
              predicate
              (field-tag accessor setter ...) ...)
           (and (for-all identifier?
                         #'(type constructor constructor-tag ... predicate
                                 field-tag ... accessor ... setter ... ...))
                (for-all (lambda (s) (<= 0 (length s) 1))
                         #'((setter ...) ...))
                (for-all (lambda (ct)
                           (memp (lambda (ft) (bound-identifier=? ct ft))
                                 #'(field-tag ...)))
                         #'(constructor-tag ...)))
           (with-syntax (((field-clause ...)
                          (map (lambda (clause)
                                 (if (= 2 (length clause))
                                     #`(immutable . #,clause)
                                     #`(mutable . #,clause)))
                               #'((field-tag accessor setter ...) ...)))
                         ((unspec-tag ...)
                          (remp (lambda (ft)
                                  (memp (lambda (ct) (bound-identifier=? ft ct))
                                        #'(constructor-tag ...)))
                                #'(field-tag ...))))
             #'(define-record-type (type constructor predicate)
                 (protocol (lambda (ctor)
                             (lambda (constructor-tag ...)
                               (define unspec-tag) ...
                               (ctor field-tag ...))))
                 (fields field-clause ...)))))))


    ))
