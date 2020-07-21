#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (srfi srfi-8)
  (export receive)
  (import (chezscheme))

  (define-syntax receive
    (syntax-rules ()
      ((_ formals expression b b* ...)
       (call-with-values
         (lambda () expression)
         (lambda formals b b* ...)))))

)
