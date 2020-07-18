(library (arew help)
  (export define help)
  (import (rename (chezscheme) (define cs:define)))


  (cs:define docstring #f) ;; identifier to hang property on

  (define-syntax define
    (lambda (x)
      (syntax-case x ()
        [(_ (name . fmls) str body0 ... body1)
         (and (identifier? #'name) (string? (datum str)))
         #'(begin
             (cs:define (name . fmls) body0 ... body1)
             (define-property name docstring str))]
         [(_ name str expr)
          (and (identifier? #'name) (string? (datum str)))
          #'(begin
             (cs:define name expr)
             (define-property name docstring str))]
         [(_ name . rest) #'(cs:define name . rest)])))


  (define-syntax help
    (lambda (x)
      (lambda (r) ;; retrieve the syntactic environment
        (syntax-case x ()
          [(_ id) (identifier? #'id) (r #'id #'docstring)]
          [(_ 'id) (identifier? #'id) (r #'id #'docstring)])))))
