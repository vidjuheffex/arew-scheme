(library (check)
  (export check check-raise check-skip run-check)

  (import (chezscheme))
  (import (only (scheme base)  pk))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax check
      (syntax-rules ()
        ((check predicate? expected actual)
         (lambda ()
           (let ((expected* expected)
                 (actual* actual))
             (if (predicate? expected* actual*)
                 (vector #t)
                 (vector #f 'unexpected-value expected* actual*)))))
        ((check expected actual)
         (check equal? expected actual))
        ((check actual)
         (check equal? #t actual))
        ((check predicate? v0 v1 ...)
         (check (predicate? v0 v1 ...)))))

    (define-syntax-rule (check-raise predicate? actual)
      (lambda ()
        (let ((predicate?* predicate?))
          (guard (ex ((predicate?* ex) (vector #t))
                     (else (vector #f 'unexpected-exception predicate?* ex)))
            (let ((actual* actual))
              (vector #f 'no-exception predicate?* actual*))))))

    (define-syntax-rule (check-skip test expected actual)
      (lambda ()
        (vector #t)))

    ;; how to run the test suite

    (define-syntax-rule (run-check library-name check-name check)
      (begin
        (guard (obj (else (vector #f 'error obj)))
          (display "** ")
          (display 'library-name)
          (display " ")
          (display 'check-name)
          (newline)
          (let ((out (check)))
            (when (not (vector-ref out 0))
              (display out)
              (newline)
              (exit 1))))))))
