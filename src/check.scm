(library (check)
  (export test check-raise check-skip run-check)

  (import (chezscheme))
  (import (only (scheme base) pk))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax test
      (syntax-rules ()
        ((test predicate? expected actual)
         (lambda ()
           (let ((expected* expected)
                 (actual* actual))
             (if (predicate? expected* actual*)
                 (vector #t)
                 (vector #f 'unexpected-value expected* actual*)))))
        ((test expected actual)
         (test equal? expected actual))))

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
        (guard (obj (else (pk (vector #f 'error obj))))
          (let ((out (check)))
            (when (not (vector-ref out 0))
              (display "** ")
              (display 'library-name)
              (display " ")
              (display 'check-name)
              (newline)
              (display out)
              (newline)
              (exit 1))))))))
