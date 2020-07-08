(library (check-check)

  (export test-0
          test-1
          test-2)

  (import (scheme base) (check))

  (define test-0
    (test #t #t))

  (define test-1
    (test boolean=? #t #t))

  (define test-2
    (check-raise symbol? (raise 'oops)))

  )
