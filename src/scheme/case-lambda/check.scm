(library (scheme case-lambda check)

  (export test-1
          test-2)

  (import (check)
          (scheme base)
          (scheme case-lambda))

  (define add1
    (case-lambda
      ((a) (add1 a 0))
      ((a b) (+ 1 a b))))

  (define test-1
    (check (add1 1) 2))

  (define test-2
    (check (add1 1 2) 4)))
