(library (m)
  (export (rename (thruth magic)) lll foobar)
  (import (except (chezscheme) define) (arew base))

  (define thruth "test doc" 101)

  (define lll "test lambda doc"
    (lambda ()
      42))

  (define (foobar abc)
    "documentation for the win"
    (assume (number? abc) foobar)
    101))
