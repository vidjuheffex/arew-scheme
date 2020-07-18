(library (m)
  (export (rename (thruth magic)) lll foobar)
  (import (except (chezscheme) define) (arew help))

  (define thruth "test doc" 101)

  (define lll "test lambda doc"
    (lambda ()
      42))

  (define (foobar)
    "documentation for the win"
    101))
