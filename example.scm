c(import (scheme base))

(let loop ((index 100))
  (unless (zero? index)
    (pk "hello world")
    (loop (- index 1))))
