(import (scheme base))
(import (arew termbox))




(define (main)
  (tb-init)
  (tb-select-output-mode TB-OUTPUT-TRUECOLOR)
  (let loop ()
    (tb-clear)
    (tb-hide-cursor)
    (tb-change-cell 0 0 (char->integer #\space) 0 #x00FF00)
    (tb-change-cell 0 1 (char->integer #\space) 0 #x00FF00)
    (tb-change-cell 0 2 (char->integer #\space) 0 #x00FF00)
    (tb-present)
    (loop)))


(main)
