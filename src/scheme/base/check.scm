(library (scheme base check)

  (export check-0001 check-0002 check-0003 check-0004 check-0005 check-0006
check-0007 check-0008 check-0009 check-0010 check-0011 check-0012 check-0013
check-0014 check-0015 check-0016 check-0017 check-0018 check-0019 check-0020
check-0021 check-0022 check-0023 check-0024 check-0025 check-0026 check-0027
check-0028 check-0029 check-0030 check-0031 check-0032 check-0033 check-0034
check-0035 check-0036 check-0037 check-0038 check-0039 check-0040 check-0041
check-0042 check-0043 check-0044 check-0045 check-0046 check-0047 check-0048
check-0049 check-0050)

  (import (check)
          (scheme base))

  (define check-0001
    (check (+ 1 2 3 4) 10))

  (define check-0002
    (check (+ 1) 1))

  (define check-0003
    (check (* 1 2 3 4) 24))

  (define check-0004
    (check (* 1) 1))

  (define check-0005
    (check (- 1) -1))

  (define check-0006
    (check (- 1 1) 0))

  (define check-0007
    (check (/ 1 2) 1/2))

  ;; propertly guard the exception 'numerical-overflow
  (define check-0008
    (check (guard (ex (else #t)) (/ 1 0)) #t))

  (define check-0009
    (check (/ 1 2 3 4 5) 1/120))

  (define check-0010
    (check (< 1 2 3 4 5) #t))

  (define check-0011
    (check (< 1 2 3 4 5 5) #f))

  (define check-0012
    (check (< 10 5) #f))

  (define check-0013
    (check (<= 1 2 3 4 5 5) #t))

  (define check-0014
    (check (<= 1 2 3 4 5) #t))

  (define check-0015
    (check (<= 10 5) #f))

  (define check-0016
    (check (= 10 5) #f))

  (define check-0017
    (check (= 10 10 10 10) #t))

  (define check-0018
    (check (> 10111 1011 11 1) #t))

  (define check-0019
    (check (> 10 100) #f))

  (define check-0020
    (check (>= 10111 10111 1011 11 1) #t))

  (define check-0021
    (check (>= 10 100) #f))

  (define check-0022
    (check (abs -10) 10))

  (define check-0023
    (check (abs 10) 10))

  (define check-0024
    (check (and 1 2 3) 3))

  (define check-0025
    (check (and 1 #f 3) #f))

  (define check-0026
    (check (append '(1 2 3)) '(1 2 3)))

  (define check-0027
    (check (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))

  (define check-0028
    (check (apply + '(1 2 3)) 6))

  (define check-0029
    (check (apply + 1 2 3 '(4 5 6)) 21))

  (define check-0030
    (check (assoc 1 '((1 . 2) (3 . 4))) '(1 . 2)))

  (define check-0031
    (check (assoc 42 '((1 . 2) (3 . 4))) #f))

  (define check-0032
    (check (assq 1 '((1 . 2) (3 . 4))) '(1 . 2)))

  (define check-0033
    (check (assq 42 '((1 . 2) (3 . 4))) #f))

  (define check-0034
    (check (assv 1 '((1 . 2) (3 . 4))) '(1 . 2)))

  (define check-0035
    (check (assv 42 '((1 . 2) (3 . 4))) #f))

  (define check-0036
    (check (boolean=? #t #f) #f))

  (define check-0037 (check (boolean=? #t #t) #t))

  (define check-0038
    (check (boolean? #t) #t))

  (define check-0039
    (check (boolean? 23) #f))

  (define check-0040
    (check (bytevector 0 1 2 3 4 5) #vu8(0 1 2 3 4 5)))

  (define check-0041
    (check (bytevector-append #vu8(0 1 2) #vu8(3 4 5)) #vu8(0 1 2 3 4 5)))

  (define check-0042
    (check (bytevector-copy #vu8(0 1 2)) #vu8(0 1 2)))

  (define check-0043
    (check (bytevector-copy #vu8(0 1 2) 1) #vu8(1 2)))

  (define check-0044
    (check (bytevector-copy #vu8(0 1 2) 1 2) #vu8(1)))

  (define check-0045
    (check
     (let ((bv (make-bytevector 4 0)))
       (bytevector-copy! bv 1 #vu8(0 1 2 3 4) 1 3)
       bv)
     #vu8(0 1 2 0)))

  (define check-0046
    (check (bytevector-length #vu8(0 1 2)) 3))

  (define check-0047
    (check (bytevector-u8-ref #vu8(0 1 2) 2) 2))

  (define check-0048
    (check
     (let ((bv (make-bytevector 4 0)))
       (bytevector-u8-set! bv 1 42)
       bv)
     #vu8(0 42 0 0)))

  (define check-0049
    (check (bytevector? #vu8(0 1 2)) #t))

  (define check-0050 (check (bytevector? 123456) #f)))
