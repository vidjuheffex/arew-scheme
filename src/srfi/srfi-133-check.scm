(library (srfi srfi-133-check)

  (export check-000 check-001 check-002 check-003 check-004 check-005
check-006 check-007 check-008 check-009 check-010 check-011 check-012
check-013 check-014 check-015 check-016 check-017 check-018 check-019
check-020 check-021 check-022 check-023 check-024 check-025 check-026
check-027 check-028 check-029 check-030 check-031 check-032 check-033
check-034 check-035 check-036 check-037 check-038 check-039 check-040
check-041 check-042 check-043 check-044 check-045 check-046 check-047
check-048 check-049 check-050 check-051 check-052 check-053 check-054
check-055 check-056 check-057 check-058 check-059 check-060 check-061
check-062 check-063 check-064 check-065 check-066 check-067 check-068
check-069 check-070 check-071 check-072 check-073 check-074 check-075
check-076 check-077 check-078 check-079 check-080 check-081 check-082
check-083 check-084 check-085 check-086 check-087 check-088 check-089
check-090 check-091 check-092 check-093 check-094 check-095 check-096
check-097 check-098 check-099 check-100 check-101 check-102 check-103
check-104)

  (import (except (scheme base) string->vector vector->string
                  list->vector vector->list vector-copy! vector-fill! vector-map
                  vector-append vector-copy)
          (srfi srfi-133)
          (check))


  (define v (make-vector 3 3))

  (define check-000
    (check (vector? '#(1 2 3))))

  (define check-001
    (check (vector? (make-vector 10))))

  (define check-002
    (check 3 (vector-ref v 0)))

  (define check-003
    (check 3 (vector-ref v 1)))

  (define check-004
    (check 3 (vector-ref v 2)))

  (define check-005
    (check-raise (vector-ref v -1)))

  (define check-006
    (check-raise (vector-ref v 3)))

  (define check-007
    (check -32 (begin
                 (vector-set! v 0 -32)
                 (vector-ref v 0))))

  (define check-008
    (check 3 (vector-length v)))

  (define check-009
    (check 0 (vector-length '#())))

  (define a2i '#(a b c d e f g h i))

  (define check-010
    (check '#(0 1 2 3 4) (vector 0 1 2 3 4)))

  (define check-011
    (check '#(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
           (vector-unfold (lambda (i x) (values x (- x 1))) 10 0)))

  (define check-012
    (check '#(0 1 2 3 4 5 6) (vector-unfold values 7)))

  (define check-013
    (check '#((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
           (vector-unfold-right (lambda (i x) (values (cons i x) (+ x 1))) 5 0)))

  (define check-014
    (check a2i (vector-copy a2i)))

  (define check-015
    (check (not (eqv? a2i (vector-copy a2i)))))

  (define check-016
    (check '#(g h i) (vector-copy a2i 6)))

  (define check-017
    (check '#(d e f) (vector-copy a2i 3 6)))

  (define check-018
    (check '#(1 2 3 4) (vector-reverse-copy '#(5 4 3 2 1 0) 1 5)))

  (define check-019
    (check '#(x y) (vector-append '#(x) '#(y))))

  (define check-020
    (check '#(a b c d) (vector-append '#(a) '#(b c d))))

  (define check-021
    (check '#(a #(b) #(c)) (vector-append '#(a #(b)) '#(#(c)))))

  (define check-022
    (check '#(a b c d) (vector-concatenate '(#(a b) #(c d)))))

  (define check-023
    (check '#(a b h i) (vector-append-subvectors '#(a b c d e) 0 2 '#(f g h i j) 2 4)))

  (define check-024
    (check #f (vector-empty? '#(a))))

  (define check-025
    (check #f (vector-empty? '#(()))))

  (define check-026
    (check #f (vector-empty? '#(#()))))

  (define check-027
    (check (vector-empty? '#())))

  (define check-028
    (check (vector= eq? '#(a b c d) '#(a b c d))))

  (define check-029
    (check #f (vector= eq? '#(a b c d) '#(a b d c))))

  (define check-030
    (check #f (vector= = '#(1 2 3 4 5) '#(1 2 3 4))))

  (define check-031
    (check #t (vector= = '#(+nan.0) '#(+nan.0))))

  (define check-032
    (check #t (let ((nan '+nan.0)) (vector= = (vector nan) (vector nan)))))

  (define check-033
    (check #t (let ((nanvec '#(+nan.0))) (vector= = nanvec nanvec))))

  (define check-034
    (check (vector= eq?)))

  (define check-035
    (check (vector= eq? '#(a))))

  (define check-036
    (check #f (vector= eq? (vector (vector 'a)) (vector (vector 'a)))))

  (define check-037
    (check (vector= equal? (vector (vector 'a)) (vector (vector 'a)))))

  (define vos '#("abc" "abcde" "abcd"))
  (define vec '#(0 1 2 3 4 5))
  (define vec2 (vector 0 1 2 3 4))
  (define vec3 (vector 1 2 3 4 5))
  (define result '())
  (define (sqr x) (* x x))

  (define check-038
    (check 5 (vector-fold (lambda (len str) (max (string-length str) len))
                          0 vos)))

  (define check-039
    (check '(5 4 3 2 1 0)
           (vector-fold (lambda (tail elt) (cons elt tail)) '() vec)))

  (define check-040
    (check 3 (vector-fold (lambda (ctr n) (if (even? n) (+ ctr 1) ctr)) 0 vec)))

  (define check-041
    (check '(a b c d) (vector-fold-right (lambda (tail elt) (cons elt tail))
                                         '() '#(a b c d))))

  (define check-042
    (check '#(1 4 9 16) (vector-map sqr '#(1 2 3 4))))

  (define check-043
    (check '#(5 8 9 8 5) (vector-map * '#(1 2 3 4 5) '#(5 4 3 2 1))))

  (define check-044
    (check '#(0 1 4 9 16) (begin
                            (vector-map! sqr vec2)
                            (vector-copy vec2))))

  (define check-045
    (check '#(0 2 12 36 80) (begin   (vector-map! * vec2 vec3) (vector-copy vec2))))

  (define check-046
    (check '(5 4 3 2 1 0) (begin
                            (vector-for-each (lambda (x) (set! result (cons x result))) vec)
                            (cons (car result) (cdr result)))))

  (define check-047
    (check 3 (vector-count even? '#(3 1 4 1 5 9 2 5 6))))

  (define check-048
    (check 2 (vector-count < '#(1 3 6 9) '#(2 4 6 8 10 12))))

  (define check-049
    (check '#(3 4 8 9 14 23 25 30 36) (vector-cumulate + 0 '#(3 1 4 1 5 9 2 5 6))))

  (define (cmp a b)
    (cond
     ((< a b) -1)
     ((= a b) 0)
     (else 1)))
  (define vbis '#(0 2 4 6 8 10 12))

  (define check-050
    (check 2 (vector-index even? '#(3 1 4 1 5 9 6))))

  (define check-051
    (check 1 (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))

  (define check-052
    (check #f (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))))

  (define check-053
    (check 5 (vector-index-right odd? '#(3 1 4 1 5 9 6))))

  (define check-054
    (check 3 (vector-index-right < '#(3 1 4 1 5) '#(2 7 1 8 2))))

  (define check-055
    (check 2 (vector-skip number? '#(1 2 a b 3 4 c d))))

  (define check-056
    (check 2 (vector-skip = '#(1 2 3 4 5) '#(1 2 -3 4))))

  (define check-057
    (check 7 (vector-skip-right number? '#(1 2 a b 3 4 c d))))

  (define check-058
    (check 3 (vector-skip-right = '#(1 2 3 4 5) '#(1 2 -3 -4 5))))

  (define check-059
    (check 0 (vector-binary-search vbis 0 cmp)))

  (define check-060
    (check 3 (vector-binary-search vbis 6 cmp)))

  (define check-061
    (check #f (vector-binary-search vbis 1 cmp)))

  (define check-062
    (check (vector-any number? '#(1 2 x y z))))

  (define check-063
    (check (vector-any < '#(1 2 3 4 5) '#(2 1 3 4 5))))

  (define check-064
    (check #f (vector-any number? '#(a b c d e))))

  (define check-065
    (check #f (vector-any > '#(1 2 3 4 5) '#(1 2 3 4 5))))

  (define check-066
    (check #f (vector-every number? '#(1 2 x y z))))

  (define check-067
    (check (vector-every number? '#(1 2 3 4 5))))

  (define check-068
    (check #f (vector-every < '#(1 2 3) '#(2 3 3))))

  (define check-069
    (check (vector-every < '#(1 2 3) '#(2 3 4))))

  (define check-070
    (check 'yes (vector-any (lambda (x) (if (number? x) 'yes #f)) '#(1 2 x y z))))

  (define check-071
    (let-values (((new off) (vector-partition number? '#(1 x 2 y 3 z))))
      (and (equal? '#(1 2 3 x y z) (vector-copy new))
           (equal? 3 (+ off 0)))))

  (define vs (vector 1 2 3))
  (define vf0 (vector 1 2 3))
  (define vf1 (vector 1 2 3))
  (define vf2 (vector 1 2 3))
  (define vr0 (vector 1 2 3))
  (define vr1 (vector 1 2 3))
  (define vr2 (vector 1 2 3))
  (define vc0 (vector 1 2 3 4 5))
  (define vc1 (vector 1 2 3 4 5))
  (define vc2 (vector 1 2 3 4 5))
  (define vrc0 (vector 1 2 3 4 5))
  (define vrc1 (vector 1 2 3 4 5))
  (define vrc2 (vector 1 2 3 4 5))
  (define vu0 (vector 1 2 3 4 5))
  (define vu1 (vector 1 2 3 4 5))
  (define vu2 (vector 1 2 3 4 5))
  (define vur0 (vector 1 2 3 4 5))
  (define vur1 (vector 1 2 3 4 5))
  (define vur2 (vector 1 2 3 4 5))

  (define check-072
    (check '#(2 1 3) (begin   (vector-swap! vs 0 1)
                              (vector-copy vs))))

  (define check-073
    (check '#(0 0 0) (begin (vector-fill! vf0 0) (vector-copy vf0))))

  (define check-074
    (check '#(1 0 0) (begin     (vector-fill! vf1 0 1)
                                (vector-copy vf1))))

  (define check-075
    (check '#(0 2 3) (begin   (vector-fill! vf2 0 0 1)
                              (vector-copy vf2))))

  (define check-076
    (check '#(3 2 1) (begin   (vector-reverse! vr0)
                              (vector-copy vr0))))

  (define check-077
    (check '#(1 3 2) (begin   (vector-reverse! vr1 1)
                              (vector-copy vr1))))

  (define check-078
    (check '#(2 1 3) (begin   (vector-reverse! vr2 0 2)
                              (vector-copy vr2))))

  (define check-079
    (check '#(1 10 20 30 5) (begin   (vector-copy! vc0 1 '#(10 20 30))
                                     (vector-copy vc0))))

  (define check-080
    (check '#(1 10 20 30 40)
           (begin
             (vector-copy! vc1 1 '#(0 10 20 30 40) 1)
             (vector-copy vc1))))

  (define check-081
    (check '#(1 10 20 30 5)
           (begin
             (vector-copy! vc2 1 '#(0 10 20 30 40) 1 4)
             (vector-copy vc2))))

  (define check-082
    (check '#(1 30 20 10 5)
           (begin
               (vector-reverse-copy! vrc0 1 '#(10 20 30))
               (vector-copy vrc0))))

  (define check-083
    (check '#(1 40 30 20 10)
           (begin
             (vector-reverse-copy! vrc1 1 '#(0 10 20 30 40) 1)
             (vector-copy vrc1))))

  (define check-084
    (check '#(1 30 20 10 5)
           (begin
             (vector-reverse-copy! vrc2 1 '#(0 10 20 30 40) 1 4)
             (vector-copy vrc2))))

  (define check-085
    (check '#(1 11 12 13 5)
           (begin
             (vector-unfold! (lambda (i) (+ 10 i)) vu0 1 4)
             (vector-copy vu0))))

  (define check-086
    (check '#(1 1 3 5 5)
           (begin
             (vector-unfold! (lambda (i x) (values (+ i x) (+ x 1))) vu1 1 4 0)
             (vector-copy vu1))))

  (define check-087
    (check '#(1 1 4 7 5)
           (begin
             (vector-unfold! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1))) vu2 1 4 0 0)
             (vector-copy vu2))))

  (define check-088
    (check '#(1 11 12 13 5)
           (begin
             (vector-unfold-right! (lambda (i) (+ 10 i)) vur0 1 4)
             (vector-copy vur0))))

  (define check-089
    (check '#(1 3 3 3 5)
           (begin
             (vector-unfold-right! (lambda (i x) (values (+ i x) (+ x 1))) vur1 1 4 0)
             (vector-copy vur1))))

  (define check-090
    (check '#(1 5 4 3 5)
           (begin
             (vector-unfold-right! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1))) vur2 1 4 0 0)
             (vector-copy vur2))))

  (define check-091
    (check '(1 2 3) (vector->list '#(1 2 3))))

  (define check-092
    (check '(2 3) (vector->list '#(1 2 3) 1)))

  (define check-093
    (check '(1 2) (vector->list '#(1 2 3) 0 2)))

  (define check-094
    (check '#(1 2 3) (list->vector '(1 2 3))))

  (define check-095
    (check '(3 2 1) (reverse-vector->list '#(1 2 3))))

  (define check-096
    (check '(3 2) (reverse-vector->list '#(1 2 3) 1)))

  (define check-097
    (check '(2 1) (reverse-vector->list '#(1 2 3) 0 2)))

  (define check-098
    (check '#(3 2 1) (reverse-list->vector '(1 2 3))))

  (define check-099
    (check "abc" (vector->string '#(#\a #\b #\c))))

  (define check-100
    (check "bc" (vector->string '#(#\a #\b #\c) 1)))

  (define check-101
    (check "ab" (vector->string '#(#\a #\b #\c) 0 2)))

  (define check-102
    (check '#(#\a #\b #\c) (string->vector "abc")))

  (define check-103
    (check '#(#\b #\c) (string->vector "abc" 1)))

  (define check-104
    (check '#(#\a #\b) (string->vector "abc" 0 2))))
