(library (srfi srfi-127 check)

  (export check-001 check-002 check-003 check-004 check-005 check-006
check-007 check-008 check-009 check-010 check-011 check-012 check-013
check-014 check-015 check-016 check-017 check-018 check-019 check-020
check-021 check-022 check-023 check-024 check-025 check-026 check-027
check-028 check-029 check-030 check-031 check-032 check-033 check-034
check-035 check-036 check-037 check-038 check-039 check-040 check-041
check-042 check-043 check-044 check-045 check-046 check-047 check-048
check-049 check-050 check-051 check-052 check-053 check-054 check-055
check-056 check-057 check-058 check-059 check-060 check-061 check-062
check-063 check-064 check-065 check-066 check-068 check-069 check-070
check-071 check-072 check-073 check-074 check-075 check-076 check-077
check-078 check-079 check-080 check-081 check-082 check-083 check-084
check-085 check-086 check-087 check-088 check-089 check-090 check-091
check-092 check-093 check-094 check-095 check-096 check-097 check-098
check-099 check-100 check-101 check-102 check-103 check-104 check-105
check-106 check-107 check-108 check-109 check-110)

  (import (scheme base)
          (srfi srfi-127)
          (check))

  ;; Make-generator for tests cloned from SRFI 121
  (define (make-generator . args)
    (lambda () (if (null? args)
                   (eof-object)
                   (let ((next (car args)))
                     (set! args (cdr args))
                     next))))

  ;; Make-lseq creates an lseq, like list, but guarantees the use of a generator
  (define (make-lseq . args) (generator->lseq (apply make-generator args)))

  (define one23 (make-lseq 1 2 3))

  (define check-001
    (check 1 (car one23)))

  (define check-002
    (check (procedure? (cdr one23))))

  (define check-003
    (check '(1 2 3) (lseq-realize one23)))

  (define check-004
    (check (lseq? '())))

  (define check-005
    (check (lseq? '(1 2 3))))

  (define check-006
    (check (lseq? (make-lseq 1 2 3))))

  (define check-007
    (check (lseq? (cons 'x (lambda () 'x)))))

  (define check-008
    (check (lseq=? = '() '())))

  (define check-009
    (check (lseq=? = '(1 2 3) '(1 2 3))))

  (define check-010
    (check (lseq=? = (make-lseq 1 2 3)
                   (make-lseq 1 2 3))))

  (define check-011
    (check (lseq=? = (make-lseq 1 2 3) '(1 2 3))))

  (define check-012
    (check-raise (lseq-car (make-generator))))

  (define check-013
    (check 1 (lseq-car (make-lseq 1 2 3))))

  (define check-014
    (check 1 (lseq-car '(1 2 3))))

  (define check-015
    (check-raise (lseq-car 2)))

  (define check-016
    (check-raise (lseq-first (make-generator))))

  (define check-017
    (check 1 (lseq-first (make-lseq 1 2 3))))

  (define check-018
    (check 1 (lseq-first '(1 2 3))))

  (define check-019
    (check-raise (lseq-first 2)))

  (define check-020
    (check-raise (lseq-cdr (make-generator))))

  (define check-021
    (check 2 (lseq-cdr '(1 . 2))))

  (define check-022
    (check 2 (lseq-car (lseq-cdr '(1 2 3)))))

  (define check-023
    (check 2 (lseq-car (lseq-cdr (make-lseq 1 2 3)))))

  (define check-024
    (check-raise (lseq-rest (make-generator))))

  (define check-025
    (check 2 (lseq-rest '(1 . 2))))

  (define check-026
    (check 2 (lseq-car (lseq-rest '(1 2 3)))))

  (define check-027
    (check 2 (lseq-car (lseq-rest (make-lseq 1 2 3)))))

  (define check-028
    (check-raise (lseq-rest 2)))

  (define check-029
    (check-raise (lseq-ref '() 0)))

  (define check-030
    (check 1 (lseq-ref '(1) 0)))

  (define check-031
    (check 2 (lseq-ref '(1 2) 1)))

  (define check-032
    (check-raise (lseq-ref (make-lseq) 0)))

  (define check-033
    (check 1 (lseq-ref (make-lseq 1) 0)))

  (define check-034
    (check 1 (lseq-ref (make-lseq 1 2) 0)))

  (define check-035
    (check 2 (lseq-ref (make-lseq 1 2) 1)))

  (define check-036
    (check-raise (lseq-take '() 1)))

  (define check-037
    (check-raise (lseq-take (make-lseq) 1)))

  (define check-038
     ; test laziness
    (check (procedure? (cdr (lseq-take '(1 2 3 4 5) 3)))))

  (define check-039
    (check '(1 2 3) (lseq-realize (lseq-take '(1 2 3 4 5) 3))))

  (define check-040
    (check-raise (lseq-drop '() 1)))

  (define check-041
    (check-raise (lseq-drop (make-lseq 1) 2)))

  (define check-042
    (check '(3 4 5) (lseq-realize (lseq-drop '(1 2 3 4 5) 2))))

  (define check-043
    (check '(3 4 5) (lseq-realize (lseq-drop (make-lseq 1 2 3 4 5) 2))))

  (define check-044
    (check '() (lseq-realize '())))

  (define check-045
    (check '(1 2 3) (lseq-realize '(1 2 3))))

  (define check-046
    (check '() (lseq-realize (make-lseq))))

  (define check-047
    (check '(1 2 3) (lseq-realize (make-lseq 1 2 3))))

  (define g (lseq->generator '(1 2 3)))

  (define check-048 (check 1 (g)))

  (define check-049 (check 2 (g)))

  (define check-050 (check 3 (g)))
  (define check-051 (check (eof-object? (g))))

  (define g1 (lseq->generator (make-lseq 1 2 3)))

  (define check-052
    (check 1 (g1)))

  (define check-053
    (check 2 (g1)))

  (define check-054
    (check 3 (g1)))

  (define check-055
    (check (eof-object? (g))))

  (define check-056
    (check 0 (lseq-length '())))

  (define check-057
    (check 3 (lseq-length '(1 2 3))))

  (define check-058
    (check 3 (lseq-length (make-lseq 1 2 3))))

  (define check-059
    (check '(1 2 3 a b c) (lseq-realize (lseq-append '(1 2 3) '(a b c)))))

  (define one23abc (lseq-append (make-lseq 1 2 3) (make-lseq 'a 'b 'c)))

  (define check-060
    (check (procedure? (cdr one23abc))))

  (define check-061
    (check (lseq-realize one23abc)))

  (define one2345 (make-lseq 1 2 3 4 5))
  (define oddeven (make-lseq 'odd 'even 'odd 'even 'odd 'even 'odd 'even))

  (define check-062
    (check '((one 1 odd) (two 2 even) (three 3 odd))
           (lseq-realize (lseq-zip '(one two three) one2345 oddeven))))

  (define check-063
    (check '() (lseq-map - '())))

  (define check-064
    (check '(-1 -2 -3) (lseq-realize (lseq-map - '(1 2 3)))))

  (define check-065
    (check '(-1 -2 -3) (lseq-realize (lseq-map - (make-lseq 1 2 3)))))

  (define check-066
    (check (procedure? (cdr (lseq-map - '(1 2 3))))))

  (define output '())
  (define out! (lambda (x) (set! output (cons x output))))

  (define check-068
    (check output (begin (lseq-for-each out! '()) '())))

  (define check-069
    (check output (begin (lseq-for-each out! '(a b c)) '(c b a))))

  (define check-070
    (check output (begin (lseq-for-each out! (make-lseq 1 2 3)) '(3 2 1 c b a))))

  (define check-071
    (check '() (lseq-filter odd? '())))

  (define odds (lseq-filter odd? '(1 2 3 4 5)))

  (define check-072
    (check (procedure? (cdr odds))))

  (define check-073
    (check '(1 3 5) (lseq-realize odds)))

  (define check-074
    (check '(1 3 5) (lseq-realize (lseq-filter odd? (make-lseq 1 2 3 4 5)))))

  (define check-075
    (check '() (lseq-remove even? '())))

  (define odds1 (lseq-remove even? '(1 2 3 4 5)))

  (define check-076
    (check (procedure? (cdr odds1))))

  (define check-077
    (check '(1 3 5) (lseq-realize odds1)))

  (define check-078
    (check '(1 3 5) (lseq-realize (lseq-remove even? (make-lseq 1 2 3 4 5)))))

  (define check-079
    (check 4 (lseq-find even? '(3 1 4 1 5 9 2 6))))

  (define check-080
    (check 4 (lseq-find even? (make-lseq 3 1 4 1 5 9 2 6))))

  (define check-081
    (check #f (lseq-find negative? (make-lseq 1 2 3 4 5))))

  (define check-082
    (check '(-8 -5 0 0) (lseq-realize (lseq-find-tail even? '(3 1 37 -8 -5 0 0)))))

  (define check-083
    (check '(-8 -5 0 0) (lseq-realize (lseq-find-tail even?
                                                      (make-lseq 3 1 37 -8 -5 0 0)))))

  (define check-084
    (check #f (lseq-find-tail even? '())))

  (define check-085
    (check #f (lseq-find-tail negative? (make-lseq 1 2 3 4 5))))

  (define check-086
    (check '(2 18) (lseq-realize (lseq-take-while even? '(2 18 3 10 22 9)))))

  (define check-087
    (check '(2 18) (lseq-realize (lseq-take-while even?
                                                  (make-lseq 2 18 3 10 22 9)))))

  (define check-088
    (check '(2 18) (lseq-realize (lseq-take-while even?
                                                  (make-lseq 2 18 3 10 22 9)))))

  (define check-089
    (check '(3 10 22 9) (lseq-drop-while even? '(2 18 3 10 22 9))))

  (define check-090
    (check '(3 10 22 9) (lseq-realize (lseq-drop-while even?
                                                       (make-lseq 2 18 3 10 22 9)))))

  (define check-091
    (check #t (lseq-any integer? '(a 3 b 2.7))))

  (define check-092
    (check #t (lseq-any integer? (make-lseq 'a 3 'b 2.7))))

  (define check-093
    (check #f (lseq-any integer? '(a 3.1 b 2.7))))

  (define check-094
    (check #f (lseq-any integer? (make-lseq 'a 3.1 'b 2.7))))

  (define check-095
    (check #t (lseq-any < '(3 1 4 1 5) '(2 7 1 8 2))))

  (define (factorial n)
    (cond
     ((< n 0) #f)
     ((= n 0) 1)
     (else (* n (factorial (- n 1))))))

  (define check-096
    (check 6 (lseq-any factorial '(-1 -2 3 4))))

  (define check-097
    (check 6 (lseq-any factorial (make-lseq -1 -2 3 4))))

  (define check-098
    (check 24 (lseq-every factorial '(1 2 3 4))))

  (define check-099
    (check 24 (lseq-every factorial (make-lseq 1 2 3 4))))

  (define check-100
    (check 2 (lseq-index even? '(3 1 4 1 5 9))))

  (define check-101
    (check 1 (lseq-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

  (define check-102
    (check #f (lseq-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

  (define check-103
    (check '(a b c) (lseq-realize (lseq-memq 'a '(a b c)))))

  (define check-104
    (check '(a b c) (lseq-realize (lseq-memq 'a (make-lseq 'a 'b 'c)))))

  (define check-105
    (check #f (lseq-memq 'a (make-lseq 'b 'c 'd))))

  (define check-106
    (check #f (lseq-memq (list 'a) '(b c d))))

  (define check-107
    (check #f (lseq-memq (list 'a) (make-lseq 'b 'c 'd))))

  (define check-108
    (check '(101 102) (lseq-realize (lseq-memv 101 (make-lseq 100 101 102)))))

  (define check-109
    (check '((a) c) (lseq-realize (lseq-member (list 'a) (make-lseq 'b '(a) 'c)))))

  (define check-110
    (check '(2 3) (lseq-realize (lseq-member 2.0 (make-lseq 1 2 3) =)))))
