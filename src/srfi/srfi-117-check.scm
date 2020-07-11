(library (srfi srfi-117-check)

  (export check-000 check-001 check-002 check-003 check-004 check-005
check-006 check-007 check-008 check-009 check-010 check-011 #;check-012
check-013 check-014 check-015 #;check-016 check-017 check-018 check-019
check-020 check-021 check-022 check-023 check-024 check-025 #;check-026
check-027 check-028 check-029 check-030 check-031 check-032 check-033)

  (import (scheme base)
          (srfi srfi-117)
          (check))

  (define check-000
    (check '(1 1 1) (list-queue-list (make-list-queue '(1 1 1)))))

  (define check-001
    (check '(1 2 3) (list-queue-list (list-queue 1 2 3))))

  (define x1bis (list 1 2 3))

  (define x2 (make-list-queue x1bis (cddr x1bis)))

  (define check-002
    (check 3 (list-queue-back x2)))

  (define y (list-queue 4 5))

  (define check-003
    (check (list-queue? y)))

  (define z (list-queue-append (list-queue 1 2 3) (list-queue 4 5)))

  (define check-004
    (check '(1 2 3 4 5) (list-queue-list
                         (list-queue-append (list-queue 1 2 3) (list-queue 4 5)))))

  (define z2 (list-queue-append! (list-queue 1 2 3) (list-queue-copy y)))

  (define check-005
    (check '(1 2 3 4 5) (list-queue-list z2)))

  (define check-006
    (check 1 (list-queue-front
              (list-queue-append (list-queue 1 2 3) (list-queue 4 5)))))

  (define check-007
    (check 5 (list-queue-back
              (list-queue-append (list-queue 1 2 3) (list-queue 4 5)))))

  (define y-008 (list-queue 4 5))
  (define t0 (list-queue-remove-front! y-008))

  (define check-008
    (check '(5) (list-queue-list y-008)))

  (define t1 (list-queue-remove-back! y))
  (define t1.bis (list-queue-remove-back! y))

  (define check-009
    (check (list-queue-empty? y)))

  (define check-010
    (check-raise (list-queue-remove-front! y)))

  (define check-011
    (check-raise (list-queue-remove-back! y)))

  (define check-012
    (check '(1 2 3 4 5) (list-queue-list z)))

  (define check-013
    (check '(1 2 3 4 5) (list-queue-remove-all! z2)))

  (define check-014
    (check (list-queue-empty? z2)))

  (define t2 (list-queue-remove-all! z))

  (define t3 (list-queue-add-front! z 1))

  (define t4 (list-queue-add-front! z 0))

  (define t5 (list-queue-add-back! z 2))

  (define t6 (list-queue-add-back! z 3))

  (define check-015
    (check '(0 1 2 3) (list-queue-list z)))

  (define a (list-queue 1 2 3))

  (define b (list-queue-copy a))

  ;; (define check-016
  ;;   (check '(1 2 3) (list-queue-list b)))

  (define t7 (list-queue-add-front! b 0))

  (define check-017
    (check '(1 2 3) (list-queue-list a)))

  (define check-018
    (check 4 (length (list-queue-list b))))

  (define c (list-queue-concatenate (list a b)))

  (define check-019
    (check '(1 2 3 0 1 2 3) (list-queue-list c)))

  (define r (list-queue 1 2 3))

  (define s (list-queue-map (lambda (x) (* x 10)) r))

  (define check-020
    (check '(10 20 30) (list-queue-list s)))

  (define t8 (list-queue-map! (lambda (x) (+ x 1)) r))

  (define check-021
    (check '(2 3 4) (list-queue-list r)))

  (define sum 0)

  (define t9 (list-queue-for-each (lambda (x) (set! sum (+ sum x))) s))

  (define check-022
    (check 60 sum))

  (define n (list-queue 5 6))

  (define t10
    (list-queue-set-list! n (list 1 2)))

  (define check-023
    (check '(1 2) (list-queue-list n)))

  (define d (list 1 2 3))

  (define e (cddr d))

  (define f (make-list-queue d e))

  (define-values (dx ex) (list-queue-first-last f))

  (define check-024
    (check (eq? d dx)))

  (define check-025
    (check (eq? e ex)))

  (define check-026
    (check '(1 2 3) (list-queue-list f)))

  (define t11 (list-queue-add-front! f 0))
  (define t12 (list-queue-add-back! f 4))

  (define check-027 (check '(0 1 2 3 4) (list-queue-list f)))

  (define g (make-list-queue d e))

  (define check-028 (check '(1 2 3 4) (list-queue-list g)))

  (define h (list-queue 5 6))

  (define t13 (list-queue-set-list! h d e))

  (define check-029
    (check '(1 2 3 4) (list-queue-list h)))

  (define (double x) (* x 2))
  (define (done? x) (> x 3))
  (define (add1 x) (+ x 1))
  (define x3 (list-queue-unfold done? double add1 0))

  (define check-030
    (check '(0 2 4 6) (list-queue-list x3)))

  (define ybis (list-queue-unfold-right done? double add1 0))

  (define check-031
    (check '(6 4 2 0) (list-queue-list ybis)))

  (define x0 (list-queue 8))
  (define x1ter (list-queue-unfold done? double add1 0 x0))

  (define check-032
    (check '(0 2 4 6 8) (list-queue-list x1ter)))

  (define y0 (list-queue 8))
  (define y1 (list-queue-unfold-right done? double add1 0 y0))

  (define check-033
    (check '(8 6 4 2 0) (list-queue-list y1))))
