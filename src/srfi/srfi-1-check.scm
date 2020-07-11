#!r6rs
;; Test suite for SRFI-1
;; 2003-12-29 / lth
;;
;; $Id: srfi-1-test.sps 5842 2008-12-11 23:04:51Z will $
;;
;; Note: In Larceny, we require that the procedures designated as
;; "linear update" variants in the spec (eg append!) side-effect their
;; arguments, and there are tests here that check that side-effecting
;; occurs.
;;
;; For linear update we only require that the cells of the result are
;; taken from the cells of the input.  We could be stricter and require
;; that the cells of the results are the cells of the input with only
;; the CDR changed, ie, values are never moved from one cell to another.
;;
;; Comments:
;;
;; - Test cases are ordered as in the spec.  R5RS procedures are left
;;   out.
;;
;; - Adapted to arew scheme tests
;;

(library (srfi srfi-1-check)

  (export check-0001
          check-0002
          check-0003
          check-0004
          check-0005
          check-0006
          check-0007
          check-0008
          check-0009
          check-0010
          check-0011
          check-0012
          check-0013
          check-0014
          check-0015
          check-0016
          check-0017
          check-0018
          check-0019
          check-0020
          check-0021
          check-0022
          check-0023
          check-0024
          check-0025
          check-0026
          check-0027
          check-0028
          check-0029
          check-0030
          check-0031
          check-0032
          check-0033
          check-0034
          check-0035
          check-0036
          check-0037
          check-0038
          check-0039
          check-0040
          check-0041
          check-0042
          check-0043
          check-0044
          check-0045
          check-0046
          check-0047
          check-0048
          check-0049
          check-0050
          check-0051
          check-0052
          check-0053
          check-0054
          check-0055
          check-0056
          check-0057
          check-0058
          check-0059
          check-0060
          check-0061
          check-0062
          check-0063
          check-0064
          check-0065
          check-0066
          check-0067
          check-0068
          check-0069
          check-0070
          check-0071
          check-0072
          check-0073
          check-0074
          check-0075
          check-0076
          check-0077
          check-0078
          check-0079
          check-0080
          check-0081
          check-0082
          check-0083
          check-0084
          check-0085
          check-0086
          check-0087
          check-0088
          check-0089
          check-0090
          check-0091
          check-0092
          check-0093
          check-0094
          check-0095
          check-0096
          check-0097
          check-0098
          check-0099
          check-0100
          check-0101
          check-0102
          check-0103
          check-0104
          check-0105
          check-0106
          check-0107
          check-0108
          check-0109
          check-0110
          check-0111
          check-0112
          check-0113
          check-0114
          check-0115
          check-0116
          check-0117
          check-0118
          check-0119
          check-0120
          check-0121
          check-0122
          check-0123
          check-0124
          check-0125
          check-0126
          check-0127
          check-0128
          check-0129
          check-0130
          check-0131
          check-0132
          check-0133
          check-0134
          check-0135
          check-0136
          check-0137
          check-0138
          check-0139
          check-0140
          check-0141
          check-0142
          check-0143
          check-0144
          check-0145
          check-0146
          check-0147
          check-0148
          check-0149
          check-0150
          check-0151
          check-0152
          check-0153
          check-0154
          check-0155
          check-0156
          check-0157
          check-0158
          check-0159
          check-0160
          check-0161
          check-0162
          check-0163
          check-0164
          check-0165
          check-0166
          check-0167
          check-0168
          check-0169
          check-0170
          check-0171)

  (import (only (chezscheme)
                if define begin quote lambda error
                call-with-current-continuation
                not eq? eqv? list? let let* or and equal?
                = let-values quasiquote even? < max + > * -
                zero? set! number? symbol? integer? remainder
                unquote)
          (check)
          (srfi srfi-1))

  (begin

    (define check-0001
      (check '(2 . 1) (xcons 1 2)))

    (define check-0002
      (check 1 (cons* 1)))

    (define check-0003
      (check '(1 2 3 4 . 5) (cons* 1 2 3 4 5)))

    (define check-0004
      (check '(#t #t #t #t #t) (make-list 5 #t)))

    (define check-0005
      (check '() (make-list 0 #f)))

    (define check-0006
      (check 3 (length (make-list 3))))

    (define check-0007
      (check '(0 1 2 3 4) (list-tabulate 5 (lambda (x) x))))

    (define check-0008
      (check '() (list-tabulate 0 (lambda (x) (error 'srfi-1-tests "tests")))))

    (define check-0009
      (check #t
            (call-with-current-continuation
             (lambda (abort)
               (let* ((c  (list 1 2 3 4 5))
	              (cp (list-copy c)))
	         (or (equal? c cp)
	             (abort #f))
	         (let loop ((c c) (cp cp))
	           (if (not (null? c))
	               (begin
		         (or (not (eq? c cp))
		             (abort #f))
		         (loop (cdr c) (cdr cp)))))
	         #t)))))

    (define check-0010
      (check '(1 2 3 . 4) (list-copy '(1 2 3 . 4))))

    (define check-0011
      (check #t (not (list? (circular-list 1 2 3)))))

    (define check-0012
      (check #t
            (let* ((a (list 'a))
	           (b (list 'b))
	           (c (list 'c))
	           (x (circular-list a b c)))
              (and (eq? a (car x))
	           (eq? b (cadr x))
	           (eq? c (caddr x))
	           (eq? a (cadddr x))))))

    (define check-0013
      (check '() (iota 0)))

    (define check-0014
      (check '(2 5 8 11 14) (iota 5 2 3)))

    (define check-0015
      (check '(2 3 4 5 6) (iota 5 2)))

    (define check-0016
      (check #t (proper-list? '(1 2 3 4 5))))

    (define check-0017
      (check #t (proper-list? '())))

    (define check-0018
      (check #f (proper-list? '(1 2 . 3))))

    (define check-0019
      (check #f (proper-list? (circular-list 1 2 3))))

    (define check-0020
      (check #f (circular-list? '(1 2 3 4 5))))

    (define check-0021
      (check #f (circular-list? '())))

    (define check-0022
      (check #f (circular-list? '(1 2 . 3))))

    (define check-0023
      (check #t (circular-list? (circular-list 1 2 3))))

    (define check-0024
      (check #f (dotted-list? '(1 2 3))))

    (define check-0025
      (check #f (dotted-list? '())))

    (define check-0026
      (check #t (dotted-list? '(1 2 . 3))))

    (define check-0027
      (check #f (dotted-list? (circular-list 1 2 3))))

    (define check-0028
      (check #t (null-list? '())))

    (define check-0029
      (check #f (null-list? '(1 2))))

    (define check-0030
      (check #f (null-list? (circular-list 1 2))))

    (define check-0031
      (check #t (not-pair? 1)))

    (define check-0032
      (check #f (not-pair? (cons 1 2))))

    (define check-0033
      (check #t (list= = '(1 2 3) '(1 2 3) '(1 2 3))))

    (define check-0034
      (check #f (list= = '(1 2 3) '(1 2 3) '(4 5 6))))

    (define check-0035
      ;; Checks that l0 is not being used when testing l2, cf spec
      (check #t (list= (lambda (a b) (not (eq? a b))) '(#f #f #f) '(#t #t #t) '(#f #f #f))))

    (define check-0036
      (check 1 (first '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0037
      (check 2 (second '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0038
      (check 3 (third '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0039
      (check 4 (fourth '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0040
      (check 5 (fifth '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0041
      (check 6 (sixth '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0042
      (check 7 (seventh '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0043
      (check 8 (eighth '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0044
      (check 9 (ninth '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0045
      (check 10 (tenth '(1 2 3 4 5 6 7 8 9 10))))

    (define check-0046
      (check #t (let-values (((a b) (car+cdr (cons 1 2))))
                 (and (= a 1) (= b 2)))))

    (define check-0047
      (check '(1 2 3) (take '(1 2 3 4 5 6) 3)))

    (define check-0048
      (check '(1) (take '(1) 1)))

    (define check-0049
      (let ((x (list 1 2 3 4 5 6)))
        (check (cdddr x) (drop x 3))))

    (define check-0050
      (let ((x (list 1 2 3)))
        (check #t (eq? x (drop x 0)))))

    (define check-0051
      (check '(4 5 6) (take-right '(1 2 3 4 5 6) 3)))

    (define check-0052
      (check '() (take-right '(1 2 3 4 5 6) 0)))

    (define check-0053
      (check '(2 3 . 4) (take-right '(1 2 3 . 4) 2)))

    (define check-0054
      (check 4 (take-right '(1 2 3 . 4) 0)))

    (define check-0055
      (check '(1 2 3) (drop-right '(1 2 3 4 5 6) 3)))

    (define check-0056
      (check '(1 2 3) (drop-right '(1 2 3) 0)))

    (define check-0057
      (check '(1 2 3) (drop-right '(1 2 3 . 4) 0)))

    (define check-0058
      (check #t (let ((x (list 1 2 3 4 5 6)))
                 (let ((y (take! x 3)))
	           (and (eq? x y)
	                (eq? (cdr x) (cdr y))
	                (eq? (cddr x) (cddr y))
	                (equal? y '(1 2 3)))))))

    (define check-0059
      (check #t (let ((x (list 1 2 3 4 5 6)))
                 (let ((y (drop-right! x 3)))
	           (and (eq? x y)
	                (eq? (cdr x) (cdr y))
	                (eq? (cddr x) (cddr y))
	                (equal? y '(1 2 3)))))))

    (define check-0060
      (check #t (let-values (((a b) (split-at '(1 2 3 4 5 6) 2)))
                 (and (equal? a '(1 2))
	              (equal? b '(3 4 5 6))))))

    (define check-0061
      (check #t (let* ((x (list 1 2 3 4 5 6))
	              (y (cddr x)))
                 (let-values (((a b) (split-at! x 2)))
                   (and (equal? a '(1 2))
	                (eq? a x)
	                (equal? b '(3 4 5 6))
	                (eq? b y))))))

    (define check-0062
      (check #t (= 37 (last '(1 2 3 37)))))

    (define check-0063
      (check #f (length+ (circular-list 1 2 3))))

    (define check-0064
      (check 4 (length+ '(1 2 3 4))))

    (define check-0065
      (check #t (let ((x (list 1 2))
	             (y (list 3 4))
	             (z (list 5 6)))
                 (let ((r (append! x y '() z)))
	           (and (equal? r '(1 2 3 4 5 6))
	                (eq? r x)
	                (eq? (cdr r) (cdr x))
	                (eq? (cddr r) y)
	                (eq? (cdddr r) (cdr y))
	                (eq? (cddddr r) z)
	                (eq? (cdr (cddddr r)) (cdr z)))))))

    (define check-0066
      (check '(1 2 3 4 5 6 7 8 9) (concatenate '((1 2 3) (4 5 6) () (7 8 9))) ))

    (define check-0067
      (check '(1 2 3 4 5 6 7 8 9)
            (concatenate! `(,(list 1 2 3) ,(list 4 5 6) () ,(list 7 8 9)))))

    (define check-0068
      (check '(1 2 3 4 5 6) (append-reverse '(3 2 1) '(4 5 6))))

    (define check-0069
      (check '(1 2 3 4 5 6) (append-reverse! '(3 2 1) '(4 5 6))))

    (define check-0070
      (check  '((1 4) (2 5) (3 6)) (zip '(1 2 3) '(4 5 6))))

    (define check-0071
      (check '() (zip '() '() '() '())))

    (define check-0072
      (check '((1 1)) (zip '(1) (circular-list 1 2))))

    (define check-0073
      (check '(1 2 3 4 5) (unzip1 '((1) (2) (3) (4) (5)))))

    (define check-0074
      (check #t
            (let-values (((a b) (unzip2 '((10 11) (20 21) (30 31)))))
              (and (equal? a '(10 20 30))
               	   (equal? b '(11 21 31))))))

    (define check-0075
      (check #t (let-values (((a b c) (unzip3 '((10 11 12) (20 21 22) (30 31 32)))))
                 (and (equal? a '(10 20 30))
	              (equal? b '(11 21 31))
	              (equal? c '(12 22 32))))))

    (define check-0076
      (check #t (let-values (((a b c d) (unzip4 '((10 11 12 13)
				                 (20 21 22 23)
				                 (30 31 32 33)))))
                 (and (equal? a '(10 20 30))
	              (equal? b '(11 21 31))
	              (equal? c '(12 22 32))
	              (equal? d '(13 23 33))))))

    (define check-0077
      (check #t (let-values (((a b c d e) (unzip5 '((10 11 12 13 14)
					           (20 21 22 23 24)
					           (30 31 32 33 34)))))
                 (and (equal? a '(10 20 30))
	              (equal? b '(11 21 31))
	              (equal? c '(12 22 32))
	              (equal? d '(13 23 33))
	              (equal? e '(14 24 34))))))

    (define check-0078
      (check 3 (count even? '(3 1 4 1 5 9 2 5 6))))

    (define check-0079
      (check 3 (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16))))

    (define check-0080
      (check 2 (count < '(3 1 4 1) (circular-list 1 10))))

    (define check-0081
      (check '(c 3 b 2 a 1) (fold cons* '() '(a b c) '(1 2 3 4 5))))

    (define check-0082
      (check '(a 1 b 2 c 3) (fold-right cons* '() '(a b c) '(1 2 3 4 5))))

    (define check-0083
      (check #t (let* ((x (list 1 2 3))
	              (r (list x (cdr x) (cddr x)))
	              (y (pair-fold (lambda (pair tail)
			              (set-cdr! pair tail) pair)
			            '()
			            x)))
                 (and (equal? y '(3 2 1))
	              (every (lambda (c) (memq c r)) (list y (cdr y) (cddr y)))
                      #t))))

    (define check-0084
      (check '((a b c) (b c) (c)) (pair-fold-right cons '() '(a b c))))

    (define check-0085
      (check 5 (reduce max 'illegal '(1 2 3 4 5))))

    (define check-0086
      (check 0 (reduce max 0 '())))

    (define check-0087
      (check '(1 2 3 4 5) (reduce-right append 'illegal '((1 2) () (3 4 5)))))

    (define check-0088
      (check '(1 4 9 16 25 36 49 64 81 100)
	    (unfold (lambda (x) (> x 10))
		    (lambda (x) (* x x))
		    (lambda (x) (+ x 1))
		    1)))

    (define check-0089
      (check '(1 4 9 16 25 36 49 64 81 100)
	    (unfold-right zero?
			  (lambda (x) (* x x))
			  (lambda (x) (- x 1))
			  10)))

    (define check-0090
      (check '(4 1 5 1)
	    (map + '(3 1 4 1) (circular-list 1 0))))

    (define check-0091
      (check '(5 4 3 2 1)
	    (let ((v 1)
		  (l '()))
	      (for-each (lambda (x y)
			  (let ((n v))
			    (set! v (+ v 1))
			    (set! l (cons n l))))
			'(0 0 0 0 0)
			(circular-list 1 2))
	      l)))

    (define check-0092
      (check '(1 -1 3 -3 8 -8)
	    (append-map (lambda (x) (list x (- x))) '(1 3 8))))

    (define check-0093
      (check '(1 -1 3 -3 8 -8)
	    (append-map! (lambda (x) (list x (- x))) '(1 3 8))))

    (define check-0094
      (check #t (let* ((l (list 1 2 3))
	              (m (map! (lambda (x) (* x x)) l)))
                 (and (equal? m '(1 4 9))
	              (equal? l '(1 4 9))))))

    (define check-0095
      (check '(1 2 3 4 5)
	    (let ((v 1))
	      (map-in-order (lambda (x)
			      (let ((n v))
				(set! v (+ v 1))
				n))
			    '(0 0 0 0 0)))))

    (define check-0096
      (check '((3) (2 3) (1 2 3))
	    (let ((xs (list 1 2 3))
		  (l '()))
	      (pair-for-each (lambda (x) (set! l (cons x l))) xs)
	      l)))

    (define check-0097
      (check '(1 9 49)
            (filter-map (lambda (x y) (and (number? x) (* x x)))
			'(a 1 b 3 c 7)
			(circular-list 1 2))))

    (define check-0098
      (check '(0 8 8 -4) (filter even? '(0 7 8 8 43 -4))))

    (define check-0099
      (check #t (let-values (((a b) (partition symbol? '(one 2 3 four five 6))))
                 (and (equal? a '(one four five))
	              (equal? b '(2 3 6))))))

    (define check-0100
      (check '(7 43) (remove even? '(0 7 8 8 43 -4))))

    (define check-0101
      (check #t (let* ((x (list 0 7 8 8 43 -4))
	              (y (pair-fold cons '() x))
	              (r (filter! even? x)))
                 (and (equal? '(0 8 8 -4) r)
	              (every (lambda (c) (memq c y)) (pair-fold cons '() r))
                      #t))))

    (define check-0102
      (check #t (let* ((x (list 'one 2 3 'four 'five 6))
	              (y (pair-fold cons '() x)))
                 (let-values (((a b) (partition! symbol? x)))
	           (and (equal? a '(one four five))
	                (equal? b '(2 3 6))
	                (every (lambda (c) (memq c y)) (pair-fold cons '() a))
	                (every (lambda (c) (memq c y)) (pair-fold cons '() b))
                        #t)))))

    (define check-0103
      (check #t (let* ((x (list 0 7 8 8 43 -4))
	              (y (pair-fold cons '() x))
	              (r (remove! even? x)))
                 (and (equal? '(7 43) r)
	              (every (lambda (c) (memq c y)) (pair-fold cons '() r))
                      #t))))

    (define check-0104
      (check 4 (find even? '(3 1 4 1 5 9 8))))

    (define check-0105
      (check '(4 1 5 9 8) (find-tail even? '(3 1 4 1 5 9 8))))

    (define check-0106
      (check '#f (find-tail even? '(1 3 5 7))))

    (define check-0107
      (check '(2 18) (take-while even? '(2 18 3 10 22 9))))

    (define check-0108
      (check #t (let* ((x (list 2 18 3 10 22 9))
	              (r (take-while! even? x)))
                 (and (equal? r '(2 18))
	              (eq? r x)
	              (eq? (cdr r) (cdr x))))))

    (define check-0109
      (check '(3 10 22 9) (drop-while even? '(2 18 3 10 22 9))))

    (define check-0110
      (check #t (let-values (((a b) (span even? '(2 18 3 10 22 9))))
                 (and (equal? a '(2 18))
	              (equal? b '(3 10 22 9))))))

    (define check-0111
      (check #t (let-values (((a b) (break even? '(3 1 4 1 5 9))))
                 (and (equal? a '(3 1))
	              (equal? b '(4 1 5 9))))))

    (define check-0112
      (check #t (let* ((x (list 2 18 3 10 22 9))
	              (cells (pair-fold cons '() x)))
                 (let-values (((a b) (span! even? x)))
                   (and (equal? a '(2 18))
	                (equal? b '(3 10 22 9))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() a))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() b))
                        #t)))))

    (define check-0113
      (check #t (let* ((x     (list 3 1 4 1 5 9))
	              (cells (pair-fold cons '() x)))
                 (let-values (((a b) (break! even? x)))
                   (and (equal? a '(3 1))
	                (equal? b '(4 1 5 9))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() a))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() b))
                        #t)))))

    (define check-0114
      (check #t (any integer? '(a 3 b 2.7))))

    (define check-0115
      (check #f (any integer? '(a 3.1 b 2.7))))

    (define check-0116
      (check #t (any < '(3 1 4 1 5) (circular-list 2 7 1 8 2))))

    (define check-0117
      (check 'yes (any (lambda (a b) (if (< a b) 'yes #f))
		      '(1 2 3) '(0 1 4))))

    (define check-0118
      (check #t (every integer? '(1 2 3))))

    (define check-0119
      (check #f (every integer? '(3 4 5.1))))

    (define check-0120
      (check #t (every < '(1 2 3) (circular-list 2 3 4))))

    (define check-0121
      (check 2 (list-index even? '(3 1 4 1 5 9))))

    (define check-0122
      (check 1 (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

    (define check-0123
      (check #f (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

    (define check-0124
      (check '(37 48) (member 5 '(1 2 5 37 48) <)))

    (define check-0125
      (check '(1 2 5) (delete 5 '(1 48 2 5 37) <)))

    (define check-0126
      (check '(1 2 7) (delete 5 '(1 5 2 5 7))))

    (define check-0127
      (check #t (let* ((x (list 1 48 2 5 37))
	              (cells (pair-fold cons '() x))
	              (r (delete! 5 x <)))
                 (and (equal? r '(1 2 5))
	              (every (lambda (x) (memq x cells)) (pair-fold cons '() r))
                      #t))))

    (define check-0128
      (check '((a . 3) (b . 7) (c . 1))
	    (delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
			       (lambda (x y) (eq? (car x) (car y))))))

    (define check-0129
      (check '(a b c z) (delete-duplicates '(a b a c a b c z) eq?)))

    (define check-0130
      (check '((a b c z))
            (let* ((x     (list 'a 'b 'a 'c 'a 'b 'c 'z))
	           (cells (pair-fold cons '() x))
	           (r     (delete-duplicates! x)))
              (and (equal? '(a b c z) r)
	           (every (lambda (x) (memq x cells)) (pair-fold cons '() r))))))

    (define check-0131
      (check '(3 . #t) (assoc 6
			     '((4 . #t) (3 . #t) (5 . #t))
			     (lambda (x y)
			       (zero? (remainder x y))))))

    (define check-0132
      (check '((1 . #t) (2 . #f)) (alist-cons 1 #t '((2 . #f)))))

    (define check-0133
      (check #t (let* ((a (list (cons 1 2) (cons 3 4)))
	              (b (alist-copy a)))
                 (and (equal? a b)
	              (every (lambda (x) (not (memq x b))) a)
	              (every (lambda (x) (not (memq x a))) b)))))

    (define check-0134
      (check '((1 . #t) (2 . #t) (4 . #t))
	    (alist-delete 5 '((1 . #t) (2 . #t) (37 . #t) (4 . #t) (48 #t)) <)))

    (define check-0135
      (check '((1 . #t) (2 . #t) (4 . #t))
	    (alist-delete 7 '((1 . #t) (2 . #t) (7 . #t) (4 . #t) (7 #t)))))

    (define check-0136
      (check '((4 . #t) (7 . #t))
            (let* ((x (list-copy '((1 . #t) (2 . #t) (7 . #t) (4 . #t) (7 . #t))))
	           (y (list-copy x))
	           (cells (pair-fold cons '() x))
	           (r (alist-delete! 7 x)))
              (and (equal? r '((1 . #t) (2 . #t) (4 . #t)))
	           (every (lambda (x) (memq x cells)) (pair-fold cons '() r))
	           (every (lambda (x) (memq x y)) r)))))

    (define check-0137
      (check #t (lset<= eq? '(a) '(a b a) '(a b c c))))

    (define check-0138
      (check #f (lset<= eq? '(a) '(a b a) '(a))))

    (define check-0139
      (check #t (lset<= eq?)))

    (define check-0140
      (check #t (lset<= eq? '(a))))

    (define check-0141
      (check #t (lset= eq? '(b e a) '(a e b) '(e e b a))))

    (define check-0142
      (check #f (lset= eq? '(b e a) '(a e b) '(e e b a c))))

    (define check-0143
      (check #t (lset= eq?)))

    (define check-0144
      (check #t (lset= eq? '(a))))

    (define check-0145
      (check '(u o i a b c d c e)
	    (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)))

    (define check-0146
      (check '(u o i a b c d e)
	    (lset-union eq? '(a b c d e) '(a e i o u))))

    (define check-0147
      (check '(x a a c) (lset-union eq? '(a a c) '(x a x))))

    (define check-0148
      (check #t (null? (lset-union eq?))))

    (define check-0149
      (check '(a b c) (lset-union eq? '(a b c))))

    (define check-0150
      (check '(a e) (lset-intersection eq? '(a b c d e) '(a e i o u))))

    (define check-0151
      (check '(a x a) (lset-intersection eq? '(a x y a) '(x a x z))))

    (define check-0152
      (check '(a b c) (lset-intersection eq? '(a b c))))

    (define check-0153
      (check '(b c d) (lset-difference eq? '(a b c d e) '(a e i o u))))

    (define check-0154
      (check '(a b c) (lset-difference eq? '(a b c))))

    (define check-0155
      (let ((x '(d c b i o u)))
        (check x (lset= eq? x (lset-xor eq? '(a b c d e) '(a e i o u))))))

    (define check-0156
      (check #t (lset= eq? '() (lset-xor eq?))))

    (define check-0157
      (let ((x '(a b c d e)))
        (check '(e) (lset= eq? x (lset-xor eq? '(a b c d e))))))

    (define check-0158
      (check #t (let-values (((d i) (lset-diff+intersection eq? '(a b c d e) '(c d f))))
                 (and (equal? d '(a b e))
	              (equal? i '(c d))))))

    ;; FIXME: For the following five procedures, need to check that
    ;; cells returned are from the arguments.

    (define check-0159
      (check '(u o i a b c d e)
	    (lset-union! eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))))

    (define check-0160
      (check '(x a a c) (lset-union! eq? (list 'a 'a 'c) (list 'x 'a 'x))))

    (define check-0161
      (check #t (null? (lset-union! eq?))))

    (define check-0162
      (check '(a b c) (lset-union! eq? (list 'a 'b 'c))))

    (define check-0163
      (check '(a e) (lset-intersection! eq?
                                       (list 'a 'b 'c 'd 'e)
				       (list 'a 'e 'i 'o 'u))))

    (define check-0164
      (check '(a x a) (lset-intersection! eq?
                                         (list 'a 'x 'y 'a)
					 (list 'x 'a 'x 'z))))
    (define check-0165
      (check '(a b c) (lset-intersection! eq? (list 'a 'b 'c))))

    (define check-0166
      (check '(b c d) (lset-difference! eq?
                                       (list 'a 'b 'c 'd 'e)
				       (list 'a 'e 'i 'o 'u))))

    (define check-0167
      (check '(a b c) (lset-difference! eq? (list 'a 'b 'c))))

    (define check-0168
      (let ((x '(d c b i o u)))
        (check x (lset= eq? x
                       (lset-xor! eq?
                                  (list 'a 'b 'c 'd 'e)
				  (list 'a 'e 'i 'o 'u))))))

    (define check-0169
      (check #t (lset= eq? '() (lset-xor! eq?))))

    (define check-0170
      (check '(e) (lset= eq? '(a b c d e) (lset-xor! eq? (list 'a 'b 'c 'd 'e)))))

    (define check-0171
      (check #t (let-values (((d i) (lset-diff+intersection! eq?
                                                            (list 'a 'b 'c 'd 'e)
						            (list 'c 'd 'f))))
                 (and (equal? d '(a b e))
	              (equal? i '(c d))))))))
