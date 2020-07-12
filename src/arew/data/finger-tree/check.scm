(define-library (arew data finger-tree check)
  (import
   (arew data finger-tree)
   (scheme generator)
   (scheme base)
   (scheme list)
   (check))

  (export check-000 check-001 check-002 check-003 check-004 check-005
          check-006 check-007 check-008 check-009 check-010 check-011 check-012
          check-013 check-014 check-015 check-016 check-017 check-018
          check-018.1 check-018.2 check-018.3 check-019 check-020 check-021
          check-022 check-023 check-024 check-025 check-026 check-027 check-028
          check-029 check-030 check-031 check-032 check-033 check-034 check-035
          check-036 check-037 check-038 check-039 check-040 check-041 check-042
          check-043 check-044 check-045 check-046 check-047 check-048 check-049
          check-050 check-051 check-052 check-053 check-054 check-055 check-056
          check-057 check-058 check-059 check-060 check-061 check-062 check-063
          check-064 check-065 check-066 check-067 check-068 check-069 check-070
          check-071 check-072 check-073 check-074 check-075 check-076 check-077
          check-078 check-079 check-080 check-081 check-082 check-083 check-084
          check-085 check-086 check-087 check-088 check-089 check-090 check-091
          check-092 check-093 check-094 check-095 check-096 check-097 check-098
          check-099 check-100 check-101 check-102 check-103 check-104 check-105
          check-106 check-107 check-108 check-109 check-110 check-111 check-112
          check-113 check-114 check-115 check-116 check-117 check-118 check-119
          check-120 check-121 check-122 check-123 check-124 check-125 check-126
          check-127 check-128 check-129 check-130 check-131 check-132 check-133
          check-134 check-135 check-136 check-137 check-138 check-139 check-140
          check-141 check-142 check-143 check-144 check-145 check-146 check-147
          check-148 check-149 check-150 check-151 check-152 check-153 check-154
          check-155 check-156 check-157 check-158 check-159 check-160 check-161
          check-162 check-163 check-164 check-165 check-166 check-167 check-168
          check-169 check-170 check-171 check-172 check-173 check-174 check-175
          check-176 check-177 check-178 check-179 check-180 check-181 check-182
          check-183 check-184 check-185 check-186 check-187 check-188 check-189
          check-190 check-191 check-192 check-193 check-194 check-195 check-196
          check-197 check-198 check-199 check-200 check-201 check-202 check-203
          check-204 check-205 check-206 check-207 check-208 check-209 check-210
          check-211 check-212 check-213 check-214 check-215 check-216 check-217
          check-218 check-219 check-220 check-221 check-222 check-223 check-224
          check-225 check-226 check-227 check-228 check-229 check-230 check-231
          check-232 check-233 check-234 check-235 check-236 check-237 check-238
          check-generators check-239 check-240 check-241 check-242 check-243
          check-244 check-245 check-246 check-247 check-248 check-249 check-250
          check-251 check-252 check-253 check-254)

  (begin

    (define (add left right)
      right)

    (define (measure element)
      element)

    (define (naturals n)
      (list->finger-tree add measure (iota n)))

    ;; empty case
    (define t0 (make-finger-tree))
    ;; single case
    (define t1 (naturals 1))
    ;; smallest deep case
    (define t2 (naturals 2))

    ;; deep with one full digit
    (define t5 (naturals 5))
    (define t8 (naturals 8))       ;; deep with both digits full, empty spine
    (define t9 (naturals 9))       ;; smallest deep with nonempty spine
    (define t33 (naturals 33))     ;; depth 3
    (define t999 (naturals 999))   ;; large (ish)

    (define seed '(1))

    (define (proc element accum)
      (cons (remainder (+ (* 7 (car accum))
                          element)
                       10)
            accum))

  ;;; make-finger-tree
    (define check-000
      (check #t (finger-tree? t0)))

    (define check-001
      (check #t (finger-tree-empty? t0)))

    (define check-002
      (check 0 (finger-tree-length t0)))

    ;; finger-tree

    (define check-003
      (let ((n 0))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-004
      (let ((n 1))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-005
      (let ((n 2))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-006
      (let ((n 3))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-007
      (let ((n 4))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-008
      (let ((n 4))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-009
      (let ((n 5))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-010
      (let ((n 6))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-011
      (let ((n 7))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-012
      (let ((n 8))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-013
      (let ((n 9))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-014
      (let ((n 10))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-015
      (let ((n 11))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-016
      (let ((n 12))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-017
      (let ((n 13))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-018
      (let ((n 14))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-018.1
      (let ((n 15))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

    (define check-018.2
      (let ((n 16))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))


    (define check-018.3
      (let ((n 33))
        (check (iota n)
               (finger-tree->list
                (apply finger-tree add measure (iota n))))))

  ;;; finger-tree?

    (define check-019
      (check #t (finger-tree? t0)))

    (define check-020
      (check #t (finger-tree? t1)))

    (define check-021
      (check #t (finger-tree? t2)))

    (define check-022
      (check #t (finger-tree? t5)))

    (define check-023
      (check #t (finger-tree? t8)))

    (define check-024
      (check #t (finger-tree? t9)))

    (define check-025
      (check #t (finger-tree? t33)))

    (define check-026
      (check #t (finger-tree? t999)))

    (define check-027
      (check #f (finger-tree? 42)))

    (define check-028
      (check #f (finger-tree? (list 42))))

    ;; finger-tree-empty?
    (define check-029
      (check #t (finger-tree-empty? t0)))

    (define check-030
      (check #f (finger-tree-empty? t1)))

    (define check-031
      (check #f (finger-tree-empty? t2)))

    (define check-032
      (check #f (finger-tree-empty? t5)))

    (define check-033
      (check #f (finger-tree-empty? t8)))

    (define check-034
      (check #f (finger-tree-empty? t9)))

    (define check-035
      (check #f (finger-tree-empty? t33)))

    (define check-036
      (check #f (finger-tree-empty? t999)))

    ;; finger-tree-length
    (define check-037
      (check 0 (finger-tree-length t0)))

    (define check-038
      (check 1 (finger-tree-length t1)))

    (define check-039
      (check 2 (finger-tree-length t2)))

    (define check-040
      (check 5 (finger-tree-length t5)))

    (define check-041
      (check 8 (finger-tree-length t8)))

    (define check-042
      (check 9 (finger-tree-length t9)))

    (define check-043
      (check 33 (finger-tree-length t33)))

    (define check-044
      (check 999 (finger-tree-length t999)))

    ;; finger-tree-left
    (define check-045
      (check 0 (finger-tree-left t1)))

    (define check-046
      (check 0 (finger-tree-left t2)))

    (define check-047
      (check 0 (finger-tree-left t5)))

    (define check-048
      (check 0 (finger-tree-left t8)))

    (define check-049
      (check 0 (finger-tree-left t9)))

    (define check-050
      (check 0 (finger-tree-left t33)))

    (define check-051
      (check 0 (finger-tree-left t999)))

    ;; finger-tree-right
    (define check-052
      (check 0 (finger-tree-right t1)))

    (define check-053
      (check 1 (finger-tree-right t2)))

    (define check-054
      (check 4 (finger-tree-right t5)))

    (define check-055
      (check 7 (finger-tree-right t8)))

    (define check-056
      (check 8 (finger-tree-right t9)))

    (define check-057
      (check 32 (finger-tree-right t33)))

    (define check-058
      (check 998 (finger-tree-right t999)))

    ;; finger-tree-add-left

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax-rule (check-add-left t)
      (check (cons #t (finger-tree->list t))
             (finger-tree->list (finger-tree-add-left add measure t #t))))

    (define check-059
      (check-add-left t0))

    (define check-060
      (check-add-left t1))

    (define check-061
      (check-add-left t2))

    (define check-062
      (check-add-left t5))

    (define check-063
      (check-add-left t8))

    (define check-064
      (check-add-left t9))

    (define check-065
      (check-add-left t33))

    (define check-066
      (check-add-left t999))

    ;; finger-tree-test-add-right
    (define-syntax-rule (check-add-right t)
      (check (append (finger-tree->list t)
                     (list #t))
             (finger-tree->list (finger-tree-add-right add measure t #t))))

    (define check-067
      (check-add-right t0))

    (define check-068
      (check-add-right t1))

    (define check-069
      (check-add-right t2))

    (define check-070
      (check-add-right t5))

    (define check-071
      (check-add-right t8))

    (define check-072
      (check-add-right t9))

    (define check-073
      (check-add-right t33))

    (define check-074
      (check-add-right t999))

    ;; finger-tree-test-remove-left
    (define-syntax-rule (check-remove-left t)
      (check (cdr (finger-tree->list t))
             (finger-tree->list (finger-tree-remove-left t))))

    (define check-075
      (check-remove-left t1))

    (define check-076
      (check-remove-left t2))

    (define check-077
      (check-remove-left t5))

    (define check-078
      (check-remove-left t8))

    (define check-079
      (check-remove-left t9))

    (define check-080
      (check-remove-left t33))

    (define check-081
      (check-remove-left t999))

    ;; finger-tree-test-remove-right
    (define-syntax-rule (check-remove-right t)
      (check (drop-right (finger-tree->list t) 1)
             (finger-tree->list (finger-tree-remove-right t))))

    (define check-082
      (check-remove-right t1))

    (define check-083
      (check-remove-right t2))

    (define check-084
      (check-remove-right t5))

    (define check-085
      (check-remove-right t8))

    (define check-086
      (check-remove-right t9))

    (define check-087
      (check-remove-right t33))

    (define check-088
      (check-remove-right t999))

    ;; finger-tree-append
    (define-syntax-rule (check-append left right)
      (check (append (finger-tree->list left)
                     (finger-tree->list right))
             (finger-tree->list
              (finger-tree-append add measure left right))))

    (define check-089
      (check-append t0 t0))

    (define check-090
      (check-append t0 t1))
    (define check-091
      (check-append t0 t2))

    (define check-092
      (check-append t0 t5))

    (define check-093
      (check-append t0 t8))

    (define check-094
      (check-append t0 t9))

    (define check-095
      (check-append t0 t33))

    (define check-096
      (check-append t0 t999))

    (define check-097
      (check-append t1 t0))

    (define check-098
      (check-append t1 t1))

    (define check-099
      (check-append t1 t2))

    (define check-100
      (check-append t1 t5))

    (define check-101
      (check-append t1 t8))

    (define check-102
      (check-append t1 t9))

    (define check-103
      (check-append t1 t33))

    (define check-104
      (check-append t1 t999))

    (define check-105
      (check-append t2 t0))

    (define check-106
      (check-append t2 t1))

    (define check-107
      (check-append t2 t2))

    (define check-108
      (check-append t2 t5))

    (define check-109
      (check-append t2 t8))

    (define check-110
      (check-append t2 t9))

    (define check-111
      (check-append t2 t33))

    (define check-112
      (check-append t2 t999))

    (define check-113
      (check-append t5 t0))

    (define check-114
      (check-append t5 t1))

    (define check-115
      (check-append t5 t2))

    (define check-116
      (check-append t5 t5))

    (define check-117
      (check-append t5 t8))

    (define check-118
      (check-append t5 t9))

    (define check-119
      (check-append t5 t33))

    (define check-120
      (check-append t5 t999))

    (define check-121
      (check-append t8 t0))

    (define check-122
      (check-append t8 t1))

    (define check-123
      (check-append t8 t2))

    (define check-124
      (check-append t8 t5))

    (define check-125
      (check-append t8 t8))

    (define check-126
      (check-append t8 t9))

    (define check-127
      (check-append t8 t33))

    (define check-128
      (check-append t8 t999))

    (define check-129
      (check-append t9 t0))

    (define check-130
      (check-append t9 t1))

    (define check-131
      (check-append t9 t2))

    (define check-132
      (check-append t9 t5))

    (define check-133
      (check-append t9 t8))

    (define check-134
      (check-append t9 t9))

    (define check-135
      (check-append t9 t33))

    (define check-136
      (check-append t9 t999))

    (define check-137
      (check-append t33 t0))

    (define check-138
      (check-append t33 t1))

    (define check-139
      (check-append t33 t2))

    (define check-140
      (check-append t33 t5))

    (define check-141
      (check-append t33 t8))

    (define check-142
      (check-append t33 t9))

    (define check-143
      (check-append t33 t33))

    (define check-144
      (check-append t33 t999))

    (define check-145
      (check-append t999 t0))

    (define check-146
      (check-append t999 t1))

    (define check-147
      (check-append t999 t2))

    (define check-148
      (check-append t999 t5))

    (define check-149
      (check-append t999 t8))

    (define check-150
      (check-append t999 t9))

    (define check-151
      (check-append t999 t33))

    (define check-152
      (check-append t999 t999))

    ;; three operands
    (define check-153
      (check (append (finger-tree->list t33)
                     (finger-tree->list t9)
                     (finger-tree->list t999))
             (finger-tree->list
              (finger-tree-append add measure t33 t9 t999))))

    (define check-154
      ;; five operands
      (check (append (finger-tree->list t999)
                     (finger-tree->list t33)
                     (finger-tree->list t1)
                     (finger-tree->list t2)
                     (finger-tree->list t999))
             (finger-tree->list
              (finger-tree-append add measure t999 t33 t1 t2 t999))))

    ;; finger-tree-filter
    (define-syntax-rule (test-finger-tree-filter/proc proc t)
      (check (filter proc (finger-tree->list t))
             (finger-tree->list (finger-tree-filter add measure proc t))))

    (define check-155
      (test-finger-tree-filter/proc number? t0))

    (define check-156
      (test-finger-tree-filter/proc pair? t0))

    (define check-157
      (test-finger-tree-filter/proc even? t0))

    (define check-158
      (test-finger-tree-filter/proc odd? t0))

    (define check-159
      (test-finger-tree-filter/proc number? t1))

    (define check-160
      (test-finger-tree-filter/proc pair? t1))

    (define check-161
      (test-finger-tree-filter/proc even? t1))

    (define check-162
      (test-finger-tree-filter/proc odd? t1))

    (define check-163
      (test-finger-tree-filter/proc number? t2))

    (define check-164
      (test-finger-tree-filter/proc pair? t2))

    (define check-165
      (test-finger-tree-filter/proc even? t2))

    (define check-166
      (test-finger-tree-filter/proc odd? t2))

    (define check-167
      (test-finger-tree-filter/proc number? t5))

    (define check-168
      (test-finger-tree-filter/proc pair? t5))

    (define check-169
      (test-finger-tree-filter/proc even? t5))

    (define check-170
      (test-finger-tree-filter/proc odd? t5))

    (define check-171
      (test-finger-tree-filter/proc number? t8))

    (define check-172
      (test-finger-tree-filter/proc pair? t8))

    (define check-173
      (test-finger-tree-filter/proc even? t8))

    (define check-174
      (test-finger-tree-filter/proc odd? t8))

    (define check-175
      (test-finger-tree-filter/proc number? t9))

    (define check-176
      (test-finger-tree-filter/proc pair? t9))

    (define check-177
      (test-finger-tree-filter/proc even? t9))

    (define check-178
      (test-finger-tree-filter/proc odd? t9))

    (define check-179
      (test-finger-tree-filter/proc number? t33))

    (define check-180
      (test-finger-tree-filter/proc pair? t33))

    (define check-181
      (test-finger-tree-filter/proc even? t33))

    (define check-182
      (test-finger-tree-filter/proc odd? t33))

    (define check-183
      (test-finger-tree-filter/proc number? t999))

    (define check-184
      (test-finger-tree-filter/proc pair? t999))

    (define check-185
      (test-finger-tree-filter/proc even? t999))

    (define check-186
      (test-finger-tree-filter/proc odd? t999))

    ;; finger-tree-fold-left
    (define check-186.1
      (check (fold proc seed (finger-tree->list t0))
             (finger-tree-fold-left proc seed t0)))

    ;; finger-tree-fold-right
    (define check-187
      (check (fold-right proc seed (finger-tree->list t0))
             (finger-tree-fold-right proc seed t0)))

    ;; finger-tree-for-each
    (define check-188
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t0)
               accum)
             (fold proc seed (finger-tree->list t0))))

    ;; finger-tree-fold-left
    (define check-189
      (check (fold proc seed (finger-tree->list t1))
             (finger-tree-fold-left proc seed t1)))

    ;; finger-tree-fold-right
    (define check-190
      (check (fold-right proc seed (finger-tree->list t1))
             (finger-tree-fold-right proc seed t1)))

    ;; finger-tree-for-each
    (define check-191
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t1)
               accum)
             (fold proc seed (finger-tree->list t1))))

    ;; finger-tree-fold-left
    (define check-192
      (check (fold proc seed (finger-tree->list t2))
             (finger-tree-fold-left proc seed t2)))

    ;; finger-tree-fold-right
    (define check-193
      (check (fold-right proc seed (finger-tree->list t2))
             (finger-tree-fold-right proc seed t2)))

    ;; finger-tree-for-each
    (define check-194
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t2)
               accum)
             (fold proc seed (finger-tree->list t2))))


    ;; finger-tree-fold-left
    (define check-195
      (check (fold proc seed (finger-tree->list t5))
             (finger-tree-fold-left proc seed t5)))

    ;; finger-tree-fold-right
    (define check-196
      (check (fold-right proc seed (finger-tree->list t5))
             (finger-tree-fold-right proc seed t5)))

    ;; finger-tree-for-each
    (define check-197
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t5)
               accum)
             (fold proc seed (finger-tree->list t5))))

    ;; finger-tree-fold-left
    (define check-198
      (check (fold proc seed (finger-tree->list t8))
             (finger-tree-fold-left proc seed t8)))

    ;; finger-tree-fold-right
    (define check-199
      (check (fold-right proc seed (finger-tree->list t8))
             (finger-tree-fold-right proc seed t8)))

    ;; finger-tree-for-each
    (define check-200
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t8)
               accum)
             (fold proc seed (finger-tree->list t8))))


    ;; finger-tree-fold-left
    (define check-201
      (check (fold proc seed (finger-tree->list t9))
             (finger-tree-fold-left proc seed t9)))

    ;; finger-tree-fold-right
    (define check-202
      (check (fold-right proc seed (finger-tree->list t9))
             (finger-tree-fold-right proc seed t9)))

    ;; finger-tree-for-each
    (define check-203
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t9)
               accum)
             (fold proc seed (finger-tree->list t9))))

    ;; finger-tree-fold-left
    (define check-204
      (check (fold proc seed (finger-tree->list t33))
             (finger-tree-fold-left proc seed t33)))

    ;; finger-tree-fold-right
    (define check-205
      (check (fold-right proc seed (finger-tree->list t33))
             (finger-tree-fold-right proc seed t33)))

    ;; finger-tree-for-each
    (define check-206
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t33)
               accum)
             (fold proc seed (finger-tree->list t33))))

    ;; finger-tree-fold-left
    (define check-207
      (check (fold proc seed (finger-tree->list t999))
             (finger-tree-fold-left proc seed t999)))

    ;; finger-tree-fold-right
    (define check-208
      (check (fold-right proc seed (finger-tree->list t999))
             (finger-tree-fold-right proc seed t999)))

    ;; finger-tree-for-each
    (define check-209
      (check (let ((accum seed))
               (finger-tree-for-each (lambda (element)
                                       (set! accum (proc element accum)))
                                     t999)
               accum)
             (fold proc seed (finger-tree->list t999))))

    ;; multi-argument
    (define check-210
      (check (fold + 0 (finger-tree->list t33) (finger-tree->list t33) (finger-tree->list t9))
             (finger-tree-fold-left + 0 t33 t33 t9)))

    (define (dg a b c d)
      (pk a b c d)
      (+ a b c d))

    (define check-211
      (check (fold-right + 0
                         (finger-tree->list t33)
                         (finger-tree->list t33)
                         (finger-tree->list t9))
             (finger-tree-fold-right + 0 t33 t33 t9)))

    (define-syntax-rule (test-map t)
      (check (map - (finger-tree->list t))
             (finger-tree->list
              (finger-tree-map add measure - t))))

    (define check-212
      (test-map t0))

    (define check-213
      (test-map t1))

    (define check-214
      (test-map t2))

    (define check-215
      (test-map t5))

    (define check-216
      (test-map t8))

    (define check-217
      (test-map t9))

    (define check-218
      (test-map t33))

    (define check-219
      (test-map t999))

    ;; finger-tree-reverse
    (define-syntax-rule (test-reverse1 t)
      ;; reverse once
      (check (reverse (finger-tree->list t))
             (finger-tree->list (finger-tree-reverse add measure t))))

    (define-syntax-rule (test-reverse2 t)
      ;; reverse twice (identity)
      (check (finger-tree->list t)
             (finger-tree->list
              (finger-tree-reverse add measure
                                   (finger-tree-reverse add measure t)))))

    (define check-220
      (test-reverse1 t0))

    (define check-221
      (test-reverse1 t1))

    (define check-222
      (test-reverse1 t2))

    (define check-223
      (test-reverse1 t5))

    (define check-224
      (test-reverse1 t8))

    (define check-225
      (test-reverse1 t9))

    (define check-226
      (test-reverse1 t33))

    (define check-227
      (test-reverse1 t999))

    (define check-228
      (test-reverse2 t0))

    (define check-229
      (test-reverse2 t1))

    (define check-230
      (test-reverse2 t2))

    (define check-231
      (test-reverse2 t5))

    (define check-232
      (test-reverse2 t8))

    (define check-233
      (test-reverse2 t9))

    (define check-234
      (test-reverse2 t33))

    (define check-235
      (test-reverse2 t999))

    (define check-236
      ;; generator->finger-tree with specified length
      ;; use entire generator
      (check (iota 100)
             (finger-tree->list
              (generator->finger-tree add measure
                                      (make-iota-generator 100)
                                      100))))

    (define check-237
      ;; stop early
      (check (iota 95)
             (finger-tree->list
              (generator->finger-tree add measure
                                      (make-iota-generator 100)
                                      95))))
    (define check-238
      ;; empty
      (check '()
             (finger-tree->list
              (generator->finger-tree add measure (generator) 0))))


    (define check-generators
      (check #t
             (let ((out #t))
               (do ((n 0 (+ 1 n)))
                   ((= n 1000))
                 (let* ((lst (iota n))
                        (tree (list->finger-tree add measure lst)))

                   ;; list->finger-tree
                   (set! out (and out (equal? lst (finger-tree->list tree))))
                   (set! out
                         (and out (equal? lst
                                          (finger-tree->list
                                           (generator->finger-tree add
                                                                   measure
                                                                   (make-iota-generator n))))))

                   (set! out (and out (equal? lst
                                              (generator->list
                                               (finger-tree->generator tree)))))
                   (set! out (and out (equal? (reverse lst)
                                              (generator->list
                                               (finger-tree->reverse-generator tree)))))))
               out)))

    ;; finger-tree-scan
    (define-syntax-rule (test-scan e t)
      (check e
             (let ((out '())
                   (scan (lambda (query)
                           (finger-tree-scan add
                                             measure
                                             (lambda (element)
                                               (>= element query))
                                             #f
                                             t
                                             (lambda (element)
                                               (= element query))
                                             (lambda ()
                                               #f)))))
               (finger-tree-for-each (lambda (i)
                                       (set! out (cons (list (scan i)
                                                             (scan (+ i 1/2))
                                                             (scan (- i 1/2)))
                                                       out)))
                                     t)
               out)))

    (define (oracle count)
      (let loop ((index 0)
                 (out '()))
        (if (= index count)
            out
            (loop (+ index 1) (cons (list #t #f #f) out)))))

    (define check-239
      (test-scan '() t0))

    (define check-240
      (test-scan (oracle 1) t1))

    (define check-241
      (test-scan (oracle 2) t2))

    (define check-242
      (test-scan (oracle 5) t5))

    (define check-243
      (test-scan (oracle 8) t8))

    (define check-244
      (test-scan (oracle 9) t9))

    (define check-245
      (test-scan (oracle 33) t33))

    (define check-246
      (test-scan (oracle 999) t999))

    (define-syntax-rule (test-split t)
      (check #t
             (let ((out #t)
                   (split (lambda (query)
                            (finger-tree-split add
                                               measure
                                               (lambda (element)
                                                 (>= element query))
                                               #f
                                               t
                                               (lambda (prefix element suffix)
                                                 (values (finger-tree->list prefix)
                                                         element
                                                         (finger-tree->list suffix)))
                                               (lambda ()
                                                 #f))))
                   (lst (finger-tree->list t)))
               (finger-tree-for-each (lambda (i)
                                       (let*-values (((pre rest) (split-at lst i))
                                                     ((x suf) (car+cdr rest)))
                                         ;; succeed on matching element
                                         (call-with-values (lambda () (split i))
                                           (lambda (pre* x* suf*)
                                             (set! out (and out
                                                            (equal? pre pre*)
                                                            (equal? x x*)
                                                            (equal? suf suf*)))))
                                         ;; succeed on mismatched element
                                         (call-with-values (lambda () (split i))
                                           (lambda (pre* x* suf*)
                                             (set! out (and out
                                                            (equal? pre pre*)
                                                            (equal? x x*)
                                                            (equal? suf suf*)))))))
                                     t)
               (and out (not (split (length lst)))))))

    (define check-247
      (test-split t0))

    (define check-248
      (test-split t1))

    (define check-249
      (test-split t2))

    (define check-250
      (test-split t5))

    (define check-251
      (test-split t8))

    (define check-252
      (test-split t9))

    (define check-253
      (test-split t33))

    (define check-254
      (test-split t999))

    ))
