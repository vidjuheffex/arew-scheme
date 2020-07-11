(library (srfi srfi-113-check)

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
check-104 check-105 check-106 check-107 check-108 check-109 check-110
check-111 check-112 check-113 check-114 check-115 check-116 check-117
check-118 check-119 check-120 check-121 check-122 check-123 check-124
check-125 check-126 check-127 check-128 check-129 check-130 check-131
check-132 check-133 check-134 check-135 check-136 check-137 check-138
check-139 check-140 check-141 check-142 check-143 check-144 check-145
check-146 check-147 check-148 check-149 check-150 check-151 check-152
check-153 check-154 check-155 check-156 check-157 check-158 check-159
check-160 check-161 check-162 check-163 check-164 check-165 check-166
check-167 check-168 check-169 check-170 check-171 check-172 check-173
check-174 check-175 check-176 check-177 check-178 check-179 check-180
check-181 check-182 check-183 check-184 check-185 check-186 check-187
check-188 check-189 check-190 check-191 check-192 check-193 check-194
check-195 check-196 check-197 check-198 check-199 check-200 check-201
check-202 check-203 check-204 check-205 check-206 check-207 check-208
check-209 check-210 check-211 check-212 check-213 check-214 check-215
check-216 check-217 check-218 check-219 check-220 check-221 check-222
check-223 check-224 check-225 check-226 check-227 check-228 check-229
check-230 check-231 check-232 check-233 check-234 check-235 check-236
check-237 check-238 check-239 check-240 check-241 check-242 check-243
check-244 check-245 check-246 check-247 check-248 check-249 check-250
check-251 check-252 check-253 check-254 check-255 check-256 check-257
check-258 check-259 check-260 check-261 check-262 check-263 check-264
check-265 check-266 check-267 check-268 check-269 check-270 check-271
check-272 check-273 check-274 check-275 check-276 check-277 check-278
check-279 check-280 check-281)

  (import (scheme base)
          (scheme char)
          (scheme complex)
          (srfi srfi-113)
          (srfi srfi-128)
          (check))

  ;; Below are some default comparators provided by SRFI-114,
  ;; but not SRFI-128, which this SRFI has transitioned to
  ;; depend on. See the rationale for SRFI-128 as to why it is
  ;; preferred in usage compared to SRFI-114.

  ;; Most if not all of this code is taken from SRFI-114

  (define (make-comparison=/< = <)
    (lambda (a b)
      (cond
       ((= a b) 0)
       ((< a b) -1)
       (else 1))))

  ;; Comparison procedure for real numbers only
  (define (real-comparison a b)
    (cond
     ((< a b) -1)
     ((> a b) 1)
     (else 0)))

  ;; Comparison procedure for non-real numbers.
  (define (complex-comparison a b)
    (let ((real-result (real-comparison (real-part a) (real-part b))))
      (if (= real-result 0)
          (real-comparison (imag-part a) (imag-part b))
          real-result)))

  (define number-comparator
    (make-comparator number? = complex-comparison number-hash))

  (define char-comparison (make-comparison=/< char=? char<?))

  (define char-comparator
    (make-comparator char? char=? char-comparison char-hash))

  ;; Makes a hash function that works vectorwise
  (define limit (expt 2 20))

  (define (make-vectorwise-hash hash length ref)
    (lambda (obj)
      (let loop ((index (- (length obj) 1)) (result 5381))
        (if (= index 0)
            result
            (let* ((prod (modulo (* result 33) limit))
                   (sum (modulo (+ prod (hash (ref obj index))) limit)))
              (loop (- index 1) sum))))))

  (define string-comparison (make-comparison=/< string=? string<?))

  (define string-ci-comparison (make-comparison=/< string-ci=? string-ci<?))

  (define string-comparator
    (make-comparator string? string=? string-comparison string-hash))

  (define string-ci-comparator
    (make-comparator string? string-ci=? string-ci-comparison string-ci-hash))

  (define eq-comparator
    (make-comparator
     #t
     eq?
     #f
     default-hash))

  (define eqv-comparator
    (make-comparator
     #t
     eqv?
     #f
     default-hash))

  (define equal-comparator
    (make-comparator
     #t
     equal?
     #f
     default-hash))


  (define (big x) (> x 5))


  (define nums (set number-comparator))

  (define syms (set eq-comparator 'a 'b 'c 'd))

  (define nums2 (set-copy nums))

  (define syms2 (set-copy syms))

  (define esyms (set eq-comparator))
  (define check-000
    (check (set-empty? esyms)))

  (define total 0)

  (define check-001
    (check (set? nums)))

  (define check-002
    (check (set? syms)))

  (define check-003
    (check (set? nums2)))

  (define check-004
    (check (set? syms2)))

  (define check-005
    (check (not (set? 'a))))

  (define check-006
    (check 4
           (begin (set-adjoin! nums 2)
                  (set-adjoin! nums 3)
                  (set-adjoin! nums 4)
                  (set-adjoin! nums 4)
                  (set-size (set-adjoin nums 5)))))
  (define check-007
    (check 3 (set-size nums)))

  (define check-008
    (check 3 (set-size (set-delete syms 'd))))

  (define check-009
    (check 2 (set-size (set-delete-all syms '(c d)))))

  (define check-010
    (check 4 (set-size syms)))

  (define check-011
    (check 4 (begin (set-adjoin! syms 'e 'f) (set-size (set-delete-all! syms '(e f))))))

  (define check-012
    (check 0 (set-size nums2)))

  (define check-013
    (check 4 (set-size syms2)))

  (define check-014
    (check 2 (begin (set-delete! nums 2) (set-size nums))))

  (define check-015
    (check 2 (begin (set-delete! nums 1)
                    (set-size nums))))

  (define check-016
    (check (begin (set! nums2 (set-map number-comparator (lambda (x) (* 10 x)) nums))
                  (set-contains? nums2 30))))

  (define check-017
    (check (not (set-contains? nums2 3))))

  (define check-018
    (check 70 (begin   (set-for-each (lambda (x) (set! total (+ total x))) nums2)
                       total)))

  (define check-019
    (check 10 (set-fold + 3 nums)))

  (define check-020
    (check
     (begin
       (set! nums (set eqv-comparator 10 20 30 40 50))

       (set=? nums (set-unfold
                    (lambda (i) (= i 0))
                    (lambda (i) (* i 10))
                    (lambda (i) (- i 1))
                    5
                    eqv-comparator)))))

  (define check-021
    (check '(a) (set->list (set eq-comparator 'a))))

  (define check-022
    (check 2 (begin
               (set! syms2 (list->set eq-comparator '(e f)))
               (set-size syms2))))

  (define check-023
    (check (set-contains? syms2 'e)))

  (define check-024
    (check (set-contains? syms2 'f)))

  (define check-025
    (check 4 (begin
               (list->set! syms2 '(a b))
               (set-size syms2))))

  (define yam (set char-comparator #\y #\a #\m))

  (define (failure/insert insert ignore)
    (insert 1))

  (define (failure/ignore insert ignore)
    (ignore 2))

  (define (success/update element update remove)
    (update #\b 3))

  (define (success/remove element update remove)
    (remove 4))

  (define yam! (set char-comparator #\y #\a #\m #\!))

  (define bam (set char-comparator #\b #\a #\m))

  (define ym (set char-comparator #\y #\m))

  (define-values (set1 obj1)
    (set-search! (set-copy yam) #\! failure/insert error))

  (define check-026
    (check (set=? yam! set1)))

  (define check-027
    (check 1 obj1))

  (define-values (set2 obj2)
    (set-search! (set-copy yam) #\! failure/ignore error))

  (define check-028
    (check (set=? yam set2)))

  (define check-029
    (check 2 obj2))

  (define-values (set3 obj3)
    (set-search! (set-copy yam) #\y error success/update))

  (define check-030
    (check (set=? bam set3)))

  (define check-031
    (check 3 obj3))

  (define-values (set4 obj4)
    (set-search! (set-copy yam) #\a error success/remove))

  (define check-032
    (check (set=? ym set4)))

  (define check-033
    (check 4 obj4))

  (define set2-bis (set number-comparator 1 2))
  (define other-set2 (set number-comparator 1 2))
  (define set3-bis (set number-comparator 1 2 3))
  (define set4-bis (set number-comparator 1 2 3 4))
  (define sety (set number-comparator 1 2 4 5))
  (define setx (set number-comparator 10 20 30 40))

  (define check-034
    (check (set=? set2-bis other-set2)))

  (define check-035
    (check (not (set=? set2-bis set3-bis))))

  (define check-036
    (check (not (set=? set2-bis set3-bis other-set2))))

  (define check-037
    (check (set<? set2-bis set3-bis set4-bis)))

  (define check-038
    (check (not (set<? set2-bis other-set2))))

  (define check-039
    (check (set<=? set2-bis other-set2 set3-bis)))

  (define check-040
    (check (not (set<=? set2-bis set3-bis other-set2))))

  (define check-041
    (check (set>? set4-bis set3-bis set2-bis)))

  (define check-042
    (check (not (set>? set2-bis other-set2))))

  (define check-043
    (check (set>=? set3-bis other-set2 set2-bis)))

  (define check-044
    (check (not (set>=? other-set2 set3-bis set2-bis))))

  (define check-045
    (check (not (set<? set2-bis other-set2))))

  (define check-046
    (check (not (set<? set2-bis setx))))

  (define check-047
    (check (not (set<=? set2-bis setx))))

  (define check-048
    (check (not (set>? set2-bis setx))))

  (define check-049
    (check (not (set>=? set2-bis setx))))

  (define check-050
    (check (not (set<?  set3-bis sety))))

  (define check-051
    (check (not (set<=? set3-bis sety))))

  (define check-052
    (check (not (set>?  set3-bis sety))))

  (define check-053
    (check (not (set>=? set3-bis sety))))

  (define abcd (set eq-comparator 'a 'b 'c 'd))
  (define efgh (set eq-comparator 'e 'f 'g 'h))
  (define abgh (set eq-comparator 'a 'b 'g 'h))
  (define other-abcd (set eq-comparator 'a 'b 'c 'd))
  (define other-efgh (set eq-comparator 'e 'f 'g 'h))
  (define other-abgh (set eq-comparator 'a 'b 'g 'h))
  (define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (set eq-comparator))
  (define ab (set eq-comparator 'a 'b))
  (define cd (set eq-comparator 'c 'd))
  (define ef (set eq-comparator 'e 'f))
  (define gh (set eq-comparator 'g 'h))
  (define cdgh (set eq-comparator 'c 'd 'g 'h))
  (define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))

  (define check-054
    (check (set-disjoint? abcd efgh)))

  (define check-055
    (check (not (set-disjoint? abcd ab))))

  (define check-056
    (check set=? abcd (set-union abcd)))

  (define check-057
    (check set=? all (set-union abcd efgh)))

  (define check-058
    (check set=? abcdgh (set-union abcd abgh)))

  (define check-059
    (check set=? abefgh (set-union efgh abgh)))


  (define efgh2 (set-copy efgh))

  (define check-060
    (check set=? efgh (begin (set-union! efgh2) efgh2)))

  (define check-061
    (check set=? abefgh (begin (set-union! efgh2 abgh) efgh2)))

  (define check-062
    (check set=? abcd (set-intersection abcd)))

  (define check-063
    (check set=? none (set-intersection abcd efgh)))

  (define abcd2 (set-copy abcd))

  (define check-064
    (check set=?
           abcd
           (begin
             (set-intersection! abcd2)
             abcd2)))
  (define check-065
    (check set=? none (begin (set-intersection! abcd2 efgh) abcd2)))

  (define check-066
    (check set=? ab (set-intersection abcd abgh)))

  (define check-067
    (check set=? ab (set-intersection abgh abcd)))

  (define check-068
    (check set=? abcd (set-difference abcd)))

  (define check-069
    (check set=? cd (set-difference abcd ab)))

  (define check-070
    (check set=? abcd (set-difference abcd gh)))

  (define check-071
    (check set=? none (set-difference abcd abcd)))


  (define abcd3 (set-copy abcd))

  (define check-072
    (check set=?
           abcd
           (begin
             (set-difference! abcd3)
             abcd3)))

  (define check-073
    (check set=? none (begin (set-difference! abcd3 abcd)
                             abcd3)))

  (define check-074
    (check set=? cdgh (set-xor abcd abgh)))

  (define check-075
    (check set=? all (set-xor abcd efgh)))

  (define check-076
    (check set=? none (set-xor abcd other-abcd)))

  (define abcd4 (set-copy abcd))
  (define check-077
    (check set=? none (set-xor! abcd4 other-abcd)))

  (define check-078
    (check set=? other-abcd abcd))

  (define check-079
    (check set=? other-efgh efgh))

  (define check-080
    (check set=? other-abgh abgh))

  (define nums3 (set number-comparator 1 2 3))
  (define syms3 (set eq-comparator 'a 'b 'c))

  (define check-081
    (check-raise (set=? nums3 syms3)))

  (define check-082
    (check-raise (set<? nums3 syms3)))

  (define check-083
    (check-raise (set<=? nums3 syms3)))

  (define check-084
    (check-raise (set>? nums3 syms3)))

  (define check-085
    (check-raise (set>=? nums3 syms3)))

  (define check-086
    (check-raise (set-union nums3 syms3)))

  (define check-087
    (check-raise (set-intersection nums3 syms3)))

  (define check-088
    (check-raise (set-difference nums3 syms3)))

  (define check-089
    (check-raise (set-xor nums3 syms3)))

  (define check-090
    (check-raise (set-union! nums3 syms3)))

  (define check-091
    (check-raise (set-intersection! nums3 syms3)))

  (define check-092
    (check-raise (set-difference! nums3 syms3)))

  (define check-093
    (check-raise (set-xor! nums3 syms3)))

  (define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (set-copy whole))
  (define whole3 (set-copy whole))
  (define whole4 (set-copy whole))
  (define bottom (set eqv-comparator 1 2 3 4 5))
  (define top (set eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (set-partition big whole))

  (define check-094
    (check set=? top (begin
                       (set-partition! big whole4)
                       (set-filter big whole))))

  (define check-095
    (check set=? bottom (set-remove big whole)))

  (define check-096
    (check (begin
             (set-filter! big whole2)
             (not (set-contains? whole2 1)))))

  (define check-097
    (check (begin
             (set-remove! big whole3)
             (not (set-contains? whole3 10)))))

  (define check-098
    (check set=? top topx))

  (define check-099
    (check set=? bottom bottomx))

  (define check-100
    (check set=? top whole4))

  (define check-101
    (check 5 (set-count big whole)))

  (define hetero (set eqv-comparator 1 2 'a 3 4))
  (define homo (set eqv-comparator 1 2 3 4 5))

  (define check-102
    (check 'a (set-find symbol? hetero (lambda () (error "wrong")))))

  (define check-103
    (check-raise  (set-find symbol? homo (lambda () (error "wrong")))))

  (define check-104
    (check (set-any? symbol? hetero)))

  (define check-105
    (check (set-any? number? hetero)))

  (define check-106
    (check (not (set-every? symbol? hetero))))

  (define check-107
    (check (not (set-every? number? hetero))))

  (define check-108
    (check (not (set-any? symbol? homo))))

  (define check-109
    (check (set-every? number? homo)))

  (define bucket (set string-ci-comparator "abc" "def"))

  (define check-110
    (check string-ci-comparator (set-element-comparator bucket)))

  (define check-111
    (check (set-contains? bucket "abc")))

  (define check-112
    (check (set-contains? bucket "ABC")))

  (define check-113
    (check "def" (set-member bucket "DEF" "fqz")))

  (define check-114
    (check "fqz" (set-member bucket "lmn" "fqz")))

  (define nums4 (set number-comparator 1 2 3))

  (define nums42 (set-replace nums4 2.0))

  (define check-115
    (check (set-any? inexact? nums42)))

  (define check-116
    (check (begin (set-replace! nums4 2.0)
                  (set-any? inexact? nums4))))

  (define sos
    (set set-comparator
         (set equal-comparator '(2 . 1) '(1 . 1) '(0 . 2) '(0 . 0))
         (set equal-comparator '(2 . 1) '(1 . 1) '(0 . 0) '(0 . 2))))

  (define check-117
    (check 1 (set-size sos)))

  (define nums5 (bag number-comparator))
  (define syms-bis (bag eq-comparator 'a 'b 'c 'd))
  (define nums52 (bag-copy nums5))
  (define syms2-bis (bag-copy syms-bis))
  (define esyms-bis (bag eq-comparator))

  (define check-118
    (check (bag-empty? esyms-bis)))

  (define total-bis 0)

  (define check-119
    (check (bag? nums5)))

  (define check-120
    (check (bag? syms-bis)))

  (define check-121
    (check (bag? nums52)))

  (define check-122
    (check (bag? syms2-bis)))

  (define check-123
    (check (not (bag? 'a))))

  (define check-124
    (check 4 (begin
               (bag-adjoin! nums5 2)
               (bag-adjoin! nums5 3)
               (bag-adjoin! nums5 4)

               (bag-size (bag-adjoin nums5 5)))))

  (define check-125
    (check 3 (bag-size nums5)))

  (define check-126
    (check 3 (bag-size (bag-delete syms-bis 'd))))

  (define check-127
    (check 2 (bag-size (bag-delete-all syms-bis '(c d)))))

  (define check-128
    (check 4 (bag-size syms-bis)))

  (define check-129
    (check 4 (begin (bag-adjoin! syms-bis 'e 'f)
                    (bag-size (bag-delete-all! syms-bis '(e f))))))

  (define check-130
    (check 3 (bag-size nums5)))

  (define check-131
    (check 3 (begin (bag-delete! nums5 1) (bag-size nums5))))

  (define check-132
    (check (begin   (set! nums52 (bag-map number-comparator (lambda (x) (* 10 x)) nums5))
                    (bag-contains? nums52 30))))

  (define check-133
    (check (not (bag-contains? nums52 3))))

  (define check-134
    (check 90 (begin (bag-for-each (lambda (x) (set! total-bis (+ total-bis x))) nums52)
                     total-bis)))
  (define check-135
    (check 12 (bag-fold + 3 nums5)))

  (define check-136
    (check
     (begin
       (set! nums5 (bag eqv-comparator 10 20 30 40 50))
       (bag=? nums5 (bag-unfold
                     (lambda (i) (= i 0))
                     (lambda (i) (* i 10))
                     (lambda (i) (- i 1))
                     5
                     eqv-comparator)))))

  (define check-137
    (check '(a) (bag->list (bag eq-comparator 'a))))

  (define check-138
    (check 2 (begin (set! syms2-bis (list->bag eq-comparator '(e f))) (bag-size syms2-bis))))

  (define check-139
    (check (bag-contains? syms2-bis 'e)))

  (define check-140
    (check (bag-contains? syms2-bis 'f)))

  (define check-141
    (check 4 (begin (list->bag! syms2-bis '(e f))
                    (bag-size syms2-bis))))

  (define yam2 (bag char-comparator #\y #\a #\m))

  (define (failure/insert/bis insert ignore)
    (insert 1))

  (define (failure/ignore/bis insert ignore)
    (ignore 2))

  (define (success/update/bis element update remove)
    (update #\b 3))

  (define (success/remove/bis element update remove)
    (remove 4))

  (define yam2! (bag char-comparator #\y #\a #\m #\!))

  (define bam2 (bag char-comparator #\b #\a #\m))

  (define ym-bis (bag char-comparator #\y #\m))

  (define-values (bag1 obj1-bis)
    (bag-search! (bag-copy yam2) #\! failure/insert/bis error))

  (define check-142
    (check (bag=? yam2! bag1)))

  (define check-143
    (check 1 obj1-bis))

  (define-values (bag2 obj2-bis)
    (bag-search! (bag-copy yam2) #\! failure/ignore/bis error))

  (define check-144
    (check (bag=? yam2 bag2)))

  (define check-145
    (check 2 obj2-bis))

  (define-values (bag3 obj3-bis)
    (bag-search! (bag-copy yam2) #\y error success/update/bis))

  (define check-146
    (check (bag=? bam2 bag3)))

  (define check-147
    (check 3 obj3-bis))

  (define-values (bag4 obj4-bis)
    (bag-search! (bag-copy yam2) #\a error success/remove/bis))

  (define check-148
    (check (bag=? ym-bis bag4)))

  (define check-149
    (check 4 obj4-bis))

  (define mybag (bag eqv-comparator 1 1 1 1 1 2 2))

  (define check-150
    (check 5 (bag-element-count mybag 1)))

  (define check-151
    (check 0 (bag-element-count mybag 3)))

  (define bag2-bis (bag number-comparator 1 2))
  (define other-bag2 (bag number-comparator 1 2))
  (define bag3-bis (bag number-comparator 1 2 3))
  (define bag4-bis (bag number-comparator 1 2 3 4))
  (define bagx (bag number-comparator 10 20 30 40))
  (define bagy (bag number-comparator 10 20 20 30 40))

  (define check-152
    (check (bag=? bag2-bis other-bag2)))

  (define check-153
    (check (not (bag=? bag2-bis bag3-bis))))

  (define check-154
    (check (not (bag=? bag2-bis bag3-bis other-bag2))))

  (define check-155
    (check (bag<? bag2-bis bag3-bis bag4-bis)))

  (define check-156
    (check (not (bag<? bag2-bis other-bag2))))

  (define check-157
    (check (bag<=? bag2-bis other-bag2 bag3-bis)))

  (define check-158
    (check (not (bag<=? bag2-bis bag3-bis other-bag2))))

  (define check-159
    (check (bag>? bag4-bis bag3-bis bag2-bis)))

  (define check-160
    (check (not (bag>? bag2-bis other-bag2))))

  (define check-161
    (check (bag>=? bag3-bis other-bag2 bag2-bis)))

  (define check-162
    (check (not (bag>=? other-bag2 bag3-bis bag2-bis))))

  (define check-163
    (check (not (bag<? bag2-bis other-bag2))))

  (define check-164
    (check (bag<=? bagx bagy)))

  (define check-165
    (check (not (bag<=? bagy bagx))))

  (define check-166
    (check (bag<? bagx bagy)))

  (define check-167
    (check (not (bag<? bagy bagx))))

  (define check-168
    (check (bag>=? bagy bagx)))

  (define check-169
    (check (not (bag>=? bagx bagy))))

  (define check-170
    (check (bag>? bagy bagx)))

  (define check-171
    (check (not (bag>? bagx bagy))))

  (define one (bag eqv-comparator 10))

  (define two (bag eqv-comparator 10 10))

  (define check-172
    (check (not (bag=? one two))))

  (define check-173
    (check (bag<? one two)))

  (define check-174
    (check (not (bag>? one two))))

  (define check-175
    (check (bag<=? one two)))

  (define check-176
    (check (not (bag>? one two))))

  (define check-177
    (check (bag=? two two)))

  (define check-178
    (check (not (bag<? two two))))

  (define check-179
    (check (not (bag>? two two))))

  (define check-180
    (check (bag<=? two two)))

  (define check-181
    (check (bag>=? two two)))

  (define check-182
    (check '((10 . 2))
           (let ((result '()))
             (bag-for-each-unique
              (lambda (x y) (set! result (cons (cons x y) result)))
              two)
             result)))

  (define check-183
    (check 25 (bag-fold + 5 two)))

  (define check-184
    (check 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two)))

  (define bag-abcd (bag eq-comparator 'a 'b 'c 'd))
  (define bag-efgh (bag eq-comparator 'e 'f 'g 'h))
  (define bag-abgh (bag eq-comparator 'a 'b 'g 'h))
  (define bag-other-abcd (bag eq-comparator 'a 'b 'c 'd))
  (define bag-other-efgh (bag eq-comparator 'e 'f 'g 'h))
  (define bag-other-abgh (bag eq-comparator 'a 'b 'g 'h))
  (define bag-all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define bag-none (bag eq-comparator))
  (define bag-ab (bag eq-comparator 'a 'b))
  (define bag-cd (bag eq-comparator 'c 'd))
  (define bag-ef (bag eq-comparator 'e 'f))
  (define bag-gh (bag eq-comparator 'g 'h))
  (define bag-cdgh (bag eq-comparator 'c 'd 'g 'h))
  (define bag-abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
  (define bag-abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))

  (define check-185
    (check (bag-disjoint? bag-abcd bag-efgh)))

  (define check-186
    (check (not (bag-disjoint? bag-abcd bag-ab))))

  (define check-187
    (check bag=? bag-abcd (bag-union bag-abcd)))

  (define check-188
    (check bag=? bag-all (bag-union bag-abcd bag-efgh)))

  (define check-189
    (check bag=? bag-abcdgh (bag-union bag-abcd bag-abgh)))

  (define check-190
    (check bag=? bag-abefgh (bag-union bag-efgh bag-abgh)))


  (define bag-efgh2-bis (bag-copy bag-efgh))

  (define check-191
    (check bag=? bag-efgh (begin (bag-union! bag-efgh2-bis)
                                 bag-efgh2-bis)))

  (define check-192
    (check bag=? bag-abefgh (begin (bag-union! bag-efgh2-bis bag-abgh) bag-efgh2-bis)))

  (define check-193
    (check bag=? bag-abcd (bag-intersection bag-abcd)))

  (define check-194
    (check bag=? bag-none (bag-intersection bag-abcd bag-efgh)))

  (define bag-abcd2-bis (bag-copy bag-abcd))

  (define check-195
    (check bag=? bag-abcd (begin (bag-intersection! bag-abcd2-bis) bag-abcd2-bis)))

  (define check-196
    (check bag=? bag-none (begin (bag-intersection! bag-abcd2-bis bag-efgh) bag-abcd2-bis)))

  (define check-197
    (check bag=? bag-ab (bag-intersection bag-abcd bag-abgh)))

  (define check-198
    (check bag=? bag-ab (bag-intersection bag-abgh bag-abcd)))

  (define check-199
    (check bag=? bag-abcd (bag-difference bag-abcd)))

  (define check-200
    (check bag=? bag-cd (bag-difference bag-abcd bag-ab)))

  (define check-201
    (check bag=? bag-abcd (bag-difference bag-abcd bag-gh)))

  (define check-202
    (check bag=? bag-none (bag-difference bag-abcd bag-abcd)))

  (define bag-abcd3-bis (bag-copy bag-abcd))

  (define check-203
    (check bag=? bag-abcd (begin (bag-difference! bag-abcd3-bis) bag-abcd3-bis)))

  (define check-204
    (check bag=? bag-none (begin (bag-difference! bag-abcd3-bis bag-abcd) bag-abcd3-bis)))

  (define check-205
    (check bag=? bag-cdgh (bag-xor bag-abcd bag-abgh)))

  (define check-206
    (check bag=? bag-all (bag-xor bag-abcd bag-efgh)))

  (define check-207
    (check bag=? bag-none (bag-xor bag-abcd bag-other-abcd)))

  (define bag-abcd4-bis (bag-copy bag-abcd))

  (define check-208
    (check bag=? bag-none (bag-xor! bag-abcd4-bis bag-other-abcd)))

  (define bag-abab (bag eq-comparator 'a 'b 'a 'b))

  (define check-209
    (check bag=? bag-ab (bag-sum bag-ab)))

  (define bag-ab2 (bag-copy bag-ab))

  (define check-210
    (check bag=? bag-ab (bag-sum! bag-ab2)))

  (define check-211
    (check bag=? bag-abab (bag-sum! bag-ab2 bag-ab)))

  (define check-212
    (check bag=? bag-abab bag-ab2))

  (define check-213
    (check bag=? bag-abab (bag-product 2 bag-ab)))

  (define bag-ab3 (bag-copy bag-ab))

  (define check-214
    (check bag=? bag-abab (begin (bag-product! 2 bag-ab3) bag-ab3)))

  (define check-215
    (check bag=? bag-other-abcd bag-abcd))

  (define check-216
    (check bag=? bag-other-abcd bag-abcd))

  (define check-217
    (check bag=? bag-other-efgh bag-efgh))

  (define check-218
    (check bag=? bag-other-abgh bag-abgh))

  (define bag-nums (bag number-comparator 1 2 3))

  (define bag-syms (bag eq-comparator 'a 'b 'c))

  (define check-219
    (check-raise (bag=? bag-nums bag-syms)))

  (define check-220
    (check-raise (bag<? bag-nums bag-syms)))

  (define check-221
    (check-raise (bag<=? bag-nums bag-syms)))

  (define check-222
    (check-raise (bag>? bag-nums bag-syms)))

  (define check-223
    (check-raise (bag>=? bag-nums bag-syms)))

  (define check-224
    (check-raise (bag-union bag-nums bag-syms)))

  (define check-225
    (check-raise (bag-intersection bag-nums bag-syms)))

  (define check-226
    (check-raise (bag-difference bag-nums bag-syms)))

  (define check-227
    (check-raise (bag-xor bag-nums bag-syms)))

  (define check-228
    (check-raise (bag-union! bag-nums bag-syms)))

  (define check-229
    (check-raise (bag-intersection! bag-nums bag-syms)))

  (define check-230
    (check-raise (bag-difference! bag-nums bag-syms)))

  (define bag-whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define bag-whole2 (bag-copy bag-whole))
  (define bag-whole3 (bag-copy bag-whole))
  (define bag-whole4 (bag-copy bag-whole))
  (define bag-bottom (bag eqv-comparator 1 2 3 4 5))
  (define bag-top (bag eqv-comparator 6 7 8 9 10))

  (define-values (topx-bis bottomx-bis)
    (bag-partition big bag-whole))

  (define check-231
    (check bag=? bag-top (begin (bag-partition! big bag-whole4) (bag-filter big bag-whole))))

  (define check-232
    (check bag=? bag-bottom (bag-remove big bag-whole)))

  (define check-233
    (check (begin (bag-filter! big bag-whole2) (not (bag-contains? bag-whole2 1)))))

  (define check-234
    (check (begin
             (bag-remove! big bag-whole3)
             (not (bag-contains? bag-whole3 10)))))

  (define check-235
    (check bag=? bag-top topx-bis))

  (define check-236
    (check bag=? bag-bottom bottomx-bis))

  (define check-237
    (check bag=? bag-top bag-whole4))

  (define check-238
    (check 5 (bag-count big bag-whole)))

  (define hetero-bis (bag eqv-comparator 1 2 'a 3 4))
  (define homo-bis (bag eqv-comparator 1 2 3 4 5))

  (define check-239
    (check 'a (bag-find symbol? hetero-bis (lambda () (error "wrong")))))

  (define check-240
    (check-raise  (bag-find symbol? homo-bis (lambda () (error "wrong")))))

  (define check-241
    (check (bag-any? symbol? hetero-bis)))

  (define check-242
    (check (bag-any? number? hetero-bis)))

  (define check-243
    (check (not (bag-every? symbol? hetero-bis))))

  (define check-244
    (check (not (bag-every? number? hetero-bis))))

  (define check-245
    (check (not (bag-any? symbol? homo-bis))))

  (define check-246
    (check (bag-every? number? homo-bis)))

  (define bag-bucket (bag string-ci-comparator "abc" "def"))

  (define check-247
    (check string-ci-comparator (bag-element-comparator bag-bucket)))

  (define check-248
    (check (bag-contains? bag-bucket "abc")))

  (define check-249
    (check (bag-contains? bag-bucket "ABC")))

  (define check-250
    (check "def" (bag-member bag-bucket "DEF" "fqz")))

  (define check-251
    (check "fqz" (bag-member bag-bucket "lmn" "fqz")))

  (define ter-nums (bag number-comparator 1 2 3))

  (define ter-nums2 (bag-replace ter-nums 2.0))

  (define check-252
    (check (bag-any? inexact? ter-nums2)))

  (define check-253
    (check (begin (bag-replace! ter-nums 2.0) (bag-any? inexact? ter-nums))))

  (define bob
    (bag bag-comparator
         (bag eqv-comparator 1 2)
         (bag eqv-comparator 1 2)))

  (define check-254
    (check 2 (bag-size bob)))

  (define mybag-bis (bag number-comparator 1 2))

  (define check-255
    (check 2 (bag-size mybag-bis)))

  (define check-256
    (check 3 (begin
               (bag-adjoin! mybag-bis 1)
               (bag-size mybag-bis))))

  (define check-257
    (check 2 (bag-unique-size mybag-bis)))

  (define check-258
    (check 2 (begin
                 (bag-delete! mybag-bis 2)
                 (bag-delete! mybag-bis 2)
                 (bag-size mybag-bis))))

  (define check-259
    (check 5 (begin
               (bag-increment! mybag-bis 1 3)
               (bag-size mybag-bis))))

  (define check-260
    (check (bag-decrement! mybag-bis 1 2)))

  (define check-261
    (check 3 (bag-size mybag-bis)))

  (define check-262
    (check 0 (begin
               (bag-decrement! mybag-bis 1 5)
               (bag-size mybag-bis))))

  (define multi (bag eqv-comparator 1 2 2 3 3 3))
  (define single (bag eqv-comparator 1 2 3))
  (define singleset (set eqv-comparator 1 2 3))
  (define minibag (bag eqv-comparator 'a 'a))
  (define alist '((a . 2)))

  (define check-263
    (check alist (bag->alist minibag)))

  (define check-264
    (check (bag=? minibag (alist->bag eqv-comparator alist))))

  (define check-265
    (check (set=? singleset (bag->set single))))

  (define check-266
    (check (set=? singleset (bag->set multi))))

  (define check-267
    (check (bag=? single (set->bag singleset))))

  (define check-268
    (check (not (bag=? multi (set->bag singleset)))))

  (define check-269
    (check (begin (set->bag! minibag singleset)
                  (bag-contains? minibag 1))))

  (define abb (bag eq-comparator 'a 'b 'b))

  (define aab (bag eq-comparator 'a 'a 'b))

  (define total-ter (bag-sum abb aab))

  (define check-270
    (check 3 (bag-count (lambda (x) (eqv? x 'a)) total-ter)))

  (define check-271
    (check 3 (bag-count (lambda (x) (eqv? x 'b)) total-ter)))

  (define check-272
    (check 12 (bag-size (bag-product 2 total-ter))))

  (define bag1-bis (bag eqv-comparator 1))

  (define check-273
    (check 2 (begin (bag-sum! bag1-bis bag1-bis) (bag-size bag1-bis))))

  (define check-274
    (check 4 (begin (bag-product! 2 bag1-bis)
                    (bag-size bag1-bis))))

  (define a (set number-comparator 1 2 3))
  (define b (set number-comparator 1 2 4))
  (define aa (bag number-comparator 1 2 3))
  (define bb (bag number-comparator 1 2 4))

  (define check-275
    (check (not (=? set-comparator a b))))

  (define check-276
    (check (=? set-comparator a (set-copy a))))

  (define check-277
    (check-raise (<? set-comparator a b)))

  (define check-278
    (check (not (=? bag-comparator aa bb))))

  (define check-279
    (check (=? bag-comparator aa (bag-copy aa))))

  (define check-280
    (check-raise (<? bag-comparator aa bb)))

  (define check-281
    (check (not (=? (make-default-comparator) a aa))))

  )
