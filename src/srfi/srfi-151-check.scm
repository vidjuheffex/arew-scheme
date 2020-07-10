(library (srfi srfi-151-check)

  (export check-001 check-002 check-003 check-004 check-005 check-006
check-007 check-008 check-009 check-010 check-011 check-012 check-013
check-014 check-015 check-016 check-017 check-018 check-019 check-020
check-021 check-022 check-023 check-024 check-025 check-026 check-027
check-028 check-029 check-030 check-031 check-032 check-033 check-034
check-035 check-036 check-037 check-038 check-039 check-040 check-041
check-042 check-043 check-044 check-045 check-046 check-047 check-048
check-049 check-050 check-051 check-052 check-053 check-054 check-055
check-056 check-057 check-058 check-059 check-060 check-061 check-062
check-063 check-064 check-065 check-066 check-067 check-068 check-069
check-070 check-071 check-072 check-073 check-074 check-075 check-076
check-077 check-078 check-079 check-080 check-081 check-082 check-083
check-084 check-085 check-086 check-087 check-088 check-089 check-090
check-091 check-092 check-093 check-094 check-095 check-096 check-097
check-098 check-099 check-100 check-101 check-102 check-103 check-104
check-105 check-106 check-107 check-108 check-109 check-110 check-111
check-112 check-113 check-114 check-115 check-116 check-117 check-118
check-119 check-120 check-121 check-122 check-123 check-124 check-125
check-126 check-127 check-128 check-129 check-130 check-131 check-132
check-133 check-134 check-135 check-136 check-137 check-138 check-139
check-140 check-141 check-142 check-143 check-144 check-145 check-146
check-147 check-148 check-149 check-150 check-151 check-152 check-153
check-154 check-155 check-156 check-157 check-158 check-159 check-160
check-161 check-162 check-163 check-164 check-165 check-166 check-167
check-168 check-169 check-170 check-171 check-172 check-173 check-174
check-175 check-176 check-177 check-178 check-179 check-180 check-181
check-182 check-183 check-184 check-185 check-186 check-187 #;check-188
check-189 check-190 check-191 check-192 #;check-193 check-194 check-195
check-196 check-197 check-198 check-199 check-200 check-201 check-202
check-203 check-204 check-205 check-206 check-207 check-208 check-209
check-210 check-211 check-212 check-213 check-214 check-215 check-216
check-217 check-218 check-219 check-220 check-221 check-222 check-223
check-224 check-225 check-226 check-227 check-228 check-229 check-230
check-231 check-232 check-233 check-234 check-235 check-236 check-237
check-238 check-239 check-240 check-241 check-242 check-243)

  (import (scheme base)
          (check)
          (srfi srfi-151))

  (define check-001
    (check -1 (bitwise-not 0)))

  (define check-002
    (check 0 (bitwise-not -1)))

  (define check-003
    (check -11 (bitwise-not 10)))

  (define check-004
    (check 36 (bitwise-not -37)))

  (define check-005
    (check 0 (bitwise-and #b0 #b1)))

  (define check-006
    (check 1680869008 (bitwise-and -193073517 1689392892)))

  (define check-007
    (check 3769478 (bitwise-and 1694076839 -4290775858)))

  (define check-008
    (check 6 (bitwise-and 14 6)))

  (define check-009
    (check 10 (bitwise-and 11 26)))

  (define check-010
    (check 4 (bitwise-and 37 12)))

  (define check-011
    (check 1 (bitwise-and #b1 #b1)))

  (define check-012
    (check 0 (bitwise-and #b1 #b10)))

  (define check-013
    (check #b10 (bitwise-and #b11 #b10)))

  (define check-014
    (check #b101 (bitwise-and #b101 #b111)))

  (define check-015
    (check #b111 (bitwise-and -1 #b111)))

  (define check-016
    (check #b110 (bitwise-and -2 #b111)))

  (define check-017
    (check 3769478 (bitwise-and -4290775858 1694076839)))

  (define check-018
    (check -4294967295 (bitwise-ior 1 (- -1 #xffffffff))))

  (define check-019
    (check -18446744073709551615 (bitwise-ior 1 (- -1 #xffffffffffffffff))))

  (define check-020
    (check 14 (bitwise-ior 10 12)))

  (define check-021
    (check 11 (bitwise-ior 3  10)))

  (define check-022
    (check -4294967126 (bitwise-xor #b10101010 (- -1 #xffffffff))))

  (define check-023
    (check -18446744073709551446 (bitwise-xor #b10101010 (- -1 #xffffffffffffffff))))

  (define check-024
    (check -2600468497 (bitwise-ior 1694076839 -4290775858)))

  (define check-025
    (check -184549633 (bitwise-ior -193073517 1689392892)))

  (define check-026
    (check -2604237975 (bitwise-xor 1694076839 -4290775858)))

  (define check-027
    (check -1865418641 (bitwise-xor -193073517 1689392892)))

  (define check-028
    (check 6 (bitwise-xor 10 12)))

  (define check-029
    (check 9 (bitwise-xor 3 10)))

  (define check-030
    (check (bitwise-not -4294967126) (bitwise-eqv #b10101010 (- -1 #xffffffff))))

  (define check-031
    (check -42 (bitwise-eqv 37 12)))

  (define check-032
    (check -1 (bitwise-nand 0 0)))

  (define check-033
    (check -1 (bitwise-nand 0 -1)))

  (define check-034
    (check -124 (bitwise-nand -1 123)))

  (define check-035
    (check -11 (bitwise-nand 11 26)))

  (define check-036
    (check -28 (bitwise-nor  11 26)))

  (define check-037
    (check 0 (bitwise-nor -1 123)))

  (define check-038
    (check 16 (bitwise-andc1 11 26)))

  (define check-039
    (check 1 (bitwise-andc2 11 26)))

  (define check-040
    (check -2 (bitwise-orc1 11 26)))

  (define check-041
    (check -1 (bitwise-nor 0 0)))

  (define check-042
    (check 0 (bitwise-nor 0 -1)))

  (define check-043
    (check 0 (bitwise-andc1 0 0)))

  (define check-044
    (check -1 (bitwise-andc1 0 -1)))

  (define check-045
    (check 123 (bitwise-andc1 0 123)))

  (define check-046
    (check 0 (bitwise-andc2 0 0)))

  (define check-047
    (check -1 (bitwise-andc2 -1 0)))

  (define check-048
    (check -1 (bitwise-orc1 0 0)))

  (define check-049
    (check -1 (bitwise-orc1 0 -1)))

  (define check-050
    (check 0 (bitwise-orc1 -1 0)))

  (define check-051
    (check -124 (bitwise-orc1 123 0)))

  (define check-052
    (check -1 (bitwise-orc2 0 0)))

  (define check-053
    (check -1 (bitwise-orc2 -1 0)))

  (define check-054
    (check 0 (bitwise-orc2 0 -1)))

  (define check-055
    (check -124 (bitwise-orc2 0 123)))

  ;; bitwise/integer

  (define check-056
    (check #x1000000000000000100000000000000000000000000000000
          (arithmetic-shift #x100000000000000010000000000000000 64)))

  (define check-057
    (check #x8e73b0f7da0e6452c810f32b809079e5
          (arithmetic-shift #x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b -64)))

  (define check-058
    (check 2 (arithmetic-shift 1 1)))

  (define check-059
    (check 0 (arithmetic-shift 1 -1)))

  (define check-060
    (check 1 (arithmetic-shift 1 0)))

  (define check-061
    (check 4 (arithmetic-shift 1 2)))

  (define check-062
    (check 8 (arithmetic-shift 1 3)))

  (define check-063
    (check 16 (arithmetic-shift 1 4)))

  (define check-064
    (check (expt 2 31) (arithmetic-shift 1 31)))

  (define check-065
    (check (expt 2 32) (arithmetic-shift 1 32)))

  (define check-066
    (check (expt 2 33) (arithmetic-shift 1 33)))

  (define check-067
    (check (expt 2 63) (arithmetic-shift 1 63)))

  (define check-068
    (check (expt 2 64) (arithmetic-shift 1 64)))

  (define check-069
    (check (expt 2 65) (arithmetic-shift 1 65)))

  (define check-070
    (check (expt 2 127) (arithmetic-shift 1 127)))

  (define check-071
    (check (expt 2 128) (arithmetic-shift 1 128)))

  (define check-072
    (check (expt 2 129) (arithmetic-shift 1 129)))

  (define check-073
    (check 3028397001194014464 (arithmetic-shift 11829675785914119 8)))

  (define check-074
    (check -1 (arithmetic-shift -1 0)))

  (define check-075
    (check -2 (arithmetic-shift -1 1)))

  (define check-076
    (check -4 (arithmetic-shift -1 2)))

  (define check-077
    (check -8 (arithmetic-shift -1 3)))

  (define check-078
    (check -16 (arithmetic-shift -1 4)))

  (define check-079
    (check (- (expt 2 31)) (arithmetic-shift -1 31)))

  (define check-080
    (check (- (expt 2 32)) (arithmetic-shift -1 32)))

  (define check-081
    (check (- (expt 2 33)) (arithmetic-shift -1 33)))

  (define check-082
    (check (- (expt 2 63)) (arithmetic-shift -1 63)))

  (define check-083
    (check (- (expt 2 64)) (arithmetic-shift -1 64)))

  (define check-084
    (check (- (expt 2 65)) (arithmetic-shift -1 65)))

  (define check-085
    (check (- (expt 2 127)) (arithmetic-shift -1 127)))

  (define check-086
    (check (- (expt 2 128)) (arithmetic-shift -1 128)))

  (define check-087
    (check (- (expt 2 129)) (arithmetic-shift -1 129)))

  (define check-088
    (check 0 (arithmetic-shift 1 -63)))

  (define check-089
    (check 0 (arithmetic-shift 1 -64)))

  (define check-090
    (check 0 (arithmetic-shift 1 -65)))

  (define check-091
    (check 32 (arithmetic-shift 8 2)))

  (define check-092
    (check 4 (arithmetic-shift 4 0)))

  (define check-093
    (check 4 (arithmetic-shift 8 -1)))

  (define check-094
    (check -79 (arithmetic-shift -100000000000000000000000000000000 -100)))

  (define check-095
    (check 2 (bit-count 12)))

  (define check-096
    (check 0 (integer-length  0)))

  (define check-097
    (check 1 (integer-length  1)))

  (define check-098
    (check 0 (integer-length -1)))

  (define check-099
    (check 3 (integer-length  7)))

  (define check-100
    (check 3 (integer-length -7)))

  (define check-101
    (check 4 (integer-length  8)))

  (define check-102
    (check 3 (integer-length -8)))

  (define check-103
    (check 9 (bitwise-if 3 1 8)))

  (define check-104
    (check 0 (bitwise-if 3 8 1)))

  (define check-105
    (check 3 (bitwise-if 1 1 2)))

  (define check-106
    (check #b00110011 (bitwise-if #b00111100 #b11110000 #b00001111)))

  ;; bitwise/single

  (define check-107
    (check #t (bit-set? 0 1)))

  (define check-108
    (check #f (bit-set? 1 1)))

  (define check-109
    (check #f (bit-set? 1 8)))

  (define check-110
    (check #t (bit-set? 10000 -1)))

  (define check-111
    (check #t (bit-set? 1000 -1)))

  (define check-112
    (check #t (bit-set? 64 #x10000000000000000)))

  (define check-113
    (check #f (bit-set? 64 1)))

  (define check-114
    (check #t (bit-set? 3 10)))

  (define check-115
    (check #t (bit-set? 2 6)))

  (define check-116
    (check #f (bit-set? 0 6)))

  (define check-117
    (check 0 (copy-bit 0 0 #f)))

  (define check-118
    (check 0 (copy-bit 30 0 #f)))

  (define check-119
    (check 0 (copy-bit 31 0 #f)))

  (define check-120
    (check 0 (copy-bit 62 0 #f)))

  (define check-121
    (check 0 (copy-bit 63 0 #f)))

  (define check-122
    (check 0 (copy-bit 128 0 #f)))

  (define check-123
    (check -1 (copy-bit 0 -1 #t)))

  (define check-124
    (check -1 (copy-bit 30 -1 #t)))

  (define check-125
    (check -1 (copy-bit 31 -1 #t)))

  (define check-126
    (check -1 (copy-bit 62 -1 #t)))

  (define check-127
    (check -1 (copy-bit 63 -1 #t)))

  (define check-128
    (check -1 (copy-bit 128 -1 #t)))

  (define check-129
    (check 1 (copy-bit 0 0 #t)))

  (define check-130
    (check #x106 (copy-bit 8 6 #t)))

  (define check-131
    (check 6 (copy-bit 8 6 #f)))

  (define check-132
    (check -2 (copy-bit 0 -1 #f)))

  (define check-133
    (check 0 (copy-bit 128 #x100000000000000000000000000000000 #f)))

  (define check-134
    (check #x100000000000000000000000000000000
	  (copy-bit 128 #x100000000000000000000000000000000 #t)))

  (define check-135
    (check #x100000000000000000000000000000000
	  (copy-bit 64 #x100000000000000000000000000000000 #f)))

  (define check-136
    (check #x-100000000000000000000000000000000
	  (copy-bit 64 #x-100000000000000000000000000000000 #f)))

  (define check-137
    (check #x-100000000000000000000000000000000
	  (copy-bit 256 #x-100000000000000000000000000000000 #t)))

  (define check-138
    (check #b100 (copy-bit 2 0 #t)))

  (define check-139
    (check #b1011 (copy-bit 2 #b1111 #f)))

  (define check-140
    (check #b1 (copy-bit 0 0 #t)))

  (define check-141
    (check #b1011 (bit-swap 1 2 #b1101)))

  (define check-142
    (check #b1011 (bit-swap 2 1 #b1101)))

  (define check-143
    (check #b1110 (bit-swap 0 1 #b1101)))

  (define check-144
    (check #b10000000101 (bit-swap 3 10 #b1101)))

  (define check-145
    (check 1 (bit-swap 0 2 4)))

  (define check-146
    (check #t (any-bit-set? 3 6)))

  (define check-147
    (check #f (any-bit-set? 3 12)))

  (define check-148
    (check #t (every-bit-set? 4 6)))

  (define check-149
    (check #f (every-bit-set? 7 6)))

  (define check-150
    (check -1 (first-set-bit 0)))

  (define check-151
    (check 0 (first-set-bit 1)))

  (define check-152
    (check 0 (first-set-bit 3)))

  (define check-153
    (check 2 (first-set-bit 4)))

  (define check-154
    (check 1 (first-set-bit 6)))

  (define check-155
    (check 0 (first-set-bit -1)))

  (define check-156
    (check 1 (first-set-bit -2)))

  (define check-157
    (check 0 (first-set-bit -3)))

  (define check-158
    (check 2 (first-set-bit -4)))

  (define check-159
    (check 128 (first-set-bit #x100000000000000000000000000000000)))

  (define check-160
    (check 1 (first-set-bit 2)))

  (define check-161
    (check 3 (first-set-bit 40)))

  (define check-162
    (check 2 (first-set-bit -28)))

  (define check-163
    (check 99 (first-set-bit (expt  2 99))))

  (define check-164
    (check 99 (first-set-bit (expt -2 99))))

  ;; bitwise/field

  (define check-165
    (check 0 (bit-field 6 0 1)))

  (define check-166
    (check 3 (bit-field 6 1 3)))

  (define check-167
    (check 1 (bit-field 6 2 999)))

  (define check-168
    (check 1 (bit-field #x100000000000000000000000000000000 128 129)))

  (define check-169
    (check #b1010 (bit-field #b1101101010 0 4)))

  (define check-170
    (check #b101101 (bit-field #b1101101010 3 9)))

  (define check-171
    (check #b10110 (bit-field #b1101101010 4 9)))

  (define check-172
    (check #b110110 (bit-field #b1101101010 4 10)))

  (define check-173
    (check #t (bit-field-any? #b101101 0 2)))

  (define check-174
    (check #t (bit-field-any? #b101101 2 4)))

  (define check-175
    (check #f (bit-field-any? #b101101 1 2)))

  (define check-176
    (check #f (bit-field-every? #b101101 0 2)))

  (define check-177
    (check #t (bit-field-every? #b101101 2 4)))

  (define check-178
    (check #t (bit-field-every? #b101101 0 1)))

  (define check-179
    (check #b100000 (bit-field-clear #b101010 1 4)))

  (define check-180
    (check #b101110 (bit-field-set #b101010 1 4)))

  (define check-181
    (check #b111 (bit-field-replace #b110 1 0 1)))

  (define check-182
    (check #b110 (bit-field-replace #b110 1 1 2)))

  (define check-183
    (check #b010 (bit-field-replace #b110 1 1 3)))

  (define check-184
    (check #b100100 (bit-field-replace #b101010 #b010 1 4)))

  (define check-185
    (check #b1001 (bit-field-replace-same #b1111 #b0000 1 3)))

  (define check-186
    (check #b110  (bit-field-rotate #b110 1 1 2)))

  (define check-187
    (check #b1010 (bit-field-rotate #b110 1 2 4)))

  ;; TODO: FIXME: negative rotation is invalid according to chez
  (define check-188
    (check #b1011 (bit-field-rotate #b0111 -1 1 4)))

  (define check-189
    (check #b0  (bit-field-rotate #b0 128 0 256)))

  (define check-190
    (check #b1  (bit-field-rotate #b1 128 1 256)))

  (define check-191
    (check #x100000000000000000000000000000000
	  (bit-field-rotate #x100000000000000000000000000000000 128 0 64)))

  (define check-192
    (check #x100000000000000000000000000000008
	  (bit-field-rotate #x100000000000000000000000000000001 3 0 64)))

  ;; TODO: FIXME: negative rotation is invalid according to chez
  (define check-193
   (check #x100000000000000002000000000000000
         (bit-field-rotate #x100000000000000000000000000000001 -3 0 64)))

  (define check-194
    (check #b110 (bit-field-rotate #b110 0 0 10)))

  (define check-195
    (check #b110 (bit-field-rotate #b110 0 0 256)))

  (define check-196
    (check 1 (bit-field-rotate #x100000000000000000000000000000000 1 0 129)))

  (define check-197
    (check 6 (bit-field-reverse 6 1 3)))

  (define check-198
    (check 12 (bit-field-reverse 6 1 4)))

  (define check-199
    (check #x80000000 (bit-field-reverse 1 0 32)))

  (define check-200
    (check #x40000000 (bit-field-reverse 1 0 31)))

  (define check-201
    (check #x20000000 (bit-field-reverse 1 0 30)))

  (define check-202
    (check (bitwise-ior (arithmetic-shift -1 32) #xFBFFFFFF)
	  (bit-field-reverse -2 0 27)))

  (define check-203
    (check (bitwise-ior (arithmetic-shift -1 32) #xF7FFFFFF)
	  (bit-field-reverse -2 0 28)))

  (define check-204
    (check (bitwise-ior (arithmetic-shift -1 32) #xEFFFFFFF)
	  (bit-field-reverse -2 0 29)))

  (define check-205
    (check (bitwise-ior (arithmetic-shift -1 32) #xDFFFFFFF)
	  (bit-field-reverse -2 0 30)))

  (define check-206
    (check (bitwise-ior (arithmetic-shift -1 32) #xBFFFFFFF)
	  (bit-field-reverse -2 0 31)))

  (define check-207
    (check (bitwise-ior (arithmetic-shift -1 32) #x7FFFFFFF)
	  (bit-field-reverse -2 0 32)))

  (define check-208
    (check 5 (bit-field-reverse #x140000000000000000000000000000000 0 129)))

  ;; bitwise/conversion

  (define check-209
    (check '(#t #f #t #f #t #t #t) (bits->list #b1110101)))

  (define check-210
    (check '(#f #t #f #t) (bits->list #b111010 4)))

  (define check-211
    (check #b1110101 (list->bits '(#t #f #t #f #t #t #t))))

  (define check-212
    (check #b111010100 (list->bits '(#f #f #t #f #t #f #t #t #t))))

  (define check-213
    (check '(#t #t) (bits->list 3)))

  (define check-214
    (check '(#f #t #t #f) (bits->list 6 4)))

  (define check-215
    (check '(#f #t) (bits->list 6 2)))

  (define check-216
    (check '(#t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	       #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)
	  (bits->list 1 128)))

  (define check-217
    (check '(#f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	    #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t)
	  (bits->list #x100000000000000000000000000000000)))

  (define check-218
    (check 6 (list->bits '(#f #t #t))))

  (define check-219
    (check 12 (list->bits '(#f #f #t #t))))

  (define check-220
    (check 6 (list->bits '(#f #t #t #f))))

  (define check-221
    (check 2 (list->bits '(#f #t))))

  (define check-222
    (check 1 (list->bits
	     '(#t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
		  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))))

  (define check-223
    (check #x100000000000000000000000000000000
	  (list->bits
	   '(#f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
	     #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t))))

  (define check-224
    (check #x03FFFFFF (list->bits '(#t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define check-225
    (check #x07FFFFFF (list->bits '(#t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define check-226
    (check #x0FFFFFFF (list->bits '(#t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define check-227
    (check #x1FFFFFFF (list->bits '(#t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define check-228
    (check #x3FFFFFFF (list->bits '(#t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define check-229
    (check #x7FFFFFFF (list->bits '(#t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))
  (define check-230
    (check #xFFFFFFFF (list->bits '(#t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t
				      #t #t #t #t #t #t #t #t))))

  (define check-231
    (check #x1FFFFFFFF (list->bits '(#t
				    #t #t #t #t #t #t #t #t
				    #t #t #t #t #t #t #t #t
				    #t #t #t #t #t #t #t #t
				    #t #t #t #t #t #t #t #t))))

  (define check-232
    (check 1 (list->bits '(#t #f))))

  (define check-233
    (check #b1110101 (vector->bits '#(#t #f #t #f #t #t #t))))

  (define check-234
    (check #b00011010100 (vector->bits '#(#f #f #t #f #t #f #t #t))))

  (define check-235
    (check '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9)))

  (define check-236
    (check '#(#t #t #t #f #t #f #t #f #f) (bits->vector #b1010111 9)))

  (define check-237
    (check #b1110101 (bits #t #f #t #f #t #t #t)))

  (define check-238
    (check 0 (bits)))

  (define check-239
    (check #b111010100 (bits #f #f #t #f #t #f #t #t #t)))

  ;; bitwise/fold

  (define check-240
    (check '(#t #f #t #f #t #t #t) (bitwise-fold cons '() #b1010111)))

  (define check-241
    (check 5
          (let ((count 0))
            (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                              #b1010111)
            count)))

  (define check-242
    (check #b101010101
          (bitwise-unfold (lambda (i) (= i 10)) even? (lambda (i) (+ i 1)) 0)))

  (define check-243
    (check #t
          (let ((g (make-bitwise-generator #b110)))
            (and
             (equal? #f (g))
             (equal? #t (g))
             (equal? #t (g))
             (equal? #f (g)))))))
