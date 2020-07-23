(library (srfi srfi-143 check)

  (export check-00 check-01 check-02 check-03 check-04 check-05
check-06 check-07 check-08 check-09 check-10 check-11 check-12
check-13 check-14 check-15 check-16 check-17 check-18 check-19
check-20 check-21 check-22 check-23 check-24 check-25 check-26
check-27 check-28 check-29 check-30 check-31 check-32 check-33
check-34 check-35 check-36 check-37 check-38 check-39 check-40
check-41 check-42 check-43 check-44 check-45 check-46 check-47
check-48 check-49 check-50 check-51 check-52 check-53 check-54
check-55 check-56 check-57 check-58 check-59 check-60 check-61
check-62 check-63 check-64 check-65 check-66 check-67 check-68
check-69 check-70 check-71 check-72 check-73 #;check-74 check-75
check-76 #;check-77 #;check-78 #;check-79 #;check-80 #;check-81 #;check-82
#;check-83 #;check-84 check-85 check-86 check-87 check-88 check-89
check-90 #;check-91 check-92 check-93 check-94 check-95 check-96
check-97 check-98 check-99 check-100 check-101 check-102 check-103
check-104 check-105 check-106 check-107 check-108 check-109 check-110
#;check-111 #;check-112 check-113 #;check-114 #;check-115 check-116 check-117
check-118 check-119 check-120 check-121 check-122 check-123 check-124
check-125 check-126 check-127 check-128 check-129 check-130 check-131
check-132 check-133 check-134 check-135 check-136 check-137 check-138
check-139 #;check-140)

  (import (scheme base) (check) (srfi srfi-143))

  (define check-00
    (check #t (fixnum? 32767)))

  (define check-01
    (check #f (fixnum? 1.1)))

  (define check-02
    (check #t (fx=? 1 1 1)))

  (define check-03
    (check #f (fx=? 1 2 2)))

  (define check-04
    (check #f (fx=? 1 1 2)))

  (define check-05
    (check #f (fx=? 1 2 3)))

  (define check-06
    (check #t (fx<? 1 2 3)))

  (define check-07
    (check #f (fx<? 1 1 2)))

  (define check-08
    (check #t (fx>? 3 2 1)))

  (define check-09
    (check #f (fx>? 2 1 1)))

  (define check-10
    (check #t (fx<=? 1 1 2)))

  (define check-11
    (check #f (fx<=? 1 2 1)))

  (define check-12
    (check #t (fx>=? 2 1 1)))

  (define check-13
    (check #f (fx>=? 1 2 1)))

  (define check-14
    (check '(#t #f) (list (fx<=? 1 1 2) (fx<=? 2 1 3))))

  (define check-15
    (check #t (fxzero? 0)))

  (define check-16
    (check #f (fxzero? 1)))

  (define check-17
    (check #f (fxpositive? 0)))

  (define check-18
    (check #t (fxpositive? 1)))

  (define check-19
    (check #f (fxpositive? -1)))

  (define check-20
    (check #f (fxnegative? 0)))

  (define check-21
    (check #f (fxnegative? 1)))

  (define check-22
    (check #t (fxnegative? -1)))

  (define check-23
    (check #f (fxodd? 0)))

  (define check-24
    (check #t (fxodd? 1)))

  (define check-25
    (check #t (fxodd? -1)))

  (define check-26
    (check #f (fxodd? 102)))

  (define check-27
    (check #t (fxeven? 0)))

  (define check-28
    (check #f (fxeven? 1)))

  (define check-29
    (check #t (fxeven? -2)))

  (define check-30
    (check #t (fxeven? 102)))

  (define check-31
    (check 4 (fxmax 3 4)))

  (define check-32
    (check 5 (fxmax 3 5 4)))

  (define check-33
    (check 3 (fxmin 3 4)))

  (define check-34
    (check 3 (fxmin 3 5 4)))

  (define check-35
    (check 7 (fx+ 3 4)))

  (define check-36
    (check 12 (fx* 4 3)))

  (define check-37
    (check -1 (fx- 3 4)))

  (define check-38
    (check -3 (fxneg 3)))

  (define check-39
    (check 7 (fxabs -7)))

  (define check-40
    (check 7 (fxabs 7)))

  (define check-41
    (check 1764 (fxsquare 42)))

  (define check-42
    (check 4 (fxsquare 2)))

  (define check-43
    (check 2 (fxquotient 5 2)))

  (define check-44
    (check -2 (fxquotient -5 2)))

  (define check-45
    (check -2 (fxquotient 5 -2)))

  (define check-46
    (check 2 (fxquotient -5 -2)))

  (define check-47
    (check 1 (fxremainder 13 4)))

  (define check-48
    (check -1 (fxremainder -13 4)))

  (define check-49
    (check 1 (fxremainder 13 -4)))

  (define check-50
    (check -1 (fxremainder -13 -4)))

  (define check-51
    (check 35 (let*-values (((root rem) (fxsqrt 32)))
               (* root rem))))
  (define check-52
    (check -1 (fxnot 0)))

  (define check-53
    (check  0 (fxand #b0 #b1)))

  (define check-54
    (check  6 (fxand 14 6)))

  (define check-55
    (check  14 (fxior 10 12)))

  (define check-56
    (check  6 (fxxor 10 12)))

  (define check-57
    (check 0 (fxnot -1)))

  (define check-58
    (check 9 (fxif 3 1 8)))

  (define check-59
    (check 0 (fxif 3 8 1)))

  (define check-60
    (check 2 (fxbit-count 12)))

  (define check-61
    (check 0 (fxlength 0)))

  (define check-62
    (check 8 (fxlength 128)))

  (define check-63
    (check 8 (fxlength 255)))

  (define check-64
    (check 9 (fxlength 256)))

  (define check-65
    (check -1 (fxfirst-set-bit 0)))

  (define check-66
    (check 0 (fxfirst-set-bit 1)))

  (define check-67
    (check 0 (fxfirst-set-bit 3)))

  (define check-68
    (check 2 (fxfirst-set-bit 4)))

  (define check-69
    (check 1 (fxfirst-set-bit 6)))

  (define check-70
    (check 0 (fxfirst-set-bit -1)))

  (define check-71
    (check 1 (fxfirst-set-bit -2)))

  (define check-72
    (check 0 (fxfirst-set-bit -3)))

  (define check-73
    (check 2 (fxfirst-set-bit -4)))

  (define check-74
    ;; TODO: investigate why this is not good
    (check #t (fxbit-set? 0 1)))

  (define check-75
    (check #f (fxbit-set? 1 1)))

  (define check-76
    (check #f (fxbit-set? 1 8)))

  (define check-77
    (check #t (fxbit-set? 10000 -1)))

  (define check-78
    (check #t (fxbit-set? 1000 -1)))

  (define check-79
    (check 0 (fxcopy-bit 0 0 #f)))

  (define check-80
    (check -1 (fxcopy-bit 0 -1 #t)))

  (define check-81
    (check 1 (fxcopy-bit 0 0 #t)))

  (define check-82
    (check #x106 (fxcopy-bit 8 6 #t)))

  (define check-83
    (check 6 (fxcopy-bit 8 6 #f)))

  (define check-84
    (check -2 (fxcopy-bit 0 -1 #f)))

  (define check-85
    (check 0 (fxbit-field 6 0 1)))

  (define check-86
    (check 3 (fxbit-field 6 1 3)))

  (define check-87
    (check 2 (fxarithmetic-shift 1 1)))

  (define check-88
    (check 0 (fxarithmetic-shift 1 -1)))

  (define check-89
    (check #b110  (fxbit-field-rotate #b110 1 1 2)))

  (define check-90
    (check #b1010 (fxbit-field-rotate #b110 1 2 4)))

  (define check-91
    (check #b1011 (fxbit-field-rotate #b0111 -1 1 4)))

  (define check-92
    (check #b110 (fxbit-field-rotate #b110 0 0 10)))

  (define check-93
    (check 6 (fxbit-field-reverse 6 1 3)))

  (define check-94
    (check 12 (fxbit-field-reverse 6 1 4)))

  (define check-95
    (check -11 (fxnot 10)))

  (define check-96
    (check 36 (fxnot -37)))

  (define check-97
    (check 11 (fxior 3  10)))

  (define check-98
    (check 10 (fxand 11 26)))

  (define check-99
    (check 9 (fxxor 3 10)))

  (define check-100
    (check 4 (fxand 37 12)))

  (define check-101
    (check 32 (fxarithmetic-shift 8 2)))

  (define check-102
    (check 4 (fxarithmetic-shift 4 0)))

  (define check-103
    (check 4 (fxarithmetic-shift 8 -1)))

  (define check-104
    (check 0 (fxlength  0)))

  (define check-105
    (check 1 (fxlength  1)))

  (define check-106
    (check 0 (fxlength -1)))

  (define check-107
    (check 3 (fxlength  7)))

  (define check-108
    (check 3 (fxlength -7)))

  (define check-109
    (check 4 (fxlength  8)))

  (define check-110
    (check 3 (fxlength -8)))

  (define check-111
    ;; TODO: investigate
    (check #t (fxbit-set? 3 10)))

  (define check-112
    ;; TODO: investigate
    (check #t (fxbit-set? 2 6)))

  (define check-113
    (check #f (fxbit-set? 0 6)))

  (define check-114
    (check #b100 (fxcopy-bit 2 0 #t)))

  (define check-115
    (check #b1011 (fxcopy-bit 2 #b1111 #f)))

  (define check-116
    (check 1 (fxfirst-set-bit 2)))

  (define check-117
    (check 3 (fxfirst-set-bit 40)))

  (define check-118
    (check 2 (fxfirst-set-bit -28)))

  (define check-119
    (check 1 (fxand #b1 #b1)))

  (define check-120
    (check 0 (fxand #b1 #b10)))

  (define check-121
    (check #b10 (fxand #b11 #b10)))

  (define check-122
    (check #b101 (fxand #b101 #b111)))

  (define check-123
    (check #b111 (fxand -1 #b111)))

  (define check-124
    (check #b110 (fxand -2 #b111)))

  (define check-125
    (check 1 (fxarithmetic-shift 1 0)))

  (define check-126
    (check 4 (fxarithmetic-shift 1 2)))

  (define check-127
    (check 8 (fxarithmetic-shift 1 3)))

  (define check-128
    (check 16 (fxarithmetic-shift 1 4)))

  (define check-129
    (check -1 (fxarithmetic-shift -1 0)))

  (define check-130
    (check -2 (fxarithmetic-shift -1 1)))

  (define check-131
    (check -4 (fxarithmetic-shift -1 2)))

  (define check-132
    (check -8 (fxarithmetic-shift -1 3)))

  (define check-133
    (check -16 (fxarithmetic-shift -1 4)))

  (define check-134
    (check #b1010 (fxbit-field #b1101101010 0 4)))

  (define check-135
    (check #b101101 (fxbit-field #b1101101010 3 9)))

  (define check-136
    (check #b10110 (fxbit-field #b1101101010 4 9)))

  (define check-137
    (check #b110110 (fxbit-field #b1101101010 4 10)))

  (define check-138
    (check 3 (fxif 1 1 2)))

  (define check-139
    (check #b00110011 (fxif #b00111100 #b11110000 #b00001111)))

  (define check-140
    (check #b1 (fxcopy-bit 0 0 #t))))
