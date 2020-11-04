(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init ))
  (loop a))

(define (loop-chars f a b)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (op x y) (f))
  (accumulate op id 0 a 1+ b))

(define (loop-rows f a b)
  (define (id x) x)
  (define (1+ x) (+ x 1))
  (define (op x y) (f x b))
  (accumulate op id 0 a 1+ b))

(define (display-vert-space n)
  (define (display-v-s) (display "│ "))
  (loop-chars display-v-s 1 n))

(define (display-horizontal n)
  (define (display-h) (display #\─))
  (loop-chars display-h 1 n))

(define (display-space-vert n)
  (define (display-s-v) (display " │"))
  (loop-chars display-s-v 1 n))

(define (display-up-part n)
  (define (display-up-row i n)
    (display-vert-space (- n i))                       ;(- i 1))
    (display #\┌)
    (display-horizontal (- (* 4 i) 3))                 ;(- (* 4 (+ n (- 1 i))) 3))
    (display #\┐)
    (display-space-vert (- n i))                       ;(- i 1))
    (display #\newline))
  
  (loop-rows display-up-row 1 n))

(define (display-bottom-part n)
  (define (display-bottom-row i n)
    (display-vert-space (- i 1))                       ;(- n i))
    (display #\└)
    (display-horizontal (- (* 4 (+ n (- 1 i))) 3))     ;(- (* 4 i) 3))
    (display #\┘)
    (display-space-vert (- i 1))                       ;(- n i))
    (display #\newline))
  
  (loop-rows display-bottom-row 1 n))

(define (squares n)
  (display-up-part n)
  (display-bottom-part n))

; --------------------------------------------------
; ---------------- Examples ------------------------
; --------------------------------------------------

;(squares 1)
;(squares 3)
;(squares 30)

; --------------------------------------------------
; -------------- Calculations ----------------------
; --------------------------------------------------

;(define down_right #\u250C)
;(define down_left #\u2510)
;(define up_right #\u2514)
;(define up_left #\u2518)
;(define horizontal #\u2500)
;(define vertical #\u2502)

; for n = 1: 1 horizontal, 0 vertical
; horizontal space; vertical no empty line
; for n = 2: 5 horizontal, 2 vertical
; for n = 3: 9 horizontal, 4 vertical
;            +4            +2
; for n:     4*n-3         2*n-2

; for n = 3:
; on row i:            (i-1) times vertical+space         (4*(n-i+1)-3) horizontal                  (i-1) times space+vertical
; on row 1:                                       | 1_down_right 9_horizontal 1_down_left |                
; on row 2:                    1_vertical 1_space | 1_down_right 5_horizontal 1_down_left | 1_space 1_vertical
; on row 3: 1_vertical 1_space 1_vertical 1_space | 1_down_right 1_horizontal 1_down_left | 1_space 1_vertical 1_space 1_vertical

; on row i:            (n-i) times vertical+space         (4*i-3) horizontal                   (n-i) times space+vertical
; on row 1: 1_vertical 1_space 1_vertical 1_space | 1_up_right   1_horizontal  1_up_left  | 1_space 1_vertical 1_space 1_vertical
; on row 2:                    1_vertical 1_space | 1_up_right   5_horizontal  1_up_left  | 1_space 1_vertical
; on row 3:                                       | 1_up_right   9_horizontal  1_up_left  |                