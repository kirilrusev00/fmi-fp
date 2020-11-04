; --------------------------------------------------
; ------------- Helper functions -------------------
; --------------------------------------------------

(define (++ n)
  (+ n 1))

(define (-- n)
  (- n 1))

(define (divides2? a)
    (= 0 (modulo a 2)))

(define (cut-last-digit number)
  (quotient number 10))

(define (last-digit number)
  (remainder number 10))

(define (toBinary n)
  (define (loop binary decimal power-of-10)
    (define new-decimal
      (quotient decimal 2))
    
    (define new-binary
      (if (divides2? decimal)
          binary
          (+ binary (expt 10 power-of-10))))
    
    (if (= decimal 0)
        binary
        (loop new-binary new-decimal (++ power-of-10))))
  
  (loop 0 n 0))

(define (max-element set)
  (define (loop number result)
    (if (= number 0)
        result
        (loop (cut-last-digit number) (++ result))))
  
  (loop (toBinary set) -1))

; --------------------------------------------------
; -------------- Set functions ---------------------
; --------------------------------------------------

(define (set-empty? set)
  (= set 0))

(define (set-contains? set elem)
  (define (loop i set elem)
    (if (< i elem)
        (loop (+ i 1) (cut-last-digit set) elem)
        (= 1 (last-digit set))))
  
  (loop 0 (toBinary set) elem))

(define (set-add set elem)
  (if (not (set-contains? set elem))
      (+ set (expt 2 elem))
      set))

(define (set-remove set elem)
  (if (set-contains? set elem)
      (- set (expt 2 elem))
      set))

(define (set-size set)
  (define (loop result decimal)
    (define new-decimal
      (quotient decimal 2))
    
    (define new-result
      (if (divides2? decimal)
          result
          (++ result)))
    
    (if (= decimal 0)
        result
        (loop new-result new-decimal)))
  
  (loop 0 set))

(define (two-sets-generic op s1 s2)
  (define max-size
    (max (max-element s1) (max-element s2)))
  
  (define (loop i set end)
    (define update-set
      (if (op (set-contains? s1 i) (set-contains? s2 i))
          (set-add set i)
          set))
    (if (> i end)
        set
        (loop (++ i) update-set end)))

  (loop 0 0 max-size))

(define (set-intersect s1 s2)
  (two-sets-generic (lambda (x y) (and x y)) s1 s2))

(define (set-union s1 s2)
  (two-sets-generic (lambda (x y) (or x y)) s1 s2))

(define (set-difference s1 s2)
  (two-sets-generic (lambda (x y) (and x (not y))) s1 s2))

; --------------------------------------------------
; ------------ Knapsack problem --------------------
; --------------------------------------------------

; maximize    Σ (p_i * x_i)       [i: 0->n-1; x_i={0,1}]
; subject to  Σ (w_i * x_i) ≤ c   [i: 0->n-1; x_i={0,1}]

(define (knapsack c n w p)
  
  (define (max-price c n)
    (cond ((= c 0) 0)
          ((= n 0) 0)
          ((> (w (-- n)) c) (max-price c (-- n)))
          (else (max (max-price c (-- n))
                     (+ (p (-- n)) (max-price (- c (w (-- n))) (-- n)))))))

  #|(display "Optimal price: ")
  (display (max-price c n))
  (display "\n")|#
  
  (define (loop max-p i weight set)
    (cond ((and (= max-p 0) (<= weight c)) set)
          ((= i 0) 0)
          ((> (w (-- i)) max-p) (loop max-p (-- i) weight set))
          (else (max (loop (- max-p (p (-- i))) (-- i) (+ weight (w (-- i))) (set-add set (-- i)))
                     (loop max-p (-- i) weight set)))))

  (loop (max-price c n) n 0 0))

; --------------------------------------------------
; ---------------- Examples ------------------------
; --------------------------------------------------

; Example 1
#|(define (w i)
  (case i
    ((0) 1)
    ((1) 1)
    ((2) 2)
    ((3) 4)
    ((4) 12)
    (else 1)))

(define (p i)
  (case i
    ((0) 1)
    ((1) 2)
    ((2) 2)
    ((3) 10)
    ((4) 4)
    (else 1)))

(toBinary (knapsack 15 5 w p))|#; solution : 0, 1, 2, 3 ; price: 15

; Example 2
#|(define (p1 i)
  (case i
    ((0) 24)
    ((1) 18)
    ((2) 18)
    ((3) 10)
    (else 26)))

(define (w1 i)
  (case i
    ((0) 24)
    ((1) 10)
    ((2) 10)
    ((3) 7)
    (else -1)))

(toBinary (knapsack 25 4 w1 p1))|#; solution: 1 and 2 ; price: 36

; Example 3
#|(define (w2 i)
  (case i
    ((0) 1)
    ((1) 2)
    ((2) 5)
    ((3) 6)
    ((4) 7)
    (else 12)))

(define (p2 i)
  (case i
    ((0) 1)
    ((1) 6)
    ((2) 18)
    ((3) 22)
    ((4) 28)
    (else -1)))

(toBinary (knapsack 11 5 w2 p2))|#; solution: 2 and 3 ; price: 40

;(define set 35)
;(toBinary set)
;(set-contains? set 2)
;(set-add set 5)
;(set-add set 6)
;(set-size 99)
;(set-remove set 6)
;(set-size set)
;(toBinary 35)
;(toBinary 26)
;(toBinary (set-union 35 26))
;(toBinary (set-intersect 35 26))
;(toBinary (set-difference 35 26))
;(toBinary (set-union 35 0))
;(toBinary (set-intersect 35 0))
;(toBinary (set-difference 35 0))

