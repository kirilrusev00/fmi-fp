(define (char-operation? c)
  (cond ((char=? c #\+) #t)
        ((char=? c #\-) #t)
        ((char=? c #\*) #t)
        ((char=? c #\/) #t)
        ((char=? c #\^) #t)
        (else #f)))

(define (expr-valid? expr)
  (define (all-chars-valid?)
    (define (char-valid? c)
      (cond ((char-numeric? c) #t)
            ((char-whitespace? c) #t)
            ((char-operation? c) #t)
            (else #f)))
    
    (define (loop i)
      (cond ((= i 0) #t)
            ((char-valid? (string-ref expr (- i 1))) (loop (- i 1)))
            (else #f)))
    (loop (string-length expr)))

  (define (correct-whitespaces?)
    (define (loop i was-there-whitespace? last-char-before-whitespace)
      (cond ((= i 0) #t)
            ((char-whitespace? (string-ref expr (- i 1))) (loop (- i 1) #t last-char-before-whitespace))
            ((and (char-numeric? (string-ref expr (- i 1))) (char-numeric? last-char-before-whitespace) was-there-whitespace?) #f)
            (else (loop (- i 1) #f (string-ref expr (- i 1))))))
    (loop (string-length expr) #f #\space))

  (define (correct-operations?)
    (define (loop i was-there-operation?)
      (if (= i 0)
          (if was-there-operation? #f #t)
          (if (char-operation? (string-ref expr (- i 1)))
              (if was-there-operation? #f (loop (- i 1) #t))
              (loop (- i 1) #f))))
    (loop (string-length expr) #t))

  #|(define (number-starting-with-0?)
    (define (loop i)
      (if (= i (- (string-length expr) 1))
          #f
          (if (and (char=? (string-ref expr i) #\0) (char-numeric? (string-ref expr (+ i 1))) (not (char-numeric? (string-ref expr (- i 1)))))
              #t
              (loop (+ i 1)))))
    (if (and (char=? (string-ref expr 0) #\0) (char-numeric? (string-ref expr 1)))
        #t
        (loop 1)))|#

  (if (= 0 (string-length (remove-whitespace expr)))
      #t
      (and (all-chars-valid?) (correct-whitespaces?) (correct-operations?) #|(not (number-starting-with-0?))|#))); 001 is valid number? uncomment if not

(define (remove-whitespace str)
  (define (loop i cleared-str)
    (if (= i (string-length str))
        cleared-str
        (if (char-whitespace? (string-ref str i))
            (loop (+ i 1) cleared-str)
            (loop (+ i 1) (string-append cleared-str (string (string-ref str i)))))))
  (loop 0 ""))

(define (expr-rp expr)
  (define (first-char str)
    (if (< 0 (string-length str))
        (string-ref str 0)
        #\space))

  (define (remove-first-char str)
    (if (< 1 (string-length str))
        (substring str 1 (string-length str))
        ""))

  (define (operator-precedes? op1 op2)
    (cond ((char=? op1 #\^) #t
           (if (char=? op2 #\^) #f #t))
          ((or (char=? op1 #\*) (char=? op1 #\/))
           (if (or (char=? op2 #\^) (char=? op2 #\*) (char=? op2 #\/)) #f #t))
          ((or (char=? op1 #\+) (char=? op1 #\-))
           (if (or (char=? op2 #\^) (char=? op2 #\*) (char=? op2 #\/) (char=? op2 #\+) (char=? op2 #\-)) #f #t))
          (else #f)))

  (define (loop i reversed-polish-expr operators-stack expr)
    (define (loop-precedence operator reversed-polish-expr operators-stack)
      (if (operator-precedes? operator (first-char operators-stack))
          (loop (+ i 1)
                (string-append reversed-polish-expr ",")
                (string-append (string operator) operators-stack)
                expr)
          (loop-precedence operator
                           (string-append reversed-polish-expr (string (first-char operators-stack)))
                           (remove-first-char operators-stack))))
    
    (if (= i (string-length expr))
        (string-append reversed-polish-expr operators-stack)
        (if (char-numeric? (string-ref expr i))
            (loop (+ i 1)
                  (string-append reversed-polish-expr (string (string-ref expr i)))
                  operators-stack
                  expr)
            (loop-precedence (string-ref expr i) reversed-polish-expr operators-stack))))

  (if (expr-valid? expr)
      (loop 0 "" "" (remove-whitespace expr))
      #f))

(define (expr-eval expr)

  (define (eval-op op-char op-to-exec expr)

    (define (char-part-of-number? c)
        (if (or (char-numeric? c)
                (and (char=? op-char #\+) (char=? c #\/))
                (and (char=? op-char #\-) (char=? c #\/))
                (and (char=? op-char #\*) (char=? c #\/))) #t #f))
    
    (define (loop op? new-expr i left-number right-number)

      (define (extract-number i number)
        (if (or (= i (string-length expr)) (not (char-part-of-number? (string-ref expr i))))
            (if (= 0 (string-length left-number))
                (loop op? new-expr i number right-number)
                (if (= 0 (string-length right-number))
                    (loop op? new-expr i left-number number)
                    (loop op? new-expr i right-number number)))
            (extract-number (+ i 1) (string-append number (string (string-ref expr i))))))
    
      (if (< i (string-length expr))
          (if (char-part-of-number? (string-ref expr i))
              (extract-number i "")
              (if (char=? (string-ref expr i) op-char)
                  (if op?
                      (loop #t new-expr (+ i 1)
                            (number->string (op-to-exec (string->number left-number) (string->number right-number))) "")
                      (loop #t new-expr (+ i 1) left-number right-number))
                  (if op?
                      (loop #f
                            (string-append new-expr
                                           (number->string (op-to-exec (string->number left-number) (string->number right-number)))
                                           (string (string-ref expr i))) 
                            (+ i 1) "" "")
                      (loop #f
                            (string-append new-expr left-number (string (string-ref expr i)))
                            (+ i 1) right-number ""))))
    
          (if op?
              (string-append new-expr (number->string (op-to-exec (string->number left-number) (string->number right-number))))
              (string-append new-expr left-number))))

    (loop #f "" 0 "" ""))
  
  (if (expr-valid? expr)
      (if (= 0 (string-length (remove-whitespace expr)))
          0
          (string->number (eval-op #\- -
                                   (eval-op #\+ +
                                            (eval-op #\* *
                                                     (eval-op #\/ /
                                                              (eval-op #\^ expt (remove-whitespace expr))))))))
      #f))

; Examples

(expr-valid? "0 +0  -0")
(expr-valid? "+2+3+4+5")
(expr-valid? "2+3+4+  05")
(expr-valid? "10   + 20"); → #t
(expr-valid? "10 20 + 5"); → #f
(expr-valid? "++++ 5"); → #f
(expr-valid? "+++"); → #f

(remove-whitespace "0 +0  -0")
(remove-whitespace "10   + 20")

(expr-rp "10   + 20 -6^7*24/34325-0")
(expr-rp "10^2+200-25/5/5")
(expr-rp "3+4*2/4^2^3")
(expr-rp "10+20*30^40+50*60-70+80*90^100+110")
(expr-rp "1+6^34545^4*3/5-23/5*6+80^0")
(expr-rp "1^6^5^4")

(expr-valid? "")
(expr-eval " 63/2/3 - 4*3+ 2^2^3^2  -8  ")
(expr-eval " 63/2/3 - 5 - 1/2 - 3^2 -5")
(expr-eval "10^2+200-25/5/5")
(expr-eval "10+20*30"); → 610
(expr-eval ""); → 0