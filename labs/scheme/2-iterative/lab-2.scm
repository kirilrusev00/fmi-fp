(remainder 4 2)
(odd? 1)
(even? 1)
#b101010
#x1234abd
#o2324716

; от предния път
(define (++ n)
  (+ n 1))

(define (-- n)
  (- n 1))

(define (fact n)
  (if (= n 0)
      1
      (* n
         (fact (-- n)))))

(define (fact-iter n)
  (define (loop i result)
    (define next (++ i))
    (if (= n i)
        result
        (loop next (* result next))))
  (loop 0 1))

(define (fib n)
  (cond
    ((= n 1) 1)
    ((= n 2) 1)
    (+ (fib (-- n))
       (fib (- n 2)))))

(define (fib-iter n)
  (define (loop i result next)
    (if (= i n)
        result
        (loop (++ i) next (+ next result))))
  (loop 1 1 1))

; Задача 4
(define (reverse-int n)
  (define (loop i result)
    (if (= i 0)
        result
        (loop (quotient i 10) (+ (* 10 result) (remainder i 10)))))
  (loop n 0))

; Задача 5
(define (palindrome? n)
  (= n (reverse-int n)))

; Задача 6
(define (divisors-sum n)
  (define (loop i sum)
    (define update-sum
      (if (= (remainder n i) 0)
          (+ sum i)
          sum))
    (if (> i n)
        sum
        (loop (++ i) update-sum)))
  (loop 1 0))

; Задача 7
(define (perfect? n)
  (= n (- (divisors-sum n) n)))

; Задача 8
(define (prime? n)
  (define (divides? a b)
    (= (modulo b a) 0))
  (define (loop i)
    (cond
      ((> i (quotient n 2)) #t)
      ((divides? i n) #f)
      (else (loop (++ i)))))
  (if (= n 1)
      #f
      (loop 2)))

; Задача 9
(define (increasing n)
  (define (loop previous-digit num)
    (define next-digit
      (remainder num 10))

    (define new-number
      (quotient num 10))

    (cond
      ((= num 0) #t)
      ((< previous-digit next-digit) #f)
      (else (loop next-digit new-number))))

  (loop 10 n))

; Задача 10
(define (toBinary n)
  (define (divides2? a)
    (= 0 (modulo a 2)))
  (define (loop binary-number number power-of-10)
    (define new-number
      (quotient number 2))
    (define new-binary-number
      (if (divides2? number)
          binary-number
          (+ binary-number (expt 10 power-of-10))))
    (if (= number 0)
        binary-number
        (loop new-binary-number new-number (++ power-of-10))))
  (loop 0 n 0))
     

; Задача 11
(define (toDecimal n)
  (define (last-digit number)
      (remainder number 10))
  (define (loop binary-number decimal-number power-of-2)
    (define new-binary-number
      (quotient binary-number 10))
    (define new-decimal-number
      (+ decimal-number (* (expt 2 power-of-2) (last-digit binary-number))))
    (if (= binary-number 0)
        decimal-number
        (loop new-binary-number new-decimal-number (++ power-of-2))))
  (loop n 0 0))