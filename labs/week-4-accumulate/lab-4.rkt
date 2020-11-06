(define (otherwise) #t)

(define (% x y)
  (modulo x y))

(define (// x y)
  (quotient x y))

(define (id n) n)

(define (++ n)
  (+ n 1))

(define (iaccumulate operation
                     transform
                     initial   
                     start     
                     next
                     end)

  (define (loop i result)
    (if (> i end)
        result
        (loop (next i)
              (operation result (transform i))))
    )

  (loop start initial)
  )

(define (filter-iaccumulate should-process?
                            operation
                            transform
                            initial
                            start
                            next
                            end)

  (define (loop i result)
    (cond
      ((> i end) result)
      ((should-process? i) (loop (next i)
                                 (operation result (transform i))))
      ((otherwise) (loop (next i) result)))
    )

  (loop start initial)
  )

; Задача 1
(define (!! n)
  (filter-iaccumulate (lambda (i) (= (% i 2)
                                     (% n 2)))
                      *
                      id
                      1
                      1
                      ++
                      n))

; Задача 2
(define (binomial n k)
  (define (! n)
    (iaccumulate * id 1 1 ++ n))
    
  (/ (! n) (! k) (! (- n k))))

; Задача 2.1
(define (binomial n k)
  (iaccumulate *
               (lambda (i) (/ (- n i) (- k i)))
               1
               0
               ++
               (- k 1)))

; Задача 3
(define (2^ n)
  (iaccumulate *
               (lambda (i) 2)
               1
               1
               ++
               n))

; Задача 3.1
(define (2^ n)
  (iaccumulate +
               (lambda (i) (binomial n i))
               0
               0
               ++
               n))

; Задача 4
(define (divisors-sum n)
  (iaccumulate +
               (lambda (i) (if (= (% n i) 0) i 0))
               0
               1
               ++
               n))

(define (divisors-sum n)
  (define (divides-n? i)
    (= (% n i) 0))
  
  (filter-iaccumulate divides-n?
                      +
                      id
                      0
                      1
                      ++
                      n))

; Задача 5
(define (count p? a b)
  (filter-iaccumulate p?
                      +
                      (lambda (i) 1)
                      0
                      a
                      ++
                      b))

; Задача 6
(define (all? p? a b)
  (iaccumulate (lambda (x y) (and x y))
               p?
               #t
               a
               ++
               b))

(define (any? p? a b)
  (iaccumulate (lambda (x y) (or x y))
               p?
               #f
               a
               ++
               b))

; Задача 7
(define (prime? n)
  (define (divides-n? i)
    (= (% n i) 0))
 
  (cond ((= n 1) #f)
        ((any? divides-n? 2 (- n 1)) #f)
        (else #t)))
      