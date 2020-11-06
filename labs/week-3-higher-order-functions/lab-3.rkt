;;Задача 1
(define (constantly c)
  (lambda (x) c)
  )

;;Задача 2
(define (flip f)
  (lambda (x y) (f y x)))

;;Задача 3
(define (complement p)
  (lambda (x) (not (p x))))

;;Задача 4
(define (compose f g)
  (lambda (x) (f (g x))))

;;Задача 5
(define (repeat n f)
  (define (loop i rep)
    (if (= i n)
        rep
        (loop (+ i 1) (compose rep f))))
  (loop 0 (lambda (x) x)))

;;Задача 6
(define dx 1/100000)

(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

;;Задача 7
(define (derive-n n f)
  (define (loop i der)
    (if (= i n)
        der
        (loop (+ i 1) (derive der))))
  (loop 0 f))

;------------------------------------------------------

(define (accumulate-p operation
                      transform
                      initial
                      start
                      next
                      shouldStop?
                      )
  (define (helper i)
    (if (shouldStop? i)
        initial
        (operation (transform i)
                   (helper (next i)))
        )
    )

  (helper start)
)

(define (accumulate operation
                   transform
                   initial
                   start
                   next
                   end
                   )
  (define (helper i)
    (if (> i end)
        initial
        (operation (transform i)
                   (helper (next i)))
        )
    )

  (helper start)
)

(define accumulate-r accumulate)

(define (accumulate-i operation
                      transform
                      initial
                      start
                      next
                      end
                      )
  (define (loop i result)
    (if (> i end)
        result
        (loop (next i)
              (operation result
                         (transform i))
        )
    )

  
)
(loop start initial)
  )

(define accumulate-l accumulate-r)

(define (sum-from-1-to n)
  (accumulate +
              id
              0
              1
              ++
              n)
  )

(define (sum-square-from-1-to n)
  (accumulate +
              (lambda (x) (* x x))
              0
              1
              ++
              n)
  )

;;Задача 8
(define (!! n)
  (accumulate-i *
                (lambda (x) x)
                1
                (if (even? n) 2 1)
                (lambda (x) (+ x 2))
                n
                )
  )

(define (df n)
  (accumulate-p *
                (lambda (x) x)
                1
                n
                (lambda (x) (- x 2))
                (lambda (x) (< x 1))
                )
  )