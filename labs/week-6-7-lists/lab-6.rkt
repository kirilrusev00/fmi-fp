#lang racket

(define nil '())

;; '() е списък
;; Ако l е списък (cons x l) е списък

(define p (cons "first" "second"))

(define l '((11 12) (21 22)))

;; map, filter, foldl, foldr

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (imap f l)
  (define (loop left result)
    (if (null? left)
        result
        (loop (cdr left)
              (cons (f (car left)) result))))
  (reverse (loop l '())))

(define (filter p? l)
  (cond
    ([null? l] '())
    ([p? (car l)] (cons (car l) (filter p? (cdr l))))
    (else (filter p? (cdr l)))))

(define (foldr op init l)
  (if (null? l)
      init
      (op (car l)
          (foldr op init (cdr l)))))

(define (foldl op init l)
  (define (loop left result)
    (if (null? left)
        result
        (loop (cdr left) (op result (car left)))))
  (loop l init))

(define (ifoldr op init l)
  (define (loop left result)
    (if (null? left)
        result
        (loop (cdr left) (op (car left) result))))
  (loop (reverse l) init))

(define (lfoldl op init l)
  (define (loop left result)
    (if (null? left)
        (cdr (reverse result))
        (loop (cdr left)
              (cons (op (car result) (car left))
                     result))))
  (loop l (list init)))

; Задaча 1
(define (take n lst)
  (cond
    ((null? lst) '())
    ((= n 0) '())
    (else (cons (car lst) (take (- n 1) (cdr lst))))))

(define (drop n lst)
  (cond
    ((null? lst) '())
    ((= n 0) lst)
    (else (drop (- n 1) (cdr lst)))))

; Задaча 2
(define (all? p? lst)
  (cond
    ((null? lst) #t)
    ((not (p? (car lst))) #f)
    (else (all? p? (cdr lst)))))

(define (any? p? lst)
  (cond
    ((null? lst) #f)
    ((p? (car lst)) #t)
    (else (any? p? (cdr lst)))))

; Задaча 3
(define (zip lst1 lst2)
  (define (loop left1 left2 result)
    (if (or (null? left1) (null? left2))
        (reverse result)
        (loop (cdr left1)
              (cdr left2)
              (cons (cons (car left1) (car left2)) result))))
  (loop lst1 lst2 '()))

; Задaча 4
(define (zipWith f lst1 lst2)
  (define (loop left1 left2 result)
    (if (or (null? left1) (null? left2))
        (reverse result)
        (loop (cdr left1)
              (cdr left2)
              (cons (f (car left1) (car left2)) result))))
  (loop lst1 lst2 '()))

; (define (zip lst1 lst2)
;   (zipWith cons lst1 lst2))

; Задaча 5
(define (sorted? lst)
  (cond
    ((null? lst) #t)
    ((null? (cdr lst)) #t)
    (else (if (> (car lst) (car (cdr lst)))
          (sorted? (cdr lst))
          #f))))

; Задaча 6
(define (uniques lst)
  (define (loop left result)
    (cond
      ((null? left) result)
      ((any? (lambda (el) (equal? el (car left))) result) (loop (cdr left) result))
      (else (loop (cdr left)
                  (cons (car left) result)))))
  (reverse (loop lst '())))

; Задaча 7
(define (insert val lst)
  (cond
    ((null? lst) (list val))
    ((< val (car lst)) (cons val lst))
    (else (cons (car lst)
                (insert val (cdr lst))))))

; Задaча 8
(define (insertion-sort lst)
  (define (loop left result)
    (if (null? left)
        result
        (loop (cdr left) (insert (car left) result))))
  (loop lst '()))

; Задaча 9
(define (longest-interval-subsets il)
  (define (longest-interval)
    (define (interval-length i)
      (if (null? i)
          0
          (- (cdr i) (car i))))
    
    (define (loop left result)
      (cond
        ((null? left) result)
        ((> (interval-length (car left)) (interval-length result)) (loop (cdr left) (car left)))
        (else (loop (cdr left) result))))
    (loop il '()))

  (define (loop left result longest)
    (cond
      ((null? left) result)
      ((and (>= (car (car left)) (car longest)) (<= (cdr (car left)) (cdr longest)))
       (loop (cdr left) (cons (car left) result) longest))
      (else (loop (cdr left) result longest))))

  (define (insertion-sort-intervals lst)
    (define (insert-interval int lst)
      (cond
        ((null? lst) (list int))
        ((< (car int) (car (car lst))) (cons int lst))
        (else (cons (car lst)
                    (insert-interval int (cdr lst))))))
    
    (define (loop left result)
      (if (null? left)
          result
          (loop (cdr left) (insert-interval (car left) result))))
    (loop lst '()))
  
  (insertion-sort-intervals (loop il '() (longest-interval))))
