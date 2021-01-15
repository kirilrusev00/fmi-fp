#lang racket/base

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

;;;;;;;

;; = - сравнява числа
;; eq? - сравнява дали 2 обекта са записани на едно място в паметта
;; eqv? - работи като eq?, но работи по специален начинeq с числа
;; equal? - работи като eqv?, но проверява с eqv? списъци в дълбочина

;; намират пъвото място в списъка, където се среща този елемент
;; връща списък или #f
;; (memq el l) - eq?
;; (memv el l) - eqv?
;; (member el l) - equal?

;(define (sum . args)
;  (foldl + 0 args)
;  )

(define (sum arg1 . args)
  (foldl + arg1 args)
  )

;; Задача 10
(define (compose . fns)
  (define (id x) x)
  (define (single-compose f1 f2)
    (lambda (x)
      (f1 (f2 x))))
  (foldr single-compose
         id
         fns)
  )

;; Задача 11
(define (any? p? lst)
  (cond
    ((null? lst) #f)
    ((p? (car lst)) #t)
    (else (any? p? (cdr lst)))))

(define (uniques lst)
  (define (loop left result)
    (cond
      ((null? left) result)
      ((any? (lambda (el) (equal? el (car left))) result) (loop (cdr left) result))
      (else (loop (cdr left)
                  (cons (car left) result)))))
  (reverse (loop lst '())))

(define (get-results-list el f lst result)
  (cond
    ((null? lst) result)
    ((equal? el (f (car lst))) (get-results-list el
                                                 f
                                                 (cdr lst)
                                                 (cons (car lst) result)))
    (else (get-results-list el f (cdr lst) result))))

(define (group-by f lst)
  (map (lambda (x)
         (list x (get-results-list x f lst '())))
       (uniques (map f lst))))

;; Задача 12
(define (zipWith f lst1 lst2)
  (define (loop left1 left2 result)
    (if (or (null? left1) (null? left2))
        (reverse result)
        (loop (cdr left1)
              (cdr left2)
              (cons (f (car left1) (car left2)) result))))
  (loop lst1 lst2 '()))

(define (zipWith* f . lsts)
  (let*
      ((combined (foldr (lambda (l1 l2) (zipWith cons l1 l2))
                        (map (lambda (x) '()) (car lsts))
                        lsts))
       (ziped (map (lambda (lst) (apply f lst))
                   combined))
       )
    ziped))

;;;;;;;

;'() - празно дърво
;'(root left-subtree right-subtree) - root - произволен елемент, left-subtree и right-subtree - дървета

(define tree::empty '())
(define (tree::new root left right)
  (list root left right)
  )

(define tree::empty? null?)
(define tree::root car)
(define tree::left cadr)
(define tree::right caddr)

(define a-tree '(6 (4 ()
                      (5 ()
                         ()))
                   (12 (7 ()
                          ())
                       (33 (20 ()
                               ())
                           ()))))

(define (tree::max-depth tree)
  (if (tree::empty? tree)
      0
      (+ 1
         (max (tree::max-depth (tree::left tree))
              (tree::max-depth (tree::right tree))))))
