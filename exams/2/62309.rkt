#lang racket/base

(provide (all-defined-out)) 

;;;; Task 1

(define root car)
(define left-tree cadr)
(define right-tree caddr)

(define (weight-balanced? tree)
  (define (weight tree)
    (if (null? tree)
        0
        (+ 1 (weight (left-tree tree)) (weight (right-tree tree)))))
  
  (cond
    ((null? tree) #t)
    ((> (abs (- (weight (left-tree tree)) (weight (right-tree tree)))) 1) #f)
    (else (and (weight-balanced? (left-tree tree)) (weight-balanced? (right-tree tree))))))
      
;;; Task 2

(define get-fn-from-result car)
(define get-subject-from-result cadr)

(define (included? element ls)
  (foldl (lambda (item result) (or result (equal? item element))) #f ls))

(define (attempts subj res)
  (let*
      (
       (filter-input (filter (lambda (result) (equal? (get-subject-from-result result) subj)) res))
       (list-of-fn (map get-fn-from-result filter-input))
       (list-without-repetitions (reverse (foldl (lambda (item result) (if (included? item result)
                                                                           result
                                                                           (cons item result))) '() list-of-fn)))
       )
    (map cons list-without-repetitions (map (lambda (fn)
                                              (foldr (lambda (item result)
                                                       (if (equal? item fn)
                                                           (+ result 1)
                                                           result)) 0 list-of-fn)) list-without-repetitions))))