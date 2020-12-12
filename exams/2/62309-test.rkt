#lang racket/base

(require "62309.rkt") 

(require rackunit rackunit/gui)

(test/gui

 (test-suite
  "weight-balanced?"

  (test-true   "empty tree is balanced" (weight-balanced? '()))
  (test-true   "tree with one node is balanced" (weight-balanced? '(5 () ())))
  (test-false  "tree from example is not balanced" (weight-balanced? '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                                                                     ))

  (test-case
   "Correctly identifies balanced trees"
   (check-true  (weight-balanced? '(5 (22 () ()) ())))
   (check-true  (weight-balanced? '(5 (22 (2 () ()) (6 () ())) (1 () (3 () ())))))
   (check-true  (weight-balanced? '(5 (22 (2 (35 () ()) ()) (6 (45 () ()) ())) (1 (24 () ()) (3 (111 () ()) ())))))
   )
  )

 (test-suite
  "attempts"

  (test-equal?   "empty list returns empty attempts list" (attempts "a" '()) '())

  (test-equal?
   "list with results where the subject is not included returns empty attempts list"
   (attempts "d" (list (list 12345 "a" 5)
                       (list 12345 "b" 6)
                       (list 54321 "b" 6)))
   '())

  (test-equal?
   "list with attempts includes all attempts for the subject sorted correctly"
   (attempts "a" (list (list 12345 "a" 5)
                       (list 12345 "b" 6)
                       (list 12345 "b" 6)
                       (list 54321 "a" 5)
                       (list 54321 "b" 6)
                       (list 54321 "a" 5)
                       (list 54321 "b" 6)
                       (list 54321 "c" 6)))
   (list (cons 12345 1)
         (cons 54321 2)))

  (test-equal?
   "list with attempts includes all attempts for the subject with 3 distinct fn sorted correctly"
   (attempts "b" (list (list 12345 "a" 5)
                       (list 12345 "b" 6)
                       (list 12345 "b" 6)
                       (list 54321 "a" 5)
                       (list 54321 "b" 6)
                       (list 54321 "a" 5)
                       (list 23443 "b" 5)
                       (list 54321 "b" 6)
                       (list 54321 "c" 6)))
   (list (cons 12345 2)
         (cons 54321 2)
         (cons 23443 1)))

  (test-equal?
   "list with attempts includes all attempts for the subject with 4 distinct fn sorted correctly"
   (attempts "b" (list (list 12345 "a" 5)
                       (list 12345 "b" 6)
                       (list 12345 "b" 6)
                       (list 54321 "a" 5)
                       (list 54321 "b" 6)
                       (list 32345 "b" 5)
                       (list 23443 "b" 5)
                       (list 54321 "b" 6)
                       (list 54321 "c" 6)))
   (list (cons 12345 2)
         (cons 54321 2)
         (cons 32345 1)
         (cons 23443 1)))
  )
 
)
