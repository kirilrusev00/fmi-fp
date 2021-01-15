#lang racket

(define print-x-once-return-3 (delay (begin (display "x\n")
                                            3)))

(define (fib n)
  (case n
    ((1 2) 1)
    (else (+ (fib (- n 1))
             (fib (- n 2))))))

(define fib42 (delay (fib 42)))

;(define (cons-stream h t)
;  (cons h (delay t)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail strm) (force (cdr strm)))

(define n-fibn (cons-stream 7 (fib 7)))

(define-syntax do
  (syntax-rules (=)
    ([do (a = b)] (set! a b))
    ([do (a = b) exprs ...] (begin (do (a = b))
                                   (do exprs ...)))
    ([do expr] expr)))

(define x 6)

(do (x = (fib x))
  (x = (fib x))
  ;; (* x x)
  )

(define ones (stream-cons 1 ones))
(define (nats-from n)
  (stream-cons n (nats-from (+ n 1))))

(define nats (nats-from 0))

(define 1-2-3 (stream-cons 1 (stream-cons 2 (stream-cons 3 empty-stream))))

(stream->list (stream-take (stream-map fib nats) 10))

;;;;;;;;;;;;;;;;;;;;;

(define empty-tree '())
(define (tree-new-node root left right)
  (list root left right)
  )
(define (tree-new-leaf root) (tree-new-node empty-tree empty-tree))

(define tree-empty? null?)
(define tree-root car)
(define tree-left cadr)
(define tree-right caddr)

(define (tree? tree)
  (cond
    ((not (list? tree)) #f)
    ((null? tree) #t)
    ((not (= (length tree) 3)) #f)
    (else (and (tree? (tree-left tree))
               (tree? (tree-right tree))))))

(define (tree-max-depth tree)
  (if (tree-empty? tree)
      0
      (+ 1 (max (tree-max-depth (tree-left tree))
                (tree-max-depth (tree-right tree))))))

(define (tree-balanced1? tree)
  (cond
    ((tree-empty? tree) #t)
    (else
     (and (tree-balanced? (tree-left tree))
          (tree-balanced? (tree-right tree))
          (> 2 (abs (- (tree-max-depth (tree-left tree))
                     (tree-max-depth (tree-right tree)))))))))

(define (tree-balanced? tree)
  (if
    (tree-empty? tree) 0
    (let*
         (
          (left-depth (tree-balanced? (tree-left tree)))
          (right-depth (tree-balanced? (tree-left tree)))
          (dep-diff (lambda () (abs (- left-depth
                                       right-depth))))
          (balanced-by-dept (lambda () (if (< dep-diff 2)
                                           (+ 1 (max left-depth right-depth))
                                           #f
                                           )))
                              
          )
      (and left-depth right-depth (balanced-by-dept)))))

(define (ordered? tree)
  (define (ordered-helper tree min-check? max-check?)
    (cond
      ((tree-empty? tree) #t)
      ((and (min-check? (tree-root tree))
            (max-check? (tree-root tree))) (and (ordered-helper (tree-left tree)
                                                                min-check?
                                                                (lambda (child-root)
                                                                  (<= child-root (tree-root tree))))
                                                (ordered-helper (tree-right tree)
                                                                (lambda (child-root)
                                                                  (>= child-root (tree-root tree)))
                                                                max-check?)))
      (else #f)
    ))

  (ordered-helper tree (lambda (x) #f)))

(define (tree->stream tree)
  (if (tree-empty? tree)
      empty-stream
      (stream-append (tree->stream (tree-left tree))
                     (stream-cons (tree-root tree) (tree->stream (tree-right tree)))
      )))

(define (tsl tree)
  (stream->list (tree->stream tree)))
  
(define a-tree
  (tree-new-node 5
                 (tree-new-node 1
                                empty-tree
                                (tree-new-leaf 4))
                 (tree-new-node 10
                                (tree-new-leaf 6)
                                (tree-new-leaf 19))))