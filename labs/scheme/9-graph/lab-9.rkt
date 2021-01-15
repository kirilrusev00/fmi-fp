#lang racket

(define graph '(
                (ver1 . (ver2 ver3 ver5))
                (ver2 . (ver2))
                (ver3 . (ver1 ver5))
                (ver4 . (ver2 ver3))
                (ver5 . (ver1))
                )
  )

(define (get-edges-from graph vertex)
  (let
      ((edges (assq vertex graph)))
    (if edges
        (cdr edges)
        '()
        )))

(define (add-edges-from-to graph from to)
  (define (add-unique-vertex vertex-list to-add)
    (cond
      ((null? vertex-list) `(,to-add))
      ((eq? (car vertex-list) to-add) vertex-list)
      (else (cons (car vertex-list)
                  (add-unique-vertex (cdr vertex-list) to-add)))))

  (define (remove-vertex graph vertex)
    (cond
      ((null? graph) graph)
      ((eq? (caar graph) vertex) (cdr graph))
      (else (cons (car graph)
                  (remove-vertex (cdr graph) vertex)))))

  (let*
      (
       (edges-from (get-edges-from graph from))
       (edges-with-to (add-unique-vertex graph from))
       (graph-without-from (remove-vertex graph from))
       (new-graph (cons `(,from . ,edges-with-to) (graph-without-from))
                  )
       )
       new-graph
       
    )
  )

(define (remove-edge-from-to graph from to)
  (define (remove-vertex graph vertex)
    (cond
      ((null? graph) graph)
      ((eq? (caar graph) vertex) (cdr graph))
      (else (cons (car graph)
                  (remove-vertex (cdr graph) vertex)))))

  (let*
      (
       (edges-from (get-edges-from graph from))
       (edges-without-to (filter (lambda (x) (not (eq? x to))) edges-from))
       (graph-without-from (remove-vertex graph from))
       (new-graph (cons `(,from . ,edges-without-to) graph-without-from))
       )
    new-graph
    )
  )

(define (remove-vertex-from-graph graph vertex)
  (define (remove-vertex graph vertex)
    (cond
      ((null? graph) graph)
      ((eq? (caar graph) vertex) (cdr graph))
      (else (cons (car graph)
                  (remove-vertex (cdr graph) vertex)))))

  (map (lambda (x) (cons (car x)
                         (filter (lambda (y) (not (eq? y vertex))) (cdr x))))
       (remove-vertex graph vertex))
  )