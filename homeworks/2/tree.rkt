#lang racket/base

(require racket/stream)
(provide remove-whitespace tree? tree-list? string->tree balanced? ordered? tree->string compare-list-and-stream tree->stream)
;(provide (all-defined-out))

(define (string-null? str)
  (= (string-length str) 0))

; function that removes all whitespaces from string str
(define (remove-whitespace str)
  (define (loop i cleared-str)
    (if (= i (string-length str))
        cleared-str
        (if (char-whitespace? (string-ref str i))
            (loop (+ i 1) cleared-str)
            (loop (+ i 1) (string-append cleared-str (string (string-ref str i)))))))
  (loop 0 ""))

#|
Algorithm for validating the string:

- check if all chard in the string are ok [0-9*{}]
- check if there is whitespace between digits

---- remove all whitespace ----

- check if the count of { and } is the same and the string begins with { and ends with }

1) if the string is null or * => ok
2) extract substring in the beginning starting with the symbol after { and ending with the symbol before the next { or *
3) check if the substring from 2) is a number

For the left child:
[substring beginning after the number which is * or in which the count of { and } is the same]
* extract the substring and go back to 1) for left child

For the right child:
[the part of the string after the left child and before the last symbol } is the right child]
* extract the substring and go back to 1) for right child

|#

(define (tree? str)
  ; function that returns true if all chars in the string are some of these: 0-9*{} or whitespace
  (define (all-chars-valid?)
    (define (char-valid? c)
      (cond ((char-numeric? c) #t)
            ((char-whitespace? c) #t)
            ((char=? c #\*) #t)
            ((char=? c #\{) #t)
            ((char=? c #\}) #t)
            (else #f)))
    
    (define (loop i)
      (cond ((= i 0) #t)
            ((char-valid? (string-ref str (- i 1))) (loop (- i 1)))
            (else #f)))
    (loop (string-length str)))

  ; function that returns true if there is no whitespace between digits
  (define (correct-whitespaces?)
    (define (loop i was-there-whitespace? last-char-before-whitespace)
      (cond ((= i 0) #t)
            ((char-whitespace? (string-ref str (- i 1))) (loop (- i 1) #t last-char-before-whitespace))
            ((and (char-numeric? (string-ref str (- i 1))) (char-numeric? last-char-before-whitespace) was-there-whitespace?) #f)
            (else (loop (- i 1) #f (string-ref str (- i 1))))))
    (loop (string-length str) #f #\space))

  ; function that returns true if the count of { and } is the same
  ; and the string starts with { and ends with }
  (define (correct-count-brackets? str)
    (define (loop i count-left-br count-right-br)
      (cond ((= 0 (string-length str)) #t)
            ((and (= 1 (string-length str)) (char=? (string-ref str 0) #\*)) #t)
            ((= i (string-length str))
             (and (= count-left-br count-right-br)
                  (char=? (string-ref str 0) #\{)
                  (char=? (string-ref str (- (string-length str) 1)) #\})))
            ((char=? (string-ref str i) #\{) (loop (+ i 1) (+ count-left-br 1) count-right-br))
            ((char=? (string-ref str i) #\}) (loop (+ i 1) count-left-br (+ count-right-br 1)))
            (else (loop (+ i 1) count-left-br count-right-br))))
    (loop 0 0 0))

  ; function that returns true if string (with removed whitespace) is correct representation of a tree
  (define (check-string i str number left-child right-child)
    ; function that returns true if the string has number for the root element
    (define (check-number i number)
      (if (or (= i (string-length str)) (char=? (string-ref str i) #\}))
          #f
          (if (or (char=? (string-ref str i) #\*) (char=? (string-ref str i) #\{))
              (if (string-null? number)
                  #f
                  (check-string i str number left-child right-child))
              (check-number (+ i 1) (string-append number (string (string-ref str i)))))))
    ; function that extracts the supposed left child from the string 
    (define (get-left-child i left-child count-left-br count-right-br)
      (define (update-left-count count)
        (if (char=? (string-ref str i) #\{) (+ count 1) count))
      (define (update-right-count count)
        (if (char=? (string-ref str i) #\}) (+ count 1) count))
      (define (update-string s)
        (string-append s (string (string-ref str i))))
      
      (cond
        ((and (char=? (string-ref str i) #\*) (= count-left-br 0))
         (check-string (+ i 1) str number "*" right-child))
        ((and (> count-left-br 0) (= count-left-br count-right-br))
         (check-string i str number left-child right-child))
        (else (get-left-child (+ i 1) (update-string left-child) (update-left-count count-left-br) (update-right-count count-right-br)))))
    
    (cond
      ((string-null? str) #t)
      ((string=? str "*") #t)
      ((string-null? number) (check-number i ""))
      ((not (string->number number)) #f)
      ((string-null? left-child) (get-left-child i "" 0 0))
      ((string-null? right-child) (check-string i str number left-child (substring str i (- (string-length str) 1))))
      (else (and (check-string 1 left-child "" "" "") (check-string 1 right-child "" "" "")))))
  
  (if (and (all-chars-valid?) (correct-whitespaces?) (correct-count-brackets? (remove-whitespace str)))
      (check-string 1 (remove-whitespace str) "" "" "")
      #f)
  )

#|
Algorithm for string->tree:

---- remove all whitespace ----

1) if the string is null or * => '()
2) extract substring in the beginning starting with the symbol after { and ending with the symbol before the next { or *
3) the substring from 2) is the root number

For the left child:
[substring beginning after the number which is * or in which the count of { and } is the same]
* extract the substring and go back to 1) for left child

For the right child:
[the part of the string after the left child and before the last symbol } is the right child]
* extract the substring and go back to 1) for right child

In the end : construct list (number left-child-as-list right-child-as-list)
|#

(define (string->tree str)
  ; function that extracts the parts of the tree: root, left child and right child
  (define (extract-strings i str number left-child right-child)
    ; function that extracts the root as string
    (define (get-number-as-string i number)
      (if (or (char=? (string-ref str i) #\*) (char=? (string-ref str i) #\{))
          (extract-strings i str number left-child right-child)
          (get-number-as-string (+ i 1) (string-append number (string (string-ref str i))))))
    ; function that extracts the left child
    (define (get-left-child i left-child count-left-br count-right-br)
      (define (update-left-count count)
        (if (char=? (string-ref str i) #\{) (+ count 1) count))
      (define (update-right-count count)
        (if (char=? (string-ref str i) #\}) (+ count 1) count))
      (define (update-string s)
        (string-append s (string (string-ref str i))))
      
      (cond
        ((and (char=? (string-ref str i) #\*) (= count-left-br 0))
         (extract-strings (+ i 1) str number "*" right-child))
        ((and (> count-left-br 0) (= count-left-br count-right-br))
         (extract-strings i str number left-child right-child))
        (else (get-left-child (+ i 1) (update-string left-child) (update-left-count count-left-br) (update-right-count count-right-br)))))
    
    (cond
      ((string-null? str) '())
      ((string=? str "*") '())
      ((string-null? number) (get-number-as-string i ""))
      ((string-null? left-child) (get-left-child i "" 0 0))
      ((string-null? right-child) (extract-strings i str number left-child (substring str i (- (string-length str) 1))))
      (else (list (string->number number) (extract-strings 1 left-child "" "" "") (extract-strings 1 right-child "" "" "")))))
  
  (extract-strings 1 (remove-whitespace str) "" "" ""))

(define (root tree)
  (car tree))
(define (left-subtree tree)
  (car (cdr tree)))
(define (right-subtree tree)
  (car (cdr (cdr tree))))

; function that checks if a list is correct representation of a tree
(define (tree-list? tree)
  (cond
    ((not (list? tree)) #f)
    ((null? tree) #t)
    ((not (= (length tree) 3)) #f)
    ((not (number? (root tree))) #f)
    (else (and (tree-list? (left-subtree tree)) (tree-list? (right-subtree tree))))))

(define (balanced? tree)
  (define (height subtree)
    (if (null? subtree)
        0
        (+ 1 (max (height (left-subtree subtree))
                  (height (right-subtree subtree))))))

  (if (tree-list? tree)
      (or (null? tree)
          (and (<= (abs (- (height (left-subtree tree)) (height (right-subtree tree)))) 1)
               (balanced? (left-subtree tree))
               (balanced? (right-subtree tree))))
      #f))

(define (ordered? tree)
  (define (loop subtree l-number r-number)
    (cond
      ((null? subtree) #t)
      ((and (number? l-number) (< (root subtree) l-number)) #f)
      ((and (number? r-number) (> (root subtree) r-number)) #f)
      (else (and (loop (left-subtree subtree) l-number (root subtree))
                 (loop (right-subtree subtree) (root subtree) r-number)))))
  (if (tree-list? tree)  
      (loop tree #f #f)
      #f))

(define (tree->string tree)
  (define (append-to-string str subtree)
    (if (null? subtree)
        (string-append str "*")
        (string-append str "{" (number->string (root subtree))
                       " " (append-to-string str (left-subtree subtree))
                       " " (append-to-string str (right-subtree subtree)) "}")))

  (if (tree-list? tree)
      (append-to-string "" tree)
      #f))

(define (tree->stream tree order)
  (define (loop tree)
    (if (null? tree)
        empty-stream
        (case order
          ('preorder
           (stream-append (stream (root tree)) (loop (left-subtree tree)) (loop (right-subtree tree))))
          ('inorder
           (stream-append (loop (left-subtree tree)) (stream (root tree)) (loop (right-subtree tree))))
          ('postorder
           (stream-append (loop (left-subtree tree)) (loop (right-subtree tree)) (stream (root tree))))
          (else empty-stream))))

  (if (tree-list? tree)
      (loop tree)
      #f))

; function that checks if a streams and list contain the same elements (for the tests)
(define (compare-list-and-stream stream list)
  (cond ((and (null? list) (stream-empty? stream)) #t)
        ((or (and (null? list) (not (stream-empty? stream)))
             (and (stream-empty? stream) (not (null? list)))) #f)
        ((eq? (car list) (stream-first stream)) (compare-list-and-stream (stream-rest stream) (cdr list)))
        (else #f)))

; function that returns the count of digits in a number
(define (number-length number)
  (string-length (number->string number)))

(define (visualize tree)
  ; function that returns a tree where each number is replaced with
  ; (number width-visualization height-visualization)
  (define (dimensions-tree tree)
    ; helper function to get the tree where the root is replaced with
    ; (root width-visualization height-visualization)
    (define (dimensions-list tree)
      ; function that returns the width of the rectangle within which tree is visualized 
      (define (width tree)
        (if (null? tree)
            0
            (if (null? (right-subtree tree))
                (max (number-length (root tree))
                     (width (left-subtree tree)))
                (+ (max (number-length (root tree))
                        (width (left-subtree tree)))
                   (width (right-subtree tree))
                   2))))
      ; function that returns the height of the rectangle within which tree is visualized 
      (define (height tree)
        (if (null? tree)
            0
            (if (null? (left-subtree tree))
                (max 1 (height (right-subtree tree)))
                (if (< 1 (height (right-subtree tree)))
                    (+ (height (right-subtree tree)) (height (left-subtree tree)))
                    (+(height (left-subtree tree)) 2)))))
    
      (if (null? tree)
          '()
          (list (list (root tree) (width tree) (height tree))
                (dimensions-list (left-subtree tree))
                (dimensions-list (right-subtree tree)))))

    (dimensions-list tree))

  ; function that returns a matrix with each symbol from the visualization of tree
  (define (matrix dimensions-tree)
    ; function that returns the number of rows in matrix
    (define (get-height matrix)
      (length matrix))
    ; function that returns the number of columns in matrix
    (define (get-width matrix)
      (if (null? matrix)
          0
          (length (car matrix))))
    ; returns the root of the dimension tree : (number width-visualization height-visualization)
    (define (root-tree)
      (root dimensions-tree))
    ; returns the left child of root of the dimension tree
    (define (left)
      (left-subtree dimensions-tree))
    ; returns the right child of root of the dimension tree
    (define (right)
      (right-subtree dimensions-tree))
    ; returns list of the list of the chars included in the number in root
    (define (root-number-as-list) (list (string->list (number->string (root (root-tree))))))
    ; helper function to get the matrix with each symbol from the visualization of tree
    (define (get-matrix cond-case width height root-matrix left-child-matrix right-child-matrix)
      ; function that appends symbol to start-list (times) times [adds symbol (times) times to a row of the matrix]
      (define (append-horizontal-symbol start-list symbol times)
        (define (loop i new-list)
          (if (= i times)
              new-list
              (loop (+ i 1) (append new-list (list symbol)))))

        (loop 0 start-list))
      ; function that adds times-lines lines to the matrix [start-list]; each line starts with
      ; symbol-to-start-as-list (a list containing a symbol) and than has times-spaces spaces
      (define (append-vertical-empty-lines start-list symbol-to-start-as-list times-spaces times-lines)
        (define (loop i new-list)
          (if (= i times-lines)
              new-list
              (loop (+ i 1) (append new-list
                                    (list (append-horizontal-symbol (list symbol-to-start-as-list) #\space times-spaces))))))

        (loop 0 start-list))
      ; appends the root number to the matrix of the right child
      (define (append-root-right-matrix first-symbol-on-lines)
        ; 1) add -- on the first line (width dimensions root) - (right-matrix width) - (length root)
        ; 2) add empty lines (width dimensions root) - (right-matrix width); (length dimensions root) - 1
        ; 3) append the root matrix and the right child matrix
        (let*
            (
             (first-line-of-root-matrix (list (append-horizontal-symbol (car root-matrix)
                                                                        #\-
                                                                        (- width
                                                                           (get-width right-child-matrix)
                                                                           (get-width root-matrix)))))
             (updated-root-matrix (append-vertical-empty-lines first-line-of-root-matrix
                                                               first-symbol-on-lines
                                                               (- width (get-width right-child-matrix) 1)
                                                               (- (get-height right-child-matrix) 1)))
             )
          ; appends two matrices so that matrix1 is left to matrix2
          (map append updated-root-matrix right-child-matrix)
          )
        )
      ; function that appends symbol to each element of matrix (times) times
      ; [adds symbol (times) times to all rows of the matrix]
      (define (add-spaces-to-child-matrix matrix times)
        (map (lambda (row) (append-horizontal-symbol row #\space times)) matrix))
    
      (case cond-case
        ('left (append-root-right-matrix #\space))
      
        ('right
         (let*
             (
              ; 1) get first line of root matrix
              ; if length(width) of root number is bigger than child matrix width returns root-matrix
              ; else add spaces to root-matrix
              (first-line-of-root-matrix (if (> (get-width root-matrix) (get-width left-child-matrix))
                                             root-matrix
                                             (list (append-horizontal-symbol (car root-matrix)
                                                                             #\space
                                                                             (- width
                                                                                (get-width root-matrix))))))
              ; 2) add (|  ...) lines  to the first-line-of-root-matrix
              (updated-root-matrix (append-vertical-empty-lines first-line-of-root-matrix
                                                                #\|
                                                                (- width 1)
                                                                (- height (get-height left-child-matrix) 1)))
              ; 3) update child matrix
              ; if length(width) of root number is bigger than child matrix width add spaces to the rows of child-matrix
              ; else returns child-matrix
              (updated-left-child-matrix (if (> (get-width root-matrix) (get-width left-child-matrix))
                                             (add-spaces-to-child-matrix left-child-matrix
                                                                         (- (get-width root-matrix) (get-width left-child-matrix)))
                                             left-child-matrix))
              )
           ; 4) append the root matrix and the left child matrix
           (append updated-root-matrix updated-left-child-matrix)
           ))

        ('left-right
         (let*
             (
              ; append the root matrix and the right child matrix
              ; [adds -- on the first line and add (|  ...) lines to the root matrix than append]
              (root-right-matrix (append-root-right-matrix #\|))
              ; add (|  ...) line to the appended root and right child matrices
              (updated-root-right-matrix (append-vertical-empty-lines root-right-matrix
                                                                      #\|
                                                                      (- width 1)
                                                                      (- height
                                                                         (get-height left-child-matrix)
                                                                         (get-height root-right-matrix))))
              ; add spaces to the lines in child matrix
              (updated-left-child-matrix (map (lambda (x) (append-horizontal-symbol x
                                                                                    #\space
                                                                                    (- (get-width root-right-matrix)
                                                                                       (get-width left-child-matrix))))
                                              left-child-matrix))
              )
           ; append the appended root and right child matrices to the left matrix
           (append updated-root-right-matrix updated-left-child-matrix)
           )
         )
        (else '())
        )
      )
  
    (cond
      ((null? dimensions-tree) '())
      ((and (null? (left)) (null? (right))) (root-number-as-list))
      ((null? (left)) (get-matrix 'left (cadr (root-tree)) (caddr (root-tree)) (root-number-as-list) '() (matrix (right))))
      ((null? (right)) (get-matrix 'right (cadr (root-tree)) (caddr (root-tree)) (root-number-as-list) (matrix (left)) '()))
      (else (get-matrix 'left-right (cadr (root-tree)) (caddr (root-tree)) (root-number-as-list) (matrix (left)) (matrix (right))))
      ))
  
  ; function to display the visualization matrix
  (define (display-matrix m)
    ; function to display a row of the matrix
    (define (display-list l)
      (if (null? l)
          (display "\n")
          (begin
            (display (car l))
            (display-list (cdr l)))))

    (if (null? m)
          (display "\n")
          (begin
            (display-list (car m))
            (display-matrix (cdr m)))))
  
  (if (tree-list? tree)
      (display-matrix (matrix (dimensions-tree tree)))
      #f))

; Examples for visualize:

;(visualize '(1 (2 () ()) (3 (23 () ()) (5 () ()))))
;(visualize '(10 (15 (2 () ()) (7 () ())) (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
;(visualize '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 (9 () ()) (7 (8 () ()) ()))))