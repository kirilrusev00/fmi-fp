#lang racket/base

(require rackunit rackunit/gui racket/stream)
(require "tree.rkt")

(test/gui

 (test-suite
  "remove-whitespace"

  (test-equal?
   "Strings without whitespace are not changed"
   (remove-whitespace "{1**}")
   "{1**}")

  (test-equal?
   "All whitespace is removed"
   (remove-whitespace "{ \t3  {2 \t **} {1\t* * }}")
   "{3{2**}{1**}}")

  (test-equal?
   "Strings with only whitespace remain empty"
   (remove-whitespace "  \t \n \t  ")
   "")
  
  )
 
 (test-suite
  "tree?"

  (test-true
   "Empty tree is a tree"
   (tree? "*")
   )

  (test-case
   "Strings with no digits are not trees"
   (check-false (tree? "{}"))
   (check-false (tree? "}{"))
   (check-false (tree? "{}{}{}{}"))
   (check-false (tree? "{{{}}}"))
   (check-false (tree? "{***}"))
   )

  (test-false
   "String with whitespace between digits is not a tree"
   (tree? "{2 25{4 * *} *}")
   )

  (test-false
   "String with symbol different from whitespace or (0-9*{}) is not a tree"
   (tree? "{2 {4 *+ *} *}")
   )

  (test-case
   "String with spaces in the beginning/end are trees"
   (check-true (tree? "    {2 {4 * *} *}"))
   (check-true (tree? "{1{2{4**}{5**}}{3**}}     "))
   (check-true (tree? "    {2324324 {4 {56 * {4 * *}} *} {34 * *}}    "))
   )
  
  (test-false
   "String with swapped places of { and } is not a tree"
   (tree? "}2 }4 * *{ *{")
   )

  (test-false
   "String with not equal count of { and } is not a tree"
   (tree? "{2 {4 * *}} *}")
   )
   
  (test-false
   "String with more brackets than required is not a tree"
   (tree? "{2 {{4 * *}} *}")
   )

  (test-case
   "Correct strings are recognised"
   (check-true (tree? "{2 {4 * *} *}"))
   (check-true (tree? "{1{2{4**}{5**}}{3**}}"))
   (check-true (tree? "{2324324 {4 {56 * {4 * *}} *} {34 * *}}"))
   (check-true (tree? "{2    {  4 {56* {4*   *}  }*} { 34 **} }"))
   )
  )

 (test-suite
  "tree-list?"

  (test-case
   "Returns true when tree is correct"
   (check-true (tree-list? '()))
   (check-true (tree-list? '(1 () ())))
   (check-true (tree-list? '(1 (2 (4 () ()) ()) (3 () ())) ))
   )
  
  (test-case
   "Returns false when tree is not correct"
   (check-false (tree-list? '(1 2 3 4)))
   (check-false (tree-list? '(1)))
   (check-false (tree-list? '(1 #\* #\*)))
   (check-false (tree-list? '(1 () 2)))
   (check-false (tree-list? '(1 2 3)))
   (check-false (tree-list? '(1 (2 ()))))
   (check-false (tree-list? "aeff"))
   )
  
  )
 
 (test-suite
  "balanced?"

  (test-case
   "Returns false when tree is not correct"
   (check-false (balanced? '(1 2 3 4)))
   (check-false (balanced? '(1)))
   (check-false (balanced? '(1 2 3)))
   (check-false (balanced? '(1 (2 ()))))
   )

  (test-true
   "Empty tree is balanced"
   (balanced? '())
   )

  (test-case
   "Trees with height 1 are balanced"
   (check-true (balanced? '(1 () ()) )) ; "{1**}"
   (check-true (balanced? '(23241 () ()) )) ; "{23241**}"
   )

  (test-case
   "Trees with height 2 are identified correctly as balanced/not balanced"
   (check-true (balanced? '(1 (2 () ()) ()) )) ; "{1{2**}*}"
   (check-true (balanced? '(1 (2 () ()) (3 () ())) )) ; "{1{2**}{3**}}"
   (check-true (balanced? '(1 () (3 () ())) )) ; "{1*{3**}}"
   )
  
  (test-case
   "Trees with height 3 are identified correctly as balanced/not balanced"
   (check-false (balanced? '(1 (2 (4 () ()) ()) ()) )) ; "{1{2{4**}*}*}"
   
   (check-true (balanced? '(1 (2 (4 () ()) ()) (3 () ())) )) ; "{1{2{4**}*}{3**}}"
   (check-true (balanced? '(1 (2 (4 () ()) (5 () ())) (3 () ())) )) ; "{1{2{4**}{5**}}{3**}}"
   )

  (test-case
   "Trees with height 4 are identified correctly as balanced/not balanced"
   (check-false (balanced? '(1 (2 (4 () ()) (5 (6 () ()) ())) ()) )) ; "{1{2{4**}{5{6**}*}}*}"
   (check-false (balanced? '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 () ())) )) ; "{1{2{4**}{5{6**}*}}{3**}}"
   
   (check-true (balanced? '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 () (7 () ()))) )) ; "{1{2{4**}{5{6**}*}}{3*{7**}}}"
   (check-true (balanced? '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 (9 () ()) (7 (8 () ()) ()))) )) ; "{1{2{4**}{5{6**}*}}{3{9**}{7{8**}*}}}"
   )
  )

 (test-suite
  "ordered?"

  (test-case
   "Returns false when tree is not correct"
   (check-false (ordered? '(1 2 3 4)))
   (check-false (ordered? '(1)))
   (check-false (ordered? '(1 2 3)))
   (check-false (ordered? '(1 (2 ()))))
   )
  
  (test-true
   "Empty tree is ordered"
   (ordered? (string->tree "*"))
   )

  (test-case
   "Trees with height 1 are ordered"
   (check-true (ordered? '(1 () ()) )) ; "{1**}"
   (check-true (ordered? '(23241 () ()) )) ; "{23241**}"
   )

  (test-case
   "Trees with height 2 are identified correctly as ordered/not ordered"
   (check-true (ordered? '(2 (1 () ()) ()) )) ; "{2{1**}*}"
   (check-true (ordered? '(2 (1 () ()) (3 () ())) )) ; "{2{1**}{3**}}"
   (check-true (ordered? '(1 () (3 () ())) )) ; "{1*{3**}}"
   
   (check-false (ordered? '(3 () (1 () ())) )) ; "{3*{1**}}"
   (check-false (ordered? '(1 (2 () ()) (3 () ())) )) ; "{1{2**}{3**}}"
   )

  (test-case
   "Bigger trees are identified correctly as ordered"
   (check-true (ordered? '(4 (2 (1 () ()) ()) ()) )) ; "{4{2{1**}*}*}"
   (check-true (ordered? '(6 (3 (2 () ()) (5 () ())) (78 () ())) )) ; "{6{3{2**}{5**}}{78**}}"
   (check-true (ordered? '(23 (12 () (22 () ())) (25 (24 () ()) ())) )) ; "{23{12*{22**}}{25{24**}*}}"
   )

  (test-false
   "Tree is not ordered when in left subtree is a node greater than root"
   (ordered? '(3 (2 (4 () ()) ()) (5 () ())) ) ; "{3{2{4**}*}{5**}}"
   )

  (test-false
   "Tree is not ordered when in right subtree is a node less than root"
   (ordered? '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 () ())) ) ; "{1{2{4**}{5{6**}*}}{3**}}"
   )

  )

 (test-suite
  "tree->string"

  (test-case
   "Returns false when tree is not correct"
   (check-false (tree->string '(1 2 3 4)))
   (check-false (tree->string '(1)))
   (check-false (tree->string '(1 2 3)))
   (check-false (tree->string '(1 (2 ()))))
   )

  (test-equal?
   "Empty tree is converted correctly"
   (tree->string '())
   "*"
   )

  (test-equal?
   "Bigger trees are converted correctly"
   (tree->string '(2324324 (4 (56 () (4 () ())) ()) (34 () ())) )
   "{2324324 {4 {56 * {4 * *}} *} {34 * *}}"
   )

  (test-equal?
   "Example from task is converted correctly"
   (tree->string '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) )
   "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"
   )

  (test-case
   "There is only one space between elements when converted to string"
   (check-equal? (tree->string '(2 (4 () ()) ()) ) "{2 {4 * *} *}")
   (check-not-equal? (tree->string '(2 (4 () ()) ()) ) "{ 2 { 4 * * } * }")
   (check-not-equal? (tree->string '(2 (4 () ()) ()) ) "{2{4**}*}")
   (check-not-equal? (tree->string '(2 (4 () ()) ()) ) "{2   {4   *   *}   *}")
   )
  )

 (test-suite
  "compare-list-and-stream"

  (test-true
   "Empty list and empty stream are equal"
   (compare-list-and-stream empty-stream '())
   )

  (test-true
   "Empty and finite stream is equal to list"
   (compare-list-and-stream (stream 0 1 2 3) '(0 1 2 3))
   )
  
  )

 (test-suite
  "tree->stream"

  (test-case
   "Returns false when tree is not correct"
   (check-false (tree->stream '(1 2 3 4) 'preorder))
   (check-false (tree->stream '(1) 'inorder))
   (check-false (tree->stream '(1 2 3) 'postorder))
   (check-false (tree->stream '(1 (2 ())) 'preorder))
   )
  
  (test-true
   "Empty tree is converted to empty stream"
   (compare-list-and-stream (tree->stream '() 'postorder) '())
   )

  (test-case
   "Returns empty stream when the order is not valid (preorder/inorder/postorder)"
   (check-true (compare-list-and-stream (tree->stream '() 'order) '()))
   (check-true (compare-list-and-stream (tree->stream '(1 () ()) 'order) '()))
   )

  (test-case
   "Returns correctly preorder stream"
   (check-true (compare-list-and-stream (tree->stream '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 (9 () ()) (7 (8 () ()) ())))
                                                      'preorder) ; "{1{2{4**}{5{6**}*}}{3{9**}{7{8**}*}}}"
                                        '(1 2 4 5 6 3 9 7 8))) 
   (check-true (compare-list-and-stream (tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                                                      'preorder) ; първия пример от условието
                                        '(5 22 2 6 1 3 111)))
   (check-true (compare-list-and-stream (tree->stream '(10 (15 (2 () ()) (7 () ()))
                                                           (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
                                                      'preorder) ; втория пример от условието
                                        '(10 15 2 7 5 22 2 6 1 3 111)))
   )

  (test-case
   "Returns correctly inorder stream"
   (check-true (compare-list-and-stream (tree->stream '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 (9 () ()) (7 (8 () ()) ())))
                                                      'inorder) ; "{1{2{4**}{5{6**}*}}{3{9**}{7{8**}*}}}"
                                        '(4 2 6 5 1 9 3 8 7)))
   (check-true (compare-list-and-stream (tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                                                      'inorder) ; ""{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}""
                                        '(2 22 6 5 1 111 3)))
   (check-true (compare-list-and-stream (tree->stream '(10 (15 (2 () ()) (7 () ()))
                                                           (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
                                                      'inorder) ; втория пример от условието
                                        '(2 15 7 10 2 22 6 5 1 111 3)))
   )

  (test-case
   "Returns correctly postorder stream"
   (check-true (compare-list-and-stream (tree->stream '(1 (2 (4 () ()) (5 (6 () ()) ())) (3 (9 () ()) (7 (8 () ()) ())))
                                                      'postorder) ; "{1{2{4**}{5{6**}*}}{3{9**}{7{8**}*}}}"
                                        '(4 6 5 2 9 8 7 3 1)))
   (check-true (compare-list-and-stream (tree->stream '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
                                                      'postorder) ; ""{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}""
                                        '(2 6 22 111 3 1 5)))
   (check-true (compare-list-and-stream (tree->stream '(10 (15 (2 () ()) (7 () ()))
                                                           (5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
                                                      'postorder) ; втория пример от условието
                                        '(2 7 15 2 6 22 111 3 1 5 10)))
   )
  
  )
 
 )

