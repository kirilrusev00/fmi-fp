(define (number-valid? n)
  
  (define (loop prev-0? new-n)
    (if (> new-n 0)
    (if (= (modulo new-n 10) 0)
        (if prev-0?
            #f
            (loop #t (quotient new-n 10)))
        (loop #f (quotient new-n 10)))
    #t))
  (loop #f n))


(define (toBinary n)
  (define (loop new-bin new-dec power-of-10)
    (if (= new-dec 0)
        new-bin
        (if (= (modulo new-dec 2) 0)
            (loop new-bin (quotient new-dec 2) (+ 1 power-of-10))
            (loop (+ (expt 10 power-of-10) new-bin) (quotient new-dec 2) (+ 1 power-of-10)))))
  (loop 0 n 0))


(define (set-contains? set elem)
  (define (loop i new-set)
    (if (= i elem)
        (= 1 (modulo new-set 2))
        (loop (+ i 1) (quotient new-set 10))))
  (loop 0 (toBinary set)))

(define (set-add set elem)
  (if (and (number? elem) (>= elem 0))
      (if (set-contains? set elem)
          set
          (+ set (expt 2 elem)))
      set))

(define (valid->nset n)
  (define (add-to-set set str)
    (define (extract-number i num set)
      (if (= i (string-length str))
          (if (= 0 (string-length num))
              set
              (set-add set (string->number num)))
          (if (char=? (string-ref str i) #\0)
              (extract-number (+ i 1) "" (set-add set (string->number num)))
              (extract-number (+ i 1) (string-append num (string (string-ref str i))) set))))
    (extract-number 0 "" 0))
  
  (if (number-valid? n)
      (add-to-set 0 (number->string n))
      #f))

(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (set-add-changed elem set)
  (set-add set elem))

(define (make-nset a b pred?)
  (accumulate set-add-changed
              (lambda (i) (if (pred? i) i -1))
              0
              a
              (lambda (x) (+ 1 x))
              b))
