# Упражнение 3 – Функции от по-висок ред

## Функции като арументи и върнати стойности

### Задача 1
Да се напише функция от по-висок ред `(constantly c)`, която по дадена константа `c` връща функцията `f(x)=c`:

```scheme
(define forever-21 (constantly 21))
(forever-21 5) -> 21
(forever-21 10) -> 21
((constantly 21) 10) -> 21
```

### Задача 2
Да се напише функция от по-висок ред `(flip f)`, която приема двуместна функция като аргумент и връща същата функция, но с разменени места на нейните аргументи:

```scheme
(define f (flip -))
(f 4 10) -> 6 ; = (- 10 4)
((flip f) 4 10) -> -6
```

### Задача 3
Да се напише функцията от по-висок ред `(complement p)`, която по даден предикат връща неговото отрицание:

```scheme
(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))
(f 3) ; => #f
(f 5) ; => #t
(f 7) ; => #t
```

### Задача 4
Да се напише функцията от по-висок ред `(compose f g)`, която връща композицията на две дадени числови функции:

```scheme
(define f (compose (lambda (x) (+ x 1)) (lambda (x) (* x x)))) ; ((x^2)+1)
(f 3) -> 10
```

### Задача 5*
Да се напише функцията от по-висок ред `(repeat n f)`, която връща `n`-кратната композиция на дадена функция `f`:

```scheme
(define f (repeat 5 (lambda (x) (+ x 1))))
(f 10) -> 15
((repeat 0 (lambda (x) (+ x 1))) 10) -> ?
```

### Задача 6*
Да се напише функция `(derive f)`, която връща производната на дадена функция `f` (може да използвате `dx=0.000001` за прецизност)

### Задача 7*
Да се напише функция `(derive-n n f)`, която връща `n`-тата производна на дадена функция `f`.