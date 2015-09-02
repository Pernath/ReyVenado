#lang plai

(print-only-errors true)

;;;;;;; Section 1 ;;;;;;;;

;Power function. 
(define (pow a b)
    (cond
      [(zero? b) 1]
      [(eq? b 1) a]
      [else (* a (pow a (- b 1)))]))
            
;Auxiliary function.
(define (average_aux lst t l)
    (cond
      [(empty? lst) (floor (/ t l))]
      [else (average_aux (cdr lst) (+ (car lst) t) (+ l 1))]))

;Average function.
(define (average lst)
    (cond
      [(empty? lst) 0]
      [else (average_aux lst 0 0)]))

;Says if a number is prime
(define (is-prime a b)
    (cond
      [(< a 2) #f]
      [(eq? a b) #t]
      [(zero? (modulo a b)) #f ]
      [else (is-prime a (+ b 1))]))

;Auxiliary function
(define (pprimes a b)
  (cond
    [(eq? b (+ a 1)) '()]
    [(is-prime b 2) (cons b (pprimes a (+ b 1)))]
    [else (pprimes a (+ b 1))]))

;Returns a list of prime numbers up to a given number
(define (primes a)
    (pprimes a 0))

;Zip function (not ready, yet)
(define (zip lst1 lst2) ;recieves two lists as parameters
    (cond
      [(empty? lst1) empty] ;if the first list is empty returns the empty list
      [(empty? lst2) empty] ;if the second list is empty returns the empty list
      [else (cons (list (car lst1) (car lst2)) ;if the list has two elements then the first element from lst1 and the first element from lst2, 
                 (zip (cdr lst1) (cdr lst2)))])) ;they both go to the first list (left side list), 
                                                 ;and taking the second elements from lst1 and lst2,
                                                 ;they both go to the second list (right side list).

;Returns the result of applying a binary function iteratively to every two elements of a list.
(define (reduce f lst)
  (cond
    [(empty? (cdr lst)) (car lst)]
    [else (f (car lst) (reduce f (cdr lst)))]))

;;;;;;;; Section 2 ;;;;;;;;

;Auxiliary function. Returns the reverse of a list
 (define (rreverse lst1 lst2)
   (cond
     [(empty? lst1) lst2]     
     [else (rreverse (cdr lst1) (cons (car lst1) lst2))]))

;Auxiliary function
(define (mconcat-aux lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else (mconcat-aux (cdr lst1) (cons (car lst1) lst2))]))

;Concatenates two lists
(define (mconcat lst1 lst2)
  (mconcat-aux (rreverse lst1 '()) lst2))

;Applies a function to every element of a list
(define (mmap f lst)
  (cond
    [(empty? lst) '()]
    [else (cons (f (car lst)) (mmap f (cdr lst)))]))

(test (pow 4003 0) 1)
(test (pow 6 4) 1296)
(test (pow 3 3) 27)
(test (average '()) 0)
(test (average '(3)) 3)
(test (average '(2 3 2 3 5 1 9)) 3)
(test (average '(3 4 3 5 5 12)) 5)
(test (primes 1) '())
(test (primes 2) '(2))
(test (primes 11) '(2 3 5 7 11))
(test (primes 41) '(2 3 5 7 11 13 17 19 23 29 31 37 41))
(test (zip '() '()) '())
(test (zip '(1 2 3) '(1 2 3)) '((1 1) (2 2) (3 3)))
(test (zip '(1 2 3) '(1 2)) '((1 1) (2 2)))
(test (zip '(1 2) '(1 2 3)) '((1 1) (2 2)))
(test (reduce + '(1 2 3 4 5 6 7)) 28)
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9))) '((1 (4 7)) (2 (5 8)) (3 (6 9))))
(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '(1 2 3 4)) '(1 2 3 4))
(test (mconcat '(1 2 3 4) '()) '(1 2 3 4))
(test (mconcat '(2 3 4) '(4 3)) '(2 3 4 4 3))
(test (mconcat '(2 3) '(4 4 3)) '(2 3 4 4 3))
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
