#lang plai

(print-only-errors true)

;;;;;;; Section 1 ;;;;;;;;

;1. Power function. 
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

;2. Average function.
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

;3. Returns a list of prime numbers up to a given number
(define (primes a)
    (pprimes a 0))

;4. Zip function
(define (zip lst1 lst2) ;recieves two lists as parameters
    (cond
      [(empty? lst1) empty] ;if the first list is empty returns the empty list
      [(empty? lst2) empty] ;if the second list is empty returns the empty list
      [else (cons (list (car lst1) (car lst2)) ;if the list has two elements then the first element from lst1 and the first element from lst2, 
                 (zip (cdr lst1) (cdr lst2)))])) ;they both go to the first list (left side list), 
                                                 ;and taking the second elements from lst1 and lst2,
                                                 ;they both go to the second list (right side list).

;5. Returns the result of applying a binary function iteratively to every two elements of a list.
(define (reduce f lst)
  (cond
    [(empty? lst) empty]
    [(empty? (cddr lst)) (f (car lst) (cadr lst))] 
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

;1. Concatenates two lists
(define (mconcat lst1 lst2)
  (mconcat-aux (rreverse lst1 '()) lst2))

;2. Applies a function to every element of a list
(define (mmap f lst)
  (cond
    [(empty? lst) '()]
    [else (cons (f (car lst)) (mmap f (cdr lst)))]))

;3. Given a boolean function and a list of elements, returns a sublist of the elements which met the predicate of the function.
(define (mfilter f lst)
  (cond
    [(empty? lst) '()]
    [else (if (f (car lst))
          (cons (car lst) (mfilter f (cdr lst)))
          (mfilter f (cdr lst)))]))

;4. Returns true if any of the elements contained in the list meets the predicate of the boolean function, otherwise returns false.
(define (any? f lst)
  (cond
    [(empty? lst) #f]
    [else (or (f (car lst)) (any? f (cdr lst)))]))

;5. Returns true if each one of the elements contained in the list meets the predicate of the boolean function, otherwise returns false.
(define (every? f lst)
  (cond
    [(empty? lst) #t]
    [else (and (f (car lst)) (every? f (cdr lst)))]))

; Auxiliary function for the mpowerset function
(define (mpowerset-aux a lst)
  (cond
    [(empty? lst) '()]
    [else (mconcat
           (list (mconcat (list a) (car lst)))
           (mpowerset-aux a (cdr lst)))]))

;6. Given a list of elements, returns the power set of it.
(define (mpowerset lst)
  (cond
    [(empty? lst) '(())]
    [else (mconcat
           (mpowerset (cdr lst))
           (mpowerset-aux (car lst) (mpowerset (cdr lst))))]))

;;;;;;;;;;; Tests ;;;;;;;;;;

;Pow function testing
(test (pow 4003 0) 1)
(test (pow 6 4) 1296)
(test (pow 3 3) 27)
(test (pow 1 999) 1)
(test (pow 2 8) 256)

;Average function testing
(test (average '()) 0)
(test (average '(3)) 3)
(test (average '(2 3 2 3 5 1 9)) 3)
(test (average '(3 4 3 5 5 12)) 5)
(test (average '(7 6 4 10)) 6)

;Primes function testing
(test (primes 1) '())
(test (primes 2) '(2))
(test (primes 11) '(2 3 5 7 11))
(test (primes 41) '(2 3 5 7 11 13 17 19 23 29 31 37 41))
(test (primes 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

;Zip function testing
(test (zip '() '()) '())
(test (zip '(1 2 3) '(1 2 3)) '((1 1) (2 2) (3 3)))
(test (zip '(1 2 3) '(1 2)) '((1 1) (2 2)))
(test (zip '(1 2) '(1 2 3)) '((1 1) (2 2)))
(test (zip '(1) '(2 3)) '((1 2)))


;Reduce function testing
(test (reduce + '(1 2 3 4 5 6 7)) 28)
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9))) '((1 (4 7)) (2 (5 8)) (3 (6 9))))
;(test (reduce pow '(1 2 3 4)) 1)
(test (reduce - '(74 3 1 8 12)) 50)

;mconcat function testing
(test (mconcat '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(test (mconcat '() '(1 2 3 4)) '(1 2 3 4))
(test (mconcat '(1 2 3 4) '()) '(1 2 3 4))
(test (mconcat '(2 3 4) '(4 3)) '(2 3 4 4 3))
(test (mconcat '(2 3) '(4 4 3)) '(2 3 4 4 3))

;mmap function testing
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))

;mfilter function testing
(test (mfilter (lambda (x) (not (zero? x))) '(2 0 1 4 0)) '(2 1 4))
(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))

;any? function testing
(test (any? number? '()) #f)
(test (any? number? '(a b c d 1)) #t)
(test (any? symbol? '(1 2 3 4)) #f)

;every function testing
(test (every? number? '()) #t)
(test (every? number? '(1 2 3)) #t)
(test (every? number? '(1 2 3 a)) #f)

;mpowerset function testing
(test (mpowerset '()) '(()))
(test (mpowerset '(1)) '(() (1))) 
(test (mpowerset '(1 2)) '(() (2) (1) (1 2)))
(test (mpowerset '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))


