#lang plai
;Power function.
(define (pow a b)
    (cond
      [(zero? b) 1]
      [(eq? b 1) a]
      [else (* a (pow a (- b 1)))]))
      
      
;Average function.
(define (average_aux lst t l)
    (cond
      [(empty? lst) (floor (/ t l))]
      [else (average_aux (cdr lst) (+ (car lst) t) (+ l 1))])))
      
(define (average lst)
    (cond
      [(empty? lst) 0]
      [else (average_aux lst 0 0)]))) 

;Zip function (not ready, yet)
(define zip
  (lambda (lst1 lst2) ;recieves two lists as parameters
    (cond
      ((empty? lst1)empty) ;if the first list is empty returns the empty list
      ((empty? lst2)empty) ;if the second list is empty returns the empty list
      [else (cons(list (car lst1) (car lst2)) ;if the list has two elements then the first element from lst1 and the first element from lst2, 
                 (zip (cdr lst1)(cdr lst2)))]))) ;they both go to the first list (left side list), 
                                                 ;and taking the second elements from lst1 and lst2,
                                                 ;they both go to the second list (right side list).




(test (pow 4003 0) 1)
(test (pow 2 1) 1296)
(test (pow 3 3) 27)
(test (average '()) 0)
(test (average '(3)) 3)
(test (average '(2 3 2 3 5 1 9)) 3)
(test (average '(3 4 3 5 5 12)) 5)
(test (zip '() '()) '())
(test (zip '(1 2 3) '(1 2 3)) '((1 1) (2 2) (3 3)))
(test (zip '(1 2 3) '(1 2)) '((1 1) (2 2)))
(test (zip '(1 2) '(1 2 3)) '((1 1) (2 2)))
