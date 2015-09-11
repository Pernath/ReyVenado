#lang plai

;;; Laboratorio de Lenguajes de Programación ;;;
;;;;;;;;;;;;;;;;;;; Práctica 2 ;;;;;;;;;;;;;;;;;

;;;;;;;Integrantes;;;;;;;;
;;;;; Andrea González ;;;;
;;;;; Karla Esquivel ;;;;;
;;;;; Carlos Acosta ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;print-only-errors when this flag is set to #t, only tests that fail will be printed.
(print-only-errors true)

;; Seccion I
(define (every? f lst);every return #t when all predicates of list give #t
  (cond
    [(empty? lst) #t]
    [else (and (f (car lst)) (every? f (cdr lst)))]))

(define z null)
;MArray defines size of an array
(define-type Array
  [MArray (num (lambda (x)
                 (exact-nonnegative-integer? x);specifies that only positive integers are acepted.
                 (set! z x)))
          (lst (lambda (x)
                 (and (list? x) (<= (length x) z))))]);list? Returns #t if v is a list either the empty list, 
                                                      ; or a pair whose second element is a list.
(test (MArray 4 '(1 2 3)) (MArray 4 '(1 2 3)))

(define (atom? x);Determines whether or not a value is a number, a symbol, or a string.
  (not (pair? x)));is the pair containing s as the car and t as the cdr
;MList is a Type recursive data 
(define-type MList
  [MEmpty];When MList is empty
  [MCons (num atom?) (lst MList?)]);MCons is the MList constructor 
(test (MEmpty) (MEmpty))
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
;Is a data type with a null leaf TLEmpty and a type constructor Noden
(define-type NTree 
  [TLEmpty];leaf is empty.
  [NodeN (node atom?) (lst (lambda (x) (every? NTree? x)))]);determines if is a list of nodes
(test (TLEmpty) (TLEmpty))
(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty)))
      (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))
;this function indicates a position in the Cartesian plane
(define-type Position
  [2D-Point (x number?) (y number?)])
(test (2D-Point 0 0) (2D-Point 0 0))
(test (2D-Point 1 (sqrt 2)) (2D-Point 1 1.4142135623730951))
;define geometric figures
(define-type Figure
  [Circle (c Position?) (r number?)]
  [Square (p Position?) (l number?)]
  [Rectangle (p Position?) (a number?) (l number?)])
(test (Circle (2D-Point 2 2) 2) (Circle (2D-Point 2 2) 2))
(test (Square (2D-Point 0 3) 3) (Square (2D-Point 0 3) 3))
(test (Rectangle (2D-Point 0 2) 2 3) (Rectangle (2D-Point 0 2) 2 3))

;; Seccion II
;auxiliary method for function setValueA (change value)
(define (changeVal lst p v a)
  (cond
    [(empty? lst) '()]
    [(eq? p a) (cons v (changeVal (cdr lst) p v (+ a 1)))]
    [else (cons (car lst) (changeVal (cdr lst ) p v (+ a 1)))]))
    
;Give an array of type Array 1 position and 1 numeric value v, return other array with the value of v 
;exchanged in the indicated position of the original arrangement.
(define (setvalueA ar p v)
  (type-case Array ar
             [MArray (l lst)
                     (cond
                       [(<= l p) (error "Out of bounds")]
                       [else (MArray l (changeVal lst p v 0))])]))    
(test (setvalueA (MArray 5 '(0 0 0 0 0)) 2 4) (MArray 5 '(0 0 4 0 0)))
(test (setvalueA (MArray 5 '(0 0 0 0 0)) 4 4) (MArray 5 '(0 0 0 0 4)))
(test/exn (setvalueA (MArray 5 '(0 0 0 0 0)) 5 4) "Out of bounds")
(test (setvalueA (MArray 5 '(0 0 0 0 0)) 3 4) (MArray 5 '(0 0 0 4 0)))
(test (setvalueA (MArray 5 '(0 0 0 0 0)) 1 4) (MArray 5 '(0 4 0 0 0)))

;given a type data MArray, Return a list type MList Containing all elements of the original array.
(define (list2MList lst)
  (cond
    [(empty? lst) (MEmpty)]
    [else (MCons (car lst) (list2MList (cdr lst)))]))

(define (MArray2MList ar)
  (type-case Array ar
             [MArray (l lst) (list2MList lst)]))
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
(test (MArray2MList (MArray 3 '(1 2 3))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MArray2MList (MArray 4 '(1 2 3 4))) (MCons 1 (MCons 2 (MCons 3 (MCons 4 (MEmpty))))))
(test (MArray2MList (MArray 7 '(1 2 3 4 5 6 7))) (MCons 1 (MCons 2 (MCons 3 (MCons 4 (MCons 5 (MCons 6 (MCons 7(MEmpty)))))))))

;Given a list of type MList, print it in a readable format.
(define (printML mlst)
  (type-case MList mlst
             [MEmpty () "[]"]
             [MCons (a l)
                    (if (MList? a)
                        (string-append "[" (printML a) (print-aux l))
                        (string-append "[" (~a a) (print-aux l)))]))

(define (print-aux mlst)
  (type-case MList mlst
             [MEmpty () "]"]
             [MCons (a l)
                    (if (MList? a)
                        (string-append ", " (printML a) (print-aux l))
                        (string-append ", " (~a a) (print-aux l)))]))
(test (printML (MEmpty)) "[]")
(test (printML (MCons 7 (MEmpty))) "[7]")
(test (printML (MCons 7 (MCons 4 (MEmpty)))) "[7, 4]")
(test (printML (MCons (MCons 1 (MCons 2 (MEmpty))) (MCons 3 (MEmpty)))) "[[1, 2], 3]")
(test (printML (MCons (MCons 1 (MCons 2 (MEmpty)))
                      (MCons (MCons 2 (MCons 3 (MEmpty))) (MEmpty)))) "[[1, 2], [2, 3]]")
(test (printML (MCons 7  (MCons (MCons 1 (MCons 2 (MEmpty)))
                                (MCons 6 (MEmpty))))) "[7, [1, 2], 6]")

(define (concatML lst1 lst2)
  (type-case MList lst1
             [MEmpty () lst2]
             [MCons (n lst) (MCons n (concatML lst lst2))]))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty)))
      (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MCons 10 (MEmpty))))
      (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty))))))

(define (lengthML lst)
  (type-case MList lst
             [MEmpty () 0]
             [MCons (n l) (+ 1 (lengthML l))]))
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)

(define (mapML f lst)
  (type-case MList lst
             [MEmpty () (MEmpty)]
             [MCons (n l) (MCons (f n) (mapML f l))]))
(test (mapML add1 (MCons 7 (MCons 4 (MEmpty))))
      (MCons 8 (MCons 5 (MEmpty))))
(test (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty))))
      (MCons 100 (MCons 9 (MEmpty))))
