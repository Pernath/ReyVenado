#lang plai

;;; Laboratorio de Lenguajes de Programación ;;;
;;;;;;;;;;;;;;;;;;; Práctica 2 ;;;;;;;;;;;;;;;;;

;;;;;;;Integrantes;;;;;;;;
;;;;; Andrea González ;;;;
;;;;; Karla Esquivel ;;;;;
;;;;; Carlos Acosta ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(print-only-errors true)

;; Seccion I
(define (every? f lst)
  (cond
    [(empty? lst) #t]
    [else (and (f (car lst)) (every? f (cdr lst)))]))

(define z null)

(define-type Array
  [MArray (num (lambda (x)
                 (exact-nonnegative-integer? x)
                 (set! z x)))
          (lst (lambda (x)
                 (and (list? x) (<= (length x) z))))])
(test (MArray 4 '(1 2 3)) (MArray 4 '(1 2 3)))

(define (atom? x)
  (not (pair? x)))

(define-type MList
  [MEmpty]
  [MCons (num atom?) (lst MList?)])
(test (MEmpty) (MEmpty))
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))

(define-type NTree 
  [TLEmpty]
  [NodeN (node atom?) (lst (lambda (x) (every? NTree? x)))])
(test (TLEmpty) (TLEmpty))
(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty)))
      (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))

(define-type Position
  [2D-Point (x number?) (y number?)])
(test (2D-Point 0 0) (2D-Point 0 0))
(test (2D-Point 1 (sqrt 2)) (2D-Point 1 1.4142135623730951))

(define-type Figure
  [Circle (c Position?) (r number?)]
  [Square (p Position?) (l number?)]
  [Rectangle (p Position?) (a number?) (l number?)])
(test (Circle (2D-Point 2 2) 2) (Circle (2D-Point 2 2) 2))
(test (Square (2D-Point 0 3) 3) (Square (2D-Point 0 3) 3))
(test (Rectangle (2D-Point 0 2) 2 3) (Rectangle (2D-Point 0 2) 2 3))

;; Seccion II
(define (changeVal lst p v a)
  (cond
    [(empty? lst) '()]
    [(eq? p a) (cons v (changeVal (cdr lst) p v (+ a 1)))]
    [else (cons (car lst) (changeVal (cdr lst ) p v (+ a 1)))]))

(define (setvalueA ar p v)
  (type-case Array ar
             [MArray (l lst)
                     (cond
                       [(<= l p) (error "Out of bounds")]
                       [else (MArray l (changeVal lst p v 0))])]))    
(test (setvalueA (MArray 5 '(0 0 0 0 0)) 2 4) (MArray 5 '(0 0 4 0 0)))
(test (setvalueA (MArray 5 '(0 0 0 0 0)) 4 4) (MArray 5 '(0 0 0 0 4)))
(test/exn (setvalueA (MArray 5 '(0 0 0 0 0)) 5 4) "Out of bounds")
