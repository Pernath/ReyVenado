#lang plai
;Función Potencia.
(define pow
  (lambda (a b)
    (cond
      [(zero? b) 1]
      [(eq? b 1) a]
      [else (* a (pow a (- b 1)))])))
      
      
;Función Average.
(define average_aux
  (lambda (lst t l)
    (cond
      [(empty? lst) (floor (/ t l))]
      [else (average_aux (cdr lst) (+ (car lst) t) (+ l 1))])))
      
(define average
  (lambda (lst)
    (cond
      [(empty? lst) 0]
      [else (average_aux lst 0 0)]))) 

;Función zip
(define zip
  (lambda (lst1 lst2) ;recibe como parametro dos listas
    (cond
      ((empty? lst1)empty) ;si la lista 1 es vacía regresa null/vacio
      ((empty? lst2)empty) ;si la lista 2 es vacía regresa null/vacio
      [else (cons(list (car lst1) (car lst2)) ;si la lista tiene 2 elementos, entonces al primer elemento
                 (zip (cdr lst1)(cdr lst2)))])));de lst1 y lst2 se pasa a la primera lista (la del lado izquierdo)
;(car checa la cabezade la lista) y el segundo elemento de lst1 y lst2 lo deja en la segunda lista (la del lado derecho).
