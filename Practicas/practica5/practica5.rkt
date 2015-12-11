#lang plai

(require "practica5-base.rkt")

(print-only-errors true)

(define (with-nest bs wexpr)
  (cond
    [(empty? bs) wexpr]
    [else (withS (list (car bs)) (with-nest (cdr bs) wexpr))]))

(define (desugarl lFAE)
  (cond
    [(empty? lFAE) '()]
    [else (cons (desugar (car lFAE)) (desugarl (cdr lFAE)))]))

(define (desugar-aux lst)
  (cond
    [(empty? lst) '()]
    [else (cons (bind-name (car lst)) (desugar-aux (cdr lst)))]))

(define (desugar-aux2 lst)
  (cond
    [(empty? lst) '()]
    [else (cons (desugar (bind-val (car lst))) (desugar-aux2 (cdr lst)))]))
    
;; desugar : RCFAELS -> RCFAEL
;; takes a RCFAELS expression and transforms it into a RCFAEL expression
(define (desugar expr)
  (type-case RCFAELS expr
   [MEmptyS () (MEmpty)]
   [MConsS (a b) (MCons (desugar a) (desugar b))]          
   [boolS (b) (bool b)]
   [IfS (c t e) (If (desugar c) (desugar t) (desugar e))]
   [isequal?S (e1 e2) (isequal? (desugar e1) (desugar e2))]
   [opS (f e1) (op f (desugar e1))]
   [recS (id e b) (rec id (desugar e) (desugar b))]
   [numS (n) (num n)]
   [withS (bind body)
          (app (fun (desugar-aux bind) (desugar body)) (desugar-aux2 bind))]
   [with*S (bs body) (desugar (with-nest bs body))]
   [idS (n) (id n)]
   [funS (p b) (fun p (desugar b))]
   [appS (f a) (app (desugar f) (desugarl a))]
   [binopS (f l r) (binop f (desugar l) (desugar r))]))

(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (desugar (parse '{with {{x 5} {y 7} {z 2}} {+ 1 2}})) (app (fun '(x y z) (binop + (num 1) (num 2))) (list (num 5) (num 7) (num 2))))
(test (desugar (parse '{with {{x {* 5 5}} {y 7} {z {/ 10 2}}} {+ x y}})) (app (fun '(x y z) (binop + (id 'x) (id 'y))) (list (binop * (num 5) (num 5)) (num 7) (binop / (num 10) (num 2)))))

(define (cparse sexp)
  (desugar (parse sexp)))

;; AUXILIARES
(define (aux-env params args env olenv)
  (cond
    [(empty? params) env]
    [else (aux-env (cdr params) (cdr args) (aSub (car params) (interp (car args) olenv) env) olenv)]))

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup (string-append (~a name) " symbol is not in the env"))] 
    [aSub (bound-name bound-value env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name env))]
    [aRecSub (bound-name boxed-value env)
              (if (symbol=? bound-name name)
                  (unbox boxed-value)
                  (lookup name env))]))

(define (binOp f l r)
   (cond
    [(and (numV? l) (numV? r)) (f (numV-n l) (numV-n r))]
    [(and (boolV? l) (boolV? r)) (f (boolV-b l) (boolV-b r))]
    [else (error 'operandos "No es posible operar dos  tipos distintos con el procedimiento")]))


(define (Op f s)
  (cond
    [(numV? s) (f (numV-n s))]
    [(boolV? s) (f (boolV-b s))]
    [(MEmptyV? s) (f (list))]
    [(MConsV? s) (f (list (MConsV-f s) (MConsV-r s)))])
  )

;; Seccion de auxiliares para manejar a las listas
(define (isList? l)
  (or (MEmptyV? l) (MConsV? l)))

(define (listlong l)
  (cond
    [(MEmptyV? l) 0]
    [(MConsV? l) (+ (listlong (MConsV-f l)) (listlong (MConsV-r l)))]
    [else 1]))

(define (aux-idem l1 l2)
  (cond
    [(and (MEmptyV? l1) (MEmptyV? l2)) #t]
    [(and (MConsV? l2) (MConsV? l1))
     (and (compare (MConsV-f l1) (MConsV-f l2)) (compare (MConsV-r l1) (MConsV-r l2)))]
    [else #f]))

(define (idem l1 l2)
  (cond
    [(= (listlong l1) (listlong l2)) (aux-idem l1 l2)]
    [else #f]))

;;; Termina seccion auxiliar de manejo de listas

(define (compare e1 e2)
  (cond
    [(and (numV? e1) (numV? e2)) (= (numV-n e1) (numV-n e2))]
    [(and (boolV? e1) (boolV? e2)) (equal? (boolV-b e1) (boolV-b e2))]
    [(and (isList? e1) (isList? e2)) (idem e1 e2)]
    [else (error 'e "La aplicación de isequal? no es adecuada")]))

;; cyclically-bind-and-interp : symbol RCFAE env → env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box ( numV 1729 ))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
         (begin
           (set-box! value-holder named-expr-val)
           new-env)))

;; interp : FAE -> FAE
;; evaluates FAE expressions by reducing them to their corresponding values
;; return values are either num or fun
(define (interp expr env)
  (type-case RCFAEL expr
             [num (n) (numV n)]
             [id (v) (lookup v env)]
             [fun (bound-ids bound-body)
                  (closureV bound-ids bound-body env)]
             [app (func args)
                  (local ([define funv (interp func env)])
                         (if (closureV? funv)
                             (interp (closureV-body funv)
                                     (aux-env (closureV-param funv) args (closureV-env funv) env))
                             (error 'semantic "cannot apply arguments to an expression which is not a function")))]
             [binop (f l r) (local ([define res (binOp f (interp l env) (interp r env))])
                                   (cond
                                     [(boolean? res) (boolV res)]
                                     [(number? res) (numV res)]
                                     [else (error 'wut "jijode su")]))]
             [bool (b) (boolV b)]
             [If (c t e) (if (equal? (interp c env) (boolV #t)) (interp t env) (interp e env))]
             [MEmpty () (MEmptyV)]
             [MCons (f r) (MConsV (interp f env) (interp r env))]
             [isequal? (e1 e2) (boolV (compare (interp e1 env) (interp e2 env)))]
             [op (f s) (local ([define res (Op f (interp s env))])
                                   (cond
                                     [(boolean? res) (boolV res)]
                                     [(number? res) (numV res)]
                                     [(list? res) (first res)]
                                     [else res]))]
             [rec (id expr body) (interp body
                                  (cyclically-bind-and-interp id
                                                              expr
                                                              env))]
             ))



(define (rinterp expr)
  (interp expr (mtSub)))


;;;;;;;;;;;;;;;;;;; Tests practica 4 ;;;;;;;;;;;;;;;;
(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))


;;;;;;;;;;;;;;;;;;; Tests practica 5 ;;;;;;;;;;;;;;;;
(test (rinterp (cparse '{if (< {+ 2 2} {* 2 2}) 1024 2048})) (numV 2048))
(test (rinterp (cparse '{if (<= {+ 2 2} {* 2 2}) 1024 2048})) (numV 1024))
(test (rinterp (cparse '{or true false})) (boolV #t))
(test (rinterp (cparse '{equal? 4 5})) (boolV #f))
(test (rinterp (cparse '{equal? 5 5})) (boolV #t))

(test (rinterp (cparse '{inc 9})) (numV 10))
(test (rinterp (cparse '{dec 10})) (numV 9))
(test (rinterp (cparse '{zero? 0})) (boolV #t))
(test (rinterp (cparse '{num? false})) (boolV #f))
(test (rinterp (cparse '{bool? true})) (boolV #t))
(test (rinterp (cparse '{rest {lista 4 10 5}})) (MConsV (numV 10) (MConsV (numV 5) (MEmptyV))))
(test (rinterp (cparse '{first {lista 4 (lista 10 5)}})) (numV 4))
(test (rinterp (cparse '{empty? {lista }})) (boolV #t))
(test (rinterp (cparse '{list? {lista }})) (boolV #t))
(test (rinterp (cparse '{list? {with {{r 4}} {+ 3 r}}})) (boolV #f))
(test (rinterp (cparse '{and {> 4 2} {>= 6 6}})) (boolV #t))

(test (rinterp (cparse true)) (boolV true))
(test (rinterp (cparse false)) (boolV false))
(test (rinterp (cparse '(equal? 4 5))) (boolV false))
(test (rinterp (cparse '(< 3 4))) (boolV true))
(test (rinterp (cparse (< 4 3))) (boolV false))
(test (rinterp (cparse (> 7 4))) (boolV true))
(test (rinterp (cparse (> 2 3))) (boolV false))
(test (rinterp (cparse (<= 3 3))) (boolV true))
(test (rinterp (cparse (<= 4 5))) (boolV true))
(test (rinterp (cparse (>= 3 3))) (boolV true))
(test (rinterp (cparse (>= 9 5))) (boolV true))
(test (rinterp (cparse {+ 3 4})) (numV 7))
(test (rinterp (cparse {- 4 3})) (numV 1))
(test (rinterp (cparse {* 4 3})) (numV 12))
(test (rinterp (cparse {/ 8 4})) (numV 2))
(test (rinterp (cparse {add1 1})) (numV 2))
(test (rinterp (cparse {sub1 1})) (numV 0))
(test (rinterp (cparse {sub1 1})) (numV 0))
(test (rinterp (cparse {zero? 0})) (boolV true))
(test (rinterp (cparse {zero? 2})) (boolV false))
(test (rinterp (cparse {num? (num 9)})) (boolV true))
(test (rinterp (cparse {not false})) (boolV true))
(test (rinterp (cparse {not true})) (boolV false))
(test (rinterp (cparse {boolean? true})) (boolV true))
(test (rinterp (cparse {first '(1,2)})) (numV 1))
(test (rinterp (cparse {empty? '(1,2)})) (boolV false))
(test (rinterp (cparse {list? '(1,2)})) (boolV true))

(test (rinterp (cparse '{rec (fac (fun (n) (if (zero? n)
                                   1
                                   (* n (fac (dec n)))
                                   ))) (fac 3)})) (numV 6))
(test (rinterp (cparse '{rec (fac (fun (n) (if (zero? n)
                                   1
                                   (* n (fac (dec n)))
                                   ))) (fac 10)})) (numV 3628800))

(test (rinterp (cparse '{rec (fib (fun (n)
                                       (if (<= n 1)
                                           n
                                           (+ (fib (- n 1)) (fib (- n 2)))))) (fib 6)} )) (numV 8))
(test (rinterp (cparse '{rec (fib (fun (n)
                                       (if (<= n 1)
                                           n
                                           (+ (fib (- n 1)) (fib (- n 2)))))) (fib 17)} )) (numV 1597))


