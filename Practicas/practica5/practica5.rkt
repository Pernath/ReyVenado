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
    
;; desugar : FAES -> FAE
;; takes a FAES expression and transforms it into a FAE expression
(define (desugar expr)
  (type-case FAES expr
   [MEmptyS () MEmpty]
   [MConsS (a b) (MCons (desugar a) (desugar b))]          
   [boolS (b) (bool b)]
   [IfS (c t e) (If (desugar c) (desugar t) (desugar e))]
   [isequal?S (e1 e2) (isequal? (desugar e1) (desugar e2))]
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
              (lookup name env))]))

(define (binOp f l r)
  (numV (f (numV-n l) (numV-n r))))


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



;; interp : FAE -> FAE
;; evaluates FAE expressions by reducing them to their corresponding values
;; return values are either num or fun
(define (interp expr env)
  (type-case FAE expr
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
             [binop (f l r) (binOp f (interp l env) (interp r env) )]
             [bool (b) (boolV b)]
             [If (c t e) (if (equal? (interp c) (boolV #t)) (interp t env) (interp e env))]
             [MEmpty () MEmptyV]
             [MCons (f r) (MConsV (interp f env) (interp r env))]
             [isequal? (e1 e2) (boolV (compare (interp e1 env) (interp e2 env)))]
             ))



(define (rinterp expr)
  (interp expr (mtSub)))

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
