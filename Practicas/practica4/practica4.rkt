#lang plai

(require "practica4-base.rkt")

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
    [mtSub () (error 'lookup (string-append (~a name) "x symbol is not in the env"))] 
    [aSub (bound-name bound-value env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name env))]))

(define (binOp f l r)
  (numV (f (numV-n l) (numV-n r))))
