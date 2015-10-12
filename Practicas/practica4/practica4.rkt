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
