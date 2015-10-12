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
