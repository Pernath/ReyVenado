#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

(define (with-nest bs wexpr)
  (cond
    [(empty? bs) wexpr]
    [else (withS (list (car bs)) (with-nest (cdr bs) wexpr))]))
