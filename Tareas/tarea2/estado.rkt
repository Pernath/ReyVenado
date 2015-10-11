#lang plai

(define (fib n)
  (cond
    [(zero? n) 0]
    [(eq? n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (change-state s ns)
  (define x s)
  (let ([y (fib x)])
    (set! x ns)
    y))
