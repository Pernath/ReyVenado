#lang plai

(require "practica3-base.rkt")

(define (zones rheart-rate mheart-rate)  
  (define mrange (- mheart-rate rheart-rate)) ;; esta es la linea que debemos modificar? cómo debemos hacerlo?
  (list
      (resting rheart-rate (+ rheart-rate (- (* rheart-rate 0.5) 1)))
      (warm-up (aux-min rheart-rate mrange 1) (aux-max rheart-rate mrange 1))
      (fat-burning  (aux-min rheart-rate mrange 2) (aux-max rheart-rate mrange 2))
      (aerobic (aux-min rheart-rate mrange 3) (aux-max rheart-rate mrange 3))
      (anaerobic  (aux-min rheart-rate mrange 4) (aux-max rheart-rate mrange 4))
      (maximum (aux-max rheart-rate mrange 5) (aux-max rheart-rate mrange 5))))

(define (aux-min rheart-rate mrange i)
    (+ rheart-rate (* mrange (+ 0.5 (* 0.1 (- i 1))))))

(define (aux-max rheart-rate mrange i)
    (- (+ rheart-rate (* mrange (+ 0.5 (* 0.1 i)))) 1))
