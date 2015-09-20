#lang plai

(require "practica3-base.rkt")
(print-only-errors true)

(define (zones rheart-rate mheart-rate)  
  (define mrange (- mheart-rate rheart-rate)) ;; esta es la linea que debemos modificar? cómo debemos hacerlo?
  (list
      (resting rheart-rate (+ rheart-rate (- (* mrange 0.5) 1)))
      (warm-up (aux-min rheart-rate mrange 1) (aux-max rheart-rate mrange 1))
      (fat-burning  (aux-min rheart-rate mrange 2) (aux-max rheart-rate mrange 2))
      (aerobic (aux-min rheart-rate mrange 3) (aux-max rheart-rate mrange 3))
      (anaerobic  (aux-min rheart-rate mrange 4) (aux-max rheart-rate mrange 4))
      (maximum (aux-min rheart-rate mrange 5) (aux-max rheart-rate mrange 5))))

(define (aux-min rheart-rate mrange i)
    (+ rheart-rate (* mrange (+ 0.5 (* 0.1 (- i 1))))))

(define (aux-max rheart-rate mrange i)
    (- (+ rheart-rate (* mrange (+ 0.5 (* 0.1 i)))) 1))

(define my-zones (zones 50 180))

(define (get-zone sym zlst)
  (cond
    [(eq? sym 'resting) (car zlst)]
    [(eq? sym 'warm-up) (cadr zlst)]
    [(eq? sym 'fat-burning) (caddr zlst)]
    [(eq? sym 'aerobic) (cadddr zlst)]
    [(eq? sym 'anaerobic) (first (cddddr zlst))]
    [(eq? sym 'maximum) (second (cddddr zlst))]))

(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))
