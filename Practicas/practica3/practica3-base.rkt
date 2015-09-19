#lang plai

(require 2htdp/image)
(require xml)
(require racket/date)
(require (only-in srfi/19 string->date))

(print-only-errors false)

;; any? Given any value x, regresa #t
(define (any? x) #t)

;; Helper functions for handling xexpr nodes
(define (xe->string xe)
  (string-append* (filter string? xe)))

(define (xe->number xe)
  (string->number (xe->string xe)))

(define (xe-string=? xe str)
  (string=? str (xe->string xe)))

;; Data Types from section I
(define-type HRZ
  [resting (low number?)
           (high number?)]
  [warm-up (low number?)
           (high number?)]
  [fat-burning (low number?)
               (high number?)]
  [aerobic (low number?)
           (high number?)]
  [anaerobic (low number?)
             (high number?)]
  [maximum (low number?)
           (high number?)])

(define-type Coordinate
  [GPS (lat number?)
       (long number?)])

(define-type Frame
  [trackpoint (loc GPS?)
              (hr exact-integer?)
              (zone HRZ?)
              (unix-time exact-integer?)])

(define (parse-position xe)
  (match xe
    [`(Position () ,lat ,long) (list (xe->number lat)
                                     (xe->number long))]))

(define (parse-hr xe)
  (match xe
    [`(HeartRateBpm () ,value) (xe->number value)]))

(define (parse-trackpoint xe)
  (define (create-trackpoint time position hr)
    (list (date->seconds (string->date (xe->string time) "~Y-~m-~dT~H:~M:~S"))
          (parse-position position)
          (parse-hr hr)))
  (match xe
    [`(Trackpoint () ,time , position ,altitude ,hr) (create-trackpoint time position hr)]
    [`(Trackpoint () ,time , position ,hr) (create-trackpoint time position hr)]))

(define (parse-track xe)
  (match xe
    [`(Track () ,trackpoints ...) (map parse-trackpoint trackpoints)]))

(define (parse-lap xe)
  (match xe
    [`(Lap ,start ,total ,distance ,calories ,avghr ,maxhr ,intensity ,trigger ,track) (parse-track track)]))

(define (parse-activity xe)
  (match xe
    [`(Activity ((Sport "Running")) ,id ,lap) (parse-lap lap)]))

(define (parse-activities xe)
  (match xe
    [`(Activities () ,activity) (parse-activity activity)]))

(define (parse-data xe)
  (match xe
    [`(TrainingCenterDatabase () ,activities) (parse-activities activities)]))

(define FILENAME "run-data.tcx")
(define run-data:xml (document-element (read-xml (open-input-file FILENAME))))

(define run-data:xml:no-ws ((eliminate-whitespace '(TrainingCenterDatabase Activities Activity Lap Track Trackpoint Position HeartRateBpm)) run-data:xml))
(define run-data:xe (xml->xexpr run-data:xml:no-ws))
(define raw-data (parse-data run-data:xe))

;; BTree
(define-type  BTree
  [EmptyBT]
  [BNode (c procedure?) ; Funcion de comparacion, recibe dos argumentos,
                        ; regresa un booleano.
         (l BTree?)
         (e any?) 
         (r BTree?)])

;; Abreviación de la hoja vacía
;
(define ebt (EmptyBT))
;(test (EmptyBT) ebt )

;; Abreviación del constructor de tipo BNode para números y para strings
;
(define-syntax-rule (bnn l v r) (BNode < l v r))
(define-syntax-rule (bns l v r) (BNode string<? l v r))
;(test (bnn ebt  4 ebt ) (BNode < (EmptyBT) 4 (EmptyBT)))
;(test (bns ebt  "hola" ebt ) (BNode string<? (EmptyBT) "hola" (EmptyBT)))

;; Ejemplos de árboles de números
;
(define arb1 (bnn ebt 1 ebt))
(define arb2 (bnn arb1 2 arb1))
(define arb3 (bnn arb2 3 arb2))
(define arb4 (bnn arb3 4 arb3))

(define arbN 
  (bnn 
   (bnn (bnn ebt 4 ebt) 2 (bnn ebt 5 ebt)) 
   1 
   (bnn (bnn (bnn ebt 7 ebt) 6 (bnn ebt 9 ebt)) 3 ebt)))

(define maxiarb (bnn arbN 10 arbN))

;; Lista de árboles numéricos
;
(define arb-list (list arb1 arb2 arb3 arb4 arbN maxiarb))

;; Ejemplos de árboles de strings
;
(define arb1s (bns ebt "a" ebt))
(define arb2s (bns arb1s "b" arb1s))
(define arb3s (bns arb2s "c" arb2s))
(define arb4s (bns arb3s "d" arb3s))

(define arbNs 
  (bns 
   (bns (bns ebt "adios" ebt) "que" (bns ebt "suc" ebt)) 
   "cloj" 
   (bns (bns (bns ebt "pow" ebt) "ext" (bns ebt "trial" ebt)) "lambda" ebt)))

(define maxiarbs (bnn arbNs "racket" arbNs))

;; Lista de árboles de strings
;
(define arbs-list (list arb1s arb2s arb3s arb4s arbNs maxiarbs))

;; Arbol base Wikipedia
;
(define arbol-base (bns (bns (bns ebt "A" ebt) "B" (bns (bns ebt "C" ebt) "D" (bns ebt "E" ebt))) 
                        "F"
                        (bns ebt "G" (bns (bns ebt "H" ebt) "I" ebt))))

;; Funciones auxiliares
;
(define (heightAB expArb)
  (type-case BTree
    expArb
    [EmptyBT () 0]
    [BNode (c l e r) (max (+ 1 (heightAB l) ) (+ 1 (heightAB r)) )]))

(define (a-label v)
    (overlay
     (if (number? v)
         (text (number->string v) 12 "black")
         (text v 12 "black"))
     (circle 15 "solid" "lightblue")))

(define white-circle (circle 15 "solid" "white"))
(define black-circle (circle 15 "solid" "black"))

(define nothing (circle 0 "solid" "white"))

(define (print-list l)
  (if (empty? l)
      nothing
      (beside (a-label (car l)) (print-list (cdr l)))))

(define (printBT-complete arb level)
  (type-case BTree arb
    [EmptyBT () (if (zero? level) nothing (above white-circle (beside (printBT-complete arb (sub1 level)) 
                                                                     (printBT-complete arb (sub1 level)))))]
    [BNode (c l v r) (if (zero? level) 
                       (a-label v)
                       (let* ((lblack (line (* 15 (expt 2 (- level 2))) -30 "black"))
                              (rblack (line (* -15 (expt 2 (- level 2))) -30 "black"))
                              (lwhite (line (* 15 (expt 2 (- level 2))) -30 "white"))
                              (rwhite (line (* -15 (expt 2 (- level 2))) -30 "white"))
                              (d (above (a-label v) (beside (printBT-complete l (sub1 level))
                                                             (printBT-complete r (sub1 level))))))
                         (cond 
                           ((and (EmptyBT? l) (EmptyBT? r)) d)
                           ((EmptyBT? l) (underlay/align/offset "middle" "top" (beside lwhite rblack) 0 -15 d))
                           ((EmptyBT? r) (underlay/align/offset "middle" "top" (beside lblack rwhite) 0 -15 d))
                           (else (underlay/align/offset "middle" "top" (beside lblack rblack) 0 -15 d)))))]))

;; Imprime la representación gráfica de un BTree
;
(define (printBT arb)
  (printBT-complete arb (heightAB arb)))

;; Murales de árboles de números
;
(define arb-drawings (map printBT arb-list))
(define arb-mural (apply beside arb-drawings))

;; Murales de árboles de strings
;
(define arbs-drawings (map printBT arbs-list))
(define arbs-mural (apply beside arbs-drawings))