#lang plai

(require "practica3-base.rkt")
(print-only-errors true)

;;;;Section I. Heart Rate Zones;;;;

;;Auxiliary function that calculates the minimum of the heart-rate according to the zone
(define (aux-min rheart-rate mrange i)
  (+ rheart-rate (* mrange (+ 0.5 (* 0.1 (- i 1))))))

;;Auxiliary function that calculates the maximum of the heart-rate according to the zone
(define (aux-max rheart-rate mrange i)
  (- (+ rheart-rate (* mrange (+ 0.5 (* 0.1 i)))) 1))

;; 1.
; Given the resting and the maximum heart-rate of a person, this function returns the list of
; heart-rate zones. 
(define (zones rheart-rate mheart-rate)  
  (define mrange (- mheart-rate rheart-rate)) 
  (list
   (resting rheart-rate (+ rheart-rate (- (* mrange 0.5) 1)))
   (warm-up (aux-min rheart-rate mrange 1) (aux-max rheart-rate mrange 1))
   (fat-burning  (aux-min rheart-rate mrange 2) (aux-max rheart-rate mrange 2))
   (aerobic (aux-min rheart-rate mrange 3) (aux-max rheart-rate mrange 3))
   (anaerobic  (aux-min rheart-rate mrange 4) (aux-max rheart-rate mrange 4))
   (maximum (aux-min rheart-rate mrange 5) (aux-max rheart-rate mrange 5))))

;;;; zones function testing;;;;
(test (zones 50 180)
      (list
       (resting 50 114.0)
       (warm-up 115.0 127.0)
       (fat-burning 128.0 140.0)
       (aerobic 141.0 153.0)
       (anaerobic 154.0 166.0)
       (maximum 167.0 179.0)))
(test (zones 90 100)
      (list
       (resting 90 94.0)
       (warm-up 95.0 95.0)
       (fat-burning 96.0 96.0)
       (aerobic 97.0 97.0)
       (anaerobic 98.0 98.0)
       (maximum 99.0 99.0)))
(test (zones 40 100)
      (list
       (resting 40 69.0)
       (warm-up 70.0 75.0)
       (fat-burning 76.0 81.0)
       (aerobic 82.0 87.0)
       (anaerobic 88.0 93.0)
       (maximum 94.0 99.0)))    
(test (zones 60 120)
      (list
       (resting 60 89.0)
       (warm-up 90.0 95.0)
       (fat-burning 96.0 101.0)
       (aerobic 102.0 107.0)
       (anaerobic 108.0 113.0)
       (maximum 114.0 119.0)))
(test (zones 30 50)
      (list
       (resting 30 39.0)
       (warm-up 40.0 41.0)
       (fat-burning 42.0 43.0)
       (aerobic 44.0 45.0)
       (anaerobic 46.0 47.0)
       (maximum 48.0 49.0)))


; This definition is for testing purposes
(define my-zones (zones 50 180))

;; 2
; Given the name of a heart-rate zone, this function returns the HRZ (heart-rate zone) 
; corresponding
(define (get-zone sym zlst)
  (cond
    [(eq? sym 'resting) (car zlst)]
    [(eq? sym 'warm-up) (cadr zlst)]
    [(eq? sym 'fat-burning) (caddr zlst)]
    [(eq? sym 'aerobic) (cadddr zlst)]
    [(eq? sym 'anaerobic) (first (cddddr zlst))]
    [(eq? sym 'maximum) (second (cddddr zlst))]))

;;   get-zone function testing
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 179.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))

;; Auxiliary function for deconstructing the HRZ data structure. It returns the low value of the 
;HRZ
(define (get-low hrz)
  (type-case HRZ hrz
             [resting (l h) l]
             [warm-up (l h) l]
             [fat-burning (l h) l]
             [aerobic (l h) l]
             [anaerobic (l h) l]
             [maximum (l h) l]))

;; Auxiliary function for deconstructing the HRZ data structure. It returns the high value of the
; HRZ
(define (get-high hrz)
  (type-case HRZ hrz
             [resting (l h) h]
             [warm-up (l h) h]
             [fat-burning (l h) h]
             [aerobic (l h) h]
             [anaerobic (l h) h]
             [maximum (l h) h]))

;; Auxiliary function. Given a heart-rate and a list of HRZ, filters all the HRZ for which the 
; heart-rate lies between their low and high values. 
(define (get-hrz hr hrz-lst)
  (cond
    [(empty? hrz-lst) null]
    [(and (>= hr (get-low (car hrz-lst)))
          (<= hr (get-high (car hrz-lst)))) (car hrz-lst)]    
    [else (get-hrz hr (cdr hrz-lst))]))

;; 3.
; Given a list of heart-rate values and a list of HRZ, this function returns a list of HRZ for 
; each heart-rate (contained in the first list) that lies between its low and high values.
(define (bpm->zone hr-lst hrz-lst)
  (cond
    [(empty? hr-lst) '()]
    [else (cons (get-hrz (car hr-lst) hrz-lst)
                (bpm->zone (cdr hr-lst) hrz-lst))]))    

;; bpm->zone function testing
(test (bpm->zone empty my-zones) '())
(test (bpm->zone '(50 60) my-zones)
      (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(140 141) my-zones)
      (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))
(test (bpm->zone '(63 54 98) my-zones)
      (list (resting 50 114.0) (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(55 120 130 150 170) my-zones)
      (list (resting 50 114.0) (warm-up 115.0 127.0)  (fat-burning 128.0 140.0)
            (aerobic 141.0 153.0) (maximum 167.0 179.0)))

;; Auxiliary function to transform a single list (in which the first element corresponds to a UNIX time instant, the second a list with a pair of latitude and longitude values, and the last a heart-rate value) into a trackpoint instance. 
(define (create-trackpoint lst1 hrz-lst)
  (define gps (GPS (car (cadr lst1)) (cadr (cadr lst1))))
  (trackpoint gps (caddr lst1) (get-hrz (caddr lst1) hrz-lst) (car lst1)))

;; 4.
; Given a list of lists (with the format described in the comment of the auxiliary funcition), returns a trackpoint for each list contained, all of them clustered in a list. 
(define (create-trackpoints lst hrz-lst)
  (cond
    [(empty? lst) '()]
    [else (map (lambda (x) (create-trackpoint x hrz-lst)) lst)]))

;; create-trackṕoints function testing
(test (create-trackpoints '() my-zones) '())
(test (create-trackpoints (take raw-data 4) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (create-trackpoints (take raw-data 6) my-zones)
      (append (create-trackpoints (take raw-data 4) my-zones)
              (list
               (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
               (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675))))
(test (create-trackpoints (take raw-data 10) my-zones)
      (append (create-trackpoints (take raw-data 6) my-zones)
              (list
               (trackpoint (GPS 19.490702 -99.2413217) 115 (warm-up 115.0 127.0) 1425619678)
               (trackpoint (GPS 19.4906902 -99.2413796) 115 (warm-up 115.0 127.0) 1425619681)
               (trackpoint (GPS 19.4906865 -99.241445) 120 (warm-up 115.0 127.0) 1425619685)
               (trackpoint (GPS 19.4906861 -99.2415517) 119 (warm-up 115.0 127.0) 1425619690))))
(test (create-trackpoints (take raw-data 11) my-zones)
      (append (create-trackpoints (take raw-data 10) my-zones)
              (list
               (trackpoint (GPS 19.4906905 -99.2416019) 120 (warm-up 115.0 127.0) 1425619693))))

;; Converts a number to radians
(define (to-radians v)
  (/ (* v pi) 180))

;; Auxiliary function for haversine
(define (haversine-aux l1 l2)
  (define lat1 (to-radians (GPS-lat l1)))
  (define lat2 (to-radians (GPS-lat l2)))
  (define lat-diff (to-radians (- (GPS-lat l2) (GPS-lat l1))))
  (define long-diff (to-radians (- (GPS-long l2) (GPS-long l1))))
  (+ (sqr (sin (/ lat-diff 2))) (* (cos lat1) (* (cos lat2) (sqr (/ long-diff 2))))))

;; Given two GPS type values, calculates the distance between two places
(define (haversine l1 l2)
  (define R 6371)
  (define a (haversine-aux l1 l2))
  (define c (* 2 (asin (sqrt a))))
  (*  R c))

;; Auxiliary function for total-distance
(define (distance-aux ent lst)
  (cond
    [(empty? lst) 0]
    [else (haversine (trackpoint-loc ent) (trackpoint-loc (car lst)))]))

;; 5
; Given a trackpoints list, returns the traveled distance
(define (total-distance lst)
  (cond
    [(empty? lst) 0]
    [else (+ (distance-aux (car lst) (cdr lst)) (total-distance (cdr lst)))]))

(define sample (create-trackpoints (take raw-data 100) my-zones))
(define trackpoints (create-trackpoints raw-data my-zones))
(test (total-distance trackpoints) 5.051934549322941)
(test (total-distance sample) 0.9509291243812747)
(test (total-distance (create-trackpoints (take raw-data 4) my-zones)) 0.007864840450838653)
(test (total-distance (create-trackpoints '() my-zones)) 0)
(test (total-distance (create-trackpoints (take raw-data 50) my-zones)) 0.4610032616051818)

;; Auxiliary function for average-hr
(define (sum-all lst)
    (cond
      [(empty? lst) 0]
      [else (+ (trackpoint-hr (car lst)) (sum-all (cdr lst)))]))
      
;; 6
; Given a trackpoints list, returns the average heart rate
(define (average-hr lst)
  (cond
    [(empty? lst) 0]
    [else (floor (/ (sum-all lst) (length lst)))]))
  
(test (average-hr sample) 134)
(test (average-hr trackpoints) 150)
(test (average-hr (create-trackpoints (take raw-data 25) my-zones)) 121)
(test (average-hr (create-trackpoints '() my-zones)) 0)
(test (average-hr (create-trackpoints (take raw-data 200) my-zones)) 141)

;; Auxiliary function for max-hr
(define (max-aux m lst)
  (cond
    [(empty? lst) m]
    [(>= m (trackpoint-hr (car lst))) (max-aux m (cdr lst))]
    [else (max-aux (trackpoint-hr (car lst)) (cdr lst))]))

;; 7
; Given a trackpoints list, returns the highest heart rate
(define (max-hr lst)
  (max-aux 0 lst))
  
(test (max-hr sample) 147)
(test (max-hr trackpoints) 165)
(test (max-hr (create-trackpoints (take raw-data 42) my-zones)) 136)
(test (max-hr (create-trackpoints '() my-zones)) 0)
(test (max-hr (create-trackpoints (take raw-data 200) my-zones)) 165)

;; Auxiliary function for collapse-trackpoints
(define (collapse-aux lst e ltp)
  (cond
    [(empty? lst) (list ltp)]
    [(and (= (trackpoint-hr (car lst)) (trackpoint-hr ltp))
          (<= (haversine (trackpoint-loc ltp) (trackpoint-loc (car lst))) e))
          (collapse-aux (cdr lst) e (car lst))]
     [else (cons ltp (collapse-aux (cdr lst) e (car lst)))]))

;; 8
; Given a trackpoints list and an e epsilon, returns a new list with
; collapsed consecutive elements such that each element in a collapsed set
; has the same heart rate and the distance between each consecutive point 
; is less or equal than e
(define (collapse-trackpoints lst e)
  (cond
    [(empty? lst) '()]
    [else (collapse-aux (cdr lst) e (car lst))]))

(define sample-four (create-trackpoints (take raw-data 4) my-zones))
(test (collapse-trackpoints sample-four 0.01)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
        (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
        (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (collapse-trackpoints (create-trackpoints (take raw-data 11) my-zones) 0.01)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (warm-up 115.0 127.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (warm-up 115.0 127.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (warm-up 115.0 127.0) 1425619690)
       (trackpoint (GPS 19.4906905 -99.2416019) 120 (warm-up 115.0 127.0) 1425619693)))
(test (collapse-trackpoints (create-trackpoints '() my-zones) 0.01) '())
(test (collapse-trackpoints (create-trackpoints (take raw-data 15) my-zones) 0.01)
      (append
       (collapse-trackpoints (create-trackpoints (take raw-data 11) my-zones) 0.01)
       (list
        (trackpoint (GPS 19.4907662 -99.2417367) 122 (warm-up 115.0 127.0) 1425619702)
        (trackpoint (GPS 19.4908105 -99.2418386) 123 (warm-up 115.0 127.0) 1425619709)
        (trackpoint (GPS 19.4908799 -99.2419175) 123 (warm-up 115.0 127.0) 1425619713)
        (trackpoint (GPS 19.491003 -99.2419904) 127 (warm-up 115.0 127.0) 1425619718))))
(test (collapse-trackpoints (create-trackpoints (take raw-data 20) my-zones) 0.01)
      (append
       (collapse-trackpoints (create-trackpoints (take raw-data 15) my-zones) 0.01)
       (list
        (trackpoint (GPS 19.4910947 -99.2420837) 128 (fat-burning 128.0 140.0) 1425619722)
        (trackpoint (GPS 19.491214 -99.2421806) 128 (fat-burning 128.0 140.0) 1425619726)
        (trackpoint (GPS 19.4913238 -99.2422883) 128 (fat-burning 128.0 140.0) 1425619731)
        (trackpoint (GPS 19.4913761 -99.2423724) 130 (fat-burning 128.0 140.0) 1425619735)
        (trackpoint (GPS 19.4914257 -99.2424697) 131 (fat-burning 128.0 140.0) 1425619740))))



;;;;;;;;;; Tree Traversals ;;;;;;;;;;

;; 13.
; Given a binary tree instance, returns the elements contained in it, visiting each one of 
; them with a pre-order tree traversal.
(define (preorderBT bt)
  (type-case BTree bt
             [EmptyBT () '()]
             [BNode (c l e r) (cons e (append (preorderBT l) (preorderBT r)))]))

(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (preorderBT (EmptyBT)) '())
(test (preorderBT bt1) '(1 2 3 4 5))
(test (preorderBT bt2) '(3 4 5 8 3))
(test (preorderBT bt3) '("f" "c" "h" "ahhh" "dd" "as"))
(test (preorderBT bt4) '("eeeeeee" "ee" "e" "eee" "eeeee" "eeee" "eeeeee" "eeeeeeee"))
