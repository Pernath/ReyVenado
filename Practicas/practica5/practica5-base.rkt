#lang plai

(define-type Binding
  [bind (name symbol?) (val FAES?)])

;(define-type MyListS
;  [Mempty]
; [Mcons (expr FAES?) (expr2 FAES?)])

;(define-type MyList
;  [Mempty]
;  [Mcons (expr FAE?) (expr2 FAE?)])


(define-type FAES
  [MEmptyS]
  [MConsS (expr FAES?) (expr2 FAES?)]
  [boolS (b boolean?)]
  [IfS (c FAES?) (t FAES?) (e FAES?)]
  [isequal?S (e1 FAES?) (e2 FAES?)]
  [opS (f procedure?) (s FAES?)]
  [recS (id symbol?) (expr FAES?) (body FAES?)]
  [numS (n number?)]
  [withS (bindings (listof bind?))
         (body FAES?)]
  [with*S (bindings (listof bind?))
          (body FAES?)]
  [idS (name symbol?)]
  [funS (params (listof symbol?))
        (body FAES?)]
  [appS (fun FAES?)
        (args (listof FAES?))]
  [binopS (f procedure?)
         (l FAES?)
         (r FAES?)])


(define-type FAE
  ;[MList (l MyList?)]
  [MEmpty]
  [MCons (expr FAE?) (expr2 FAE?)]
  [If (c FAE?) (t FAE?) (e FAE?)]
  [bool (b boolean?)]
  [isequal? (e1 FAE?) (e2 FAE?)]
  [op (f procedure?) (s FAE?)]
  [rec (id symbol?) (expr FAE?) (body FAE?)]
  [num (n number?)]
  [id (name symbol?)]
  [fun (params (listof symbol?))
       (body FAE?)]
  [app (fun FAE?)
       (args (listof FAE?))]
  [binop (f procedure?)
         (l FAE?)
         (r FAE?)])


  

(define-type FAE-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [MEmptyV]
  [MConsV (f FAE-Value?) (r FAE-Value?)]
  [closureV (param (listof symbol?))
            (body FAE?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value FAE-Value?) 
        (env Env?)]
  [aRecSub (name symbol?)
           (val in-box?)
           (env Env?)])

(define (in-box? v)
  (and (box? v)
       (FAE-Value? (unbox v))))

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige-un s)
  (case s
    [(inc) add1]
    [(dec) sub1]
    [(zero?) zero?]
    [(num?) num?]
    [(neg) not]
    [(bool?)  boolean?]
    [(first) first]
    [(rest) rest]
    [(empty?) empty?]
    [(list?) list?]))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) (lambda (x y) (and x y))]
    [(or) (lambda (x y) (or x y))]))
  
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

(define (parseL l)
  (cond
    [(empty? (cdr l)) (MConsS (parse (car l)) (MEmptyS))]
    [else (MConsS (parse (car l)) (parseL (cdr l)))]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> FAES
(define (parse sexp)
  (cond
    [(equal? sexp 'true) (boolS #t)]
    [(equal? sexp 'false) (boolS #f)]
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)];; nuevo
    [(list? sexp)
     (case (car sexp)
       [(if) (IfS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))] ;; nuevo
       [(equal?) (isequal?S (parse (cadr sexp)) (parse (caddr sexp)))]
       [(lista) (if (empty? (cdr sexp))
                           (MEmptyS)    
                           (MConsS (parse (cadr sexp)) (parseL (cddr sexp))))]
       [(inc dec zero? num? neg bool? first rest empty? list?) (opS (elige-un (car sexp)) (parse (cadr sexp)))]
       [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(+ - / * > < <= >= and or) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(rec) (recS (caadr sexp) (parse (cadadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
