;--------------------------------------------------------
; Procedure Extensions
;(define (Extension bcut)
  ;(define ext (make-hash))

;--------------------------------------------------------
; Initialisation
(define B (make-hash))
(define B0 (append* (for/list ([p Pm] #:when (> (cadr p) 0)) (build-list (cadr p) (λ(i) (list (list (car p)) '(e0)))) )))
(define (initCond)
  (for/list ([b B0]) (hash-set! B (name "b" (+ 1 (hash-count B))) b)))
(initCond)

(define Bcut B)

(define E (make-hash))

(define pe (make-hash))
(define co (make-hash))
(define structuralconflictsEns (make-hash)); ondéfinit une table de hashage qui va contenir les ensembles de conflits structurel
(define effectiveconflicts (make-hash));on definit une table de hashage qui va contenir des ensembles d'évènements en conflits


;-------------------------------------------------------------------------------------------------------------------
;Vérifier si un élémént est une condition
(define (cond? elt)
  (hash-has-key? B elt))

;-------------------------------------------------------------------------------------------------------------------
;Vérifier si un élémént est un événement
(define (event? elt)
  (hash-has-key? E elt))

;-------------------------------------------------------------------------------------------------------------------
;Fonction label
(define (λUnf n)
  (cond
    [(cond? n) (for/or ([b (hash->list B)] #:when (equal? n (car b))) (caadr b))]
    [(event? n)(for/or ([e (hash->list E)] #:when (equal? n (car e))) (caadr e))]
    [else empty]))

;-------------------------------------------------------------------------------------------------------------------
; détermine les ensembles de conflits structurels
(define sconf
(cond
[(or (empty? P) (empty? T)) empty]
[else (for/list ([post P°]
#:when (> (length post) 2))(hash-set! structuralconflictsEns (car post)(cdr post)))])) 