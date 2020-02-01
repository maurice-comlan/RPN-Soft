;#lang racket
; The unfolding process produces the vents E, B. The function BuildOccurrenceNet fills a structure of type ⊙N with the relevant informations


(define n1 (rPn 1 '() '() '() '() '() '() '()))
(define nu (rPn 2 '() '() '() '() '() '() '())) ; Underlying Petri net

;(define n1 (rPn 1 P T F R M0 '() 1))
(define λ (make-hash))
(define ∑ '())

(define lpn (LPN n1 ∑ λ))

(define h (make-hash))
(define On (⊙N '() '() '() '() '()))
(define Onu (⊙N '() '() '() '() '())) ; Occurrence net of the underlying petri net


(define (Third L) (if (empty? (third L)) empty (car (third L))))

(define (suppressInitalArcs L) (for/list ([e L] #:when (not (empty? (car e)))) e))

(define (resetPn2On n)
  (for/list ([arc (rPn-Reset n1)]) (list (hash->ref h (first arc)) (hash->ref h (second arc)))))

(define (initPn2On r)
  (for/list ([p (rPn-Place r)]
             [m (rPn-M0 r)]
             #:when (equal? m 1))
       (hash->ref h p)))
;-------------------------------------------------------------------
; Interface between unfolding and occurrence net
(define (BuildOccurrenceNet On lpn)
  (set-rPn-Place! n1 P)(set-rPn-Transition! n1 T)(set-rPn-Flow! n1 F)(set-rPn-Reset! n1 R)(set-rPn-M0! n1 M0)
  (set-rPn-Place! nu P)(set-rPn-Transition! n1 T)(set-rPn-Flow! n1 F)(set-rPn-M0! n1 M0)

  (for/list ([e (append (hash->list Ed) (hash->list Bd))]) (hash-set! (LPN-λ lpn) (car e) (caadr e)))
  (for/list ([e (append (hash->list Ed) (hash->list Bd))]) (hash-set! h (car e) (caadr e)) )
  
  (set-⊙N-E! On (hash-keys Ed))(set-⊙N-B! On (hash-keys Bd))(set-⊙N-RO! On (resetPn2On (rPn-Reset n1))) (set-⊙N-M0O! On (initPn2On n1))
  (set-⊙N-E! Onu (hash-keys Ed))(set-⊙N-B! Onu (hash-keys Bd))(set-⊙N-RO! Onu empty)(set-⊙N-M0O! Onu (initPn2On n1))
  
  (define l1 (suppressInitalArcs (for/list ([e (hash->list Bd)]) (cons (Third e) (list (car e))))))
  (define l2 (suppressInitalArcs (for/list ([e (hash->list Ed)]) (cons (Third e) (list (car e))))))
  (set-⊙N-FO! On (unweighted-graph/directed (append l1 l2 )))
  (set-⊙N-FO! Onu (unweighted-graph/directed (append l1 l2 ))))
                         ; Attention la, on doit générer un graphe !!!
                   
;testing :
;(BuildOccurrenceNet On lpn)
;(LPN-λ lpn)
;(⊙N-FO On)