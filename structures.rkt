; Structures :
;------------------

;Reset Petri net structure.
;A reset Petri net structure is a tuple (P,T,F,R)
;where P and T are disjoint sets of places and transitions,
; F ⊆ (P ×T)∪(T ×P) : is a set of arcs, implemented as a graph
; and R⊆P×T is a set of reset arcs.
; gdm is its marking graph, implemented as a graph

(struct rPn (ident Place Transition Flow Reset M0 gdm kb) #:mutable)

;(define F0 (unweighted-graph/directed '()))
;(define NR (rPn 1 '() '() F0 '() '() '() '()))

; ⊙N : Occurrence Net structure
; An occurrence net is a (reset) Petri net (B,E,FO,RO,M0O)
(struct ⊙N (B E FO RO M0O) #:mutable)

; Labelled reset Petri net
; Definition 5 . A labelled reset Petri net is a tuple (NR,Σ,λ) so that: NR = (P,T,F,R,M0) is a reset Petri net, Σ is a set of transition labels,
; and λ : T → Σ ∪ {ε} is a labelling function.
(struct LPN (NR ∑ λ) #:mutable)


; ϵ : void transition
(define ϵ 'ϵ)
