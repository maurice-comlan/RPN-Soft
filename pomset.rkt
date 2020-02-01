#lang racket
(require racket/control) ; function abort
(require graph)

; Definitions of the paper Information and computation
; David Delfieu and Maurice Comlan - v1 nov 2019
; v0 => v1 use of graph

; include
;------------------
(include "structures.rkt") ; must be in first position
(include "../example10.rkt") ; lpn1 and lpn2, the labelled reset petri nets under analyze :
(include "multiset.rkt")
(include "petriNets.rkt")
(include "list.rkt")
(include "setRelations.rkt")
(include "nodeRelations.rkt")
(include "gdmCompute.rkt")
(include "goodProperties.rkt")

; Homomorphism of pn1 and pn2
;(include "../homomorphism.rkt")
; Occurrence nets: on1 and on2
;(include "../occurrenceNets.rkt")

; ρ utilities
;------------------
(define ρ (make-hash)) ; bissimulation
(define pρ (make-hash)) ; Pomset bissimulation

; ρ-set : insert in ρ only if a bisimilar couple is not present
(define (ρ-set ρ M1 M2)
  (define l (hash->list ρ))
  (if (and (not (member (cons M1 M2) l)) (not (member (cons M2 M1) l))) (hash-set! ρ M1 M2)  (display " ")))

(define (belong e ρ) (or (equal? (hash-ref ρ (car e)) (cdr e)) (equal? (hash-ref ρ (second e)) (car e))))

(define (label seq λ)
  (cond [(empty? seq) empty] 
        [(symbol? seq) (hash-ref λ seq)]
        [else (for/list ([e seq] #:when (not (equal? (hash-ref λ e) ϵ))) e)]))

;------------------------------------------------------------------------------------------------------------------------
; Definition 6 : bisimulation : ≈?
;Let (NR1,Σ1,λ1) and (NR2,Σ2,λ2) be two
;labelled reset Petri nets with NR,i = (Pi,Ti,Fi,Ri,M0,i).
;They are bisimilar if and only if it exists a relation ρ ⊆ [NR1 ⟩ × [NR2 ⟩
;(define (≈?old M1 M2)

; for all t in T1, such that λ1(t)≠ ε and M1[t⟩M1,n
; there exists a sequence t1 ...tn of transitions from T2
; and a sequence M2,1 ...M2,n of markingsofNR,2
; such that: M2[t1⟩M2,1[t2⟩...[tn⟩M2,n,λ2(t1...tn)= λ1(t) AND (M1,n,M2,n) ∈ ρ

(define (seq t1 λ1 M2 gdm2 λ2)
  (for/or ([t2 ((x• M2 gdm2)])
    (define M2n (M\[t⟩ M2 t2 gdm2))
    (if (equal? (hash-ref λ2 t2) ϵ) (append (list (car M2n) t2)  (seq t1 λ1 M2n gdm2 λ2))
        (if (equal? (hash-ref λ1 t1) (hash-ref λ2 t2)) (list (car M2n) t2) #f))))

(define (seq? s) (list? s))

(define (buildρ M1 gdm1 λ1 M2 gdm2 λ2)
   (ρ-set ρ (car M1) (car M2))
   (for/and ([t1 (M↑t⟩ M1 gdm1)]); #:when (not (equal? (hash-ref λ1 t1) ϵ) ))
        (define M1n (M\[t⟩ M1 t1 gdm1))
        (define s (if (seq? (seq t1 λ1 M2 gdm2 λ2)) (reverse (seq t1 λ1 M2 gdm2 λ2)) #f))
;        (display "\nM1= ")(display M1)(display ", t1= ")(display t1)(display ", λ1(t1)= ")(display (hash-ref λ1 t1))(display ", M2= ")(display M2)(display ", s= ")(displayln s)
        (cond
            [(equal? (hash-ref λ1 t1) ϵ) (buildρ M1n gdm1 λ1 M2 gdm2 λ2)]
            [(boolean? s) (hash-clear! ρ) #f]
            [(and (seq? s) (equal? (hash-ref λ1 t1) (hash-ref λ2 (first s))))  (buildρ M1n gdm1 λ1 (cdr s) gdm2 λ2)]
            [else (hash-clear! ρ) #f])))

(define (≈ lpn1 lpn2) 
  (define M01 (rPn-M0 (LPN-NR lpn1)))
  (define gdm1 (rPn-gdm (LPN-NR lpn1)))
  (define M02 (rPn-M0 (LPN-NR lpn2)))
  (define gdm2 (rPn-gdm (LPN-NR lpn2)))
  (and (buildρ M01 gdm1 (LPN-λ lpn1) M02 gdm2 (LPN-λ lpn2))
       (buildρ M02 gdm2 (LPN-λ lpn2) M01 gdm1 (LPN-λ lpn1))))
  
 
(define (≈? lpn1 lpn2) (if (≈ lpn1 lpn2) #t #f))

;-------------------------------------------------------------------
; Interface between unfolding and occurrence net (use in deplie.rkt)
(define (BuildOccurrenceNet On B E lpn)
  (set-⊙N-E! On (hash-keys E))
  (set-⊙N-B! On (hash-keys B))
  (for/list ([e (append (hash->list E) (hash->list B))]) (hash-set! (LPN-λ lpn) (car e) (caadr e)))
  (set-⊙N-FO! On (append (for/list ([e (hash->list B)]) (cons (Third e) (list (car e)))
                         (for/list ([e (hash->list E)]) (cons (third e) (list (car e))))))))
                   


;;-----------------------------------------------------------------------------------------------
;; Definition 7 Occurrence net
;; ∀b∈B,∀x∈B∪E: (1)|•b|≤1,
;; (2) x is not in causal relation with itself,
;; (3) x is not in conflict with itself,
;; (4) {y ∈ B∪E : y ≺ x} is finite : Causally closed
;; (5) b∈ M0O if and only if •b=∅.
;
(define (≺∗ x On) ; closed causality
  (for/list ([y (append (⊙N-B On) (⊙N-E On))] #:when (≺ y x (⊙N-FO On))) y))
  
(define (≺∗e x On) ; closed causality on events
  (for/list ([y (append (⊙N-E On))] #:when (≺ y x (⊙N-FO On))) y))
;(≺∗ 'c7 On2)

(define (≺∗? On) ; causally closed ?
  (for/and ([b (append (⊙N-B On) (⊙N-E On))]) (not (infinite? (length (≺∗ b On))))))

(define (CNSM0? On) ; Necessary Condition and sufficient condition
  (and (for/and ([b (⊙N-B On)]) (implies (empty? (• b (⊙N-FO On))) (member b (⊙N-M0O On))))
       (for/and ([b (⊙N-M0O On)]) (empty? (• b (⊙N-FO On))))))

(define (?⊙N? On) 
  (and
   (for/and ([b (⊙N-B On)]) (<= (length (• b (⊙N-FO On))) 1)) ; (1)
   (for/and ([b (⊙N-B On)])(not (≺ b b (⊙N-FO On)))) ; (2)
   (for/and ([b (⊙N-B On)])(not (♯ b b (⊙N-FO On)))) ; (3)
   (≺∗? On) ; (4)
   (CNSM0? On) ; (5)
   ))
;
;
;;-----------------------------------------------------------------------------------------------
;;  A set E form a feasible execution ? if every e ∈ E is such as the set of pred of e is ⊆ E
(define (feasibleExec? E On)
  (for/or ([e E])
    (and (⊆⊇ (cons e (≺∗e e On)) E)
         (⊆ (⊙N-M0O On) (cons e (≺∗ e On))))))
;
;;-----------------------------------------------------------------------------------------------
;;  h is an homomorphism on N1 and N2?
;;  An homomorphism is an application/function : ∀ p1 ∈ P1, ∃ ! p2 ∈ P2 such as h(p1)=p2 

(define (homomorphism? h N1 N2) 
    (for*/and ([p1 (rPn-Place N1)][p2 (rPn-Place N2)][t (rPn-Transition N1)]
               #:when (and (member (hash-ref h p1) (rPn-Place N2))(member (hash-ref h t) (rPn-Transition N2)) ))
             (⇔ (member p2 (• (hash-ref h t) (rPn-Flow N2))) (for/or ([p1a (• t (rPn-Flow N1))]) (equal? (hash-ref h p1a) p2)))
             (⇔ (member p2 (x• (hash-ref h t) (rPn-Flow N2))) (for/or ([p1a (x• t (rPn-Flow N1))]) (equal? (hash-ref h p1a) p2)))
             (⇔ (member p2 (rPn-M0 N2)) (for/or ([p1a (rPn-M0 N1)]) (equal? (hash-ref h p1a) p2)))))
;


;;-----------------------------------------------------------------------------------------------
;; Process and Branching Process
;; LetN =(P,T,F,∅,M0) be a Petri net, O = (B,E,FO,∅,M0O) be an occurrence net, and h be an homomorphism of nets from O to N.
;; Then (O,h) is a branching process of N if : 
;;              ∀e1,e2 ∈ E,(•e1 = •e2 ∧ h(e1) = h(e2)) ⇒ e1 = e2.
(define (branchingProcess? On h)
  (for*/and ([e1 (⊙N-E On)][e2 (⊙N-E On)])
    (implies (and (⊆⊇ (• e1 (⊙N-FO On)) (• e2 (⊙N-FO On)))
                  (⊆⊇ (hash-ref h e1) (hash-ref h e2)))
             (equal? e1 e2))))

;
;; Process : If, moreover, ∀b ∈ B,|b•| ≤ 1, then (O,h) is a process of N.
(define (process? On h)
  (and (branchingProcess? On h)
       (for/and ([b (⊙N-B On)]) (<= (length (• b (⊙N-FO On))) 1))))

;;-----------------------------------------------------------------------------------------------
;; Potential Process
;; Let NR = (P,T,F,R,M0), let O = (B,E,FO,RO,M0O) 
;; (O,h) is a potential process of NR if
;; (1) (O′,h) is a process of N with O′ = (B,E,FO,∅,M0O),
;; (2) ∀b ∈ B,∀e ∈ E,(b,e) ∈ RO if and only if (h(b),h(e)) ∈ R.
;
(define (potentialProcess? On h Nr)
  (define Oa (⊙N (⊙N-B On)  (⊙N-E On) (⊙N-FO On) empty? (⊙N-M0O On)))
  (and (process? Oa h)
       (for*/and ([b (⊙N-B On)][e (⊙N-E On)])
         (⇔ (member (list b e) (⊙N-RO On))
            (member (list (hash-ref h b) (hash-ref h e)) (rPn-Reset Nr))))))

;;-----------------------------------------------------------------------------------------------
;;Definition 11 (processes of a reset Petri net).
;;Let NR =(P,T,F,R,M0) be a reset Petri net,
;;    O = (B,E,FO,RO,M0O) be an occurrence net,
;;    h be an homomorphism of nets from O to NR.
;;    Then (O, h) is a process of NR if
;;      (1) (O,h) is a potential process of NR,
;;      (2) if E = {e1,...,en} then
;;         ∃M1,...,Mn ⊆ B so that M0O[ek1⟩ M1[ek2⟩...[ekn⟩ Mn with {k1,...,kn} = {1,...,n}.
;; It exists a sequence S of markings, containning at least, every events of E
;
(define (consume E h n M)
  (if (empty? E) #t
      (for/or ([e E] #:when (enabled? (hash-ref h e) M n))
          (consume (remove e E) h n (⇒ (hash-ref h e) M n)))))

(define (rprocess? On h n)
  (and (potentialProcess? On h n)
       (consume (⊙N-E On) h n (rPn-M0 n) )))


;;-----------------------------------------------------------------------------------------------
;;Definition 12 (maximal markings).
;;Let P = (O,h) be a process with set of events E = {e1,...,en} and initial marking M0O of a reset Petri net.
;;    The set Mmax(P) of maximal markings of P contains exactly the markings M so that
;;       ∃M1,...,Mn-1, verifying M0O[ek1⟩M1[ek2⟩...Mn-1[ekn⟩M for some {k1,...,kn} = {1,...,n}.
;; Question à Loic : E is a set, non ordered set of element. Ne faudrait-il  pas écrire {k1 ... kn} ∈ Combinaisons{1, .... n}
;
(define (firableSequence? seq h n M) ; verify if all the events e of seq are firable in an ordered sequence
  (cond
    [(empty? seq) #t]
    [(not (enabled? (hash-ref h (car seq)) M n)) #f]
    [else (firableSequence? (cdr seq) h n (⇒ (hash-ref h (car seq)) M n))]))

(define (maxM run h n M) ; gives the last marking of a sequence of events (run)
  (if (empty? run) M
      (maxM (cdr run) h n (⇒ (hash-ref h (car run)) M n))))

(define (maximumMarkings On h n) ; gives all the maximals markings
  (for/list ([run (permutations (⊙N-E On))] #:when (firableSequence? run h n (rPn-M0 n)))
            (maxM run h n (rPn-M0 n))))
         
;;-----------------------------------------------------------------------------------------------
;;Definition 13 : Pomset abstraction of processes.
;;Let (NR, Σ, λ) be a labelled reset Petri net and(O,h) be a process of NR with O=(B,E,FO,RO,M0O).
;; Define E′ ={e∈E : λ(h(e)!= ε}.
;; Define λ′ : E′ → Σ as the function so that
;;     ∀e ∈ E′, λ′(e) = λ(h(e)).
;; Define  < :  E′ × E′ as the relation so that
;;     e1 < e2 if and only if e1 ≺ e2 
;; Then, (E′, < , λ′) is the pomset abstraction of (O, h).
;
;; def of the structure LPN line 24
(define (Ea On λ h)
  (for/list ([e (⊙N-E On)] #:when (not (equal? (hash-ref λ (hash-ref h e)) ϵ))) e))

(define (λa Ea λ h)
  (for/hash ([e Ea]) (values e (hash-ref λ (hash-ref h e)))))

;;def of (≺ x y F) line 193
(define (<a x y Ea F)
  (if (xor (not (member x Ea)) (not (member y Ea))) #f (≺ x y F)))

; Pomset abstraction of processes of n1 and n2 :
;(define Ea1 (Ea On1 λ1 h1))
;(print "pomset abstraction of On1") (println Ea1)
;(define Ea2 (Ea On2 λ2 h2))
;(define λa1 (λa Ea1 λ1 h1))
;(define λa2 (λa Ea2 λ2 h2))
;(define (<a1 x y) (<a x y Ea1 F1))
;(define (<a2 x y) (<a x y Ea2 F2))
;  

;;-----------------------------------------------------------------------------------------------
;;Definition 14 : Pomset Equivalence
;; Let (E, < , λ) and (E′, < ′, λ′) be the pomset abstractions of two processes P and P′.
;; These processes are pomset equivalent, noted P ≡ P′ if and only
;; if there exists a bijection f : E → E′ so that ∀e1,e2 ∈ E:
;; (1) λ(e1) = λ′(f(e1)),
;; (2) e1 < e2 if and only if f(e1) <′ f(e2).
;; NB : the function surjections of P1 on P2 gives every bijections on P1/P2 when lenght(P1)=length(P2)
;
(define (correspond a S L1 L2)
  (for/list ([b S] #:when (and (hash-has-key? L1 a) (hash-has-key? L2 b) (equal? (hash-ref L1 a) (hash-ref L2 b)))) (list a b)))
      
(define (associations S1 S2 L1 L2)
    (if (empty? S1) empty
        (append (associations (cdr S1) S2 L1 L2) (correspond (car S1) S2 L1 L2))))

(define (relation S1 S2 L1 L2)
  (if (<= (length S1) (length S2)) (associations S1 S2 L1 L2) (associations S2 S1 L2 L1)))

;(relations Ea2 Ea1  λa2 λa1)
(define (≡ Ea1 Ea2 λa1 λa2 <a1 <a2) 
  (define f (relation Ea1 Ea2 λa1 λa2 )) 
  (for*/and ([e1 Ea1] [e2 Ea2]
           #:when (and (hash-has-key? λa1 e1) (hash-has-key? λa2 (image e1 f))
                       (equal? (hash-ref λa1 e1) (hash-ref λa2 (image e1 f)))  
                       (⇔ (<a1 e1 e2) (<a2 (image e1 f) (image e2 f))))) #t)) 

;-----------------------------------------------------------------------------------------------
;Definition 15 linear abstraction.
; Let (N R, Σ, λ) be a labelled reset Petri net,
; let P = (O,h) be a process of NR with O = (B,E,FO,RO,M0O),
; and let M be a reachable marking in O.
; Define λ′ : E → Σ as the function so that ∀e ∈ E,λ′(e) = λ(h(e)).
; The linear abstraction of P with respect to M is the set lin(M,P) so that
;  a sequence of labels ω is in lin(M,P) if and only if in O there exist markings M1,...,Mn−1
;  and events e1,...,en so that M0O[e1⟩M1[e2⟩ . . . Mn−1[en⟩M and λ′(e1 . . . en) = ω.
(define (firableSubsequence w h M n λa)
  (cond
    [(empty? w) empty]
    [(not (enabled? (hash-ref h (car w)) M n)) empty]
    [(enabled? (hash-ref h (car w)) M n)
       (if (not (empty? (hash-ref λa (hash-ref h (car w)))))
           (cons (hash-ref λa (hash-ref h (car w))) (firableSubsequence (cdr w) h (⇒ (hash-ref h (car w)) M n) n λa))
           (firableSubsequence (cdr w) h (⇒ (hash-ref h (car w)) M n) n λa))]))

(define (linearAbstraction E λa On h M n)
   (remove empty (remove-duplicates (for/list ([w (permutations (⊙N-E On))])
     (firableSubsequence w h M n λa)))))
;; test            
; (linearAbstraction E2 λ2 On2 h2 '(p1) n2)
;'((ϵ) (b) (ϵ ϵ) (ϵ b) (ϵ ϵ a) (ϵ ϵ b))


;-----------------------------------------------------------------------------------------------
;Definition 16 Pomset bisimulation for reset nets
;Let (NR1, Σ1, λ1) and (NR2,Σ2,λ2) be two labelled reset Petri nets with NR,i =(Pi,Ti,Fi,Ri,M0,i).
; They are pomset bisimilar if and only if there exists a relation ρ ⊆ [NR,1⟩ × [NR,2⟩ so that:
; 1. (M0,1, M0,2) ∈ ρ,
; 2. if (M1,M2)∈ρ, then
;     (a) for every process P1 of (P1,T1,F1,R1,M1) there exists a process P2 of (P2,T2,F2,R2, M2) so that P1 ≡ P2 and
;         (a1) ∀M′1 ∈ Mmax(P1),∃M′2 ∈ Mmax(P2) so that (M′1,M′2) ∈ ρ,
;         (a2) ∀M′1 ∈ Mmax(P1), ∀M′2 ∈ Mmax(P2), (M′1,M′2) ∈ ρ ⇒ lin(M′1,P1) = lin(M′2,P2).
;     (b) the other way around (for every process P2. . . )

(define (Subprocess w h M n λa)
  (cond
    [(empty? w) empty]
    [(not (enabled? (hash-ref h (car w)) M n)) empty]
    [(enabled? (hash-ref h (car w)) M n)
     (cons (car w) (Subprocess (cdr w) h (⇒ (hash-ref h (car w)) M n) n λa))]))
      
(define (processes λa On h M n)
   (remove empty (remove-duplicates (for/list ([w (permutations (⊙N-E On))])
     (Subprocess w h M n λa)))))

;(processes λa2 On2 h2 (rPn-M0 n2) n2)
;(hash-set! pρ (rPn-M0 n1) (rPn-M0 n2))

;(define (≡p M1 M2 n1 n2)
;  (for/and ([p1 (processes λa1 On1 h1 M1 n1)])
;    (for/or ([p2 (processes λa2 On2 h2 M2 n2)] #:when (≡ p1 p2 λa1 λa2 <a1 <a2))
;      (print "p1 : ")(println p1)(print "p2 : ")(println p2)
;      (and (for/and ([Ma1 (maximumMarkings On1 h1 n1)])
;             (for/or ([Ma2 (maximumMarkings On2 h2 n2)])
;               (belong pρ (list Ma1 Ma2))))
;           (for/and ([Ma1 (maximumMarkings On1 h1 n1)])
;             (for/or ([Ma2 (maximumMarkings On2 h2 n2)])
;               (implies (belong pρ (list Ma1 Ma2))
;                        (⊆⊇  (linearAbstraction λa1 On1 h1 M1 n1) (linearAbstraction λa2 On2 h2 M2 n2)))))))))

; il y a des passage de paramètres qui beugguent sur ≡ et maximumMarkings p1 doit rentrer dans maximum markings
            
;(≡p (rPn-M0 n1) (rPn-M0 n2) n1 n2)

;(maximumMarkings On1 h1 n1)