; All the important properties of the paper

(define gdm1 (unweighted-graph/directed '()))
(define gdm2 (unweighted-graph/directed '()))

;(launchComputeGDM n1 gdm1)

;------------------------------------------------------------------------------------------------------------------------
;Safe 
; A reset Petri net (P,T,F,R,M0) is said to be safe if for any reachable marking M and any transition t ∈ T,
; if M enables t then (t• \ (•t ∪ ⋄t)) ∩ M = ∅.
;(define (safe? rpn) body) ==> boolean?
; Remarks : need to compute the marking graph ?
(define (safe? n)
  (for/and ([M (get-vertices (rPn-gdm n))] #:when (places? M (rPn-Place n)))
           (not (check-duplicates M))))

(define (label seq λ)
  (cond [(empty? seq) empty] 
        [(symbol? seq) (hash-ref λ seq)]
        [else (for/list ([e seq] #:when (not (equal? (hash-ref λ e) ϵ))) (hash-ref λ e))]))

(define (labelseq l)
  (cond
    [(symbol? l) l]
    [(equal? (length l) 1) (car l)]
    [else l]))

;------------------------------------------------------------------------------------------------------------------------
; Definition 6 : bisimulation : ≈
;Let (NR1,Σ1,λ1) and (NR2,Σ2,λ2) be two
;labelled reset Petri nets with NR,i = (Pi,Ti,Fi,Ri,M0,i).
;They are bisimilar if and only if we can a bissimulation relation ρ ⊆ [NR1 ⟩ × [NR2 ⟩

(define ρ (make-hash)) ; bissimulation

(define (≈M? ρ M1 M2)
  (cond 
    [(hash-has-key? ρ M1) (equal? (hash-ref ρ M1) M2)]
    [(hash-has-key? ρ M2) (equal? (hash-ref ρ M2) M1)]
    [else #f]))
  
(define (computeρ M1 gdm1 λ1 n1 M2 gdm2 λ2 n2)
  (for/and ([t (x• M1 gdm1)])
           (define M1n (⇒ t M1 n1))
           (for/or ([chemin (everyPaths M2 gdm2)]
                    #:when (and (equal? (labelseq (label t λ1)) (labelseq (label (omitMarkings chemin) λ2)))))
                    (define M2n (last chemin))
                   (if (≈M? ρ M1n M2n) #t
                       (begin
                         (hash-set! ρ M1n M2n)
                         (if (computeρ M1n gdm1 λ1 n1 M2n gdm2 λ2 n2) #t (abort #f)    ))))))
               

(define (≈ lpn1 lpn2)
  (hash-clear! ρ)
  (define n1 (LPN-NR lpn1))
  (define n2 (LPN-NR lpn2))
  (define M01 (rPn-M0 (LPN-NR lpn1)))
  (define gdm1 (rPn-gdm (LPN-NR lpn1)))
  (define M02 (rPn-M0 (LPN-NR lpn2)))
  (define gdm2 (rPn-gdm (LPN-NR lpn2)))
  (hash-set! ρ M01 M02)
  (computeρ M01 gdm1 (LPN-λ lpn1) n1 M02 gdm2 (LPN-λ lpn2) n2)) 


(define (≈? l1 l2) (if (and (≈ l1 l2) (≈ l2 l1)) #t #f))
;(≈? lpn1 lpn2)

;;-----------------------------------------------------------------------------------------------
;; Definition 7 Occurrence net
;; ∀b∈B,∀x∈B∪E: (1)|•b|≤1,
;; (2) x is not in causal relation with itself,
;; (3) x is not in conflict with itself,
;; (4) {y ∈ B∪E : y ≺ x} is finite : Causally closed
;; (5) b∈ M0O if and only if •b=∅.
;
(define (≺∗ x On) ; transitive causality
  (for/list ([y (append (⊙N-B On) (⊙N-E On))] #:when (≺ y x (⊙N-FO On))) y))
  
(define (≺∗e x On) ;  transitive causality on events
  (for/list ([y (⊙N-E On)] #:when (≺ y x (⊙N-FO On))) y))
;(≺∗ 'c7 On2) ==> '(c1 c2 c4 e1 e3 e6)
;(≺∗e 'c7 On2) ==> '(e1 e3 e6)

(define (finiteset? l) (not (infinite? (length l))))

(define (finite≺ x On)
  (for/list ([y (append (⊙N-B On) (⊙N-E On))] #:when (and (not (equal? x y))(path? (⊙N-FO On) y x))) y))

(define (•b b on)
  (cond [(and (member b (⊙N-B On)) (not (member b (get-vertices (⊙N-FO On))))) empty]
        [(member b (⊙N-B On)) (get-neighbors (transpose (⊙N-FO On)) b)]
        [else #f]))

(define (minO On) ; (5)  b∈ M0O if and only if •b=∅.
  (for/and ([b (⊙N-B On)]) (⇔ (member b (⊙N-M0O On)) (empty?  (•b b On)) )))

(define (minO? On) (not (empty? (minO On))))

(define (?⊙N? On) 
  (and
   (for/and ([b (⊙N-B On)])
     (<= (length (•b b (⊙N-FO On))) 1) ; (1)
     (not (≺ b b (⊙N-FO On))) ; (2)
     (not (♯ b b (⊙N-FO On))) ; (3)
     (finite≺ b On)) ; (4)
     (minO? On))) ; (5)
; test (?⊙N? On)

;;-----------------------------------------------------------------------------------------------
; Feasable set E in On
; E is feasable in On if :
; E is causality closed, E is conflict-free, ∀e ∈ E the set of predecessors of e is an overset of minSet of On (minSet (⊙N-E On) On)

; Closure causality of the event e in the occurrence net On
(define (∥x∥ e On)
  (define ancest (ancestors e (⊙N-FO On)))
  (for/list ([ei (⊙N-E On)] #:when (member ei ancest)) ei))

; Closure causality of the set E in the occurrence net On : ∥E∥={ ∀x ∈ X, ∥x∥ ⊆ X }
(define (∥E∥ X On)
  (for/and ([x X]) (⊆ (∥x∥ x On) X)))
  
;  (for/or ([ordre (permutations E)])
;    (finitePreceded? ordre g)))

(define (conflictFree? E On) ; E is without conflict in On
  (define edges (get-edges (⊙N-FO On)))
  (for*/and ([e1 edges] [e2 edges] #:when (and (not (equal? e1 e2)) (member (cadr e1) E) (member (cadr e2) E) ))
            (not (equal? (car e1) (car e2)))))

; E is a feasable execution from the initial marking if
; min(E) ⊆ min(On->E) and E is conflicr free and E is causally closed
(define (feasableExecution E On)
  (and (⊆ E (⊙N-E On))
       (conflictFree? E On)
       (⊆ (minSet E On)(minSet (⊙N-E On) On))
       (∥E∥ E On) ))
;(define s2 '(e1 e2 e3))
;(define s1 '(e1 e3 e5))
;(feasableSeq s1 On) #t ;(feasableSeq s2 On) #f

;;-----------------------------------------------------------------------------------------------
; Initial marking and maximal marking of set of event E in an occurrence net On
(define (minSet E On)
  (remove-duplicates (for/list ([e E]
    #:when (and (not (empty? (ancestors e (⊙N-FO On))))(<= (length (ancestors e (⊙N-FO On))) 2) ))
    (for/or ([c (ancestors e (⊙N-FO On))] #:when (member c (⊙N-B On))) c) )))

(define (maxSet E On)
  (remove-duplicates (for/list ([e E]
    #:when (and (not (empty? (successors e (⊙N-FO On))))(<= (length (successors e (⊙N-FO On))) 2) ))
    (for/or ([c (successors e (⊙N-FO On))] #:when (member c (⊙N-B On))) c) )))



;;-----------------------------------------------------------------------------------------------
;Definition 8 Homomorphism of nets
;Let N1 and N2 be two Petri nets such that Ni = (Pi,Ti,Fi,∅,M0,i).
;A mapping h : P1 ∪ T1 → P2 ∪ T2 is a homomorphism of nets from N1 to N2 if ∀p1 ∈ P1,∀p2 ∈ P2,∀t ∈ T1:
;(1) h(p1) ∈ P2,
;(2) h(t) ∈ T2, ;
;(3) p2 ∈ •h(t) ⇔ ∃p′1 ∈ •t,h(p′1) = p2,
;(4) p2 ∈ h(t)• ⇔ ∃p′1 ∈ t•,h(p′1) = p2,
;(5) p2 ∈ M0,2 ⇔ ∃p′1 ∈ M0,1,h(p′1) = p2.
(define (hash-search-key T value)
  (for/or ([kv (hash->list T)] #:when (equal? value (cdr kv))) (car kv)))

(define (hash->ref T e)
  (if (hash-has-key? T e ) (hash-ref T e) (hash-search-key T e)))

(define (homomorphism? h N1 N2) 
    (for*/and ([p1 (rPn-Place N1)][p2 (rPn-Place N2)][t (rPn-Transition N1)]
               #:when (and (member (hash->ref h p1) (rPn-Place N2))(member (hash->ref h t) (rPn-Transition N2)) ))
             (implies (member p2 (• (hash->ref h t) (rPn-Flow N2))) (for/or ([p1a (• t (rPn-Flow N1))]) (equal? (hash->ref h p1a) p2)))
             (implies (member p2 (x• (hash->ref h t) (rPn-Flow N2))) (for/or ([p1a (x• t (rPn-Flow N1))]) (equal? (hash->ref h p1a) p2)))
             (implies (member p2 (rPn-M0 N2)) (for/or ([p1a (rPn-M0 N1)]) (equal? (hash->ref h p1a) p2)))))


;;-----------------------------------------------------------------------------------------------
;; Process and Branching Process
;; LetN =(P,T,F,∅,M0) be a Petri net, O = (B,E,FO,∅,M0O) be an occurrence net, and h be an homomorphism of nets from O to N.
;; Then (O,h) is a branching process of N if : 
;;              ∀e1,e2 ∈ E,(•e1 = •e2 ∧ h(e1) = h(e2)) ⇒ e1 = e2.
(define (branchingProcess? On h)
  (for*/and ([e1 (⊙N-E On)][e2 (⊙N-E On)])
    (implies (and (⊆⊇ (• e1 (⊙N-FO On)) (• e2 (⊙N-FO On)))
                  (⊆⊇ (hash->ref h e1) (hash->ref h e2)))
             (equal? e1 e2))))
; test : (branchingProcess? On h)

;; Process : If, moreover, ∀b ∈ B,|b•| ≤ 1, then (O,h) is a process of N.
(define (process? On h)
  (and (branchingProcess? On h)
       (for/and ([b (⊙N-B On)]) (<= (length (• b (⊙N-FO On))) 1))))
;(process? On h)
;;-----------------------------------------------------------------------------------------------
;; Potential Process
;; Let NR = (P,T,F,R,M0), R this underlying, let O = (B,E,FO,RO,M0O), (O,h) is a potential process of NR if
;; (1) (O′,h) is a process of N with O′ = (B,E,FO,∅,M0O),
;; (2) ∀b ∈ B,∀e ∈ E, (b,e) ∈ RO if and only if (h(b),h(e)) ∈ R.
(define (potentialProcess? On h Nr)
  (and (process? Onu h)
       (for*/and ([b (⊙N-B On)][e (⊙N-E On)])
         (⇔ (member (list b e) (⊙N-RO On))
            (member (list (hash->ref h b) (hash->ref h e)) (rPn-Reset Nr))))))

;test (potentialProcess? On h n1)

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
      (for/or ([e E] #:when (enabled? (hash->ref h e) M n))
          (consume (remove e E) h n (⇒ (hash->ref h e) M n)))))

(define (rprocess? On h n)
  (and (potentialProcess? On h n)
       (consume (⊙N-E On) h n (rPn-M0 n) )))

; test (rprocess? On h n1)

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
    [(not (enabled? (hash->ref h (car seq)) M n)) #f]
    [else (firableSequence? (cdr seq) h n (⇒ (hash->ref h (car seq)) M n))]))

(define (maxM run h n M) ; gives the last marking of a sequence of events (run)
  (if (empty? run) M
      (maxM (cdr run) h n (⇒ (hash->ref h (car run)) M n))))

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
  (for/list ([e (⊙N-E On)] #:when (not (equal? (hash-ref λ (hash->ref h e)) ϵ))) e))

(define (λa Ea λ h)
  (for/hash ([e Ea]) (values e (hash-ref λ (hash->ref h e)))))

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
    [(not (enabled? (hash->ref h (car w)) M n)) empty]
    [(enabled? (hash->ref h (car w)) M n)
       (if (not (empty? (hash-ref λa (hash->ref h (car w)))))
           (cons (hash-ref λa (hash->ref h (car w))) (firableSubsequence (cdr w) h (⇒ (hash->ref h (car w)) M n) n λa))
           (firableSubsequence (cdr w) h (⇒ (hash->ref h (car w)) M n) n λa))]))

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
    [(not (enabled? (hash->ref h (car w)) M n)) empty]
    [(enabled? (hash->ref h (car w)) M n)
     (cons (car w) (Subprocess (cdr w) h (⇒ (hash->ref h (car w)) M n) n λa))]))
      
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