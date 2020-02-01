; gdm functions
;------------------


; Comparison between a marking and all the markings of the marking graph
(define (>∃ M gdm P) (for/first ([Mi (get-vertices gdm)] #:when (and (places? Mi P) (≫? M Mi P )))#t))

; Input transitions of a marking M in a gdm
(define (•M M gdm) (car (hash-ref gdm M)))

; Output transitions of a marking in a gdm
(define (M• M gdm) (cdr (hash-ref gdm M)))



; TODO : list of markings issued of a marking M in the marking graph gdm
(define (M↓t⟩ M gdm)
  (if (empty? (flatten (M• M gdm)))
      empty
      (for/list ([arc (flat (M• M gdm))] #:when (not (empty? (flatten arc)))) 
         (if (symbol? (car arc)) (cadr arc)
           (cadar arc)))))

; TODO : M[t⟩ ==>  gives M' in the marking graph gdm
(define (M\[t⟩ M t gdm)
 (if (empty? (x• M gdm))
      empty
      (for/or ([arc (x• M gdm)] #:when (and (not (empty? arc))  (equal? arc t)) ) 
         (if (symbol? (car arc))
             (cadr arc)
             (cadar arc)))))

; enabled? : M enables t ∈ T if  •t ⊆ M.
(define (enabled? t M rpn)
  (⊆ (• t (rPn-Flow rpn)) M ))

; enabled(M) ={∀ t ∈ T, t is enabled in M}
(define (enabled M rpn)
  (for/list ([t (rPn-Transition rpn)] #:when (enabled? t M rpn)) t))


;M --> t M' 
;If (Enabled? t M),  t can be fired from M, leading to the new marking M'
; M′ = (|M|(•t ∪ ◇t) ∪ t•. The fact that M enables t and that firing t leads to M′ is denoted by M[t⟩M′.
(define (⇒ t M rpn) 
  (∪∑  (⊖ M (∪∑ (◇ t (rPn-Reset rpn)) (• t (rPn-Flow rpn))(rPn-Place rpn)) (rPn-Place rpn))
       (x• t (rPn-Flow rpn)) (rPn-Place rpn)))


; computation of marking graph
(define (computeGdM M E n gdm)
  (cond 
    [(empty? E) empty]
    [else (for/list ([t E])
           (define Mi (⇒ t M n))
           (add-directed-edge! gdm M t)
           ;(hash-set! gdm M (cons (• M gdm) (cons (list t Mi) (x• M gdm)))) 
           ;    (hash-set! gdm Mi (cons (cons (list M t) (• Mi gdm)) (x• Mi gdm)))
           (if (>∃ Mi gdm (rPn-Place n)); New marking :  If M > to an existing marking : abortion
                   (begin (set-rPn-kb! n -1)(print "unbounded") (abort #f))
                   (begin (add-vertex! gdm Mi)
                          (add-directed-edge! gdm t Mi)     
                          (computeGdM Mi (enabled Mi n) n gdm)) ))]))       ; in-depth relaunch first



;-------------------------------------------------------------------------------------------------------------------
; Initiation of the computation of marking graph
(define (launchComputeGDM n gdm)
      (set-rPn-kb! n 1)
      (add-vertex! gdm (rPn-M0 n))
      (computeGdM (rPn-M0 n) (enabled (rPn-M0 n) n) n gdm))

(define (kbounded? n) (if (equal? (rPn-kb n) 1) #t #f))

