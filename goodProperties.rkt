; good properties
;------------------

;Safe 
; A reset Petri net (P,T,F,R,M0) is said to be safe if for any reachable marking M and any transition t ∈ T,
; if M enables t then (t• \ (•t ∪ ⋄t)) ∩ M = ∅.
;(define (safe? rpn) body) ==> boolean?
; Remarks : need to compute the marking graph ?
(define (safe? n)
  (for/and ([M (get-vertices (rPn-gdm n))] #:when (places? M (rPn-Place n)))
           (not (check-duplicates M))))

;(test ♯ P2 T2)
(define (kbounded? n) (if (equal? (rPn-kb n) 1) #t #f))

;(kbounded? n1) 
