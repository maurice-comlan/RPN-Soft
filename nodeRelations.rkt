; nodes relations
;-----------------

;------------------------------------------------------------------------------------------------------------------------
; Causality : ≺  
; For two nodes x,y ∈ P ∪ T, we say that: x is a causal predecessor of y, noted x ≺ y,
; if there exists a sequence of nodes x1 ...xn with n ≥ 2
; so that ∀i∈[1..n-1],(xi,xi+1)∈F, x1=x,and xn=y. 
(define (≺ x y g)
  (if (not (equal? x y))  (path? g x y) #f))

;------------------------------------------------------------------------------------------------------------------------
; Conflict : ♯
; The Nodes x and y are in conflict, noted x#y :
; if there exists two sequences of nodes x1 . . . xn with n ≥ 2
; and ∀i ∈[1..n−1],(xi,xi+1) ∈ F, and y1...ym with m ≥ 2 and ∀i ∈ [1..m−1],(yi,yi+1) ∈ F,
; such that x1 = y1 is a place, x2 ≠ y2, ; xn = x, and ym=y.
(define (♯ x y g)
  (for*/or ([y1 (ancestors y g)][x1 (ancestors x g)] #:when (and         (not (equal? y1 y)) (not (equal? x1 x))
                                                                        (place? x1) (place? y1)
                                                                        (>= (length (path g x1 x)) 2)
                                                                        (>= (length (path g y1 y)) 2)
                                                                        (not (equal? (second (fewest-vertices-path g x1 x))
                                                                                (second (fewest-vertices-path g y1 y)))))) 
           (equal? x1 y1)))

;------------------------------------------------------------------------------------------------------------------------
; Concurrency : ≀ 
(define (≀ x y F)
  (not (or (≺ x y F) (♯ x y F))))