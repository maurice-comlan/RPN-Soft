; list functions
;------------------

; suppress overnumber of parenthesis 
(define (flat l)
  (cond [(> (length l) 1) l][(symbol? l) l][(symbol? (car l)) l][(symbol? (caar l)) l][else (flat (car l))]))

(define (premier l) (if (empty? l) empty (first l)))

(define (remove e l) (remf* (lambda(x) (equal? e x)) l))

; remove sublist l in L
(define (removel l L) (filter-not (lambda(x) (member x l)) L))

(define (⇔ x y) (and (implies x y) (implies y x)))
  
; add a prefix of l2 to reach the length of l1
(define (adjust l1 l2) ; |l1| > |l2|
  (if (equal? (length l1) (length l2)) l2 (append l2 (take l2 (- (length l1) (length l2))))))

; intersection of lists
(define (∩ L1 L2)
  (if (or (empty? L1) (empty? L2)) empty
      (for/list ([e L1] #:when (member e L2)) e)))

(define (∩? L1 L2) (not (empty? (∩ L1 L2))))