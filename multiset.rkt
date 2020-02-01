; Multiset functions
;------------------

; Support : ∀ p ∈ P, if p ∈ M, then p ∈ Supp(M) 
(define (Supp M P) (for/list ([p P] #:when (member p M)) p))


; Restriction M: M\\S ={x ∈ M, such as, x ∉ S}, S is a set, and M a multi-set
(define (\\ S M)
    (if (symbol? S) (for/list ([p M] #:when (not (equal? p S))) p)
        (for/list ([p M] #:when (not (member p S))) p)))

; |M|p ∈ N : is the number of occurences of a place p in the marking, |M|p is the Multiticity of p in M
(define (∥ M p)
  (count (lambda (x) (equal? x p)) M))

; Multiset Inclusion ==> boolean?
(define (⊆ A B) (for/and ([p A]) (<= (∥ A p) (∥ B p))))

; Multiset Strict Inclusion ==> boolean?
(define (⊂ A B)
  (and (for/and ([p A]) (<= (∥ A p) (∥ B p)))
       (for/or ([p B]) (< (∥ A p) (∥ B p)))))

; Multiset Equality ==> boolean?
(define (⊆⊇ A B)
  (cond
    [(xor (symbol? A) (symbol? B)) #f]
    [(and (symbol? A) (symbol? B)) (equal? A B)]
    [else (and (⊆ A B) (⊆ B A))]))
  
; Multiset comparison ==> boolean?
(define (≫? M1 M2 P)
  (cond
    [(⊂ (Supp M1 P) (Supp M2 P)) #f]
    [(⊆ (Supp M1 P) (Supp M2 P)) ( M2 . ⊂ . M1)]
    [else #f]))

; Minimal Union of multisets : (a a b b) ∪min (a a a b) = (a a b)
(define (∪min A B P); ==> list?
  (flatten (for/lists (l)
           ([p P])
             (make-list (min (∥ A p) (∥ B p)) p))))

; Maximal Union of multisets : (a a b b) ∪min (a a a b) = (a a a b b)
(define (∪max A B P)
  (flatten (for/lists (l)
           ([p P])
             (make-list (max (∥ A p) (∥ B p)) p))))

; Union : Sum of multisets
(define (∪∑ A B P)
  (flatten (for/lists (l)
           ([p P])
             (make-list (+ (∥ A p) (∥ B p)) p))))

; Intersection  : Susbtraction of multisets
(define (⊖ A B P)
  (flatten (for/lists (l)
           ([p P])
             (make-list (max (- (∥ A p) (∥ B p)) 0) p))))
