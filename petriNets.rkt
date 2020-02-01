; petri nets
;------------------

(define (place? s)
  (if (symbol? s)(or (equal? #\p (string-ref (symbol->string s) 0))(equal? #\P (string-ref (symbol->string s) 0))) #f))

(define (list-places? l)
  (cond [(symbol? l) #f]
        [(list? l) (for/and ([e l]) (place? e))]
        [else #f]))

(define (places? M P)
  (cond
    [(empty? M) #f]
    [(list? M) (for/and ([m M]) (member m P))]
    [else #f]))

(define (transition? s)
  (if (symbol? s)(or (equal? #\t (string-ref (symbol->string s) 0)) (equal? #\T (string-ref (symbol->string s) 0))) #f))

; Rechable? ==> determines if M is reachable in a rpn
(define (reachable? M rpn) (hash-has-key? (rPn-gdm rpn) M))

;------------------------------------------------------------------------------
; preset : •x
; An element x ∈ P ∪ T is called a node and has a preset •x = {y ∈ P ∪ T : (y,x)∈F}
;(define (• x F) body) ==> 
; x : node, F : Flow relation.
; If the relation Flow is a graph, get-neihbors is called
(define (• x F)
  (cond
    [(not (or (member x P) (member x T))) empty] 
    [(list? F) (for/list ([arc F] #:when (equal? x (second arc)))(first arc))]
    [(graph? F) (get-neighbors (transpose F) x)]
    [else empty]))

;------------------------------------------------------------------------------
; postset : x•
; a post set x•={y∈P∪T : (x,y)∈F}.
;(define (x• x F) body) ==> list?
; x : node, F : Flow relation
(define (x• x F)
  (cond
       [(list? F) (for/list ([arc F] #:when (equal? x (first arc)))(second arc))]
       [(graph? F) (get-neighbors F x)]
       [else empty]))

;----------------------------------------------------------------------------
; Reset : ⋄x
; Reset x is a transition, it has a set of resets ⋄x={y∈P : (y,x)∈R}.
; (define (◇ x R) body) ==> list?
; x : node, R : reset arcs
(define (◇ x R)
  (for/list ([arc R] #:when (equal? x (second arc)))
                     (first arc)))

;----------------------------------------------------------------------------
(define (underlying rpn)
  (list (rPn-Place rpn) (rPn-Transition rpn) (rPn-Flow rpn) empty (rPn-M0 rpn)))

