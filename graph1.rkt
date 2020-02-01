(define (•• x g)
  (for/list ([node (• x g)])(• node g)))

(define (graphe g)
  (if (graph? g) g (unweighted-graph/directed g)))

; Give the list of the ancestors of source in g
(define (ancestors x g)
    (for/list ([y (get-vertices (graphe g))] #:when (path? (graphe g) y x)) y))
  

; Give the list of the successors of source in g
(define (successors x g)
  (for/list ([y (get-vertices g)] #:when (path? g x y)) y))

; Give the list of the nodes in the sequence between source and target
(define (path g source target)
  (cond
    [(or (not (member source (get-vertices g)))(not (member target (get-vertices g)))) empty]
    [(list? (fewest-vertices-path g source target)) (fewest-vertices-path g source target)]
    [else empty]))

(define (omitMarkings path)
  (for/list ([e path] #:when (transition? e)) e))

(define (ingraph? g x) (not (empty? (member x (get-vertices g)))))

(define (path? g source target) (if (and (ingraph? g source) (ingraph? g target)) (not (empty? (path g source target))) #f))

;define all the paths from  source in graph g --> list
(define (everyPaths source g)
  (remove-duplicates (for/fold ([sequences null])
            ([target (successors source g)] #:when (and (list-places? target) (not (empty? (path g source target)))))
            (cons (path g source target) sequences))))

(define (preceeds? e1 e2 g) (fewest-vertices-path g e1 e2))

(define (finitePreceed? S g)
  (cond [(empty? S) #t]
        [(equal? (length S) 1) (ingraph? g (car S))]
        [(>= (length S) 2) (and (preceeds? (first S) (second S) g)
                                (finitePreceed? (cdr S) g))]))
