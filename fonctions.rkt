(define (changeName container name)
  (send container set-label name))

(define (getfile label extension)
  (define choix (get-file label #f #f #f extension))
  (define f
    (if (boolean? choix) (abort) choix))
  (define ext (substring (path->string choix) (- (string-length (path->string choix)) 3)))
  (if (string=? ext extension)
      (path->string choix)
      (if (equal? (message-box "Extension du fichier" (string-append "Vous devez choisir un fichier " extension) #f '(ok-cancel)) 'ok) "" "")
      ))

(define (openOrImport)
  (define file (getfile "Choose file" "xml"))
  (define in (open-input-file file))
  (define r (cdddr (syntax->datum (syntax:read-xml in))))
  
  (define (putPlace ens)
    (define allElts (flatten (for/list ([l ens] #:when (list? l)) l)))
    (send RdP addPlace (new Place% [id (search 'id allElts)] [label (symbol->string (search 'label allElts))]  
                            [mark (search 'initialMarking allElts)] [position (list (search 'x allElts) (search 'y allElts))])))
  
  (define (putTransition ens)
    (define allElts (flatten (for/list ([l ens] #:when (list? l)) l)))
    (send RdP addTransition (new Transition% [id (search 'id allElts)] [label (symbol->string (search 'label allElts))]  [eft (search 'eft allElts)]
                                 [lft (if (equal? (search 'lft allElts) 'inf) +inf.0 (search 'lft allElts))] [position (list (search 'x allElts) (search 'y allElts))])))
  
  (define (putArc ens)
    (define allElts (flatten (for/list ([l ens] #:when (list? l)) l)))
    (send RdP addArc (new Arc% [place (send RdP getNoeud (search 'place allElts) 'P)] [transition (send RdP getNoeud (search 'transition allElts) 'T)] [type (if (equal? (search 'type allElts) 'PlaceTransition) 'PT 'TP)] [poids (search 'weight allElts)])))
  
  (send RdP reinit)
  (send RdP setPath file)
  
  (send frame set-label (string-append "PENELOPE - " (send RdP fileName)))
  
  (for/list ([l r] #:when (and (list? l) (member (car l) '(place transition arc)))) 
    (cond 
      [(equal? (car l) 'place) (putPlace l)]
      [(equal? (car l) 'transition) (putTransition l)]
      [(equal? (car l) 'arc) (putArc l)]
      )))


(define (objectUnf)
  
   (map (lambda (b)
          (send Unf addPlace (new Place% 
                                  [id (string->number (substring(symbol->string (car b)) 1))] 
                                  [label (symbol->string (car b))]  
                                  [position (list (+ 100 (* 60 (cadr (hash-ref law (car b))))) (+ 100 (* 60 (car (hash-ref law (car b))))))])))
          (hash->list B))
  
  
     (map (lambda (e)
          (send Unf addTransition (new Transition% 
                                  [id (string->number (substring(symbol->string (car e)) 1))] 
                                  [label (symbol->string (car e))]  
                                  [position (list (+ 100 (* 60 (cadr (hash-ref law (car e))))) (+ 100 (* 60 (car (hash-ref law (car e))))))])))
          (hash->list E))
  
  
  ;(for/list([p (send Unf getPlaces)]) (send p draw dcUnf))
  (send Unf repaintUnf)
  
;  (define (putPlace ens)
;    (define allElts (flatten (for/list ([l ens] #:when (list? l)) l)))
;    (send Unf addPlace (new Place% [id (search 'id allElts)] [label (symbol->string (search 'label allElts))]  
;                            [mark (search 'initialMarking allElts)] [position (list (search 'x allElts) (search 'y allElts))])))
;  
;  (define (putTransition ens)
;    (define allElts (flatten (for/list ([l ens] #:when (list? l)) l)))
;    (send Unf addTransition (new Transition% [id (search 'id allElts)] [label (symbol->string (search 'label allElts))]  [eft (search 'eft allElts)]
;                                 [lft (if (equal? (search 'lft allElts) 'inf) +inf.0 (search 'lft allElts))] [position (list (search 'x allElts) (search 'y allElts))])))
;  
;  (define (putArc ens)
;    (define allElts (flatten (for/list ([l ens] #:when (list? l)) l)))
;    (send Unf addArc (new Arc% [place (send RdP getNoeud (search 'place allElts) 'P)] [transition (send RdP getNoeud (search 'transition allElts) 'T)] [type (if (equal? (search 'type allElts) 'PlaceTransition) 'PT 'TP)] [poids (search 'weight allElts)])))
; #t
  )


(define (search key ens)
  (define result (string-append* (string-split (cadr (member key ens)))))
  (if (number? (string->number result)) (string->number result) (string->symbol result))  
  )
