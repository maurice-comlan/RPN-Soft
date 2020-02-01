(define (underlyingrdp)
  (define framunderrdp (new frame% [label "Underlying Petri Net"]
                   [parent frame]        
                   [width 800]
                   [height 500]
                   [border 0]))

 

  (define canvas (new canvas% [parent framunderrdp] 
                    [paint-callback 
                     (lambda(can dc) (send RdP repaint dc))]
                    ))

   (send framunderrdp show #t)
)


(define (underlyingunf)
   (if computeUnf #f (load "deplie.rkt"));

   (define frameunderunf (new frame% [label "Unfolding of underlying Petri Net"]
                   [parent frame]        
                   [width 800]
                   [height 500]
                   [border 0]))

  (define canvas (new canvas% [parent frameunderunf] 
                    [paint-callback 
                     (lambda(can dc) (drawUnf dc))]
                    ))
  
   (send frameunderunf show #t)
)

(define (drawUnf dc)
  
   (map (lambda (b)
          (send Unf addPlace (new Place% 
                                  [id (string->number (substring(symbol->string (car b)) 1))] 
                                  [label (symbol->string (car b))]  
                                  [position (list (+ 100 (* 60 (cadr (hash-ref law (car b))))) (+ 100 (* 60 (car (hash-ref law (car b))))))])))
          (hash->list Bd))
  
  
     (map (lambda (e)
          (send Unf addTransition (new Transition% 
                                  [id (string->number (substring(symbol->string (car e)) 1))] 
                                  [label (symbol->string (car e))]  
                                  [position (list (+ 100 (* 60 (cadr (hash-ref law (car e))))) (+ 100 (* 60 (car (hash-ref law (car e))))))])))
          (hash->list Ed))

  (for/list([e (hash->list Ed)])
      (for/list([b (caddr e)])
        (send Unf addArc (new Arc%
                              [place (send Unf getNoeud (string->number (substring(symbol->string b) 1)) 'P) ]
                              [transition (send Unf getNoeud (string->number (substring(symbol->string (car e)) 1))  'T) ]
                              [type 'PT]))))

  (for/list([b (hash->list Bd)])
      (for/list([e (caddr b)])
         (send Unf addArc (new Arc%
                              [place (send Unf getNoeud (string->number (substring (symbol->string (car b)) 1)) 'P) ]
                              [transition (send Unf getNoeud (string->number (substring (symbol->string e) 1))  'T) ]
                              [type 'TP]))))

  
  (send Unf repaint dc)
  
  )



