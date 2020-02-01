(set! P 
  (map (lambda (p)
         (string->symbol (send p getLabel))) (send RdP getPlaces)))

(set! T
  (map (lambda (t)
         (string->symbol (send t getLabel))) (send RdP getTransitions)))

(set! T2
  (map (lambda (t)
         (list (string->symbol (send t getLabel)) (send t getEft) (send t getLft)) ) (send RdP getTransitions)))

(set! W
(map (lambda (a)
       ;(send a getType)) (send RdP getArcs)))
       (cond
         ;[(equal? (send a getType) 'PT) (list (car (send RdP getLabel (send a getPlace) 'P)) (car (send RdP getLabel (send a getTransition) 'T))(send a getPoids))]
         ;[(equal? (send a getType) 'TP) (list (car (send RdP getLabel (send a getTransition) 'T))(car (send RdP getLabel (send a getPlace) 'P))(send a getPoids))])) (send RdP getArcs)))

         [(equal? (send a getType) 'PT) (list  
                                         (string->symbol (send (send a getPlace) getLabel)) 
                                         (string->symbol (send (send a getTransition) getLabel)) 
                                         (send a getPoids))]
         [(equal? (send a getType) 'TP) (list  (string->symbol (send (send a getTransition) getLabel)) 
                                               (string->symbol (send (send a getPlace) getLabel)) 
                                               (send a getPoids))])) 
     (send RdP getArcs)))

(set! F
(map (lambda (a)
       (cond
         [(equal? (send a getType) 'PT) (list  
                                         (string->symbol (send (send a getPlace) getLabel)) 
                                         (string->symbol (send (send a getTransition) getLabel)))]
         [(equal? (send a getType) 'TP) (list  (string->symbol (send (send a getTransition) getLabel)) 
                                               (string->symbol (send (send a getPlace) getLabel)) )])) 
     (send RdP getArcs)))

(set! R
(map (lambda (a)
       (list
           (string->symbol (send (send a getPlace) getLabel)) 
           (string->symbol (send (send a getTransition) getLabel)))
       ) 
     (send RdP getDiamonds)))


(set! M0 
  (map (lambda (p)
         (send p getMark)) (send RdP getPlaces)))

(set! Pm
  (map (lambda (p)
         (list (string->symbol (send p getLabel)) (send p getMark))) (send RdP getPlaces)))


(set! °P
  (for/list ([p P])
    (append (list p)(for/list ([a W] #:when (equal? (cadr a) p)) (car a)))))


(set! P°
  (for/list ([p P])
    (append (list p)(for/list ([a W] #:when (equal? (car a) p)) (cadr a)))))

(set! °T
  (for/list ([t T])
    (append (list t)(for/list ([a W] #:when (equal? (cadr a) t)) (car a)))))


(set! T°
  (for/list ([t T])
    (append (list t)(for/list ([a W] #:when (equal? (car a) t)) (cadr a)))))


(set! Pbar
      (map (lambda (l)
             (car l))
           R))

(set! Tbar
       (flatten (for/list ([t T] #:when (> (length (◇ t R)) 0)) t))
       )

(define (appartenance e ens)
  (if (member e ens) #t #f))

(define (addbar e)
  (string->symbol (string-append (symbol->string e) "bar")))


(set! Fbar
      (append
               (list (flatten (list (for/list([t Tbar]) 
                   (for/list([p P] #:when(appartenance (list p t) F) ) (list p (addbar t)) )))))

               (list (flatten (list (for/list([t Tbar]) 
                   (for/list([p P] #:when(appartenance (list t p) F) ) (list (addbar t) p) )))))
               

               (list (flatten (list (for/list([p Pbar]) 
                   (for/list([t T] #:when(appartenance (list t p) F) ) (list (addbar p) t) )))))
                
               (list (flatten (list (for/list([p Pbar]) 
                   (for/list([t T] #:when(appartenance (list p t) F) ) (list t (addbar p)) )))))
               

               (list (flatten (list (for/list([p Pbar]) 
                   (for/list([t Tbar] #:when(appartenance (list t p) F) ) (list (addbar p) (addbar t)) )))))
               
               (list (flatten (list (for/list([t Tbar]) 
                   (for/list([p Pbar] #:when(appartenance (list p t) F) ) (list (addbar t) (addbar p)) )))))

               (map (lambda (l)
                      (append l (list (cadr l) (addbar (car l)))  (list (addbar (car l)) (addbar (cadr l)))  (list (addbar (cadr l)) (addbar (car l)))   ))
                    R)

               
      ))


(set! M0bar
      (for/list ([p Pbar] #:when (not (member p M0))) p))

(set! Pbar
      (map (lambda (p)
             (addbar p))
           Pbar))

(set! Tbar
      (map (lambda (t)
             (addbar t))
           Tbar))

;--------------------------------------------------------
;Retourner le nom d'un noeud
(define (name type num)
  (string->symbol (string-append type (number->string num))))

  