(define (membre elt L)
  (cond
    [(empty? L) #f]
    [(equal? (car L) elt) #t]
    [else (membre elt (cdr L))]))


(define (membre2 elt L)
  (cond
    [(empty? L) #f]
    [(equal? (car L) elt) #t]
    [(list? (car L))(or (membre2 elt (car L))(membre2 elt (cdr L)))]
    [else (membre2 elt (cdr L))]))

(define (conjoints elt liste) 
  (cond 
    [(empty? liste) empty]
    [(if (equal? (caar liste) elt) (cdr (car liste)) (conjoints elt (cdr liste)))]))

(define (° pt)
  (if (membre pt T)(conjoints pt °T)(conjoints pt °P)))

(define (^ pt)
  (if (membre pt T)(conjoints pt T°)(conjoints pt P°)))


;-------------------------------------------------------------------------------------------------------------------
;Déclaration des variables et des tables de hashage
(define m 1)
(define n 1)
(define nl 1)

(define structuralconflictsEns (make-hash)); ondéfinit une table de hashage qui va contenir les ensembles de conflits structurel
(define effectiveconflicts (make-hash));on definit une table de hashage qui va contenir des ensembles d'évènements en conflits
;(define R (make-hash)) ; definit les relations entre tous les evts
(define B0 (append* (for/list ([p Pm] #:when (> (cadr p) 0)) (build-list (cadr p) (lambda(i) (list (car p))))))) ; on crée les conditions initiales


;-------------------------------------------------------------------------------------------------------------------
; Faire la concaténation d'une lettre (b ou e) avec un chiffre pour former le nom des conditions et des évènements
(define (& eb i)
(string->symbol(string-append eb (number->string i))))


;-------------------------------------------------------------------------------------------------------------------
;init crée les conditions initiales à partir de B0
(define init
  (let* ((j 0))(for([p B0])(hash-set! Bd (& "b" m) (list p null))(hash-set! law (& "b" m) (list 0 j))(set! j (+ 1 j))(set! m (+ m 1)))))


;-------------------------------------------------------------------------------------------------------------------
; détermine les ensembles de conflits structurels
(define sconf
(cond
[(or (empty? P) (empty? T)) empty]
[else (for/list ([post P°]
#:when (> (length post) 2))(hash-set! structuralconflictsEns (car post)(cdr post)))])) 


;-------------------------------------------------------------------------------------------------------------------
;Détermine la configuration locale d'un élément: retourne une liste
(define (°* elt)
  (let* ((ens (if (equal? (substring(symbol->string elt)0 1) "b") Bd Ed))
         (l (if(hash-has-key? ens elt)(hash-ref ens elt) (hash-ref pe elt))))
    (cond
      [(empty? l) empty] 
      [(empty? (cadr l)) (list elt)]
      [else (append* (list elt) (for/list([m (cadr l)])  (°* m)))])))


;-------------------------------------------------------------------------------------------------------------------
;Détermine si elt1 cause elt2
(define (caus? elt1 elt2)
  (if (membre  elt1 (°* elt2)) #t #f))


;-------------------------------------------------------------------------------------------------------------------
; Renvoie un sur-ensemble de conditions B' susceptible d'impacter le tirage de t
(define (°sup t)
  (let* ((lb (hash->list Bd)))
    (append* (for/list([p (° t)])
      (for/list([l lb] #:when (membre2 p l))(car l))))))
      
              


;-------------------------------------------------------------------------------------------------------------------
;Condition Nécessaire et Sufficante
(define (CNS lb lp)
  (and (= (length lb) (length lp))(CN lb lp)(CS lb lp)))


;-------------------------------------------------------------------------------------------------------------------
; A toutes les places de lp doit correspondre une condition de lb
(define (CN lb lp)
  (for/and([p lp])(for/or([l (hash->list Bd)])(and (membre2 p l)(membre2 (car l) lb)))))


;-------------------------------------------------------------------------------------------------------------------
;Il existe dans B, une condition correspondant à une place p
(define(existeCondition p)
  (membre2 p (hash->list Bd)))


;-------------------------------------------------------------------------------------------------------------------
; Toutes les conditions de lb doivent correspondre à une place p
(define (CS lb lp)
  (for/and([b lb])(if(hash-has-key? Bd b) (membre2 (caar (hash-ref Bd b)) lp) #f)))


;-------------------------------------------------------------------------------------------------------------------
;Cherche si eb est dans un conflict direct
;J'ai renommé la fonction #? en ?# parce que DrRacket semblait ne pas accepté un pareil nom
(define (?# eb)
  (let* ((B° (for/list([b (hash->list Bd)])
      (cons (car b) (for/list([e (hash->list Ed)] #:when (membre2 (car b) e))(car e))))))
   (for/or ([post B°]) (and (> (length post) 2) (membre2 eb post)))))


;-------------------------------------------------------------------------------------------------------------------
;Retourne une ensemble L privé de l'élément eb
(define (del eb L)
  (cond
    [(empty? L) empty]
    [(empty? eb) L]
    [(not(equal? eb (car L)))(cons (car L) (del eb (cdr L)))]
    [else (del eb (cdr L))]))


;-------------------------------------------------------------------------------------------------------------------
; Ramène une liste de(s) autre(s) membre(s) du conflict
(define (conflictor eb)
   (let* ((B° (for/list([b (hash->list Bd)])
      (cons (car b) (for/list([e (hash->list Ed)] #:when (membre2 (car b) e))(car e))))))
  (for/list ([post B°] #:when (and (> (length post) 2) (membre2 eb post)))(del eb post))))


;-------------------------------------------------------------------------------------------------------------------
; Cherche si eb1 et eb2 sont dans un conflict direct
(define(d#? eb1 eb2)
   (let* ((B° (for/list([b (hash->list Bd)])
      (cons (car b) (for/list([e (hash->list Ed)] #:when (membre2 (car b) e))(car e))))))
   (for/or ([post B°]) (and (> (length post) 2) (membre2 eb1 post)(membre2 eb2 post)))))


;-------------------------------------------------------------------------------------------------------------------
;Cherche si une combinaison contient un conflict
(define (combInconflict? comb)
  (for/or([elt comb])(if(?# elt)(for/and([l (car (conflictor elt))]) (membre2 l comb)) #f)))


;-------------------------------------------------------------------------------------------------------------------
;Retourne un ensemble de places ou de transitions correspondant aux conditons ou aux évènements
(define (annotation ens)
  (for/list([elt ens])(if (equal? (substring(symbol->string elt)0 1) "b")
                        (if(hash-has-key? Bd elt) (caar (hash-ref Bd elt)) '())
                        (if(hash-has-key? Ed elt) (caar (hash-ref Ed elt)) '()))))



;-------------------------------------------------------------------------------------------------------------------
;Faire la partition de tous les éléments d'une liste

(define (partitions l)
(if (null? l) '(())
(let ((part (partitions (cdr l)))) ; calcul préalable
(append (map (lambda (p) (cons (list (car l)) p)) part)
(apply append (map (lambda (p) (add (car l) p)) part))))))


(define (add e p)
(if (null? p) '()
(cons (cons (cons e (car p)) (cdr p))
(map (lambda (t) (cons (car p) t)) (add e (cdr p))))))

(define (combinaison l)
   (let* ((ens (partitions l)))
    (append* (for/list([val ens])
      (for/list([elt val])elt)))))

;-------------------------------------------------------------------------------------------------------------------
;envoie les éléments deux à deux dans une liste
(define (combine ens)
  (cond 
    [(empty? ens) empty]
    [(= (length ens) 1) (list ens)]
    [else (append (for/list([l (cdr ens)]) (list (car ens)l)) (combine (cdr ens)))]))

;-------------------------------------------------------------------------------------------------------------------
;vérifie que les deux éléments d'une liste sont concurrents
(define (concur? ens)
  (cond
    [(= (length ens) 1) #t]
    [else (not (or (caus? (car ens) (cadr ens)) (d#? (car ens) (cadr ens))))]))
  

;-------------------------------------------------------------------------------------------------------------------
;Choisir parmis les B' celles qui sont des coupes; cad telles que λ(B')=°t
(define (Coupe t)
    (let*((ens (combinaison (°sup t)))) (for/list([l ens] #:when (and (CNS l (° t))  
                                                                      (for/or([c (combine l)])(concur? c)))) l)))



;-------------------------------------------------------------------------------------------------------------------
; Ajoute une condition à B
(define (ajoutercondition p e)
  (hash-set! Bd (& "b" m) (list (list p)(list e)))
  (set! m (+ m 1)))
 
  
;-------------------------------------------------------------------------------------------------------------------
; Ajoute un évènement associé au tir de t suivant la coupe lb dans pe
(define (ajouterevenementpe t lb)
  (hash-set! pe (& "e" nl)(list (list t) lb))
  (for/or([b lb] #:when (?# b))(hash-set! co (& "e" nl)(list (list t) lb)))
  (when (membre2 t (hash->list structuralconflictsEns))(hash-set! effectiveconflicts (& "e" nl)(list (list t) lb)))
  (set! nl (+ nl 1)))


;-------------------------------------------------------------------------------------------------------------------
; Vérifie l'ensemble des conditions portant sur la création d'un nouvel évènement e
(define (conditionE++ coupe t)
  (not (or (membre2 (list (list t) coupe)(hash-values Ed))(membre2 (list (list t) coupe)(hash-values pe))(membre2 (list (list t) coupe)(hash-values co)))))



;-------------------------------------------------------------------------------------------------------------------
;calcule le mark d'un évènement
(define (mark e)
  (if(hash-has-key? Ed e) (^ (caar (hash-ref Ed e))) (^ (caar (hash-ref pe e)))))


;-------------------------------------------------------------------------------------------------------------------
;Vérifie si deux libres sont identiques meme en désordre
(define (equal2 l1 l2)
  (and (=(length l1)(length l2))(for/and([l l1])(membre2 l l2))(for/and([l l2])(membre2 l l1))))


;-------------------------------------------------------------------------------------------------------------------
;Détermine si e est cut-off
(define (cutoff? e)
  (for/or([elt (cdr (°* e))])(and (equal? (substring(symbol->string elt)0 1) "e")(equal2 (mark e)(mark elt)))))

 
;-------------------------------------------------------------------------------------------------------------------
;Calcul des évènements possibles
(define (PE)
  (for/list([t T])
    (for/list([coupe (Coupe t)] #:when (conditionE++ coupe t))(ajouterevenementpe t coupe))))


;-------------------------------------------------------------------------------------------------------------------
;On cherche l'evt de plus petite configuration
(define (ordreAdequat pe n)
  (cond
    [(empty? (hash->list pe)) empty]
 [else (let*((m (apply min (let*((ens (append*  (for/list([e (hash-values pe)])    (for/list([b (cadr e)])(°* b))))))
   (for/list([a ens])(length a))))))
   
   (for/or([e (hash->list pe)]) 
     (for/or([b (caddr e)]#:when(= m (length (°* b)))) e)))]))
   

;----------------------------------------------------------------------------------------------------------------------
;Fonctions pour faire l'affichage

(define (symbol-sort l)
  (map (lambda (be)
         (string->symbol be))
     
       (sort (map (lambda (elt)
         (symbol->string elt))
       l) string<?)
       ))

(define (ens type n)
  (for/list ([be (hash-keys law)] #:when (and (equal? (substring (symbol->string be) 0 1) type)
                                                        (equal? (car (hash-ref law be)) n))) be))

(define (modifLawByItem l)
  (for ([i (in-range (length l))])
    (begin
      (let* ([coordonnees (hash-ref law (list-ref l i))])
        (hash-set! law (list-ref l i) (list (car coordonnees) i))) 
    )
 ))

(let* ([x 1]
         [y (+ x 1)])
    (list y x))

(define (modifLaw)
  (for ([i (in-range (+ n 1))])
    (begin
      (let* ([conditions (symbol-sort (ens "b" i))]
             [events (symbol-sort (ens "e" i))])
        (if (< (length conditions) 2) #f (modifLawByItem conditions))
        (if (< (length events) 2) #f (modifLawByItem events))
       )
    )
  )
)



;-------------------------------------------------------------------------------------------------------------------
;Fonction assurant le dépliage du réseau
(PE)
(define (depliageRDP)
  (cond
    [(empty? (hash->list pe)) empty]
    [else
     (begin
       (let* ((e (ordreAdequat pe n)))
         (if (cutoff? (car e)) 
             (begin
               (hash-set*! co (& "e" n) (cdr e))
               (hash-remove! pe (car e))
               )
             (begin 
               (hash-set*! Ed (& "e" n) (cdr e))
               (hash-set*! law (& "e" n) (list (+ 1 (apply max (for/list([b (caddr e)])(car (hash-ref law b))))) 
                                              (apply max (for/list([b (caddr e)])(cadr (hash-ref law b))))))
                 (for([post (^ (caadr e))])
                 (hash-set*! Bd (& "b" m) (list (list post)(list (& "e" n))))
                   
                   (let* ((coor (hash-ref law (caadr (hash-ref Bd (& "b" m) )))))
                     (hash-set! law (& "b" m) (list (+ 1 (car coor))(cadr coor))))
                 (set! m (+ m 1)))
               (hash-remove! pe (car e))
               (set! n (+ n 1))
               (PE)
               (depliageRDP)
               ))))]))

(depliageRDP)
(set! computeUnf #t)
(modifLaw)
(BuildOccurrenceNet On lpn)



;-------------------------------------------------------------------------------------------------------------------
;Fonction permettant de convertir UNF en xml pour roméo : non uitlise
(define (Penelope_xml)
  
(call-with-output-file ("Test_unf.xml") #:exists 'replace 
  (lambda (out)
    (fprintf  out "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> \n") 
    (fprintf out (format "<TPN name=\"~a\"> \n \n" "Test_unf.xml"))
    
    (for/list([b (hash->list Bd)])
      (let* ((id (substring(symbol->string (car b)) 1) )) (fprintf out (format "  <place id=\"~a\" label=\"B ~a\" initialMarking=\"0\"> 
      <graphics color=\"0\"> 
         <position x=\"~a\" y=\"~a\"/> 
         <deltaLabel deltax=\"10\" deltay=\"10\"/> 
      </graphics> 
      <scheduling gamma=\"0\" omega=\"0\"/> 
  </place> \n \n" id id  (+ 100 (* 60 (cadr (hash-ref law (car b))))) (+ 100 (* 60 (car (hash-ref law (car b))))))))) 
    
    (for/list([e (hash->list Ed)])
      (let* ((id (substring(symbol->string (car e)) 1) )) (fprintf out (format "  <transition id=\"~a\" label=\"E ~a\" eft=\"0\" lft=\"0\"> 
      <graphics color=\"0\"> 
         <position x=\"~a\" y=\"~a\"/> 
         <deltaLabel deltax=\"10\" deltay=\"10\"/> 
      </graphics> 
  </transition> \n \n" id id  (+ 100 (* 60 (cadr (hash-ref law (car e))))) (+ 100 (* 60 (car (hash-ref law (car e)))))))))
    
    
  (for/list([e (hash->list Ed)])
      (for/list([b (caddr e)])
        (let* ((ide (substring(symbol->string (car e)) 1) )(idb (substring(symbol->string b) 1)))
    (fprintf out (format "  <arc place=\"~a\" transition=\"~a\" type=\"PlaceTransition\" weight=\"1\"> 
    <nail xnail=\"0\" ynail=\"0\"/> 
    <graphics  color=\"0\"> 
     </graphics> 
  </arc> \n \n" idb ide)))))
    
      (for/list([b (hash->list Bd)])
      (for/list([e (caddr b)])
        (let* ((ide (substring(symbol->string e) 1) )(idb (substring(symbol->string (car b)) 1)))
    (fprintf out (format "  <arc place=\"~a\" transition=\"~a\" type=\"TransitionPlace\" weight=\"1\"> 
    <nail xnail=\"0\" ynail=\"0\"/> 
    <graphics  color=\"0\"> 
     </graphics> 
  </arc> \n \n" idb ide)))))
     
      (fprintf out (format " \n <preferences> 
      <colorPlace  c0=\"SkyBlue2\"  c1=\"gray\"  c2=\"cyan\"  c3=\"green\"  c4=\"yellow\"  c5=\"brown\" /> 
 
      <colorTransition  c0=\"yellow\"  c1=\"gray\"  c2=\"cyan\"  c3=\"green\"  c4=\"SkyBlue2\"  c5=\"brown\" /> 
 
      <colorArc  c0=\"black\"  c1=\"gray\"  c2=\"blue\"  c3=\"#beb760\"  c4=\"#be5c7e\"  c5=\"#46be90\" /> 
 
  </preferences> 
 </TPN>")) 
    )))


