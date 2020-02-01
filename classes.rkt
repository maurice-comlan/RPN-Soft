;#lang racket
;(require racket/class)
;(require data/gvector)
(define tri 3)

(define Place%
  (class object% 
    (super-new) 
    
    (init-field 
     [id 0]
     [label "P"]
     [mark 0]
     [position '(15 15)]
     [color "black"])
    
    (define/public (getId) id)
    (define/public (getLabel) label)
    (define/public (getMark) mark)
    (define/public (getPosition) position)
    (define/public (getColor) color)
    
    (define/public (setId i) (set! id i))
    (define/public (setLabel l) (set! label l))
    (define/public (setMark m) (set! mark m))
    (define/public (setPosition pos) (set! position pos))
    (define/public (setColor c) (set! color c))
    
    (define/public (draw dcv) 
      (send dcv set-pen color 1 'solid)
      (send dcv set-brush "white" 'solid)
      (send dcv draw-ellipse (- (car position) 15) (- (cadr position) 15) 30 30)
      (send dcv set-text-foreground color)
      (send dcv draw-text label  (+ (car position) 17) (- (cadr position) 7))
      (cond 
        [(> mark 1)(send dcv draw-text (number->string mark)  (- (car position) 4) (- (cadr position) 10))])
      (send dcv set-brush color 'solid)
      (cond 
        [(= mark 1)(send dcv draw-ellipse (- (car position) 2) (- (cadr position) 2) 5 5)]
        [(> mark 1)(send dcv draw-ellipse (- (car position) 2) (+ (cadr position) 3) 5 5)]))
    
    (define/public (selected? x y)
      (<= (sqrt (+ (* (- x (car position))(- x (car position))) (* (- y (cadr position))(- y (cadr position))))) 15)
      )))



(define Transition%
  (class object%
    (super-new) 
    
    (init-field 
     [id 0]
     [label "T"]
     [eft 0]
     [lft +inf.0]
     [position '(50 50)]
     [color "black"])
    
    (define/public (getId) id)
    (define/public (getLabel) label)
    (define/public (getEft) eft)
    (define/public (getLft) lft)
    (define/public (getPosition) position)
    
    (define/public (setId i) (set! id i))
    (define/public (setLabel l) (set! label l))
    (define/public (setEft e) (set! eft e))
    (define/public (setLft l) (set! lft l))
    (define/public (setPosition pos) (set! position pos))
    (define/public (setColor c) (set! color c))
    
    (define/public (draw dcv) 
      (send dcv set-pen color 1 'solid)
      (send dcv set-brush color 'solid)
      (send dcv draw-rectangle (- (car position) 15) (- (cadr position) 3) 30 6)
      (send dcv set-text-foreground color)
      (send dcv draw-text label  (+ (car position) 17) (- (cadr position) 8))
      )
    
    (define/public (selected? x y)
      (and (<= (- x (- (car position) 15)) 30)(>= (- x (- (car position) 15)) 0)(<= (- y (- (cadr position) 3)) 6) (>= (- y (- (cadr position) 3)) 0))
      )
    
    ))


(define Arc%
  (class object%
    (super-new) 
    
    (init-field [place null]
                [transition null]
                [type null]
                [poids 1]
                [cdeb '(0 0)]
                [cfin '(0 0)]
                [color "black"])
    
    (define/public (getPlace) place)
    (define/public (getTransition) transition)
    (define/public (getType) type)
    (define/public (getPoids) poids)
    
    (define/public (setPlace p) (set! place p))
    (define/public (setTransition t) (set! transition  t))
    (define/public (setType t) (set! type t))
    (define/public (setPoids p) (set! poids p))
    (define/public (setColor c) (set! color c))
    
    (define/public (selected? x y)
      ;(define x1 (car (send place getPosition)))
      ;(define y1 (cadr (send place getPosition)))
      ;(define x2 (car (send transition getPosition)))
      ;(define y2 (cadr (send transition getPosition)))
      (define x1 (car cdeb))
      (define y1 (cadr cdeb))
      (define x2 (car cfin))
      (define y2 (cadr cfin))
      (define a (if (= x1 x2) +inf.0 (/ (- y1 y2) (- x1 x2))))
      (define b (if (= x1 x2) +inf.0 (/ (-(* x2 y1)(* x1 y2))(- x2 x1))))
      (if (= x1 x2) (and (= x x1)(or (and (> y y1)(< y y2))(and (> y y2)(< y y1)))) (= y (+ (* a x) b)))
      ;(and (<= (- x (- (car position) 15)) 30)(>= (- x (- (car position) 15)) 0)(<= (- y (- (cadr position) 3)) 6) (>= (- y (- (cadr position) 3)) 0))
      )
  
   
    (define/public (drawArrowTP x1 y1 x2 y2 dcv)
      
      (define hypo (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))) 
      
      ;Partir du bord de la place
      (and (= hypo 0) (set! hypo 1))
      (define rx (/ (* 15 (- x1 x2)) hypo))
      (define ry (/ (* 15 (- y1 y2)) hypo))
      
      (define rx1 (/ (* 20 (- x1 x2)) hypo))
      (define ry1 (/ (* 20 (- y1 y2)) hypo))
      
      ;Partir du bord de la transition
      (define h 3)
      (define l 15)
      
      (define x21 x2)
      (define y21 y2)
      
      (define den (if (= (- x1 x2) 0) 1 (- x1 x2)))
      
      
      (if (< (/ (abs (- y1 y2)) den) 0.3)
          (if (> x2 x1) (set! x21 (- x2 l))(set! x21 (+ x2 l)))
          (if (> y2 y1) (set! y21 (- y2 h))(set! y21 (+ y2 h))))
      
      (define xa (- x1 rx))
      (define ya (- y1 ry))
      
      (define xb (- x1 rx1))
      (define yb (- y1 ry1))
      
      (define xc (+ xa (* (sqrt 2) (- ya yb))))
      (define yc (+ ya (* (sqrt 2) (- xb xa))))
      
      (define xc1 (+ xa ( - (/ (* (- xb xa) (sqrt 3)) 2) (/ (- yb ya) 2)))) 
      (define yc1 (+ ya ( + (/ (- xb xa) 2) (/ (* (- yb ya) (sqrt 3)) 2)))) 
      
      (define xc2 (+ xa ( + (/ (* (- xb xa) (sqrt 3)) 2) (/ (- yb ya) 2)))) 
      (define yc2 (+ ya ( + (/ (- xa xb) 2) (/ (* (- yb ya) (sqrt 3)) 2)))) 
      
      (send dcv draw-line (- x1 rx) (- y1 ry) x21 y21)
      
      (set! cdeb (list (- x1 rx) (- y1 ry)))
      (set! cfin (list x21 y21))
      
      (send dcv draw-polygon (list  (make-object point% xa ya) (make-object point% xc1 yc1) (make-object point% xc2 yc2) ))
      
      ) 
    
    
    (define/public (drawArrowPT x1 y1 x2 y2 dcv)
      
       (define hypo (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))) 
      
      ;Partir du bord de la place
      (and (= hypo 0) (set! hypo 1))
      (define rx (/ (* 15 (- x1 x2)) hypo))
      (define ry (/ (* 15 (- y1 y2)) hypo))
      
      (define rx1 (/ (* 30 (- x1 x2)) hypo))
      (define ry1 (/ (* 30 (- y1 y2)) hypo))
      
      (define rx2 (/ (* 5 (- x1 x2)) hypo))
      (define ry2 (/ (* 5 (- y1 y2)) hypo))
       
      
      ;Partir du bord de la transition
      (define h 3)
      (define l 15)
      
      (define x21 x2)
      (define y21 y2)
      
      (define den (if (= (- x1 x2) 0) 1 (- x1 x2)))
      
      
      (if (< (/ (abs (- y1 y2)) den) 0.3)
          (if (> x2 x1) (set! x21 (- x2 l))(set! x21 (+ x2 l)))
          (if (> y2 y1) (set! y21 (- y2 h))(set! y21 (+ y2 h))))
      
      (send dcv draw-line (- x1 rx) (- y1 ry) x21 y21)
      
      (set! cdeb (list (- x1 rx) (- y1 ry)))
      (set! cfin (list x21 y21))
      
      (define x22 (+ x21 rx2))
      (define y22 (+ y21 ry2))
      
      (define xc3 (+ x21 ( - (/ (* (- x22 x21) (sqrt 3)) 2) (/ (- y22 y21) 2)))) 
      (define yc3 (+ y21 ( + (/ (- x22 x21) 2) (/ (* (- y22 y21) (sqrt 3)) 2)))) 
      
      (define xc4 (+ x21 ( + (/ (* (- x22 x21) (sqrt 3)) 2) (/ (- y22 y21) 2)))) 
      (define yc4 (+ y21 ( + (/ (- x21 x22) 2) (/ (* (- y22 y21) (sqrt 3)) 2)))) 
      
      (send dcv draw-polygon (list  (make-object point% x21 y21) (make-object point% xc3 yc3) (make-object point% xc4 yc4) ))
      
      ) 
    
    (define/public (draw dcv) 
      (define x1 (car (send place getPosition)))
      (define y1 (cadr (send place getPosition)))
      (define x2 (car (send transition getPosition)))
      (define y2 (cadr (send transition getPosition)))
      
      (send dcv set-pen color 1 'solid)
      (send dcv set-brush color 'solid)
          
      (if (equal? type 'PT)  (drawArrowPT x1 y1 x2 y2 dcv) (drawArrowTP x1 y1 x2 y2 dcv))
      )
    
    
    ))


(define Reset%
  (class object%
    (super-new) 
    
    (init-field [place null]
                [transition null]
                [type null]
                [poids 1]
                [cdeb '(0 0)]
                [cfin '(0 0)]
                [color "red"])
    
    (define/public (getPlace) place)
    (define/public (getTransition) transition)
    
    (define/public (setPlace p) (set! place p))
    (define/public (setTransition t) (set! transition  t))
    (define/public (setColor c) (set! color c))
     
    
    (define/public (drawDiamond x1 y1 x2 y2 dcv)
      
       (define hypo (sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))) 
      
      ;Partir du bord de la place
      (and (= hypo 0) (set! hypo 1))
      (define rx (/ (* 15 (- x1 x2)) hypo))
      (define ry (/ (* 15 (- y1 y2)) hypo))
      
      (define rx1 (/ (* 30 (- x1 x2)) hypo))
      (define ry1 (/ (* 30 (- y1 y2)) hypo))
      
      (define rx2 (/ (* 5 (- x1 x2)) hypo))
      (define ry2 (/ (* 5 (- y1 y2)) hypo))
       
      
      ;Partir du bord de la transition
      (define h 3)
      (define l 15)
      
      (define x21 x2)
      (define y21 y2)
      
      (define den (if (= (- x1 x2) 0) 1 (- x1 x2)))
      
      
      (if (< (/ (abs (- y1 y2)) den) 0.3)
          (if (> x2 x1) (set! x21 (- x2 l))(set! x21 (+ x2 l)))
          (if (> y2 y1) (set! y21 (- y2 h))(set! y21 (+ y2 h))))
      
      (send dcv draw-line (- x1 rx) (- y1 ry) x21 y21)
      
      (set! cdeb (list (- x1 rx) (- y1 ry)))
      (set! cfin (list x21 y21))
      
      (define x22 (+ x21 rx2))
      (define y22 (+ y21 ry2))
      
      (define xc3 (+ x21 ( - (/ (* (- x22 x21) (sqrt 3)) 2) (/ (- y22 y21) 2)))) 
      (define yc3 (+ y21 ( + (/ (- x22 x21) 2) (/ (* (- y22 y21) (sqrt 3)) 2)))) 
      
      (define xc4 (+ x21 ( + (/ (* (- x22 x21) (sqrt 3)) 2) (/ (- y22 y21) 2)))) 
      (define yc4 (+ y21 ( + (/ (- x21 x22) 2) (/ (* (- y22 y21) (sqrt 3)) 2)))) 
      
      (send dcv draw-polygon (list  (make-object point% x21 y21) (make-object point% xc3 yc3) (make-object point% xc4 yc4) ))
      
      ) 
    
    (define/public (draw dcv) 
      (define x1 (car (send place getPosition)))
      (define y1 (cadr (send place getPosition)))
      (define x2 (car (send transition getPosition)))
      (define y2 (cadr (send transition getPosition)))
      
      (send dcv set-pen color 1 'solid)
      (send dcv set-brush color 'solid)
          
      (drawDiamond x1 y1 x2 y2 dcv)
      )
    
    
    ))



(define RdP%
  (class object%
    (super-new) 
    
    (init-field [path ""] 
                [Places (make-gvector)]
                [Transitions (make-gvector)]
                [Arcs (make-gvector)]
                [Diamonds (make-gvector)])
    
    (define/public (getPlaces) (gvector->list Places))
    (define/public (getTransitions) (gvector->list Transitions))
    (define/public (getArcs) (gvector->list Arcs))
    (define/public (getDiamonds) (gvector->list Diamonds))
    (define/public (getPath) path)
    (define/public (getArcs2) Arcs)
    
    (define/public (setPath pa) (set! path pa))
    
    (define/public (addPlace P) (gvector-add! Places P))
    (define/public (addTransition T) (gvector-add! Transitions T))
    (define/public (addArc A) (gvector-add! Arcs  A))
    (define/public (addDiamond A) (gvector-add! Diamonds  A))
    
    (define/public (removePlace P id) (gvector-remove! Places id))
    (define/public (removeTransition T id) (gvector-remove! Transitions id))
    (define/public (removeArc A id) (gvector-remove! Arcs id))
    (define/public (removeDiamonds A id) (gvector-remove! Diamonds id))
    
    (define/private (index gv e)
      (define m -1)
      (if (for/or ([l (gvector->list gv)]) (and (set! m (+ m 1)) (equal? l e) )) m (- 1))
      )
    
    (define/public (getNoeud id l) 
      (cond
        [(equal? l 'P)(for/or ([p (getPlaces)])(and (equal? id (send p getId)) p))]
        [(equal? l 'T)(for/or ([t (getTransitions)]) (and (equal? id (send t getId)) t))]))
    
    (define/public (getLabel id l) 
      (send (getNoeud id l) getLabel)
      )
  
  (define/public (delete)
    (define del #f)
    (and (not del) (equal? type 'P)(for/or ([p (getPlaces)]) (and (equal? p modif) (>= (index Places p) 0) (gvector-remove! Places (index Places p)) 
                                                                  (for/and ([a (getArcs)]) (and (equal? p (send a getPlace)) (gvector-remove! Arcs (index Arcs a))))                            
                                                                  (set! del #t) )))
    (and (not del) (equal? type 'T)(for/or ([p (getTransitions)]) (and (equal? p modif) (>= (index Transitions p) 0) (gvector-remove! Transitions (index Transitions p)) 
                                                                       (for/and ([a (getArcs)]) (and (equal? p (send a getTransition)) (gvector-remove! Arcs (index Arcs a))))                                 
                                                                       (set! del #t))))
    (and (not del) (equal? type 'A)(for/or ([p (getArcs)]) (and (equal? p modif) (>= (index Arcs p) 0) (gvector-remove! Arcs (index Arcs p)) (set! del #t))))
    )
  
  (define/public (select x y)
    (set! select? #f)
    (set! noeudSelect null)
    (set! type null)
    (for/list([p (getPlaces)]) (if (send p selected? x y) (and (send p setColor "red") (set! select? #t) (set! noeudSelect p) (set! type 'P)) (send p setColor "black")))
    (and (not select?)(for/list([t (getTransitions)]) (if (send t selected? x y) (and (send t setColor "red")(set! select? #t) (set! noeudSelect t) (set! type 'T)) (send t setColor "black"))))
    (and (not select?)(for/list([a (getArcs)]) (if (send a selected? x y) (and (send a setColor "red") (set! select? #t) (set! noeudSelect a) (set! type 'A)) (send a setColor "black"))))
    )
  
  (define/public (select2 x y)
    (set! select2? #f)
    (set! noeudSelect2 null)
    (set! type2 null)
    (for/list([p (getPlaces)]) (if (send p selected? x y) (and (send p setColor "red") (set! select2? #t) (set! noeudSelect2 p) (set! type2 'P)) (send p setColor "black")))
    (for/list([t (getTransitions)]) (if (send t selected? x y) (and (send t setColor "red")(set! select2? #t) (set! noeudSelect2 t) (set! type2 'T)) (send t setColor "black")))
    (send modif setColor "red")
    (send RdP repaint)
    )
  
  (define/public (modifyP l m)
    (send modif setLabel l)
    (send modif setMark (if (equal? m "") 0 (string->number m)))
    )
  
  (define/public (modifyT l ef lf)
    (send modif setLabel l)
    (send modif setEft (if (equal? ef "") 0 (string->number ef)))
    (send modif setLft (if (equal? lf "") +inf.0 (string->number lf)))
    )
  
  (define/public (modifyA p)
    (send modif setPoids (string->number p))
    )
  
  (define/public (repaint dcv) 
    (send dcv erase)
    (for/list([p (getPlaces)]) (send p draw dcv))
    (for/list([t (getTransitions)]) (send t draw dcv))
    (for/list([a (getArcs)]) (send a draw dcv))
    (for/list([a (getDiamonds)]) (send a draw dcv))
    )
  
  (define/public (fileName)
    (substring (path->string (file-name-from-path path)) 0 (- (string-length (path->string (file-name-from-path path))) 4)))
  
  (define/public (reinit dcv)
    (set! noeud 0)
    (set! NP 0)
    (set! NT 0)
    (set! left-down 0)
    (set! select? #f)
    (set! noeudSelect null)
    (set! modif null)
    (set! type null)
    (set! select2? #f)
    (set! noeudSelect2 null)
    (set! type2 null)
    (set! path "")
    (set! Places (make-gvector))
    (set! Transitions (make-gvector))
    (set! Arcs (make-gvector))
    (set! Diamonds (make-gvector))
    (send dcv erase))
  
  (define/private (getfile label extension)
    (define choix (get-file label #f #f #f extension))
    (define f
      (if (boolean? choix) (abort) choix))
    (define ext (substring (path->string choix) (- (string-length (path->string choix)) 3)))
    (if (string=? ext extension)
        (path->string choix)
        (if (equal? (message-box "Extension du fichier" (string-append "Vous devez choisir un fichier " extension) #f '(ok-cancel)) 'ok) "" "")
        ))
  
  (define/public (new)
    (reinit)
    (send frame set-label (string-append "PENELOPE - noName"))
    )
  
  
  
  ;(define/public (open)
    ;#t
    ;(reinit)
    ;(define file (getfile "Choose file" "xml"))
    ;(define in (open-input-file file))
    ;(define r (cdddr (syntax->datum (syntax:read-xml in))))
    ;(include "parseO.rkt")
    ;(parse (path->string file))
    ;(repaint)
    ;(send frame set-label (string-append "PENELOPE - " r))
    ;)
  
  
  
  (define/public (saveAs)
    (define out (put-file "Save File ..." #f #f "noName" "xml"))
    (and out (set! path out))
    (save)
    )
  
  (define/public (save)
    (and (equal? path "")(saveAs))
    (define out path)
    (define o out)
    (and out 
         (call-with-output-file out #:exists 'replace 
           (lambda (out)
             (fprintf  out "<?xml version=\"1.0\" encoding=\"UTF-8\" ?> \n") 
             (fprintf out (format "<TPN name=\"~a\"> \n \n" (path->string o)))
             
             (for/list([p (getPlaces)])
               (fprintf out (format "  <place id=\"~a\" label=\"~a\" initialMarking=\"~a\"> 
      <graphics color=\"0\"> 
         <position x=\"~a\" y=\"~a\"/> 
         <deltaLabel deltax=\"10\" deltay=\"10\"/> 
      </graphics> 
      <scheduling gamma=\"0\" omega=\"0\"/> 
  </place> \n \n" (send p getId) (send p getLabel) (send p getMark) (car (send p getPosition)) (cadr (send p getPosition)))))
             
             (for/list([t (getTransitions)])
               (fprintf out (format "  <transition id=\"~a\" label=\"~a\" eft=\"~a\" lft=\"~a\"> 
      <graphics color=\"0\"> 
         <position x=\"~a\" y=\"~a\"/> 
         <deltaLabel deltax=\"10\" deltay=\"10\"/> 
      </graphics> 
  </transition> \n \n" (send t getId) (send t getLabel) (send t getEft) (if (equal? (send t getLft) +inf.0) "inf" (send t getLft)) 
                       (car (send t getPosition)) (cadr (send t getPosition)))))
             
             
             (for/list([a (getArcs)])
               (fprintf out (format "  <arc place=\"~a\" transition=\"~a\" type=\"~a\" weight=\"~a\"> 
    <nail xnail=\"0\" ynail=\"0\"/> 
    <graphics  color=\"0\"> 
     </graphics> 
  </arc> \n \n" (send (send a getPlace) getId) (send (send a getTransition) getId) (if (equal? (send a getType) 'PT) "PlaceTransition" "TransitionPlace") (send a getPoids))))
             
             (fprintf out (format " \n <preferences> 
      <colorPlace  c0=\"SkyBlue2\"  c1=\"gray\"  c2=\"cyan\"  c3=\"green\"  c4=\"yellow\"  c5=\"brown\" /> 
 
      <colorTransition  c0=\"yellow\"  c1=\"gray\"  c2=\"cyan\"  c3=\"green\"  c4=\"SkyBlue2\"  c5=\"brown\" /> 
 
      <colorArc  c0=\"black\"  c1=\"gray\"  c2=\"blue\"  c3=\"#beb760\"  c4=\"#be5c7e\"  c5=\"#46be90\" /> 
 
  </preferences> 
 </TPN>"))
             )) 
         (send frame set-label (string-append "PENELOPE - " (fileName))))
    )
    
    
    (define/public (unfolding)
      (include "rdp.rkt")
      (include "deplie.rkt")
      (send frameUnf show #t)
      ;(send Unf repaintUnf)
      (changeName frameUnf "Unfolding")
      
      )
    
    (define/public (repaintUnf) 
      (send dcUnf erase)
      (for/list([p (getPlaces)]) (send p draw dcUnf))
      (for/list([t (getTransitions)]) (send t draw dcUnf))
      (for/list([a (getArcs)]) (send a draw))
        )
  
  ))

