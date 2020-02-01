#lang racket

(require scheme/control)
(require scheme/gui/base)
(require file/resource)
(require racket/class)
(require data/gvector)
(require xml)
;(require racket/include)
;(require racket/control) ; function abort
(require graph)

(define chooseNet #f)
(define computeUnf #f)
(define noeud 0)
(define NP 0)
(define NT 0)
(define left-down 0)
(define select? #f)
(define noeudSelect null)
(define modif null)
(define type null)
(define select2? #f)
(define noeudSelect2 null)
(define type2 null)

(define P null)
(define T null)
(define T2 null)
(define W null)
(define F null)
(define R null)
(define M0 null)
(define Pm null)
(define °P null)
(define P° null)
(define °T null)
(define T° null)

(define gdm (unweighted-graph/directed '()))
(define rdp null)
(define r null)

(define Bd (make-hash)) ; on definit une table de hashage qui va contenir les conditions sous la forme ((place) (évènement))
(define Ed (make-hash)) ; on definit une table de hashage qui va contenir les évènements sous la forme ((transition) (coupe))
(define pe (make-hash)) ; on definit une table de hashage qui va contenir les évènements possibles
(define co (make-hash)); on definit une table de hashage qui va contenir les évènements cut-off
(define law (make-hash)) ; on défint des niveaux pour les elements crées



(include "structures.rkt") 
(include "multiset.rkt")
(include "list1.rkt")
(include "setRelations.rkt")
(include "petriNets.rkt")
(include "graph1.rkt")
(include "nodeRelations.rkt")
(include "gdmCompute.rkt")
(include "Unfold2OccuNet.rkt")
(include "classes.rkt")

(define RdP (new RdP%))
(define Unf (new RdP%))
;(define P (new Place% [position '(50 50)]))
;(define T (new Transition% [position '(50 100)]))
;(send RdP addPlace P)
;(send RdP addTransition T)

;----------------------------------------------------------------------------------------------
;Transformation structurelle

(define Pbar null)
(define Tbar null)
(define Fbar null)
(define M0bar null)

(define Pstr null)
(define Tstr null)

(define Fbarstr null)
(define M0barstr null)

(define Fstr null)
(define M0str null)

(define frame (new frame% [label "Properties and computing for the unfolding of Reset Petri Net"]
                   [width 900]
                   [height 600]
                   [border 0]))

(send frame show #t)

 (define frameUnf (new frame% [label "Unf"]
                   [width 700]
                   [height 500]
                   [border 15]))

(define canvasUnf (new canvas% [parent frameUnf] 
                    ))

(define dcUnf (send canvasUnf get-dc))


(define canvas (new canvas% [parent frame] 
                    [paint-callback 
                     (lambda (can dc) (send RdP repaint dc))]
                    ))
;(send canvas accept-drop-files #t)

(define dc (send canvas get-dc))

(send dc set-font (make-font #:size 12 #:family 'roman
                             #:weight 'bold))


;-------------------------------------------------------------------------------------------------------------------------------------
(define panelResultat (new horizontal-panel% 
                     [parent frame]	 
                     [min-height 10]
                     [stretchable-width #f]	 
                     [stretchable-height #f]
                   ))

;-------------------------------------------------------------------------------------------------------------------------------------
; Résultat
(define result (new text-field%	 
   	 	[label ""]	 
   	 	[parent frame]
            [enabled #f]))

;-------------------------------------------------------------------------------------------------------------------------------------
; Menu
(include "menu.rkt")

;-------------------------------------------------------------------------------------------------------------------------------------
; Dépliage
(include "properties.rkt")

