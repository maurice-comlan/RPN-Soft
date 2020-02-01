;Relations between sets
;-----------------------

; Defines a surjective application of L1 onto L2 with |L1|>=|L2|  ==> list
; element of same label are prioritary matched
(define (surjection L1 L2)
  (if (∩? L1 L2)
      (append (for/list ([e (remove-duplicates (∩ L1 L2))])(list e e))
            (surjection (removel (∩ L1 L2) L1) (adjust (removel (∩ L1 L2) L1) (removel (∩ L1 L2) L2)) ))
      (for/list ([e1 L1][e2 L2]) (list e1 e2))))

; Defines the set of all the surjective application of L1 onto L2 (|L1|>=|L2|)   
(define (surjections L1 L2); 
  (remove-duplicates (for/list ([p (permutations L1)])
                       (surjection p L2))))

; gives the image of e by f : f(e)
(define (image e f)
  (for/or ([p f] #:when (equal? (car p) e)) (second p)))


;(define Te1 '(t1 t2 t3 t4))
;(define Te3 '(a b t1 t2 c d))
;(define Te2 '(a b c d))
;(surjections Te3 Te1)
;(surjections Te1 Te2)
;'(((t1 t1) (t2 t2) (a t3) (b t4) (c t3) (d t4)) ((t1 t1) (t2 t2) (a t4) (b t3) (c t4) (d t3)))