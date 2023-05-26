#lang racket

;; the actions a player can take during a turn 

(provide
 
 (struct-out your-turn)

 #; {type Action = [rotate Deg] || [eat Pill] || [mov Pill]}
 action?
 
 (struct-out mov)
 (struct-out eat)
 (struct-out rot)
 (struct-out gup))

;; -----------------------------------------------------------------------------
;; universe --> world messages: 
(struct your-turn [state] #:prefab)

;; world --> universe messages: 
(struct mov [any] #:prefab)   ;; move according to current speed 
(struct eat [posn] #:prefab)  ;; eat the pill at/near this location 
(struct rot [angle] #:prefab) ;; rotate by this angle 
(struct gup [why] #:prefab)   ;; giving up (why)

(define (action? a)
  (or (mov? a) (eat? a) (rot? a) (gup? a)))
