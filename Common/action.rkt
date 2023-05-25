#lang racket

;; the actions a player can take during a turn 

(provide
 
 (struct-out your-turn)

 #; {type Action = [rotate Deg] || [eat Pill] || [mov Pill]}
 action?
 
 (struct-out mov)
 (struct-out eat)
 (struct-out rot))

;; -----------------------------------------------------------------------------
;; universe --> world messages: 
(struct your-turn [state] #:prefab)

;; world --> universe messages: 
(struct mov [posn] #:prefab)
(struct eat [posn] #:prefab)
(struct rot [angle] #:prefab)

(define (action? a)
  (or (mov? a) (eat? a) (rot? a)))
