#lang racket

;; the actions a player can take during a turn 

(provide
 #; {type Action = [rotate Deg] || [eat Pill] || [mov Pill]}
 action?
 
 (struct-out mov)
 (struct-out eat)
 (struct-out rot))

;; -----------------------------------------------------------------------------
(struct mov [posn] #:transparent)
(struct eat [posn] #:transparent)
(struct rot [angle] #:transparent)

(define (action? a)
  (or (mov? a) (eat? a) (rot? a)))