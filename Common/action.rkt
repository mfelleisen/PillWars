#lang racket

;; the actions a player can take during a turn 

(provide
 #; {type Action = [rotate Deg] || [eat Pill] || [mov Pill]}
 (struct-out mov)
 (struct-out eat)
 (struct-out rot)
 MAX-RAD
 STEP-RAD)

;; -----------------------------------------------------------------------------
(define MAX-RAD (/ pi 3))
(define STEP-RAD (/ MAX-RAD 30))

;; -----------------------------------------------------------------------------
(struct mov [posn] #:transparent)
(struct eat [posn] #:transparent)
(struct rot [angle] #:transparent)