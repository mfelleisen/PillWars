#lang typed/racket

;; basic game-wide constants 

(provide
  RS      ;; radius of pills 
  WIDTH   ;; width and hieght of game space 
  HEIGHT

  RADAR   ;; the dimensions of a fighter 
  FWING
  FANGLE 

  MAX-RAD) ;; the movability of a fighter
  

;; -----------------------------------------------------------------------------
(define WIDTH  : Nonnegative-Real 1200)
(define HEIGHT : Nonnegative-Real 800)
(define RS (cast (/ WIDTH 40) Nonnegative-Real))

(define MAX-RAD (/ pi 3))

(define RADAR (cast (/ WIDTH 5) Nonnegative-Real))
(define FWING  (cast (/ RADAR 3) Nonnegative-Real))
(define FANGLE (cast (/ MAX-RAD 2) Nonnegative-Real))
