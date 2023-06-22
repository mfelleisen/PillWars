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
(define RS (assert (/ WIDTH 40) positive?))

(define MAX-RAD (/ pi 3))

(define RADAR (assert (/ WIDTH 5) positive?))
(define FWING  (assert (/ RADAR 3) positive?))
(define FANGLE (assert (/ MAX-RAD 2) positive?))
