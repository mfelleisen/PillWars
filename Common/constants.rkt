#lang racket

;; basic game-wide constants 

(provide
  RS      ;; radius of pills 
  WIDTH   ;; width and hieght of game space 
  HEIGHT

  RADAR   ;; the dimensions of a fighter 
  FWING
  FANGLE 

  MAX-RAD ;; the movability of a fighter 
  STEP-RAD)

;; -----------------------------------------------------------------------------
(define WIDTH  1200)
(define HEIGHT  800)
(define RS (/ WIDTH 40))

(define MAX-RAD  (/ pi 3))
(define STEP-RAD (/ MAX-RAD 30))

(define RADAR (/ WIDTH 5))
(define FWING  (/ RADAR 3))
(define FANGLE (/ MAX-RAD 2))
