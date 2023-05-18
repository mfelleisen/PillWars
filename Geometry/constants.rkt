#lang racket

;; basic game-wide constants 

(provide
  RS      ;; radius of pills 
  WIDTH   ;; width and hieght of game space 
  HEIGHT)

;; -----------------------------------------------------------------------------
(define WIDTH 400)
(define HEIGHT 400)
(define RS (/ WIDTH 40))

