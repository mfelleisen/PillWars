#lang racket

(provide
 WIDTH
 HEIGHT
 
 SPEED        ;; magnitude of speed for all fighters 
 MIN-DISTANCE ;; the min. distances fighters have at launch time
 SHOOT-OUT-Δ  ;; the range within one fighter can hit another 
 WAIT-TIME    ;; the number of ticks a client will display an outcome 
 DAMAGE#      ;; the number of hits a fighter can take before it explodes 
 CRASH-Δ      ;; two fighters crash into each other at this distance

 ;; graphical constants 
 BACK   #; "plain scene"
 XWING  #; "fighter icon"
 TIE    #; "fighter icon"
 EXPL   #; explosion
 #; HIT    #; "small explosion") 

;; ---------------------------------------------------------------------------------------------------
(require 2htdp/image)
(require racket/runtime-path)

;; ---------------------------------------------------------------------------------------------------
(define WIDTH  1200)
(define HEIGHT 1000)

(define SPEED   20) 

(define MIN-DISTANCE (/ WIDTH 2))   

(define SHOOT-OUT-Δ  (/ WIDTH 1.5)) 

(unless (< (* 5 2 SPEED) SHOOT-OUT-Δ) ;; the 5 is from the fighter's max caps
  (error 'geometry "the fighter is too fast:\n speed = ~v\n range = ~v\n" SPEED SHOOT-OUT-Δ))

(define WAIT-TIME 5) 

(define DAMAGE# 3)  

;; ---------------------------------------------------------------------------------------------------
;; images 

(define BACK (empty-scene WIDTH HEIGHT))

(define-runtime-path xwing "../Resources/x-wing.png")
(define-runtime-path tie   "../Resources/tie-fighter.png")
(define-runtime-path expl  "../Resources/explosion-6.png")

;; icons are here for 'geometry reasons'
(define XWING (rotate 270 (center-pinhole (scale .03 (bitmap/file xwing)))))
(define TIE   (rotate 270 (center-pinhole (scale .07 (bitmap/file tie)))))
(define EXPL  (center-pinhole (scale .40 (bitmap/file expl))))
(define HIT   (center-pinhole (scale .19 (bitmap/file expl))))

(define CRASH-Δ (max (image-width XWING) (image-height XWING) (image-width TIE) (image-height TIE)))