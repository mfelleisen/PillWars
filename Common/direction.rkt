#lang racket

;; a data representation of directions 

;; -----------------------------------------------------------------------------
(provide
 #; {type Direction = Complex}

 #; {-> Direction}
 create-random-direction 
   
 #; {Direction Radian -> Direction}
 dir-rotate

 delta-angle
 
 ;; doesn't really belong here 
 rad->deg
 deg->rad)

;; -----------------------------------------------------------------------------

(define (dir-rotate vel deg)
  (* vel (exp (* 0+i (- deg))) #;(make-polar (magnitude vel) deg)))

#; {Radian -> Degree}
(define (rad->deg r)
  (* 180 (/ r pi)))

#; {Degree -> Radian}
(define (deg->rad d)
  (* pi (/ d 180)))

(define (delta-angle α θ)
  (- (angle α) (angle θ)))

(define (create-random-direction)
  (make-rectangular (/ (+ 1 (random 10)) 10) (/ (+ 1 (random 10)) 10)))