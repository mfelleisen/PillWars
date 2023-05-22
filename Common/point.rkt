#lang racket

;; a data representation of points 

;; -----------------------------------------------------------------------------
(provide
 #; {type Point = Complex}

 #; {-> Point}
 create-random-point

 make-point 
   
 #; {Point -> (values Real Real)}
 ->values

 #; {Point Point -> Real} 
 distance

 #; {Point Direction -> Point}
 direction+

 #; {Point -> Boolean}
 outside?

 #; {Point Point -> Direction}
 point->direction)

;; -----------------------------------------------------------------------------
(require PillWars/Common/constants)

;; -----------------------------------------------------------------------------
(define (->values p)
  (values (real-part p) (imag-part p)))
  
(define (distance p q)
  (magnitude (- p q)))

(define (direction+ p d)
  (+ p d))

(define (outside? p)
  (define-values [p.x p.y] (->values p))
  (or (< p.x 0) (> p.x WIDTH)
      (< p.y 0) (> p.y HEIGHT)))

(define (point->direction p q)
  (- q p))

(define make-point make-rectangular)

(define (create-random-point)
  (make-point (random WIDTH) (random HEIGHT)))
