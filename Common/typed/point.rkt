#lang typed/racket

;; a data representation of points 

;; -----------------------------------------------------------------------------
(provide
 #; {type Point = Complex}
 Point

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
(require PillWars/Common/typed/constants)
(require PillWars/Common/typed/direction)

;; -----------------------------------------------------------------------------
(define-type Point Complex)

(: ->values (-> Point (Values Real Real)))
(define (->values p)
  (values (real-part p) (imag-part p)))

(: distance (-> Point Point Real))
(define (distance p q)
  (magnitude (- p q)))

(: outside? (-> Point Boolean))
(define (outside? p)
  (define-values [p.x p.y] (->values p))
  (or (< p.x 0) (> p.x WIDTH)
      (< p.y 0) (> p.y HEIGHT)))

(: direction+ (-> Point Direction Point))
(define (direction+ p d)
  (+ p d))

(: point->direction (-> Point Point Direction))
(define (point->direction p q)
  (- q p))

(define make-point make-rectangular)

(: create-random-point (-> Point))
(define (create-random-point)
  (make-point (random (~i WIDTH)) (random (~i HEIGHT))))

(: ~i (-> Real Integer))
(define (~i x) (cast (inexact->exact (round x)) Integer))

