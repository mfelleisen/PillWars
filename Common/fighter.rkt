#lang racket

;; a data representation of fighters

;; todo: formulate fighter imagine in terms of geometry 

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Fighter = [fighter Point Direction]}
 
 fighter-posn
 fighter-velocity

 #; {Fighter Scene -> Scene}
 add-fighter

 #; {Fighter -> Fighter}
 rotate-fighter

 #; {Fighter -> Fighter}
 move-fighter)

(module+ examples
  (provide
   steps-2

   fighter0
   fighter0++
   fighter1
   fighter2
   fighter5
   fighter6
   fighter6++))
  
;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/constants)
(require PillWars/Common/direction)
(require PillWars/Common/pills)
(require PillWars/Common/point)
(require PillWars/Lib/image)
(require 2htdp/image)

(module+ examples
  (require PillWars/Common/pills)
  (require (submod PillWars/Common/pills examples)))

(module+ test
  (require (submod ".." examples))
  (require 2htdp/image)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct fighter [posn velocity] #:transparent)

(define fighter-image
  (let* ([s (wedge-centered-at-tip RADAR (rad->deg (* 2 MAX-RAD)) 'solid 'yellow)]
         [s (rotate (- (rad->deg MAX-RAD)) s)]
         [t (wedge-centered-at-tip FWING (rad->deg FANGLE) 'solid 'green)]
         [t (rotate -10 t)]
         [s (overlay t s)])
    s))

;; add fighter `f` to the given scene `s0`
(define (add-fighter this scene0)
  (match-define [fighter posn vel] this)
  (define-values (p.x p.y) (->values posn))
  (define-values [v.x v.y] (->values (+ posn (* (/ 160 (magnitude vel)) vel))))
  (let* ([s scene0]
         [r (rad->deg (angle (conjugate vel)))]
         [s (place-image (rotate r fighter-image) p.x p.y s)]
         [s (scene+line s p.x p.y v.x v.y 'black)])
    s))

;; ---------------------------------------------------------------------------------------------------
(define (move-fighter this [delta 1])
  (match-define [fighter p v] this)
  (fighter (direction+ p (* delta v)) v))

#; {Fighter Radian -> Fighter}
(define (rotate-fighter this rad)
  (match-define [fighter p v] this)
  (fighter p (dir-rotate v rad)))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define pill*0 (list red0))

  (define fighter0 (fighter 100+50i -9+6i))
  (define fighter0++ (fighter 91+56i -9+6i))
  (define fighter1 (fighter (pill-posn red0) 0+0i))
  (define steps-2  20)
  (define fighter2 (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) +0+1i))
  (define fighter5 (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) -1+1i))

  (define fighter6 (fighter 0.0 0+i))
  (define fighter6++ (fighter 0.0 +1)))

(module+ test
  (define mtf (empty-scene 200 200))
  (define-values [fx fy] (->values (fighter-posn fighter0)))
  (check-true (image? (add-fighter fighter0 mtf))))

(module+ test
  (check-within (move-fighter fighter0) fighter0++ .01)
  (check-within (rotate-fighter fighter6 (/ pi 2)) fighter6++ .001))

