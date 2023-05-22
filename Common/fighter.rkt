#lang racket

;; a data representation of fighters

;; todo: formulate fighter imagine in terms of geometry 

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Fighter = [fighter Point Direction N String]}

 #; {String -> Fighter}
 ;; a random fighter 
 create-fighter 

 fighter-posn
 fighter-velocity

 #; {Fighter Scene -> Scene}
 add-fighter

 #; {Fighter Radian -> Fighter}
 rotate-fighter

 #; {Fighter (U PositiveInteger -1) -> Fighter}
 eat-fighter

 #; {Fighter (0,1) -> Fighter}
 accelerate-fighter

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
   fighter6++
   fighter6--))
  
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
(struct fighter [posn velocity score name] #:transparent)

(define (create-fighter name)
  (fighter (create-random-point) (create-random-direction) 0 name))

;; ---------------------------------------------------------------------------------------------------

(define fighter-image
  (let* ([s (wedge-centered-at-tip RADAR (rad->deg (* 2 MAX-RAD)) 'solid 'yellow)]
         [s (rotate (- (rad->deg MAX-RAD)) s)]
         [t (wedge-centered-at-tip FWING (rad->deg FANGLE) 'solid 'green)]
         [t (rotate (/ (rad->deg FANGLE) -2) t)]
         [s (overlay t s)])
    s))

;; add fighter `f` to the given scene `s0`
(define (add-fighter this scene0)
  (match-define [fighter posn vel score name] this)
  (define-values (p.x p.y) (->values posn))
  (define-values [v.x v.y] (->values (+ posn (* (/ FWING .5 (magnitude vel)) vel))))
  (let* ([s scene0]
         [r (rad->deg (angle (conjugate vel)))]
         [g (overlay/offset (text (~a name " : " score) 12 'black) -60 0 fighter-image)]
         [f (rotate r g)]
         [s (place-image f p.x p.y s)])
    s))

(module+ test
  (add-fighter (fighter 100+100i 5+5i 22 "Benjamin") (empty-scene 400 400)))

;; ---------------------------------------------------------------------------------------------------
(define (move-fighter this [delta 1])
  (match-define [fighter p v s n] this)
  (fighter (direction+ p (* delta v)) v s n))

(define (accelerate-fighter this %)
  (match-define [fighter p v s n] this)
  (fighter p (* (+ 1 %) v) s n))

(define (rotate-fighter this rad)
  (match-define [fighter p v s n] this)
  (fighter p (dir-rotate v rad) s n))

(define (eat-fighter this score)
  (match-define [fighter p v s n] this)
  (fighter p v (max 0 (+ s score)) n))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define pill*0 (list red0))

  (define fighter0   (fighter 100+50i -9+6i 0 ""))
  (define fighter0++ (fighter  91+56i -9+6i 0 ""))
  (define fighter1   (fighter (pill-posn red0) 0+0i 0 ""))
  (define steps-2  20)
  (define fighter2   (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) +0+1i 0 ""))
  (define fighter5   (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) -1+1i 0 ""))

  (define fighter6   (fighter 0.0  0+1i         0 ""))
  (define fighter6-- (fighter 0.0  (* 1.1 0+1i) 0 ""))
  (define fighter6++ (fighter 0.0  +1+0i        0 "")))

(module+ test
  (define mtf (empty-scene 200 200))
  (define-values [fx fy] (->values (fighter-posn fighter0)))
  (check-true (image? (add-fighter fighter0 mtf))))

(module+ test
  (check-within (accelerate-fighter fighter6 .1) fighter6-- .01)
  (check-within (eat-fighter fighter6 -1) fighter6 .01)
  (check-within (move-fighter fighter0) fighter0++ .01)
  (check-within (rotate-fighter fighter6 (/ pi 2)) fighter6++ .001))

