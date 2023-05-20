#lang racket

;; a data representation of fighters

;; todo: formulate fighter imagine in terms of geometry 

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Fighter = [fighter Point Direction]}
 
 fighter-posn
 fighter-velocity
 
 add-fighter
 rotate-fighter
 move-fighter
 outside?
 
 strategy-1)

(module+ examples
  (provide
   pill*0
   steps-2

   fighter0
   fighter1
   fighter2
   fighter3
   fighter4
   fighter5))
  
;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/direction)
(require PillWars/Common/pills)
(require PillWars/Common/point)
(require (prefix-in point: (only-in PillWars/Common/point outside?)))
(require 2htdp/image)

(module+ examples
  (require PillWars/Common/pills)
  (require (submod PillWars/Common/pills examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod PillWars/Common/pills examples))
  (require PillWars/Common/pills)
  (require PillWars/Common/action)
  (require 2htdp/image)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct fighter [posn velocity] #:transparent)

(define (circle-with-cross-hair r mode color)
  (let* ([s (circle r mode color)]
         [s (scene+line s 0 r (* 2 r) r 'black)]
         [s (scene+line s r 0 r (* 2 r) 'black)])
    s))

(define (wedge-centered-at-tip l α mode c)
  (let* ([s (wedge l α mode c)]
         [d (circle l 'solid (color 255 255 255 0))]
         [delta-x
          (cond
            [(<= α 90)  0]
            [(<= α 180) (* l (cos (deg->rad (- 180 α))))]
            [else       l])]
         [delta-y
          (cond
            [(<= α 180) 0]
            [else (* l (sin (deg->rad (- α 180))))])]
         [s (place-image/align s (- l delta-x) (+ l delta-y) 'left 'bottom d)]
         [s (overlay (circle 3 'solid 'orange) s)])
    s))

(define fighter-image
  (let* ([s (wedge-centered-at-tip 160 (rad->deg (* 2 MAX-RAD)) 'solid 'yellow)]
         [s (rotate (- (rad->deg MAX-RAD)) s)]
         [t (wedge-centered-at-tip 40 20 'solid 'green)]
         [t (rotate -10 t)]
         [s (overlay t s)])
    s))

#; {Fighter Image -> Image}
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

#; {Fighter -> Fighter}
(define (move-fighter this [delta 1])
  (match-define [fighter p v] this)
  (fighter (direction+ p (* delta v)) v))

#; {Fighter Radian -> Fighter}
(define (rotate-fighter this rad)
  (match-define [fighter p v] this)
  (fighter p (dir-rotate v rad)))

#; {Figher -> Boolean}
(define (outside? this)
  (match-define [fighter p vel] this)
  (point:outside? p))

#; {Fighter [Listof Pill] -> (U #false Radian)}
;; determine whether any rotation between STEP-RAD and MAX-RAD gets `this` to any pill, if any
(define (rotate->reach this pill* [rad STEP-RAD])
  (let rotate->reach ([rad STEP-RAD])
    (define try (rotate-fighter this rad))
    (cond
      [(can-reach try pill*) rad]
      [(> rad MAX-RAD)       #false]
      [else (rotate->reach (+ rad STEP-RAD))])))

#; {Fighter [Listof Pill] -> (U #false Pill)}
;; determine the first pill in `pill*` that `this` can reach, if any 
(define (can-reach this pill*)
  (let can-reach ([this this])
    (cond
      [(on-any-pill this pill*) => identity]
      [(outside? this)          #false]
      [else (can-reach (move-fighter this))])))

#; {Fighter [Listof Pill] -> (U Pill #false)}
;; is the fighter sitting on any pill? 
;; `pill*` is the list of center points for the targeted pills
(define (on-any-pill this pill*)
  (define mine (fighter-posn this))
  (for/first ([p pill*] #:when (on-pill? p mine)) p))

;; -------------------------------------------------------------------------------------------------
;; STRATEGY: search for action that brings `this` fighter closer to some selected pill
;; 1. if it sits on a pill, _eat_ it. 
;; 2. if any of the pills is reachable in the given direction, _move_ forward.
;; 3. otherwise, change direction counter-clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; TODO 4. still not? change direction clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; 5. else: return default action. 

#; {Fighter [Listof Pill] -> Action}
(define (strategy-1 mine pill*)
  (cond
    [(on-any-pill mine pill*)   => eat]
    [(can-reach mine pill*)     => mov]
    [(rotate->reach mine pill*) => rot]
    [else                       (rot MAX-RAD)]))

;; ---------------------------------------------------------------------------------------------------

(module+ examples
  (define fighter0 (fighter 100+50i -9+6i))
  (define pill*0 (list red0))
  (define fighter1 (fighter (pill-posn red0) 0+0i))
  (define steps-2  20)
  (define fighter2 (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) +0+1i))
  (define fighter5 (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) -1+1i))
  (define fighter4 (rotate-fighter fighter2 (rotate->reach fighter2 pill*0)))
  (define fighter3 (move-fighter fighter4 (+ steps-2 3))))

(module+ test
  (add-fighter fighter2 (empty-scene 400 400)))
  
(module+ test
  (check-within (rotate-fighter (fighter 0.0 0+i) (/ pi 2)) (fighter 0.0 +1) .001))

(module+ test
  (define mtf (empty-scene 200 200))
  (define-values [fx fy] (->values (fighter-posn fighter0)))
  (check-true (image? (add-fighter fighter0 mtf))))

(module+ test
  (check-within (rotate->reach fighter2 pill*0) (- (/ pi 4) (asin (/ 10 (* (sqrt 2) steps-2)))) .1)
  (check-false (rotate->reach fighter5 pill*0)))

(module+ test
  (check-true (pill? (on-any-pill fighter1 pill*0)))
  (check-true (pill? (on-any-pill fighter3 pill*0)))
  (check-false (on-any-pill fighter0 pill*0)))

(module+ test
  (check-true (pill? (can-reach fighter1 pill*0)))
  (check-true (pill? (can-reach fighter4  pill*0)))
  (check-false (can-reach fighter0 pill*0)))

(module+ test
  (check-equal? (strategy-1 fighter1 pill*0) (eat red0))
  (check-equal? (strategy-1 fighter4 pill*0) (mov red0))
  (check-within (strategy-1 fighter2 pill*0) (rot (- (/ pi 4) (asin (/ 10 (* (sqrt 2) steps-2))))) .1)
  (check-equal? (strategy-1 fighter5 pill*0) (rot MAX-RAD)))
