#lang racket

(require (except-in 2htdp/universe state))

(require 2htdp/image)

(module image racket
  (provide add-objects)
  
  (require 2htdp/image)

  #; {Image [Listof X] [Image X -> Image] -> Image}
  ;; add the given `objects` to `scene0` 
  (define (add-objects scene0 objects add-1-object)
    (for/fold ([s scene0]) ([f objects])
      (add-1-object f s))))

(require rackunit)

;; ---------------------------------------------------------------------------------------------------
(module point racket
  (provide
   #; {type Point = Complex}
   
   #; {Point -> (values Real Real)}
   ->values

   #; {Point Point -> Real} 
   distance

   #; {Point Direction -> Point}
   direction+)
  
  (define (->values p)
    (values (real-part p) (imag-part p)))
  
  (define (distance p q)
    (magnitude (- p q)))

  (define (direction+ p d)
    (+ p d)))
(require 'point)

;; ---------------------------------------------------------------------------------------------------
(module direction racket
  (provide
   #; {type Direction = Complex}
   
   #; {Direction Radian -> Direction}
   dir-rotate

   ;; doesn't really belong here 
   rad->deg
   deg->rad)
  
  (define (dir-rotate vel deg)
    (* vel (exp (* 0+i (- deg))) #;(make-polar (magnitude vel) deg)))

  #; {Radian -> Degree}
  (define (rad->deg r)
    (* 180 (/ r pi)))

  #; {Degree -> Radian}
  (define (deg->rad d)
    (* pi (/ d 180))))
(require 'direction)


;; ---------------------------------------------------------------------------------------------------
;; global graphical constants

(module graphics racket
  (provide WIDTH HEIGHT RS BG)
  (require 2htdp/image)
  (define WIDTH 400)
  (define HEIGHT 400)
  (define RS (/ WIDTH 40))
  (define BG (empty-scene WIDTH HEIGHT)))
(require 'graphics)

;; ---------------------------------------------------------------------------------------------------
(module pill racket
  (provide 
   #; {type Pill = [blue Point] || [red Point Degree]}
   blue0
   red0
   
   pill-posn
   pill?
   red? ;; for sample run

   ;; for testing 
   RD
   BL

   add-pill

   on-pill?)

  (require (submod ".." graphics))
  (require 2htdp/image)
  (require (submod ".." point))
  (module+ test
    (require rackunit))

  (struct pill [posn] #:transparent)
  (struct blue pill [] #:transparent)
  (struct red pill [points] #:transparent)

  (define blue0 (blue (make-rectangular (/ WIDTH 2) (/ WIDTH 10))))
  (define red0 (red (make-rectangular (/ WIDTH 4) (/ WIDTH 2)) 'dummy))
  
  (define BL (circle RS 'solid 'blue))
  (define RD (circle RS 'solid 'red))

  #; {Pill Image -> Image}
  ;; add `pill` to the given `scene0` 
  (define (add-pill this s0)
    (define-values (p img)
      (match this
        [[red posn value] (values posn RD)]
        [[blue posn]      (values posn BL)]))
    (define-values [x y] (->values p))
    (place-image img x y s0))

  #; {Pill Figher -> Boolean}
  (define (on-pill? this posn)
    (<= (distance posn (pill-posn this)) RS))

  (module+ test
    (check-true (on-pill? blue0 (pill-posn blue0)))
    (check-false (on-pill? blue0 (+ RS 1 (pill-posn blue0))))

    (define mt (empty-scene 300 300))
    (define-values [bx by] (->values (pill-posn blue0)))
    (define-values [rx ry] (->values (pill-posn red0)))
    (check-equal? (add-pill blue0 mt) (place-image BL bx by mt))
    (check-equal? (add-pill red0 mt) (place-image RD rx ry mt))))
(require 'pill)

;; ---------------------------------------------------------------------------------------------------
(module action racket
  (provide
   #; {type Action = [rotate Deg] || [eat Pill] || [mov Pill]}
   (struct-out mov)
   (struct-out eat)
   (struct-out rot)
   MAX-RAD
   STEP-RAD)

  (define MAX-RAD (/ pi 3))
  (define STEP-RAD (/ MAX-RAD 30))

  (struct mov [posn] #:transparent)
  (struct eat [posn] #:transparent)
  (struct rot [angle] #:transparent))
(require 'action)

;; ---------------------------------------------------------------------------------------------------
(module fighter racket

  (provide
   add-fighter
   rotate-fighter
   move-fighter)

  (provide
   rotate->reach
   on-any-pill
   can-reach
   strategy-1
   outside?)

  (provide
   (struct-out fighter))

  (provide fighter0)
  
  
  (require (submod ".." action))
  (require (submod ".." direction))
  (require (submod ".." graphics))
  (require (submod ".." pill))
  (require (submod ".." point))
  (require 2htdp/image)
  (module+ test
    (require rackunit))

  (struct fighter [posn velocity] #:transparent)
  #; {type Fighter   = [fighter Point Direction]}

  (define fighter0 (fighter 100+50i -9+6i))

  (define FI
    (let ()
      (define FI0  (isosceles-triangle 40 20 'solid 'green))
      (define PT   (circle 3 'solid 'orange))
      (define DISK (circle (image-height FI0) 'solid (color 255 255 255 0)))
      (rotate -90 (overlay PT (overlay/align 'center 'top FI0 DISK)))))

  #; {Fighter Image -> Image}
  ;; add fighter `f` to the given scene `s0`
  (define (add-fighter this scene0)
    (match-define [fighter posn vel] this)
    (define-values (p.x p.y) (->values posn))
    (define-values [v.x v.y] (->values (+ posn (* 40 vel))))
    (let* ([s scene0]
           [s (place-image (rotate (rad->deg (angle (conjugate vel))) FI) p.x p.y s)]
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
    (define-values [p.x p.y] (->values p))
    (or (< p.x 0) (> p.x WIDTH)
        (< p.y 0) (> p.y HEIGHT)))

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
  
  (module+ test
    (check-within (rotate-fighter (fighter 0.0 0+i) (/ pi 2)) (fighter 0.0 +1) .001))

  (module+ test
    (define mtf (empty-scene 200 200))
    (define-values [fx fy] (->values (fighter-posn fighter0)))
    (check-true (image? (add-fighter fighter0 mtf))))

  (module+ test
    (define pill*0 (list red0))
    (define fighter1 (fighter (pill-posn red0) 0+0i))
    (define steps-2  20)
    (define fighter2 (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) +0+1i))
    (define fighter5 (fighter (direction+ (pill-posn red0) (* -1 steps-2 1+1i)) -1+1i))
    (define fighter4 (rotate-fighter fighter2 (rotate->reach fighter2 pill*0)))
    (define fighter3 (move-fighter fighter4 (+ steps-2 3)))
  
    (check-within (rotate->reach fighter2 pill*0) (- (/ pi 4) (asin (/ 10 (* (sqrt 2) steps-2)))) .1)
    (check-false (rotate->reach fighter5 pill*0))

    (check-true (pill? (on-any-pill fighter1 pill*0)))
    (check-true (pill? (on-any-pill fighter3 pill*0)))
    (check-false (on-any-pill fighter0 pill*0))

    (check-true (pill? (can-reach fighter1 pill*0)))
    (check-true (pill? (can-reach fighter4  pill*0)))
    (check-false (can-reach fighter0 pill*0)))

  (module+ test
    (check-equal? (strategy-1 fighter1 pill*0) (eat red0))
    (check-equal? (strategy-1 fighter4 pill*0) (mov red0))
    (check-within (strategy-1 fighter2 pill*0) (rot (- (/ pi 4) (asin (/ 10 (* (sqrt 2) steps-2))))) .1)
    (check-equal? (strategy-1 fighter5 pill*0) (rot MAX-RAD)))
  )
(require 'fighter)

;; ---------------------------------------------------------------------------------------------------
(module state racket
  (provide
   #; {type State = [state [Listof Fighter] [Listof Pill]] || the first fighter is mine}
   (struct-out state)
   draw-state
   state-fighters
   fighter-action-strategy-1)

  (provide
   execute)

  (provide
   state0)

  (require (submod ".." action))
  (require (submod ".." fighter))
  (require (submod ".." graphics))
  (require (submod ".." image))
  (require (submod ".." pill))
  (module+ test
    (require 2htdp/image)
    (require rackunit))

  (struct state [fighters pills] #:transparent)

  (define (state-my-fighter state0)
    (first (state-fighters state0)))

  (define (state-my-fighter-update state0 next)
    (match-define [state fighters pills] state0)
    (state (cons next (rest fighters)) pills))

  (define state0 (state [list fighter0] [list blue0 red0]))

  #; {State -> Image}
  (define (draw-state s)
    (match-define [state fighter* pill*] s) 
    (let* ([s BG]
           [s (add-objects s fighter* add-fighter)]
           [s (add-objects s pill* add-pill)])
      s))

  #; {State Action -> State}
  (define (execute state0 action)
    (match action
      [(rot rad)  (rotate-my-fighter state0 rad)]
      [(mov _)    (move-my-fighter state0)]
      [(eat pill) (eat-my-fighter state0 pill)]))

  #; {State Radian -> State}
  (define (rotate-my-fighter state0 rad)
    (define mine (state-my-fighter state0))
    (state-my-fighter-update state0 (rotate-fighter mine rad)))

  #; {State -> State}
  (define (move-my-fighter state0)
    (define mine (state-my-fighter state0))
    (state-my-fighter-update state0 (move-fighter mine)))

  #; {State Pill -> State}
  (define (eat-my-fighter state0 pill)
    (match-define [state fighter* pill*] state0)
    (state fighter* (remove pill pill*)))

  #; {State -> State}
  (define (fighter-action-strategy-1 state0)
    (define mine   (state-my-fighter state0))
    (define pill*  (filter-map (λ (p) (and (red? p) p)) (state-pills state0)))
    (define action (strategy-1 mine pill*))
    (execute state0 action))

  (module+ test
    (check-true (image? (draw-state state0))))

  (module+ test
    (define state1 (state (list fighter1) pill*0))
    (define state1-red (state (list fighter1) (list)))

    (define state4 (state (list fighter4) pill*0))
    (define state4-mov (state (list (move-fighter fighter4)) pill*0))

    (define state5 (state (list fighter5) pill*0))
    (define state5-max (state (list (rotate-fighter fighter5 MAX-RAD)) pill*0))

    (check-equal? (execute state1 (eat red0)) state1-red)
    (check-equal? (execute state4 (mov 'me)) state4-mov)
    (check-equal? (execute state5 (rot MAX-RAD)) state5-max))

  (module+ test
    (check-equal? (fighter-action-strategy-1 state1) state1-red)))
(require 'state)

;; ---------------------------------------------------------------------------------------------------
;; exploratory visualization 

(define (main state0)
  (big-bang state0
    [on-tick fighter-action-strategy-1 1 30]
    [to-draw draw-state]
    [stop-when (λ (s) (outside? (first (state-fighters s))))]))


(module+ test
  (main state0))