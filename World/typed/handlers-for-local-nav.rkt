#lang typed/racket

;; event handlers for a human player interaction (solo)

;; ---------------------------------------------------------------------------------------------------
(provide

 ; {State KeyEvent -> State}
 navigate-by-key

 #; {State N N MouseEvent -> State}
 act-on-button-down

 #; {State -> Boolean}
 game-over?)
 
;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/state)
(require PillWars/Common/typed/pills)
(require PillWars/Common/typed/direction)
(require/typed 2htdp/universe
               [mouse=? (-> String String Boolean)]
               [key=?   (-> String String Boolean)])

(module+ test
  (require (submod PillWars/Common/typed/state examples))
  (require PillWars/Common/typed/constants)
  (require PillWars/Common/typed/pills)
  (require PillWars/Common/typed/point)
  (require typed/rackunit))

;; ---------------------------------------------------------------------------------------------------
(define LEFT  (/ pi -60))
(define RIGHT (/ pi +60))

(: navigate-by-key {State String -> State})
;; allow player to navigate the game space via keystrokes:
;; -- ↑ for forward
;; -- ← for left
;; -- → for right
;; -- SPACE for "fire" ("destroy enemy" that the fighter is sitting on)
(define (navigate-by-key s ke)
  (cond
    [(key=? " " ke)     (eat-my-fighter s)]
    [(key=? "up" ke)    (move-my-fighter s)]
    [(key=? "right" ke) (rotate-my-fighter s RIGHT)]
    [(key=? "left" ke)  (rotate-my-fighter s LEFT)]
    [else s]))

;; ---------------------------------------------------------------------------------------------------
(: act-on-button-down {State Integer Integer String -> (U False State)})
;; allow player to navigate the game space via mouse clicks:
;; -- button-down in the yellow space of a fighter changes its direction
;; -- button-down in any white space moves "my" fighter straight ahead
;; -- button-down on an "enemy" "fires" IF the fighter is "on" the enemy 
(define (act-on-button-down s x y me)
  (cond
    [(mouse=? "button-down" me) (act s x y)]
    [else #false]))

(: act {State Integer Integer -> State})
;; ASSUME this is what happens when a player pressed the mouse at `(x,y)` in state `s`
(define (act s x y)
  (cond
    [(mouse-click-to-turn? s x y) => (λ ({θ : Radian}) (rotate-my-fighter s θ))]
    [(mouse-click-on-pill? s x y) => (λ (_) (eat-my-fighter s))]
    [else (move-my-fighter s)]))

;; ---------------------------------------------------------------------------------------------------
(: game-over? {State -> Boolean})
(define (game-over? s)
  (or (empty? (state-fighters s)) (empty? (state-pills s))))

;; ---------------------------------------------------------------------------------------------------
(module+ test

  (check-equal? (navigate-by-key state0 "up") (move-my-fighter state0))
  (check-equal? (navigate-by-key state0 "left") (rotate-my-fighter state0 LEFT))
  (check-equal? (navigate-by-key state0 "right") (rotate-my-fighter state0 RIGHT))
  (check-equal? (navigate-by-key state0 " ") (eat-my-fighter state0))
  (check-equal? (navigate-by-key state0 "a") state0)

  (check-equal? (act-on-button-down state0 10 50 "button-up") #false)
  (check-equal? (act-on-button-down state0 10 50 "button-down")
                (rotate-my-fighter state0 (assert (mouse-click-to-turn? state0 10 50) real?)))

  (define a-pill (first (state-pills state0)))
  (define-values [p.x p.y] (->values (pill-posn a-pill)))

  (: ~i (-> Real Integer))
  (define (~i x) (assert (inexact->exact (round x)) exact-integer?))

  (check-equal? (act-on-button-down state0 (~i (+ RS p.x 1)) (~i p.y) "button-down")
                (move-my-fighter state0))

  (check-false (game-over? state0) "state0 is an okay starting state"))
