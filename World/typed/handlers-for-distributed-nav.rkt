#lang typed/racket

;; event handlers for a human-player interaction in a distributed game 

;; ---------------------------------------------------------------------------------------------------
(provide

 ; {State KeyEvent -> State}
 navigate-by-key

 #; {State N N MouseEvent -> State}
 act-on-button-down

 #; {State -> Boolean}
 game-over?)
 
;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/action)
(require PillWars/Common/typed/fighter)
(require PillWars/Common/typed/state)
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

(: navigate-by-key {State String -> (U Action #false)})
;; allow player to navigate the game space via keystrokes:
;; -- ↑ for forward
;; -- ← for left
;; -- → for right
;; -- SPACE for "fire" ("destroy enemy" that the fighter is sitting on)
(define (navigate-by-key s ke)
  (cond
    [(key=? " " ke)     (eat (fighter-posn (state-my-fighter s)))]
    [(key=? "up" ke)    (mov 'dummy)]
    [(key=? "right" ke) (rot RIGHT)]
    [(key=? "left" ke)  (rot LEFT)]
    [else #false]))

;; ---------------------------------------------------------------------------------------------------
(: act-on-button-down {State Integer Integer String -> (U False Action)})
;; allow player to navigate the game space via mouse clicks:
;; -- button-down in the yellow space of a fighter changes its direction
;; -- button-down in any white space moves "my" fighter straight ahead
;; -- button-down on an "enemy" "fires" IF the fighter is "on" the enemy 
(define (act-on-button-down s x y me)
  (cond
    [(mouse=? "button-down" me) (act s x y)]
    [else #false]))

(: act {State Integer Integer -> (U False Action)})
;; ASSUME this is what happens when a player pressed the mouse at `(x,y)` in state `s`
(define (act s x y)
  (cond
    [(mouse-click-to-turn? s x y) => (λ (θ) (rot θ))]
    [(mouse-click-on-pill? s x y) => (λ (p) (eat p))]
    [else (mov 'dummy)]))

;; ---------------------------------------------------------------------------------------------------
(: game-over? {State -> Boolean})
(define (game-over? s)
  (or (empty? (state-fighters s)) (empty? (state-pills s))))

;; ---------------------------------------------------------------------------------------------------
(module+ test

  (check-equal? (navigate-by-key state0 "up") (mov 'dummy))
  (check-true (rot? (navigate-by-key state0 "left")))
  (check-true (rot? (navigate-by-key state0 "right")))
  (check-equal? (navigate-by-key state0 " ") (eat (fighter-posn (first (state-fighters state0)))))
  (check-false (navigate-by-key state0 "a"))

  (check-false (act-on-button-down state0 10 50 "button-up"))
  (check-true (rot? (act-on-button-down state0 10 50 "button-down")))

  (define a-pill (first (state-pills state0)))
  (define-values [p.x p.y] (->values (pill-posn a-pill)))

  (: ~i (-> Real Integer))
  (define (~i x) (assert (inexact->exact (round x)) exact-integer?))

  (check-equal? (act-on-button-down state0 (~i (+ RS p.x 1)) (~i p.y) "button-down") (mov 'dummy))

  (check-false (game-over? state0) "state0 is an okay starting state"))
