#lang racket

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
(require PillWars/Common/action)
(require PillWars/Common/fighter)
(require PillWars/Common/state)
(require 2htdp/universe)

(module+ test
  (require (submod PillWars/Common/state examples))
  (require PillWars/Common/constants)
  (require PillWars/Common/pills)
  (require PillWars/Common/point)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define LEFT  (/ pi -60))
(define RIGHT (/ pi +60))

#; {State KeyEvent -> (U Action #false)}
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
#; {State N N MouseEvent -> State}
;; allow player to navigate the game space via mouse clicks:
;; -- button-down in the yellow space of a fighter changes its direction
;; -- button-down in any white space moves "my" fighter straight ahead
;; -- button-down on an "enemy" "fires" IF the fighter is "on" the enemy 
(define (act-on-button-down s x y me)
  (cond
    [(mouse=? "button-down" me) (act s x y)]
    [else #false]))

#; {State N N -> State}
;; ASSUME this is what happens when a player pressed the mouse at `(x,y)` in state `s`
(define (act s x y)
  (cond
    [(mouse-click-to-turn? s x y) => (λ (θ) (rot θ))]
    [(mouse-click-on-pill? s x y) => (λ (p) (eat p))]
    [else (mov 'dummy)]))

;; ---------------------------------------------------------------------------------------------------
#; {State -> Boolean}
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

  (check-equal? (act-on-button-down state0 (+ RS p.x 1) p.y "button-down") (mov 'dummy))

  (check-false (game-over? state0) "state0 is an okay starting state"))