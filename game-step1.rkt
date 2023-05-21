#lang racket

;; exploratory visualization 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/state)
(require PillWars/World/constants)

(require 2htdp/universe)
(require 2htdp/image)

(module+ test
  (require (submod PillWars/Common/state examples)))

;; ---------------------------------------------------------------------------------------------------
;; {String -> State}
(define (main my-name [state0 #false])
  (big-bang state0
    ;; the first clause is for an AI player 
    #;
    [on-tick fighter-action-strategy-1 1 30]
    [to-draw (draw-state BG)]
    [on-mouse turn-by-mouse]
    [on-key navigate-by-key]
    [name my-name]
    [stop-when game-over? (draw-state BG)]))

#; {State KeyEvent -> State}
;; allow player to navigate the game space via keystrokes:
;; -- ↑ for forward
;; -- ← for left
;; -- → for right
;; -- SPACE for "fire" ("destroy enemy" that the fighter is sitting on)
(define (navigate-by-key s ke)
  (cond
    [(key=? " " ke)     (eat-my-fighter s)]
    [(key=? "up" ke)    (move-my-fighter s)]
    [(key=? "right" ke) (rotate-my-fighter s (/ pi -60))]
    [(key=? "left" ke)  (rotate-my-fighter s (/ pi +60))]
    [else s]))

#; {State N N MouseEvent -> State}
;; allow player to navigate the game space via mouse clicks:
;; -- button-down in the yellow space of a fighter changes its direction
;; -- button-down in any white space moves "my" fighter straight ahead
;; -- button-down on an "enemy" "fires" IF the fighter is "on" the enemy 
(define (turn-by-mouse s x y me)
  (cond
    [(mouse=? "button-down" me)
     (cond
       [(state-mouse-click-ok? s x y) => (λ (θ) (rotate-my-fighter s (- θ)))]
       [(mouse-click-on-pill? s x y) => (λ (pill) (eat-my-fighter s pill))]
       [else (move-my-fighter s)])]
    [else s]))

#; {State -> Boolean}
(define (game-over? s)
  (or (empty? (state-fighters s)) (empty? (state-pills s))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main "Benjamin" state0))