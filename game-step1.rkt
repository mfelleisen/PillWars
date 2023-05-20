#lang racket

;; exploratory visualization 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/fighter)
(require PillWars/Common/state)
(require PillWars/World/constants)
(require PillWars/Common/state)

(require 2htdp/universe)
(require 2htdp/image)

(module+ test
  (require (submod PillWars/Common/state examples)))

;; ---------------------------------------------------------------------------------------------------
(define (main state0)
  (big-bang state0
    #;
    [on-tick fighter-action-strategy-1 1 30]
    [to-draw (draw-state BG)]
    [on-mouse (λ (s x y me)
                (cond
                  [(and (mouse=? "button-down" me) (state-mouse-click-ok? s x y))
                   => (λ (θ) (rotate-my-fighter s (- θ)))]
                  [else s]))]
    [on-key (λ (s ke)
              (cond
                [(key=? " " ke)     (eat-my-fighter s)]
                [(key=? "up" ke)    (move-my-fighter s)]
                [(key=? "right" ke) (rotate-my-fighter s (/ pi -60))]
                [(key=? "left" ke)  (rotate-my-fighter s (/ pi +60))]
                [else s]))]
    [stop-when (λ (s) (outside? (first (state-fighters s))))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main state0))