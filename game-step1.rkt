#lang racket

;; exploratory visualization 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/fighter)
(require PillWars/Common/state)
(require PillWars/World/constants)
(require (submod PillWars/Common/state examples))

(require (except-in 2htdp/universe state))
(require 2htdp/image)

;; ---------------------------------------------------------------------------------------------------
(define (main state0)
  (big-bang state0
    [on-tick fighter-action-strategy-1 1 30]
    [to-draw (draw-state BG)]
    [stop-when (λ (s) (outside? (first (state-fighters s))))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main state0))