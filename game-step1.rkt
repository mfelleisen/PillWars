#lang racket

;; display and handle the game for a solo human player 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/state)
(require PillWars/World/handlers-for-human-nav)
(require PillWars/World/constants)

(require 2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
;; {String -> State}
(define (main my-name [state0 #false])
  (define start-with (or state0 (create-state my-name)))
  (big-bang start-with
    ;; the first clause is for an AI player 
    #;
    [on-tick   fighter-action-strategy-1 1 30]
    [to-draw   (draw-state BG)]
    [on-mouse  turn-by-mouse]
    [on-key    navigate-by-key]
    [name      my-name]
    [stop-when game-over? (draw-state BG)]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main "Benjamin" #; state0))