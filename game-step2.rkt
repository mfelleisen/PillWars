#lang racket

;; display and handle the game for a solo human player 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/fighter)
(require PillWars/AI/strategy-1)
(require PillWars/Common/state)
(require PillWars/Universe/handlers-for-universe)
(require PillWars/World/handlers-for-distributed-nav)
(require PillWars/World/constants)
(require 2htdp/universe)

(define (main my-name)
  (launch-many-worlds (universe-main 2) (local-main my-name) (ai-main 0)))

;; ---------------------------------------------------------------------------------------------------
#; {N -> USTate}
;; run a game for `n` players 
(define (universe-main n)
  (universe (create-ustate)
    [on-new (add-player n)]
    [on-msg end-turn]
    [on-disconnect remove-player]))

;; ---------------------------------------------------------------------------------------------------
(define [ai-main i (server-ip LOCALHOST)]
  (define my-name (~a "Darth Vadder-" i))
  (define start-with (dummy-state my-name))
  (big-bang start-with
    [to-draw    (draw-state BG)]
    [on-receive ai-receive]
    [register   server-ip]
    [name       my-name]
    [stop-when  game-over? (draw-state-with-winners BG)]))

#; {State State -> [Package State Action]}
(define (ai-receive _ msg)
  (cond
    [(not (your-turn? msg)) msg]
    [else
     (define state0 (your-turn-state msg))
     (define action [ai-action-strategy strategy-1 state0])
     (define state+ (execute state0 action))
     (make-package state+ action)]))

;; ---------------------------------------------------------------------------------------------------
;; a turn-based game betweeen a human player and an AI 
(define (local-main my-name (server-ip LOCALHOST)) 
  (define start-with (create-plus my-name))
  (define end-with
    (big-bang start-with
      [to-draw    (strip (draw-state BG))]
      [register   server-ip]
      [on-mouse   (enable turn-by-mouse)]
      [on-key     (enable navigate-by-key)]
      [on-receive human-receive]
      [name       my-name]
      [stop-when  (strip game-over?) (strip (draw-state-with-winners BG))]))
  (plus-winners end-with))

;; ---------------------------------------------------------------------------------------------------
;; a universe a two-player game, with one of them an AI, by en-/dis-abling handlers for a solo player

(struct plus [my-turn? state] #:transparent)
#; {type Plus = [plus Boolean State]}
;; INVARIANT if `my-turn?` holds, then the first player in `state` is me 
#; {type [Handler X] = (State Any ... -> X)}

#; {String -> Plus}
;; add an AI player and set it up to go first 
(define (create-plus my-name)
  (plus #false (dummy-state my-name)))

#; {Plus S-expression -> Plus}
(define (human-receive _ msg)
  (if (your-turn? msg) (plus #true (your-turn-state msg)) (plus #false msg)))

#; {[Handler State] -> Plus Any ... -> plus}
(define ((enable handler) i . others)
  (match-define [plus my-turn? state0] i)
  (cond
    [(not my-turn?) i]
    [my-turn?
     (cond
       [(apply handler state0 others)
        => (λ (action) (make-package (plus #false (execute state0 action)) action))]
       [else i])]))

#; {[Hander X] -> Plus Any ... -> X}
(define [(strip handler) i . others]
  (match-define [plus whose state] i)
  (apply handler state others))

#; {plus -> [Listof String]}
(define (plus-winners i)
  (winners (plus-state i)))

;; ---------------------------------------------------------------------------------------------------
#; {String -> State}
(define (dummy-state my-name)
  (add-pill-at-fighter (add-fighter-to-front my-name (empty-state))))
  
;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main "Benjamin"))