#lang racket

;; display and handle the game for a solo human player 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/AI/strategy-1)
(require PillWars/Common/state)
(require PillWars/Universe/handlers-for-universe)
(require PillWars/World/handlers-for-distributed-nav)
(require PillWars/World/constants)
(require 2htdp/universe)

(define (main my-name)
  (define [umain] (universe-main 1))
  (define [lmain] (local-main my-name))
  (launch-many-worlds [umain] [lmain] #;(ai-main)))

;; ---------------------------------------------------------------------------------------------------
#; {N -> USTate}
;; run a game for `n` players 
(define (universe-main n)
  (universe (create-ustate)
    [on-new (add-player n)]
    [on-msg end-turn]
    [on-disconnect remove-player]))

;; ---------------------------------------------------------------------------------------------------
(define [ai-main (server-ip LOCALHOST)]
  (define start-with (add-fighter-to-front "AI" (empty-state)))
  (define my-name "Darth Vadder")
  (big-bang start-with
    [to-draw    (draw-state BG)]
    [register   server-ip]
    [state #true]
    [on-receive ai-receive]
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
  (define start-with (create-plus (add-pill-at-fighter (add-fighter-to-front my-name (empty-state)))))
  (define end-with
    (big-bang start-with
      [to-draw    (strip (draw-state BG))]
      [register   server-ip]
      [on-mouse   (enable turn-by-mouse)]
      [on-key     (enable navigate-by-key)]
      [on-receive human-receive]
      [name       my-name]
      [state #true]
      [stop-when  (strip game-over?) (strip (draw-state-with-winners BG))]))
  (plus-winners end-with))

;; ---------------------------------------------------------------------------------------------------
;; a universe a two-player game, with one of them an AI, by en-/dis-abling handlers for a solo player

(struct plus [my-turn? state] #:transparent)
#; {type Plus = [plus Boolean State]}
;; INVARIANT if `my-turn?` holds, then the first player in `state` is me 
#; {type [Handler X] = (State Any ... -> X)}

#; {State -> Plus}
;; add an AI player and set it up to go first 
(define (create-plus state)
  (plus #false state))

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
(module+ test
  (main "Benjamin"))