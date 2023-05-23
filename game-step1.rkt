#lang racket

;; display and handle the game for a solo human player 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/state)
(require PillWars/World/handlers-for-human-nav)
(require PillWars/AI/strategy-1)
(require PillWars/World/constants)
(require 2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
;; {String -> State}
(define (main my-name [state0 #false])
  (define start-with (or state0 (create-state my-name)))
  (big-bang start-with
    [to-draw   (draw-state BG)]
    [on-mouse  turn-by-mouse]
    [on-key    navigate-by-key]
    [name      my-name]
    [stop-when game-over? (draw-state BG)]))

;; a turn-based game betweeen a human player and an AI 
(define (main/AI my-name [state0 #false])
  (define start-with (or state0 (create-state my-name)))
  (define end-with
    (big-bang (create-interactive start-with)
     [on-tick   (enable AI #;disable: HUMAN (ai-strategy strategy-1))]
     [to-draw   (strip (draw-state BG))]
     [on-mouse  (enable HUMAN #;disable: AI turn-by-mouse)]
     [on-key    (enable HUMAN #;disable: AI navigate-by-key)]
     [name      my-name]
     [stop-when (strip game-over?) (strip (draw-state-with-winners BG))]))
  (interactive-winners end-with))

;; ---------------------------------------------------------------------------------------------------
;; making a `big-bang` world a two-player game, with one of them an AI 

(struct interactive [whose-turn state] #:transparent)
#; {type Interactive = [interactive Tag State]}
#; {type Tag         = .. gensymed symbol .. }
;; INVARIANT `whose-turn` and the first player in `state` must be in sync
#; {type [Handler X] = (State Any ... -> X)}

(define AI (gensym "AI-"))
(define HUMAN (gensym "HUMAN"))

#; {State -> Interactive}
;; add an AI player and set it up to go first 
(define (create-interactive state)
  (interactive AI (add-player-to-front "AI" state)))

#; {Tag Tag [Handler State] -> Interactive Any ... -> Interactive}
(define ((enable tag other handler) i . others)
  (match-define [interactive whose state0] i)
  (cond
    [(equal? whose other) i]
    [else
     (define state++ (apply handler state0 others))
     (interactive other (swap-two-players state++))]))

#; {[Hander X] -> Interactive Any ... -> X}
(define [(strip handler) i . others]
  (match-define [interactive whose state] i)
  (apply handler state others))

#; {Interactive -> [Listof String]}
(define (interactive-winners i)
  (match-define [interactive whose state] i)
  (pretty-print state)
  (winners state))
  

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main/AI "Benjamin" #; state0))