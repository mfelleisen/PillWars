#lang racket

;; display and handle the game for a solo, local human player 
;; display and handle the game for a local human player competing against an "AI"

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/state)
(require PillWars/Common/fighter)
(require PillWars/World/handlers-for-local-nav)
(require PillWars/AI/strategy-1)
(require PillWars/World/constants)
(require 2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
;; {String [State] -> State}
;; a turn-based game betweeen a human player and an AI; the optional state can make it deterministic 
(define (main/AI my-name [state0 #false])
  (define start-with (or state0 (create-state (~a my-name))))
  (define end-with
    (big-bang (create-interactive start-with)
     [on-tick   (enable AI #;disable: HUMAN (ai-strategy strategy-1))]
     [to-draw   (strip (draw-state BG))]
     [on-mouse  (enable HUMAN #;disable: AI act-on-button-down)]
     [on-key    (enable HUMAN #;disable: AI navigate-by-key)]
     [name      (~a my-name)]
     [stop-when (strip game-over?) (strip (draw-state-with-winners BG))]))
  (interactive-winners end-with))

;; ---------------------------------------------------------------------------------------------------
;; making a `big-bang` world a two-player game, with one of them an AI
;; ASSUME the AI can't make the mistake of dropping out
;; CONSEQUENCE if the Human player drops out, we notice because the AI player is first. 

(struct interactive [whose-turn state] #:transparent)
#; {type Interactive = [interactive Tag State]}
#; {type Tag         = .. gensymed symbol .. }
;; INVARIANT `whose-turn` and the first player in `state` must be in sync
#; {type [Handler X] = (State Any ... -> X)}

(define AI (gensym "AI"))
(define HUMAN (gensym "HUMAN"))

#; {State -> Interactive}
;; add an AI player and set it up to go first 
(define (create-interactive state)
  (interactive AI (add-fighter-to-front (~a AI) state)))

#; {Tag Tag [Handler State] -> Interactive Any ... -> Interactive}
(define ((enable tag other handler) i . others)
  (match-define [interactive whose state0] i)
  (cond
    [(only-one-player state0)
     (define state++ (apply handler state0 others))
     (interactive AI state++)]
    [(equal? whose other) i]
    [else
     (define state++ (apply handler state0 others))
     (if (boolean? state++)
         i
         (interactive other (swap-two-players state++)))]))

#; {State -> Boolean}
(define (only-one-player state0)
  (define players (state-fighters state0))
  (and (cons? players) (empty? (rest players))))

#; {[Hander X] -> Interactive Any ... -> X}
(define [(strip handler) i . others]
  (match-define [interactive whose state] i)
  (apply handler state others))

#; {Interactive -> [Listof String]}
(define (interactive-winners i)
  (match-define [interactive whose state] i)
  (winners state))
  

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (main/AI "Benjamin" #; state0))
