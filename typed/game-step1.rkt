#lang typed/racket

;; display and handle the game for a local human player competing against an "AI"
;; run game between 2 "AI"s 

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/state)
(require PillWars/Common/typed/fighter)
(require PillWars/World/typed/handlers-for-local-nav)
(require PillWars/AI/typed/strategy-1)
(require PillWars/World/typed/constants)
(require PillWars/Lib/typed/image)
(require typed/2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
(: main/AI {->* (String) [State] [Listof String]})
;; a turn-based game betweeen a human player and an AI; the optional state can make it deterministic 
(define (main/AI my-name [state0 #false])
  (define start-with (or state0 (create-state (~a my-name))))
  (define end-with
    (big-bang (create-interactive start-with) : Interactive 
      [on-tick   (enable AI #;disable: HUMAN (ai-strategy strategy-1))]
      [to-draw   (strip (draw-state BG))]
      [on-mouse  (enable HUMAN #;disable: AI act-on-button-down {x Integer} {y Integer} {me String})]
      [on-key    (enable HUMAN #;disable: AI navigate-by-key {ke String})]
      [name      (~a my-name)]
      [stop-when (strip game-over?) (strip (draw-state-with-winners BG))]))
  (interactive-winners end-with))

;; the next one is added for head-less profiling; derived from the above w/ attempt to make it similar
(: 2AIs {->* () [State] [Listof String]})
(define (2AIs [state0 #false])
  (define state++    (or state0 (create-state (~a AI2))))
  (define start-with (create-interactive state++))
  (define end-with
    (big-bang/nodraw start-with : Interactive 
      [on-tick       (enable AI #;disable: AI2 (ai-strategy strategy-1))]
      [on-tick-other (enable AI2 #;disable: AI (ai-strategy strategy-1))]
      [stop-when     (strip game-over?)]))
  (interactive-winners end-with))

(define-syntax-rule (big-bang/nodraw state0 : T [on-tick th] [on-other-tick th-other] [stop-when sw?])
  (let loop : T ([state : T state0] [handle : (Pairof (T -> T) (T -> T))  (cons th th-other)])
    (cond
      [(sw? state) state]
      [else (match-define (cons f h) handle)
            (define state++ (f state))
            (loop state++ (cons h f))])))
    
;; ---------------------------------------------------------------------------------------------------
;; making a `big-bang` world a two-player game, with one of them an AI
;; ASSUME the AI can't make the mistake of dropping out
;; CONSEQUENCE if the Human player drops out, we notice because the AI player is first. 

(struct interactive [{whose-turn : Symbol} {state : State}] #:transparent #:type-name Interactive)
#; {type Interactive = [interactive Tag State]}
#; {type Tag         = .. gensymed symbol .. }
;; INVARIANT `whose-turn` and the first player in `state` must be in sync
#; {type [Handler X] = (State Any ... -> X)}

(define AI (gensym "AI"))
(define AI2 (gensym "AI"))
(define HUMAN (gensym "HUMAN"))

(: create-interactive {State -> Interactive})
;; add an AI player and set it up to go first 
(define (create-interactive state)
  (interactive AI (add-fighter-to-front (~a AI) state)))

; (: enable {-> Symbol Symbol [Handler State State] (-> Interactive Any * Interactive)})
(define-syntax-rule (enable tag other handler {rem Rem} ...)
  (λ ({i : Interactive} {rem : Rem} ...)
    (match-define [interactive whose state0] i)
    (cond
      [(only-one-player state0)
       (define state++ (handler state0 rem ...))
       (interactive AI (cast state++ State))]
      [(equal? whose other) i]
      [else
       (define state++ (handler state0 rem ...))
       (if (boolean? state++)
           i
           (interactive other (swap-two-players state++)))])))

(: only-one-player {State -> Boolean})
(define (only-one-player state0)
  (define players (state-fighters state0))
  (and (cons? players) (empty? (rest players))))

(: strip (∀ (X) {-> (State -> X) (-> Interactive X)}))
(define [(strip handler) i]
  (match-define [interactive whose state] i)
  (handler state))

(: interactive-winners {Interactive -> [Listof String]})
(define (interactive-winners i)
  (match-define [interactive whose state] i)
  (winners state))
  

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (time (2AIs))
  #;
  (main/AI "WhoSPlaying" #; state0))
