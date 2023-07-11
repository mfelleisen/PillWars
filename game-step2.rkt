#lang racket

(provide
 #; {N -> UState}
 universe-main

 #; {name:String ip:String -> Any}
 single-client)

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/state)
(require PillWars/AI/strategy-1)
(require PillWars/World/handlers-for-distributed-nav)
(require PillWars/World/constants)
(require (only-in PillWars/Common/geometry TIE XWING))
(require PillWars/Universe/handlers-for-universe) ;; accident 
(require 2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
(define server "antarctica.ccs.neu.edu")

(define (main-run-locally my-name)
  (launch-many-worlds (universe-main 2) (local-main my-name) (ai-main 0)))

(define (main-clients my-name [server server])
  (launch-many-worlds (local-main my-name server) (ai-main 0 server)))

(define (main-clients-local my-name) (main-clients my-name LOCALHOST))

(define (single-client my-name [server server])
  (local-main my-name server))

;; ---------------------------------------------------------------------------------------------------
;; run a game for `n` players 
(define (universe-main n)
  (universe (create-ustate)
    [on-new (add-player n)]
    [on-msg end-turn]
    [on-disconnect remove-player]))

;; ---------------------------------------------------------------------------------------------------
(define [ai-main i (server-ip LOCALHOST)]
  (define my-name (~a "Darth Vadder-" i))
  (define start-with (dummy-state my-name TIE))
  (big-bang start-with
    [to-draw    (draw-state AI-BG)]
    [on-receive ai-receive]
    [register   server-ip]
    [name       my-name]
    [close-on-stop 30]
    [stop-when  game-over? (draw-state-with-winners BG)]))

; {State State -> [Package State Action]}
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
  (define start-with (create-plus my-name XWING))
  (define end-with
    (big-bang start-with
      [to-draw    (strip (draw-explosions BG))]
      [register   server-ip]
      [on-mouse   (enable act-on-button-down)]
      [on-key     (enable navigate-by-key)]
      [on-receive human-receive]
      [name       my-name]
      [close-on-stop 30]
      [stop-when  (strip game-over?) (strip (draw-state-with-winners BG))]))
  (plus-winners end-with))

;; ---------------------------------------------------------------------------------------------------
;; "override" `draw-state` with a function that shows explosions when a fighter 'eats' a pill 

(module explosions racket
  (provide draw-explosions)

  (require PillWars/Common/pills)
  (require PillWars/AI/strategy-1)
  (require PillWars/Common/state)
  (require PillWars/World/constants)
  (require PillWars/Lib/image)
  (require PillWars/Common/point)
  (require PillWars/Common/geometry)
  (require 2htdp/image)

  (module+ test
    (require rackunit)
    (require (submod PillWars/AI/strategy-1 examples)))

  #; {Scene -> State -> Scene}
  (define (draw-explosions BG)
    (define pills-1 '())
    (define (draw-explosions-then-state state0)
      (define pills-0 (state-pills state0))
      (define scene0  (add-explosions BG (explosion-posns pills-1 pills-0)))
      (set! pills-1 pills-0)
      [(draw-state scene0) state0])
    draw-explosions-then-state)

  #; {[Listof Pill] [Listof Pill] -> [Listof Point]}
  (define (explosion-posns pills-1 pills-0)
    (define set-pills-1 (apply set pills-1))
    (define set-pills-0 (apply set pills-0))
    (define delta (set-subtract set-pills-1 set-pills-0))
    (set-map delta pill-posn))
      
  #; {Scene [Listof Point] -> Scene}
  (define (add-explosions BG posns)
    (add-objects BG posns (λ (p s) (let-values ([(x y) (->values p)]) (place-image EXPL x y s)))))
  
  (module+ test
    (check-true (cons? (set-map (set-subtract (apply set pill*1) (apply set pill*0)) pill-posn)))
    (check-true (image? (add-explosions BG (explosion-posns pill*1 pill*0))))
    (check-true (image? (add-explosions BG [list 10+33i 100+200i]))))

  (module+ test ;; for demo file
    (require PillWars/Common/fighter)
    (define dv (create-fighter "Darth Vadder" 'tie))
    (define bf (create-fighter  "Benjamin" 'xwing))
    (define s* (plain-state))
    (define s0 (add-fighter-to-front dv 'default (add-fighter-to-front bf 'default s*)))
    (define p* (first (state-pills s*)))
    (define s1 (eat-my-fighter s0 p*))

    (define shared-draw (draw-explosions BG))
    (void [shared-draw s0])
    (define demo.png (scale .5 [shared-draw s1]))
    (save-image demo.png "Resources/demo.png")))

(require 'explosions)
;; for demo
#;
(require (submod "." explosions test))

;; ---------------------------------------------------------------------------------------------------
;; a universe a two-player game, with one of them an AI, by en-/dis-abling handlers for a solo player

(struct plus [my-turn? state] #:transparent)
#; {type Plus = [plus Boolean State]}
;; INVARIANT if `my-turn?` holds, then the first player in `state` is me 
#; {type [Handler X] = (State Any ... -> X)}

#; {String Symbol -> Plus}
;; add an AI player and set it up to go first 
(define (create-plus my-name fighter-type)
  (plus #false (dummy-state my-name fighter-type)))

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
#; {String Symbol -> State}
(define (dummy-state my-name fighter-type)
  (add-pill-at-fighter (add-fighter-to-front my-name fighter-type (empty-state))))
  
;; ---------------------------------------------------------------------------------------------------
(module+ server 
  (universe-main 2))

(module+ server3
  (universe-main 3))

(module+ client
  (main-clients "Benjamin"))

(module+ cf
  (single-client "Christopher"))

(module+ mf
  (single-client "Matthias"))

(module+ local
  (main-clients "Benjamin" LOCALHOST))
