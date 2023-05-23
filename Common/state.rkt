#lang racket

;; a data representation of the global, universal state; also used as local state for now

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type State = [state [Listof Fighter] [Listof Pill]] || the first fighter is mine}

 #; {String -> State}
 ;; a random state 
 create-state

 #; {(U String Symbol) State -> State}
 ;; make a player with the give name and add it to the front of the player list of `state`
 add-player-to-front

 state-my-fighter 
 state-fighters
 state-pills

 #; {State -> State}
 ;; swap the first two (and should be only) players
 swap-two-players

 #; {State Scene -> Scene}
 draw-state
 draw-state-with-winners 

 #; {State N N -> (U Pill #false)}
 state-mouse-click-ok?

 #; {State N N -> (U Radian #false)}
 mouse-click-on-pill?

 #; {State Radian -> State}
 rotate-my-fighter

 #; {State -> State}
 move-my-fighter

 #; {State Pill -> State}
 eat-my-fighter

 #; {State -> [Listof String]}
 winners
 
 #; {[Fighter [Listof Pill] -> Action] -> [State -> State]}
 (contract-out
  [ai-strategy (-> (-> fighter? (listof pill?) action?) (-> state? state?))])

 #; {State Action -> State}
 execute)

(module+ examples
  (provide state0))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/fighter)
(require PillWars/Common/pills)
(require PillWars/Common/direction)
(require PillWars/Common/constants)
(require (prefix-in point: PillWars/Common/point))
(require PillWars/AI/strategy-1)
(require PillWars/Lib/image)

(require 2htdp/image)

(module+ examples
  (require (submod PillWars/Common/fighter examples))
  (require (submod PillWars/Common/pills examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod PillWars/Common/fighter examples))
  (require (submod PillWars/Common/pills examples))
  (require (submod PillWars/AI/strategy-1 examples))
  (require PillWars/Common/fighter)
  (require PillWars/Common/action)
  (require PillWars/World/constants)
  (require 2htdp/image)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct state [fighters pills] #:transparent)

(define (create-state name)
  (define my-fighter (create-fighter name))
  (define the-pills  (create-pills 20))
  (state (list my-fighter) the-pills))

(define (add-player-to-front its-name state0)
  (define my-fighter (create-fighter its-name))
  (state (cons my-fighter (state-fighters state0)) (state-pills state0)))

(define (state-my-fighter state0)
  (first (state-fighters state0)))

(define (state-my-fighter-update state0 next)
  (match-define [state fighters pills] state0)
  (state (cons next (rest fighters)) pills))

(define (swap-two-players state0)
  (match state0
    [[state (list f1 f2) pill*] (state (list f2 f1) pill*)]
    [_ (eprintf "warning -- no two players\n") state0]))

;; ---------------------------------------------------------------------------------------------------
(define ((draw-state BG) s)
  (match-define [state fighter* pill*] s) 
  (let* ([s BG]
         [s (add-objects s pill* add-pill)]
         [s (add-objects s fighter* add-fighter)])
    s))

(define ((draw-state-with-winners BG) s)
  (define scene0 [(draw-state BG) s])
  (define ranks  (winners s))
  (define texts  (map (λ (r) (text r 12 'purple)) ranks))
  (define 1text  (foldl (λ (next-r so-far) (above/join 5 so-far next-r)) empty-image texts))
  (place-image 1text (/ WIDTH 10) (/ HEIGHT 10) scene0))

;; ---------------------------------------------------------------------------------------------------
(define (mouse-click-on-pill? state0 x y)
  (match-define [state fighter* pill*] state0)
  (define posn (point:make-point x y))
  (find-pill posn pill*))

(define (state-mouse-click-ok? state x y)
  (define f (state-my-fighter state))
  (define p (fighter-posn f))
  (define q (point:make-point x y))
  (define θ (turn-angle p (fighter-velocity f) q))
  (and (<= (point:distance p q) RADAR) (<= (abs θ) MAX-RAD) θ))

#; {Point Direction Point -> Radian}
(define (turn-angle this-point this-dir other)
  (define direction-from-p-to-q (point:point->direction this-point other))
  (delta-angle direction-from-p-to-q this-dir))

;; ---------------------------------------------------------------------------------------------------
(define (rotate-my-fighter state0 rad)
  (define mine (state-my-fighter state0))
  (state-my-fighter-update state0 (rotate-fighter mine rad)))

(define (move-my-fighter state0)
  (define mine (state-my-fighter state0))
  (define next (move-fighter mine))
  (if (boolean? next)
      (state (rest (state-fighters state0)) (state-pills state0))
      (state-my-fighter-update state0 next)))

(define (eat-my-fighter state0 [pill0 #false])
  (match-define [state fighter* pill*] state0)
  (define f-0   (state-my-fighter state0))
  (define posn  (fighter-posn f-0))
  (define pill  (or pill0 (find-pill posn pill*)))
  (cond
    [(boolean? pill)
     (define f-1 (eat-fighter f-0 -1))
     (state-my-fighter-update state0 f-1)]
    [(red? pill) 
     (define f-1
       (let* ([s (eat-fighter f-0 (pill-score pill))]
              [s (accelerate-fighter s (red-acceleration pill))])
         s))
     (let* ([s state0]
            [s (state fighter* (remove pill pill*))]
            [s (state-my-fighter-update s f-1)])
       s)]
    [(blue? pill)
     (define f-1 (eat-fighter f-0 (pill-score pill)))
     (let* ([s state0]
            [s (state fighter* (remove pill pill*))]
            [s (state-my-fighter-update s f-1)])
       s)]))
     
#; {Point [Listof Pill] -> (U Pill #false)}
;; find a pill that is "close to" p, if any
(define (find-pill p pills0)
  (let find-pill ([pills pills0])
    (match pills 
      ['() #false]
      [(cons fst others)
       (if (<= (point:distance (pill-posn fst) p) RS) fst (find-pill others))])))

;; ---------------------------------------------------------------------------------------------------

(define (winners state0)
  (define players (state-fighters state0))
  (define sorted  (sort players > #:key fighter-score))
  (for/list ([f sorted])
    (~a (~a (fighter-name f) #:max-width (string-length "benjamin")) ": " (fighter-score f))))
                 

;; ---------------------------------------------------------------------------------------------------
(define (ai-strategy strategy)
  (define (apply-strategy state0)
    (match-define [state (cons f fighters) pills] state0)
    (define action (strategy f pills))
    (define next   (execute state0 action))
    next)
  apply-strategy)

(define (execute state0 action)
  (match action
    [(rot rad)  (rotate-my-fighter state0 rad)]
    [(mov _)    (move-my-fighter state0)]
    [(eat pill) (eat-my-fighter state0 pill)]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define state0 (state [list fighter0] [list blue0 red0])))

(module+ test
  ([draw-state-with-winners BG] state0)
  (check-true (image? ([draw-state BG] state0))))

(module+ test
  (define state1 (state (list fighter1) pill*0))
  (define state1-red  (state (list (eat-fighter fighter1 (pill-score red0))) (list)))

  (define state2 (state (list fighter5) pill*0))
  (define state2-red- (state (list (eat-fighter fighter5 -1)) pill*0))
  
  (define state4 (state (list fighter4) pill*0))
  (define state4-mov (state (list (move-fighter fighter4)) pill*0))

  (define state5 (state (list fighter5) pill*0))
  (define state5-max (state (list (rotate-fighter fighter5 MAX-RAD)) pill*0))

  (check-equal? (eat-my-fighter state2) state2-red-)

  (check-equal? (execute state1 (eat red0)) state1-red)
  (check-equal? (execute state4 (mov 'me)) state4-mov)
  (check-equal? (execute state5 (rot MAX-RAD)) state5-max))

(module+ test 
  (check-within (turn-angle 20+20i 10+0i (point:make-point 10 10)) (- (/ pi 4) pi) 0.1 "left turn")
  (check-within (turn-angle 20+20i 10+0i (point:make-point 10 20)) pi              0.1 "flip")
  (check-within (turn-angle 20+20i 10+0i (point:make-point 30 30)) (/ pi 4)        0.1 "right turn")
  (check-within (turn-angle 0+0i 10+0i   (point:make-point 10 10)) (/ pi 4)        0.1 "right turn")
  (check-within (turn-angle 0+0i 10+0i   (point:make-point 10 00)) 0.0             0.1 "no turn"))

(module+ test
  (check-false (state-mouse-click-ok? state0 10 10))
  (check-true (number? (state-mouse-click-ok? state0 50 100))))

;; ---------------------------------------------------------------------------------------------------
;; for running an AI strategy 

#; {State -> State}
(define (fighter-action-strategy-1 state0)
  (define mine   (state-my-fighter state0))
  (define pill*  (filter-map (λ (p) (and (red? p) p)) (state-pills state0)))
  (define action (strategy-1 mine pill*))
  (execute state0 action))

(module+ test
  (check-equal? (fighter-action-strategy-1 state1) state1-red))
