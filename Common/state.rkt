#lang racket

;; a data representation of the global, universal state; also used as local state for now

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type State = [state [Listof Fighter] [Listof Pill]] || the first fighter is mine}

 #; {-> State}
 plain-state
 
 #; {String -> State}
 ;; a random state 
 create-state

 #; {-> State}
 empty-state

 #; {(U String Symbol Fighter) State -> State}
 ;; make a player with the give name and add it to the front of the player list of `state`
 add-fighter-to-front

 #; {State -> State}
 ;; add 1 pill to a state with exactly 1 fighter 
 add-pill-at-fighter

 state-my-fighter 
 state-fighters
 state-pills
 
 #; {State Fighter -> State}
 ;; re-organize the state so that the given fighter shows up as the first one
 ;; (for sending states to clients)
 fighter-first

 #; {State -> State}
 next-turn
 
 #; {State -> State}
 ;; swap the first two (and should be only) players
 swap-two-players

 #; {Scene -> State -> Scene}
 draw-state
 draw-state-with-winners 

 #; {State N N -> (U Pill #false)}
 state-mouse-click-turn?

 #; {State N N -> (U Radian #false)}
 mouse-click-on-pill?

 #; {State Radian -> State}
 rotate-my-fighter

 #; {State -> State}
 ;; move fighter unconditionally but elimimate it if it steps out of bounds 
 move-my-fighter

 #; {State Pill -> State}
 ;; 
 eat-my-fighter

 #; {State N -> State}
 ;; remove the `n`th the fighter 
 remove-fighter

 #; {State -> [Listof String]}
 winners
 
 #; {[Fighter [Listof Pill] -> Action] -> [State -> State]}
 (contract-out
  [ai-strategy        (-> (-> fighter? (listof pill?) action?) (-> state? state?))]
  [ai-action-strategy (-> (-> fighter? (listof pill?) action?) state? action?)])

 #; {State Action -> State}
 (contract-out
  [execute (-> state? action? state?)]))

(module+ examples
  (provide state0 state0--))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/fighter)
(require PillWars/Common/pills)
(require PillWars/Common/direction)
(require PillWars/Common/constants)
(require (prefix-in point: PillWars/Common/point))
(require PillWars/AI/strategy-1)
(require PillWars/Lib/image)
(require PillWars/Lib/list)

(require 2htdp/image)

(module+ examples
  (require (submod PillWars/Common/fighter examples))
  (require (submod PillWars/Common/pills examples))
  (require (submod PillWars/AI/strategy-1 examples)))

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
(struct state [fighters pills] #:prefab)

(define (empty-state)
  (state '() '[]))

(define (plain-state)
  (state '() (create-pills 20)))

(define (create-state name)
  (define my-fighter (create-fighter name))
  (define the-pills  (create-pills 20))
  (state (list my-fighter) the-pills))

(define (add-fighter-to-front its-name-or-a-fighter state0)
  (cond
    [(or (symbol? its-name-or-a-fighter) (string? its-name-or-a-fighter))
     (define my-fighter (create-fighter its-name-or-a-fighter))
     (state (cons my-fighter (state-fighters state0)) (state-pills state0))]
    [else
     (state (cons its-name-or-a-fighter (state-fighters state0)) (state-pills state0))]))

(define (add-pill-at-fighter state0)
  (match-define [state (list f) '()] state0)
  (define p (fighter-posn f))
  (state (list f) (list (pill-at p))))

(define (state-my-fighter state0)
  (first (state-fighters state0)))

(define (state-my-fighter-update state0 next)
  (match-define [state fighters pills] state0)
  (state (cons next (rest fighters)) pills))

#; {State Fighter Pill -> State}
(define (state-update state0 f-1 pill)
  (match-define [state fighter* pill*] state0)
  (let* ([s state0]
         [s (state fighter* (remove pill pill*))]
         [s (state-my-fighter-update s f-1)])
    s))

(define (flush-pills s)
  (state (state-fighters s) '[]))

(define (swap-two-players state0)
  (match state0
    [[state (list f1 f2) pill*] (state (list f2 f1) pill*)]
    [_ (eprintf "warning -- no two players\n") state0]))

;; ---------------------------------------------------------------------------------------------------
(define (fighter-first s fighter)
  (match-define [state fighter* pill*] s)
  (state (cons fighter (remove fighter fighter*)) pill*))

(define (next-turn s)
  (match-define [state fighter* pill*] s) 
  (state (list-rotate fighter*) pill*))

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
  (define fi-posn (fighter-posn (state-my-fighter state0)))
  (define mc-posn (point:make-point x y))
  (define is-pill (find-pill mc-posn (state-pills state0)))
  (if (not is-pill) #false (and (on-pill? is-pill fi-posn) is-pill)))

  
(define (state-mouse-click-turn? state x y)
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

;; ---------------------------------------------------------------------------------------------------
(define (move-my-fighter state0)
  (define mine (state-my-fighter state0))
  (define next (move-fighter mine))
  (if (boolean? next)
      (state (rest (state-fighters state0)) (state-pills state0))
      (state-my-fighter-update state0 next)))

;; ---------------------------------------------------------------------------------------------------
(define (eat-my-fighter state0 [pill0 #false])
  (match-define [state fighter* pill*] state0)
  (define f-0   (state-my-fighter state0))
  (define pill  (or (and (pill? pill0) pill0) (find-pill (fighter-posn f-0) pill*)))
  (cond
    [(boolean? pill)
     (define f-1 (eat-fighter f-0 -1))
     (state-my-fighter-update state0 f-1)]
    [(blue? pill)
     (define f-1 (eat-fighter f-0 (pill-score pill)))
     (state-update state0 f-1 pill)]
    [(red? pill) 
     (let* ([f-1 (eat-fighter f-0 (pill-score pill))]
            [f-1 (accelerate-fighter f-1 (red-acceleration pill))])
       (state-update state0 f-1 pill))]))

#; {Point [Listof Pill] -> (U Pill #false)}
;; find a pill that is "close to" p, if any
(define (find-pill p pills0)
  (let find-pill ([pills pills0])
    (match pills 
      ['() #false]
      [(cons fst others)
       (if (<= (point:distance (pill-posn fst) p) RS) fst (find-pill others))])))

;; ---------------------------------------------------------------------------------------------------
(define (remove-fighter s i)
  (match-define [state fighter* pill*] s)
  (state (remove-ref fighter* i) pill*))

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

(define (ai-action-strategy strategy state0)
  (match-define [state (cons f fighters) pills] state0)
  (strategy f pills))

(define (execute state0 action)
  (match action
    [(rot rad)  (rotate-my-fighter state0 rad)]
    [(mov _)    (move-my-fighter state0)]
    [(eat pill) (eat-my-fighter state0 pill)]
    [(gup msg)  (flush-pills state0)]))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define state0 (state [list fighter0] pill*1))
  (define state0-- (state [list] pill*1)))

(module+ test ;; drawing states 
  ([draw-state-with-winners BG] state0)
  [(draw-state BG) (state (list fighter-stuck) pills-stuck)]
  (check-true (image? ([draw-state BG] state0))))

(module+ test ;; eat-my-fighter 
  (define fighter5+ (for/fold ([f fighter5]) ([i 10])(move-fighter f)))
  (define state2 (state (list fighter5+) pill*0))
  (define state2-red- (state (list (eat-fighter fighter5+ -1)) pill*0))
  (check-equal? (eat-my-fighter state2) state2-red- "eat pill that the fighter supposedly sits on")

  (define state0-blue (state [list (eat-fighter fighter0 (pill-score blue0))] [list red0]))
  (check-equal? (eat-my-fighter state0 blue0) state0-blue "eat a specified pill"))

(module+ test ;; turn-angle 
  (check-within (turn-angle 20+20i 10+0i (point:make-point 10 10)) (- (/ pi 4) pi) 0.1 "left turn")
  (check-within (turn-angle 20+20i 10+0i (point:make-point 10 20)) pi              0.1 "flip")
  (check-within (turn-angle 20+20i 10+0i (point:make-point 30 30)) (/ pi 4)        0.1 "right turn")
  (check-within (turn-angle 0+0i 10+0i   (point:make-point 10 10)) (/ pi 4)        0.1 "right turn")
  (check-within (turn-angle 0+0i 10+0i   (point:make-point 10 00)) 0.0             0.1 "no turn"))

(module+ test ;; state-mouse-click-ok?
  (check-false (state-mouse-click-turn? state0 10 10))
  (check-true (number? (state-mouse-click-turn? state0 50 100))))

(module+ test ;; fighter-first 
  (define state8 (state (list fighter0 fighter1) '()))
  (define state8++ (state (list fighter1 fighter0) '()))
  
  (check-equal? (fighter-first state8 fighter0) state8)
  (check-equal? (fighter-first state8 fighter1) state8++)
  (check-equal? (fighter-first state0 fighter0) state0))

(module+ test ;; preparing the state for the next turn (2 fighters, arbitrary fighters)
  (check-equal? (swap-two-players state0) state0)
  (check-equal? (swap-two-players state8) state8++)
  (check-equal? (next-turn state0) state0))

(module+ test ;; remove-fighter
  (check-equal? (remove-fighter state0 0) state0--))

;; ---------------------------------------------------------------------------------------------------
;; for running an AI strategy

#; {State -> State}
(define (fighter-action-strategy-1 state0)
  (define mine   (state-my-fighter state0))
  (define pill*  (filter-map (λ (p) (and (red? p) p)) (state-pills state0)))
  (define action (strategy-1 mine pill*))
  (execute state0 action))

(module+ test ;; local exampes for `execute` and running an action-producing strategy
  (define state1 (state (list fighter1) pill*0))
  (define state1-red  (state (list (eat-fighter fighter1 (pill-score red0))) (list))))

(module+ test ;; execute a request 
  (check-equal? (execute state1 (eat red0)) state1-red)

  (define state4 (state (list fighter4) pill*0))
  (define state4-mov (state (list (move-fighter fighter4)) pill*0))
  (check-equal? (execute state4 (mov 'me)) state4-mov)

  (define state5 (state (list fighter5) pill*0))
  (define state5-max (state (list (rotate-fighter fighter5 MAX-RAD)) pill*0))
  (check-equal? (execute state5 (rot MAX-RAD)) state5-max))

(module+ test ;; run a AI fighter's strategy to produce an action 
  (check-equal? (fighter-action-strategy-1 state1) state1-red))
