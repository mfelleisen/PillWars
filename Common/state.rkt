#lang racket

;; a data representation of the global, universal state; also used as local state for now

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type State = [state [Listof Fighter] [Listof Pill]] || the first fighter is mine}

 state-fighters
 state-pills
 
 state-mouse-click-ok?
 mouse-click-on-pill?
 
 rotate-my-fighter
 move-my-fighter
 eat-my-fighter
 
 draw-state
 fighter-action-strategy-1)

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

(define (state-my-fighter state0)
  (first (state-fighters state0)))

(define (state-my-fighter-update state0 next)
  (match-define [state fighters pills] state0)
  (state (cons next (rest fighters)) pills))

(define ((draw-state BG) s)
  (match-define [state fighter* pill*] s) 
  (let* ([s BG]
         [s (add-objects s fighter* add-fighter)]
         [s (add-objects s pill* add-pill)])
    s))

#; {State N N -> (U Radian #false)}
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

(module+ test 
  (check-within (turn-angle 20+20i 10+0i (point:make-point 10 10)) (- (/ pi 4) pi) 0.1 "left turn")
  (check-within (turn-angle 20+20i 10+0i (point:make-point 10 20)) pi              0.1 "flip")
  (check-within (turn-angle 20+20i 10+0i (point:make-point 30 30)) (/ pi 4)        0.1 "right turn")
  (check-within (turn-angle 0+0i 10+0i   (point:make-point 10 10)) (/ pi 4)        0.1 "right turn")
  (check-within (turn-angle 0+0i 10+0i   (point:make-point 10 00)) 0.0             0.1 "no turn"))

(module+ test
  (check-false (state-mouse-click-ok? state0 10 10))
  (check-true (number? (state-mouse-click-ok? state0 50 100))))

#; {State N N -> (U Pill #false)}
(define (mouse-click-on-pill? state0 x y)
  (match-define [state fighter* pill*] state0)
  (define posn (point:make-point x y))
  (find-pill posn pill*))

#; {State Action -> State}
(define (execute state0 action)
  (match action
    [(rot rad)  (rotate-my-fighter state0 rad)]
    [(mov _)    (move-my-fighter state0)]
    [(eat pill) (eat-my-fighter state0 pill)]))

#; {State Radian -> State}
(define (rotate-my-fighter state0 rad)
  (define mine (state-my-fighter state0))
  (state-my-fighter-update state0 (rotate-fighter mine rad)))

#; {State -> State}
(define (move-my-fighter state0)
  (define mine (state-my-fighter state0))
  (state-my-fighter-update state0 (move-fighter mine)))

#; {State Pill -> State}
(define (eat-my-fighter state0 [pill0 #false])
  (match-define [state fighter* pill*] state0)
  (define posn  (fighter-posn (state-my-fighter state0)))
  (define pill  (or pill0 (find-pill posn pill*)))
  (state fighter* (remove pill pill*)))

#; {Point [Listof Pill] -> (U Pill #false)}
;; find a pill that is "close to" p, if any
(define (find-pill p pills0)
  (let find-pill ([pills pills0])
    (match pills 
      ['() #false]
      [(cons fst others)
       (if (<= (point:distance (pill-posn fst) p) RS) fst (find-pill others))])))

;; ---------------------------------------------------------------------------------------------------
#; {State -> State}
(define (fighter-action-strategy-1 state0)
  (define mine   (state-my-fighter state0))
  (define pill*  (filter-map (λ (p) (and (red? p) p)) (state-pills state0)))
  (define action (strategy-1 mine pill*))
  (execute state0 action))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define state0 (state [list fighter0] [list blue0 red0])))

(module+ test
  (check-true (image? ([draw-state BG] state0))))

(module+ test
  (define state1 (state (list fighter1) pill*0))
  (define state1-red (state (list fighter1) (list)))

  (define state4 (state (list fighter4) pill*0))
  (define state4-mov (state (list (move-fighter fighter4)) pill*0))

  (define state5 (state (list fighter5) pill*0))
  (define state5-max (state (list (rotate-fighter fighter5 MAX-RAD)) pill*0))

  (check-equal? (execute state1 (eat red0)) state1-red)
  (check-equal? (execute state4 (mov 'me)) state4-mov)
  (check-equal? (execute state5 (rot MAX-RAD)) state5-max))

(module+ test
  (check-equal? (fighter-action-strategy-1 state1) state1-red))