#lang racket

;; a data representation of the global, universal state; also used as local state for now

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type State = [state [Listof Fighter] [Listof Pill]] || the first fighter is mine}
 #;
 (struct-out state)
 draw-state
 state-fighters
 fighter-action-strategy-1)

(module+ examples
  (provide state0))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/fighter)
(require PillWars/Common/pills)
(require PillWars/Lib/image)

(module+ examples
  (require (submod PillWars/Common/fighter examples))
  (require (submod PillWars/Common/pills examples)))

(module+ test
  (require (submod ".." examples))
  (require (submod PillWars/Common/fighter examples))
  (require (submod PillWars/Common/pills examples))
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

#; {State -> Image}
(define ((draw-state BG) s)
  (match-define [state fighter* pill*] s) 
  (let* ([s BG]
         [s (add-objects s fighter* add-fighter)]
         [s (add-objects s pill* add-pill)])
    s))

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
(define (eat-my-fighter state0 pill)
  (match-define [state fighter* pill*] state0)
  (state fighter* (remove pill pill*)))

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