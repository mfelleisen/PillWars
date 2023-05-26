#lang racket

;; a first strategy

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {Fighter [Listof Pill] -> Action}
 strategy-1)

(module+ examples
  (provide
   red0
   pill*0
   pill*1
   
   fighter0
   fighter1
   fighter2
   fighter3
   fighter4
   fighter5
   fighter6))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/action)
(require PillWars/Common/constants)
(require PillWars/Common/fighter)
(require PillWars/Common/direction)
(require PillWars/Common/point)
(require PillWars/Common/pills)

(module+ examples
  (require (submod PillWars/Common/pills examples))
  (require (submod PillWars/Common/fighter examples)))

(module+ test
  (require (submod PillWars/Common/pills examples))
  (require (submod PillWars/Common/fighter examples))
  (require (submod ".." examples))
  (require rackunit))

;; -------------------------------------------------------------------------------------------------
(define STEP-RAD (/ MAX-RAD 30))
(define LEFT     MAX-RAD)
(define RIGHT    (- MAX-RAD))

;; STRATEGY: search for action that brings `this` fighter closer to some selected pill
;; 1. if it sits on a pill, _eat_ it. 
;; 2. if any of the pills is reachable in the given direction, _move_ forward.
;; 3. otherwise, change direction counter-clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; TODO 4. still not? change direction clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; 5. else: return default action.

(define (strategy-1 mine pill*)
  (cond
    [(empty? pill*)                   (gup "no pills")]
    [(on-any-pill mine pill*)         => eat]
    [(can-reach mine pill*)           => mov]
    [(rotate->reach mine pill* LEFT)  => rot]
    [(rotate->reach mine pill* RIGHT) => rot]

    ;; TODO:
    ;; rotate by 360deg.
    ;; If no reachable pill can be found, `(gup "no more pills reachable from here")`
    ;; Otherwise, try random or try moving in this direction? 

    [else
     (if (or (rotate->reach mine pill* #;end pi) (rotate->reach mine pill* #;end (- pi)))
         (rot MAX-RAD)
         (gup "no pill reachable"))]))

(define (random-degree)
  (let* ([s MAX-RAD]
         [s (rad->deg s)]
         [s (round s)]
         [s (inexact->exact s)]
         [s (random s)])
    (deg->rad s)))

#; {Fighter [Listof Pill] Radian -> (U #false Radian)}
;; can any rotation between `(* left-or-right STEP-RAD)` and `end` move `this` to any pill?
(define (rotate->reach this pill* end)
  (define delta (* (sign end) STEP-RAD))
  (let rotate->reach ([rad delta])
    (define try (rotate-fighter this rad))
    (cond
      [(can-reach try pill*)  rad]
      [(> (abs rad) (abs end)) #false]
      [else (rotate->reach (+ rad delta))])))

#; {Real -> {+1, -1}}
(define (sign x)
  (cond
    [(> x 0) +1]
    [(< x 0) -1]
    [else (error 'sign "bad argument: ~a" x)]))

#; {Fighter [Listof Pill] -> (U #false Pill)}
;; determine the first pill in `pill*` that `this` can reach, if any 
(define (can-reach this pill*)
  (let can-reach ([this this])
    (cond
      [(boolean? this)          #false]
      [(on-any-pill this pill*) => identity]
      [else (can-reach (move-fighter this))])))

#; {Fighter [Listof Pill] -> (U Pill #false)}
;; is the fighter sitting on any pill? 
;; `pill*` is the list of center points for the targeted pills
(define (on-any-pill this pill*)
  (define mine (fighter-posn this))
  (for/first ([p pill*] #:when (on-pill? p mine)) p))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define pill*0 (list red0))
  (define pill*1 [list red0 blue0])
  (define fighter4 (rotate-fighter fighter2 (rotate->reach fighter2 pill*0 MAX-RAD)))
  (define fighter3 (move-fighter fighter4 (+ steps-2 3))))

(module+ test ;; rotate first, then reachable 
  (check-true  (number? (rotate->reach fighter2 pill*0 MAX-RAD)))
  (check-equal? (rotate->reach fighter5 pill*0 MAX-RAD) STEP-RAD))

(module+ test ;; on-any-pill in the list 
  (check-equal? (on-any-pill fighter1 pill*0) (first pill*0))
  (check-false (on-any-pill fighter0 pill*0)))

(module+ test ;; can-reach?
  (check-equal? (can-reach fighter1 pill*0) (first pill*0))
  (check-false (can-reach fighter0 pill*0)))

#;
(module+ test ;; strategy-1
  (check-equal? (strategy-1 fighter1 pill*0) (eat red0))
  (check-equal? (strategy-1 fighter4 pill*0) (eat red0))
  (check-true (eat? (strategy-1 fighter5 pill*0))))

(module+ test ;; strategy-1

  (define fighter0+ (for/fold ([s fighter0]) ([i 60]) (rotate-fighter s (/ pi 60))))
  (check-true (rot? (strategy-1 fighter0+ pill*1)) "right turn")
  (check-true (gup? (strategy-1 fighter-stuck '())) "random rotating at the moment"))
