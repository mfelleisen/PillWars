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
(require PillWars/Common/point)
(require PillWars/Common/pills)

(module+ examples
  (require (submod PillWars/Common/pills examples))
  (require (submod PillWars/Common/fighter examples)))

(module+ test
  (require (submod ".." examples))
  (require rackunit))

;; -------------------------------------------------------------------------------------------------
;; STRATEGY: search for action that brings `this` fighter closer to some selected pill
;; 1. if it sits on a pill, _eat_ it. 
;; 2. if any of the pills is reachable in the given direction, _move_ forward.
;; 3. otherwise, change direction counter-clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; TODO 4. still not? change direction clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; 5. else: return default action. 

(define (strategy-1 mine pill*)
  (cond
    [(on-any-pill mine pill*)   => eat]
    [(can-reach mine pill*)     => mov]
    [(rotate->reach mine pill*) => rot]
    [else                       (rot MAX-RAD)]))

#; {Fighter [Listof Pill] -> (U #false Radian)}
;; determine whether any rotation between STEP-RAD and MAX-RAD gets `this` to any pill, if any
(define (rotate->reach this pill* [rad STEP-RAD])
  (let rotate->reach ([rad STEP-RAD])
    (define try (rotate-fighter this rad))
    (cond
      [(can-reach try pill*) rad]
      [(> rad MAX-RAD)       #false]
      [else (rotate->reach (+ rad STEP-RAD))])))

#; {Fighter [Listof Pill] -> (U #false Pill)}
;; determine the first pill in `pill*` that `this` can reach, if any 
(define (can-reach this pill*)
  (let can-reach ([this this])
    (cond
      [(on-any-pill this pill*) => identity]
      [(f-outside? this)        #false]
      [else (can-reach (move-fighter this))])))

(define (f-outside? this)
  (outside? (fighter-posn this)))

#; {Fighter [Listof Pill] -> (U Pill #false)}
;; is the fighter sitting on any pill? 
;; `pill*` is the list of center points for the targeted pills
(define (on-any-pill this pill*)
  (define mine (fighter-posn this))
  (for/first ([p pill*] #:when (on-pill? p mine)) p))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define pill*0 (list red0))
  (define fighter4 (rotate-fighter fighter2 (rotate->reach fighter2 pill*0)))
  (define fighter3 (move-fighter fighter4 (+ steps-2 3))))

(module+ test
  (check-true  (number? (rotate->reach fighter2 pill*0)))
  #;
  (check-false (rotate->reach fighter5 pill*0)))

(module+ test
  (check-true (pill? (on-any-pill fighter1 pill*0)))
  (check-true (pill? (on-any-pill fighter3 pill*0)))
  (check-false (on-any-pill fighter0 pill*0)))

(module+ test
  (check-true (pill? (can-reach fighter1 pill*0)))
  (check-true (pill? (can-reach fighter4  pill*0)))
  (check-false (can-reach fighter0 pill*0)))

(module+ test
  (check-equal? (strategy-1 fighter1 pill*0) (eat red0))
  (check-equal? (strategy-1 fighter4 pill*0) (mov red0))
  (check-true (rot? (strategy-1 fighter5 pill*0)))
  (check-true (rot? (strategy-1 fighter5 pill*0))))
