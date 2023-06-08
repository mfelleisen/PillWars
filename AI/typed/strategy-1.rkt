#lang typed/racket

;; a first strategy

;; ---------------------------------------------------------------------------------------------------
(provide
 Strategy

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
(require PillWars/Common/typed/action)
(require PillWars/Common/typed/constants)
(require PillWars/Common/typed/fighter)
(require PillWars/Common/typed/direction)
(require PillWars/Common/typed/point)
(require PillWars/Common/typed/pills)

(module+ examples
  (require (submod PillWars/Common/typed/pills examples))
  (require (submod PillWars/Common/typed/fighter examples)))

(module+ test
  (require (submod PillWars/Common/typed/pills examples))
  (require (submod PillWars/Common/typed/fighter examples))
  (require (submod ".." examples))
  (require typed/rackunit))

;; -------------------------------------------------------------------------------------------------
(define-type Strategy {Fighter [Listof Pill] -> Action})

(define STEP-RAD (/ MAX-RAD 30))
(define LEFT     MAX-RAD)
(define RIGHT    (- MAX-RAD))

;; STRATEGY: search for action that brings `this` fighter closer to some selected pill
;; 1. if it sits on a pill, _eat_ it. 
;; 2. if any of the pills is reachable in the given direction, _move_ forward.
;; 3. otherwise, change direction counter-clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; TODO 4. still not? change direction clockwise by STEP-RAD until a pill is reachable. _Rotate_.
;; 5. else: return default action.

(: strategy-1 Strategy)
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

(: random-degree (-> Radian))
(define (random-degree)
  (let* ([s MAX-RAD]
         [s (rad->deg s)]
         [s (round s)]
         [s (inexact->exact s)]
         [s (random (cast s Integer))])
    (deg->rad s)))

(: rotate->reach {Fighter [Listof Pill] Radian -> (U False Radian)})
;; can any rotation between `(* left-or-right STEP-RAD)` and `end` move `this` to any pill?
(define (rotate->reach this pill* end)
  (define delta (* (sign end) STEP-RAD))
  (let rotate->reach ([rad delta])
    (define try (rotate-fighter this rad))
    (cond
      [(can-reach try pill*)  rad]
      [(> (abs rad) (abs end)) #false]
      [else (rotate->reach (+ rad delta))])))

(: sign {Real -> {U +1 -1}})
(define (sign x)
  (cond
    [(> x 0) +1]
    [(< x 0) -1]
    [else (error 'sign "bad argument: ~a" x)]))

(: can-reach {Fighter [Listof Pill] -> (U False Point)})
;; determine the first pill in `pill*` that `this` can reach, if any 
(define (can-reach this pill*)
  (let can-reach ([this : (U False Fighter) this])
    (cond
      [(boolean? this)          #false]
      [(on-any-pill this pill*) => identity]
      [else (can-reach (move-fighter this))])))

(: on-any-pill (Fighter [Listof Pill] -> (U False Point)))
;; is the fighter sitting on any pill? 
;; `pill*` is the list of center points for the targeted pills
(define (on-any-pill this pill*)
  (define mine (fighter-posn this))
  (let some-first ([pill* pill*])
    (cond
      [(empty? pill*) #false]
      [else
       (define p (first pill*))
       (or (and (on-pill? p mine) (pill-posn p)) (some-first (rest pill*)))]))
  ;; rewrite to accommodate the type checker 
  #;
  (for/first : [U False Pill] ([p : Pill pill*] #:when (on-pill? p mine)) : [U False Pill] p))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define pill*0 (list red0))
  (define pill*1 [list red0 blue0])
  (define fighter4 (rotate-fighter fighter2 (cast (rotate->reach fighter2 pill*0 MAX-RAD) Degree)))
  (define fighter3 (move-fighter fighter4 (+ steps-2 3))))

(module+ test ;; rotate first, then reachable 
  (check-true  (number? (rotate->reach fighter2 pill*0 MAX-RAD)))
  (check-equal? (rotate->reach fighter5 pill*0 MAX-RAD) STEP-RAD))

(module+ test ;; on-any-pill in the list 
  (check-equal? (on-any-pill fighter1 pill*0) (pill-posn (first pill*0)))
  (check-false (on-any-pill fighter0 pill*0)))

(module+ test ;; can-reach?
  (check-equal? (can-reach fighter1 pill*0) (pill-posn (first pill*0)))
  (check-false (can-reach fighter0 pill*0)))

#;
(module+ test ;; strategy-1
  (check-equal? (strategy-1 fighter1 pill*0) (eat red0))
  (check-equal? (strategy-1 fighter4 pill*0) (eat red0))
  (check-true (eat? (strategy-1 fighter5 pill*0))))

(module+ test ;; strategy-1
  (define 30d (/ pi 60))

  (define fighter0+ (for/fold : Fighter ([s : Fighter fighter0]) ([i 60]) (rotate-fighter s 30d)))
  (check-true (rot? (strategy-1 fighter0+ pill*1)) "right turn")
  (check-true (gup? (strategy-1 fighter-stuck '())) "random rotating at the moment"))
