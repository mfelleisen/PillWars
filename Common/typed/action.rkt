#lang typed/racket

;; the actions a player can take during a turn 

(provide
 Action
 YourTurn
 
 (struct-out your-turn)

 #; {type Action = [rotate Deg] || [eat Pill] || [mov Pill]}
 action?
 
 (struct-out mov)
 (struct-out eat)
 (struct-out rot)
 (struct-out gup))

;; -----------------------------------------------------------------------------
;; should be imported 
(define-type State Any)

(require PillWars/Common/typed/point)

;; -----------------------------------------------------------------------------
;; universe --> world messages: 
(struct your-turn [{state : State}] #:prefab #:type-name YourTurn)

;; world --> universe messages:
(define-type Action (U mov eat rot gup))

(struct mov [{any : Any}] #:prefab)   ;; move according to current speed 
(struct eat [{posn : Point}] #:prefab)  ;; eat the pill at/near this location 
(struct rot [{angle : Real}] #:prefab) ;; rotate by this angle 
(struct gup [{why : String}] #:prefab)   ;; giving up (why)

(: action? (-> Any Boolean))
(define (action? a)
  (or (mov? a) (eat? a) (rot? a) (gup? a)))
