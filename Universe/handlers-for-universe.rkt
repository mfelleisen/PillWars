#lang racket

;; the handlers for the universe 

#| The Execution Stages

stage 1: universe collects players and allocates fighters
         parallel data structure for worlds and fihters
  stop-when: the universe has enough players signed up 

stage 2: the player order proceeds according to ascending order of (sign-up) age
         broadcast state to all players;
          state has to be rendered from the perspective of the player
          (player's fighter first, maintain order otherwise)
         send turn messages to player whose turn it is, with state 
         wait for action response
         execute action on state (check legality?)
         rotate players
         repeat
  stop-when: game is over, send end message????
|#

(provide
 #; {type UState =
          [ustate [Listof IWorld] State]                  ;; stage 1: during start-up phase 
          ||
          (list IWorld [ustate [Listof IWorld] State]) }  ;; stage 2: when first IWorld has turn
 #; {-> UState} 
 create-ustate

 #; {UState IWorld -> UState}
 remove-player

 #; {N -> UState IWorld -> (U UState Bundle)}
 ;; returns a bundle that starts a turn if enough players are signed up; it always adds the new player
 add-player

 #; {UState IWorld S-expression -> Bundle}
 ;; ends a turn, rotates the player order, and starts a new turn
 ;; FEATURE this handler could stop the game too; worlds are supposed to drop out at the end 
 end-turn)

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/fighter)
(require PillWars/Common/state)
(require PillWars/Common/action)
(require PillWars/Lib/list)
(require 2htdp/universe)

(module+ test
  (require (submod PillWars/Common/state examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct ustate [worlds state] #:transparent)

(define (create-ustate)
  (ustate '() (plain-state)))

;; ---------------------------------------------------------------------------------------------------
(define (remove-player us0 iw)
  (match us0
    [(ustate worlds state) (remove-player-aux us0 iw)]
    [(list (? iworld? iw-with-turn) (? ustate? us-plain))
     (cond
       [(not (equal? iw-with-turn iw))
        (list iw-with-turn (remove-player-aux us-plain iw))]
       [else
        (define us+ (remove-player-aux us-plain iw))
        (if (empty? (state-fighters (ustate-state us+)))
            (create-ustate)
            (start-turn us+))])]))

#; {UState IWorld -> UState}
(define (remove-player-aux us0 iw)
  (match-define [ustate worlds s0] us0)
  (define i (index-of worlds iw))
  (ustate (remove iw worlds) (remove-fighter s0 i)))

;; ---------------------------------------------------------------------------------------------------
(define (end-turn us0 iw msg)
  (match us0
    [(ustate _1 _2) us0]
    [(list (? iworld? iw-with-turn) (? ustate? us))
     (cond
       [(not (equal? iw-with-turn iw)) us0] ;; could eliminate for responding out of order
       [(act-on-request us msg) => start-turn]
       [else (create-ustate)])]))
             
#; {UState S-expression -> (U #false UState)}
(define (act-on-request us action)
  (match-define [ustate worlds state] us)
  (define state+ (execute state action))
  (if (empty? (state-fighters state+))
      #false
      (ustate (list-rotate worlds) (next-turn state+))))
  
;; ---------------------------------------------------------------------------------------------------
;; add a fighter for the new world until there are `n` fighters 
(define ((add-player n) us iw)
  (match us
    [(list w x) us]
    [[ustate worlds state]
     (define new-fighter (create-fighter (iworld-name iw)))
     (define new-state   (add-fighter-to-front new-fighter state))
     (define new-ustate  [ustate (cons iw worlds) new-state])
     (if (= (length (state-fighters new-state)) n) (start-turn new-ustate) new-ustate)]))

;; ---------------------------------------------------------------------------------------------------
#; {UState -> Bundle}
(define (start-turn us)
  (match-define [ustate worlds state] us)
  (define fighters (state-fighters state))
  (define mail-for-first (make-mail (first worlds) (your-turn state)))
  (define mails-for-rest (map (reorder-state state) (rest worlds) (rest fighters)))
  (make-bundle (list (first worlds) us) (cons mail-for-first mails-for-rest) '[]))

#; {State -> IWorld Fighter -> [Listof Mail]}
(define ((reorder-state state) iw fighter)
  [make-mail iw (fighter-first state fighter)])

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; create-ustate 
  (check-true (cons? (state-pills (ustate-state (create-ustate))))))

(module+ test ;; add-player 
  (define +player-2 (add-player 2))
  (define state1  (add-fighter-to-front "ok" state0))
  (define ustate1 (list iworld1 (ustate (list iworld1 iworld3) state1)))
  
  (check-true (ustate? (+player-2 (ustate '() (plain-state)) iworld3)) "add first player")
  (check-true (bundle? (+player-2 (ustate (list iworld3) state0) iworld1)) "reach limit")
  (check-equal? (+player-2 ustate1 iworld3) ustate1 "drop new player when game is running"))

(module+ test ;; remove-player 
  (define ustate1-- (list iworld1 (ustate (list iworld1) (remove-fighter state1 1))))
  (check-equal? (remove-player ustate1 iworld3) ustate1-- "remove player drops world during sign-up")

  (define ustate1++ (ustate (list iworld1) (remove-fighter state1 1)))
  (check-equal? (remove-player (second ustate1) iworld3) ustate1++ "remove player drops non-actor")

  (define state1** (remove-fighter state1 0))
  (define ustate1** (list iworld3 (ustate (list iworld3) state1**)))
  (define bundle (make-bundle ustate1** (list (make-mail iworld3 (your-turn state1**))) '[]))
  (check-equal? (remove-player ustate1 iworld1) bundle) "remove player starts new turn")

(module+ test ;; end-turn with act-on-request and start-turn (implied)
  (let* ([action  (mov 'silly)]
         [state2  (next-turn (execute state1 action))]
         [ustate2 (ustate (list iworld3 iworld1) state2)])
    
    (check-equal? (act-on-request (second ustate1) action) ustate2 "next state after player responds")
    
    (let* ([mail->1 [(reorder-state state2) iworld1 (second (state-fighters state2))]]
           [mail->3 (make-mail iworld3 (your-turn state2))]
           [bundle  (make-bundle (list iworld3 ustate2) (list mail->3 mail->1) '[])])
      
      (check-equal? (end-turn ustate1 iworld1 action) bundle "starting the next turn")

      (let* ([us (ustate (list iworld1 iworld3) state1)])
        (check-equal? (end-turn ustate1 iworld2 action) ustate1 "wrong world sending an action")

        (check-equal? (end-turn us iworld1 action) us "game not started yet")))))