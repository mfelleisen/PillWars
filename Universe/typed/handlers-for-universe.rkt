#lang typed/racket

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
(require PillWars/Common/typed/fighter)
(require PillWars/Common/typed/state)
(require PillWars/Common/typed/action)
(require PillWars/Lib/typed/list)
(require/typed 2htdp/universe
               [#:opaque IWorld iworld?]
               [#:opaque Mail   mail?]
               [#:opaque Bundle bundle?]
               [iworld-name (-> IWorld String)]
               [make-mail   (-> IWorld (U YourTurn State) Mail)]
               [make-bundle (-> UState [Listof Mail] [Listof IWorld] Bundle)]
               [iworld1     IWorld]
               [iworld2     IWorld]
               [iworld3     IWorld])

(module+ test
  (require (submod PillWars/Common/typed/state examples))
  (require typed/2htdp/image)
  (require (except-in typed/rackunit check-equal?))
  (require/typed rackunit
                 [check-equal? (∀ (X) (->* (X X) (String) Void))]))

;; ---------------------------------------------------------------------------------------------------
(struct ustate [{worlds : [Listof IWorld]} {state : State}] #:transparent #:type-name US)
(define-type UState (U [List IWorld US] US))

(: create-ustate (-> US))
(define (create-ustate)
  (ustate '() (plain-state)))

;; ---------------------------------------------------------------------------------------------------
(: remove-player (-> UState IWorld (U Bundle UState)))
(define (remove-player us0 iw)
  (match us0
    [(? ustate?) (remove-player-aux us0 iw)]
    [(list (? iworld? iw-with-turn) (? ustate? us-plain))
     (cond
       [(not (equal? iw-with-turn iw))
        (list iw-with-turn (remove-player-aux us-plain iw))]
       [else
        (define us+ (remove-player-aux us-plain iw))
        (if (empty? (state-fighters (ustate-state us+)))
            (create-ustate)
            (start-turn us+))])]))

(: remove-player-aux {UState IWorld -> US})
(define (remove-player-aux us0 iw)
  (match-define [ustate worlds s0] us0)
  (define i (index-of worlds iw))
  (define player-name (iworld-name iw))
  (eprintf "~a is done\n" player-name)
  (define remaining-players (remove iw worlds))
  (when (empty? remaining-players) (eprintf "--- game over ---\n"))
  ;; the world's state may have already been removed 
  (if (boolean? i) us0 (ustate remaining-players (remove-fighter s0 i))))

;; ---------------------------------------------------------------------------------------------------
(: end-turn (-> UState IWorld Action (U Bundle UState)))
(define (end-turn us0 iw msg)
  (match us0
    [(ustate _1 _2) us0]
    [(list (? iworld? iw-with-turn) (? ustate? us))
     (cond
       [(not (equal? iw-with-turn iw)) us0] ;; could eliminate for responding out of order
       [(act-on-request us msg) => start-turn]
       [else (create-ustate)])]))
             
(: act-on-request {UState Action -> (U #false US)})
(define (act-on-request us action)
  (match-define [ustate worlds state] us)
  (define state+ (execute state action))
  (cond
    [(empty? (state-fighters state+)) #false]
    [(= (length (state-fighters state)) (length (state-fighters state+)))
     (ustate (list-rotate worlds) (next-turn state+))]
    [else ;; meaning, the player whose turn it was requested an "illegal" action (out of bounds) 
     (ustate (rest worlds) state+)]))
  
;; ---------------------------------------------------------------------------------------------------
(: add-player (-> Natural (-> UState IWorld (U Bundle UState))))
;; add a fighter for the new world until there are `n` fighters 
(define ((add-player n) us iw)
  (match us
    [(list w x) us]
    [[ustate worlds state]
     (define player-name (iworld-name iw))
     (eprintf "~a signed up\n" player-name)
     (define new-fighter (create-fighter player-name (name->image player-name)))
     (define new-state   (add-fighter-to-front new-fighter "this-dispatch-is-a-bad-idea" state))
     (define new-ustate  [ustate (cons iw worlds) new-state])
     (if (= (length (state-fighters new-state)) n) (start-turn new-ustate) new-ustate)]))

(: name->image {String -> Bs})
(define (name->image s)
  (cond
    [(regexp-match #px"Darth" s) 'tie]
    [(regexp-match #px"Benja" s) 'xwing]
    [else "default"]))

;; ---------------------------------------------------------------------------------------------------
(: start-turn {UState -> Bundle})
(define (start-turn us)
  (match-define [ustate worlds state] us)
  (define fighters (state-fighters state))
  (define mail-for-first : Mail (make-mail (first worlds) (your-turn state)))
  (define mails-for-rest : [Listof Mail] (map (reorder-state state) (rest worlds) (rest fighters)))
  (make-bundle (list (first worlds) us) (cons mail-for-first mails-for-rest) '[]))

(: reorder-state {State -> IWorld Fighter -> Mail})
(define ((reorder-state state) iw fighter)
  [make-mail iw (fighter-first state fighter)])

;; ---------------------------------------------------------------------------------------------------
;; WARNING: THE CHECK-EQUAL? TESTS HAVE TO BE REWRITTEN FOR TYPED/RACKET BECAUSE IT DOESN'T TRY EQ?
;; ON OPAQUE IMPORTS. 
;; ---------------------------------------------------------------------------------------------------

"WARNING:
   THE CHECK-EQUAL TESTS HAVE TO BE REWRITTEN FOR TYPED BECAUSE IT DOESN'T TRY EQ? ON OPAQUE IMPORTS."

(module+ test ;; create-ustate 
  (check-true (cons? (state-pills (ustate-state (create-ustate))))))

(module+ test ;; add-player 
  (define +player-2 (add-player 2))
  (define state1  (add-fighter-to-front "Benjamin" "default" state0))
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
  (check-equal? (remove-player ustate1 iworld1) bundle "remove player starts new turn"))

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
