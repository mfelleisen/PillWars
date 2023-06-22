#lang typed/racket

;; a data representation of fighters

;; todo: formulate fighter imagine in terms of geometry 

;; ---------------------------------------------------------------------------------------------------
(provide
 #; {type Fighter = [fighter Point Direction N String]}
 Fighter

 #; {String -> Fighter}
 ;; a random fighter 
 create-fighter 

 fighter?
 fighter-posn
 fighter-velocity
 fighter-score
 fighter-name
 
 #; {Fighter Scene -> Scene}
 add-fighter

 #; {Fighter Radian -> Fighter}
 rotate-fighter

 #; {Fighter (U PositiveInteger -1) -> Fighter}
 eat-fighter

 #; {Fighter (0,1) -> Fighter}
 accelerate-fighter

 #; {Fighter -> Fighter}
 move-fighter)

(module+ examples
  (provide
   steps-2

   fighter0
   fighter0++
   fighter1
   fighter2
   fighter5
   fighter6
   fighter6++
   fighter6--
   fighter-stuck))
  
;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/constants)
(require PillWars/Common/typed/direction)
(require PillWars/Common/typed/pills)
(require PillWars/Common/typed/point)
(require PillWars/Lib/typed/image)
(require (only-in PillWars/Common/typed/geometry TIE XWING))

(module+ examples
  (require PillWars/Common/typed/pills)
  (require (submod PillWars/Common/typed/pills examples)))

(module+ test
  (require (submod ".." examples))
  (require typed/rackunit)
  (require/typed rackunit [check-within (->* (Any Any Real) (String) Void)]))

;; ---------------------------------------------------------------------------------------------------
(define-type Bs (U String (U 'default-image 'xwing 'tie)))

(struct fighter [{posn : Point} {velocity : Direction} {score : Natural} {name : String} {base : Bs}]
  #:prefab #:type-name Fighter)

(module+ deep
  (provide deep-cast-fighter)
  (: deep-cast-fighter (-> Any Fighter))
  (define (deep-cast-fighter f)
    (match-define [fighter p v s n b] (cast f Fighter))
    (fighter (cast p Complex) (cast v Complex) (cast s Natural) (cast n String) (cast b Bs))))
  
(: create-fighter (->* (String) (Bs) Fighter))
(define (create-fighter name [base 'default-image])
  (fighter (create-random-point) (create-random-direction) 0 name base))

;; ---------------------------------------------------------------------------------------------------

(define fighter-radar (color 100 77 99 33) #;'yellow)
(define fighter-color (color 0 255 0 66) #;'green)

(: fancy-image {Bs -> Image})
(define [fancy-image base]
  (define image
    (case base
      [(tie) TIE]
      [(xwing) XWING]
      [else default-image]))
  (let* ([s (wedge-centered-at-tip RADAR (rad->deg (* 2 MAX-RAD)) 'solid fighter-radar)]
         [s (rotate (- (rad->deg MAX-RAD)) s)]
         [t image]
         [s (overlay s t)])
    s))

(: default-image Image)
(define default-image
  (let* ([t (wedge-centered-at-tip FWING (rad->deg FANGLE) 'solid fighter-color)]
         [t (rotate (/ (rad->deg FANGLE) -2) t)])
    t))

(: add-fighter (-> Fighter Image Image))
;; add fighter `f` to the given scene `s0`
(define (add-fighter this scene0)
  (match-define [fighter posn vel score name image] this)
  (define-values (p.x p.y) (->values posn))
  (define-values [v.x v.y] (->values (+ posn (* (/ FWING .5 (magnitude vel)) vel))))
  (define-values [s.x s.y] (->values vel))
  (let* ([s scene0]
         [r (rad->deg (angle (conjugate vel)))]
         [n (text name 12 'black)]
         [y (text (~a "dy: " (~r s.y #:precision 2)) 12 'black)]
         [x (text (~a "sx: " (~r s.x #:precision 2)) 12 'black)]
         [c (text (~a "score : " score) 12 'black)]
         [b (rectangle 1 1 'solid 'white)]
         [g (overlay/offset (above/align 'left n b c b x b y) -60 0 [fancy-image image])]
         [f (rotate r g)]
         [s (place-image f p.x p.y s)])
    s))

(module+ test
  (define scene0 (empty-scene 400 400))
  (define scene1 (place-image (rectangle 10 10 'solid 'red) 120 120 scene0))
  (define scene2 (place-image (rectangle 10 10 'solid 'red) 200 200 scene1))
  
  (add-fighter (fighter 100+100i 5+5i 22 "Benjamin" 'default-image) scene2)
  (add-fighter (fighter 100+100i 5+5i 22 "Benjamin" 'xwing) scene2)
  (add-fighter (fighter 100+100i 5+5i 22 "Benjamin" 'tie) scene2))

;; ---------------------------------------------------------------------------------------------------
(: move-fighter (->* (Fighter) (Natural) (U False Fighter)))
(define (move-fighter this [delta 1])
  (match-define [fighter p v s n img] this)
  (define posn (direction+ p (* delta v)))
  (and (not (outside? posn)) (fighter posn v s n img)))

(: accelerate-fighter (-> Fighter Real Fighter))
(define (accelerate-fighter this %)
  (match-define [fighter p v s n img] this)
  (fighter p (* (+ 1 %) v) s n img))

(: rotate-fighter (-> Fighter Radian Fighter))
(define (rotate-fighter this rad)
  (match-define [fighter p v s n img] this)
  (fighter p (dir-rotate v rad) s n img))

(: eat-fighter (-> Fighter Integer Fighter))
(define (eat-fighter this score)
  (match-define [fighter p v s n img] this)
  (fighter p v (max 0 (+ s score)) n img))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define pill*0 (list red0))

  (define fighter0   (fighter 100+50i -9+6i 0 "" 'default-image))
  (define fighter0++ (fighter  91+56i -9+6i 0 "" 'default-image))
  (define fighter1   (fighter (pill-posn red0) 0+0i 0 "" 'default-image))
  (define steps-2  20)
  (define posn0 (pill-posn red0))
  (define fighter2   (fighter (direction+ posn0 (* -1 steps-2 1+1i)) +0+1i 0 "" 'default-image))
  (define fighter5   (fighter (direction+ posn0 (* -1 steps-2 1+1i)) -1+1i 0 "" 'default-image))

  (define fighter6   (fighter 0.0  0+1i         0 "" 'default-image))
  (define fighter6-- (fighter 0.0  (* 1.1 0+1i) 0 "" 'default-image))
  (define fighter6++ (fighter 0.0  -1+0i        0 "" 'default-image))

  (define fighter-stuck (fighter 364.464+195.141i -59.953+67.385i 0 "AI" 'default-image)))

(module+ test
  (define mtf (empty-scene 200 200))
  (define-values [fx fy] (->values (fighter-posn fighter0)))
  (check-true (image? (add-fighter fighter0 mtf))))

(module+ test
  (check-true (fighter? (create-fighter "AI")))
  (check-within (accelerate-fighter fighter6 .1) fighter6-- .01 "acc")
  (check-within (eat-fighter fighter6 -1) fighter6 .01 "eat")
  (check-within (move-fighter fighter0) fighter0++ .01 "mov")
  (check-within (rotate-fighter fighter6 (/ pi 2)) fighter6++ .001 "rot"))

