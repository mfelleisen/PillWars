#lang typed/racket

;; a data representation of pills

;; ---------------------------------------------------------------------------------------------------
(provide 
 #; {type Pill = [blue Point N] || [red Point N Degree]}
 Pill
 Red
 Blue 

 #; {N -> [Listof Pill]}
 ;; create `n` red pills and supplement them with some blue pills 
 create-pills

 #; {Point -> Pill}
 ;; make a 0-value pill for a specific place 
 pill-at

 pill?
 pill-posn
 pill-score
 red-acceleration

 ;; for sample runs 
 blue? 
 red? 
 
 add-pill

 #; {Pill Point -> Boolean}
 ;; is it sitting atop the image 
 on-pill?)

(module+ examples
  (provide
   blue0
   red0
   pills-stuck))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/constants)
(require PillWars/Common/typed/point)
(require PillWars/Lib/typed/image)


(module+ test
  (require (submod ".." examples))
  (require PillWars/Common/typed/constants)
  (require/typed PillWars/Lib/image
                 [add-objects (∀ (X) (-> Image [Listof X] [-> X Image Image] Image))])
  (require PillWars/Common/typed/point)
  (require typed/rackunit))

;; ---------------------------------------------------------------------------------------------------
(define-type Pill (U Blue Red))

(struct pill [{posn : Point} {score : Natural}] #:prefab)
(struct blue pill [] #:prefab #:type-name Blue)
(struct red pill [{acceleration : Real}] #:prefab #:type-name Red)

(module+ deep 
  (provide deep-cast-pill)
  (: deep-cast-pill (-> Any Pill))
  (define (deep-cast-pill p)
    (match p
      [(blue p s) (blue (cast p Complex) (cast s Natural))]
      [(red p s a) (red (cast p Complex) (cast s Natural) (cast a Real))])))

(define MAX-PILLS 30)

(: pill-at (-> Point Pill))
(define (pill-at p)
  (if (< (random 100) 60) (red p 0 .00) (blue p 0)))
         
(: create-pills (-> Integer [Listof Pill]))
;; create `r#` red pills; ASSUME `(< r# MAX-PILLS)` 
(define (create-pills r#)
  (append
   (create-red-pills r# red-pills)
   (create-red-pills (- MAX-PILLS r#) blue-pills)))

(define red-pills (λ _ (red (create-random-point) (+ 1 (random 5)) (/ (+ 1 (random 5)) 10))))
(define blue-pills (λ _ (blue (create-random-point) (+ 5 (random 10)))))

(: create-red-pills {Integer [Integer -> Pill] -> [Listof Pill #; red?]})
(define (create-red-pills r# creator)
  ;; rewritten for type checker 
  (let create-red-pills ([r# r#])
    (define try : [Listof Pill] (build-list r# creator))
    (define pure (remove-duplicates try))
    (if (= (length pure) r#)
        pure
        (append pure (create-red-pills (- r# (length pure)))))))

;; ---------------------------------------------------------------------------------------------------
(define BL : Image (circle RS 'solid 'blue))
(define RD : Image (circle RS 'solid 'red))

(: add-pill {Pill Image -> Image})
;; add `pill` to the given `scene0` 
(define (add-pill this scene0)
  (define-values (p img)
    (match this
      [[red posn s value] (values posn (+score RD s))]
      [[blue posn s]      (values posn (+score BL s))]))
  (define-values [x y] (->values p))
  (place-image img x y scene0))

(: +score {Image Integer -> Image})
(define (+score img n)
  (overlay (text (~a n) 12 'white) img))

;; ---------------------------------------------------------------------------------------------------
(: on-pill? (-> Pill Point Boolean))
(define (on-pill? this posn)
  (<= (distance posn (pill-posn this)) RS))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define blue0 (blue (make-point (/ WIDTH 2) (/ WIDTH 10)) 9))
  (define red0  (red  (make-point (/ WIDTH 4) (/ WIDTH 2))  4 .10))
  
  (define pills-stuck (list (blue 320+417i 14))))

(module+ test
  (add-objects (empty-scene WIDTH HEIGHT) (create-pills 20) add-pill))

(module+ test
  (check-true (on-pill? blue0 (pill-posn blue0)))
  (check-false (on-pill? blue0 (+ RS 1 (pill-posn blue0))))

  (define mt (empty-scene 300 300))
  (define-values [bx by] (->values (pill-posn blue0)))
  (define-values [rx ry] (->values (pill-posn red0)))
  (check-equal? (add-pill blue0 mt) (place-image BL bx by mt))
  (check-equal? (add-pill red0 mt) (place-image RD rx ry mt)))
