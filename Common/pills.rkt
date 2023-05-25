#lang racket

;; a data representation of pills

;; ---------------------------------------------------------------------------------------------------
(provide 
 #; {type Pill = [blue Point N] || [red Point N Degree]}

 #; {N -> [Listof Pill]}
 ;; create `n` red pills and supplement them with some blue pills 
 create-pills 

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
(require PillWars/Common/constants)
(require PillWars/Common/point)
(require 2htdp/image)

(module+ test
  (require (submod ".." examples))
  (require PillWars/Common/constants)
  (require PillWars/Lib/image)
  (require PillWars/Common/point)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct pill [posn score] #:prefab)
(struct blue pill [] #:prefab)
(struct red pill [acceleration] #:prefab)

(define MAX-PILLS 30)

;; create `r#` red pills; ASSUME `(< r# MAX-PILLS)` 
(define (create-pills r#)
  (append
   (create-red-pills r# red-pills)
   (create-red-pills (- MAX-PILLS r#) blue-pills)))

(define red-pills (λ _ (red (create-random-point) (+ 1 (random 5)) (/ (+ 1 (random 5)) 10))))
(define blue-pills (λ _ (blue (create-random-point) (+ 5 (random 10)))))

#; {N -> [Listof Pill || red?]}
(define (create-red-pills r# creator)
  (define try (build-list r# creator))
  (define pure (remove-duplicates try))
  (if (= (length pure) r#)
      pure
      (append pure (create-red-pills (- r# (length pure))))))

;; ---------------------------------------------------------------------------------------------------
(define BL (circle RS 'solid 'blue))
(define RD (circle RS 'solid 'red))

#; {Pill Image -> Image}
;; add `pill` to the given `scene0` 
(define (add-pill this scene0)
  (define-values (p img)
    (match this
      [[red posn s value] (values posn (+score RD s))]
      [[blue posn s]      (values posn (+score BL s))]))
  (define-values [x y] (->values p))
  (place-image img x y scene0))

#; {Image N -> Image}
(define (+score img n)
  (overlay (text (~a n) 12 'white) img))

;; ---------------------------------------------------------------------------------------------------
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