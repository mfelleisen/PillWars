#lang racket

;; a data representation of pills

;; ---------------------------------------------------------------------------------------------------
(provide 
 #; {type Pill = [blue Point N] || [red Point N Degree]}
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
   red0))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/constants)
(require PillWars/Common/point)
(require 2htdp/image)

(module+ test
  (require (submod ".." examples))
  (require PillWars/Common/constants)
  (require PillWars/Common/point)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct pill [posn score] #:transparent)
(struct blue pill [] #:transparent)
(struct red pill [acceleration] #:transparent)

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
  (define red0  (red  (make-point (/ WIDTH 4) (/ WIDTH 2))  4 .10)))

(module+ test
  (check-true (on-pill? blue0 (pill-posn blue0)))
  (check-false (on-pill? blue0 (+ RS 1 (pill-posn blue0))))

  (define mt (empty-scene 300 300))
  (define-values [bx by] (->values (pill-posn blue0)))
  (define-values [rx ry] (->values (pill-posn red0)))
  (check-equal? (add-pill blue0 mt) (place-image BL bx by mt))
  (check-equal? (add-pill red0 mt) (place-image RD rx ry mt)))