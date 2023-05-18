#lang racket

;; a data representation of pills

;; ---------------------------------------------------------------------------------------------------
(provide 
 #; {type Pill = [blue Point] || [red Point Degree]}
 pill?
 pill-posn

 ;; for sample runs 
 blue? 
 red? 
 
 add-pill

 on-pill?)

(module+ examples
  (provide
   blue0
   red0))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Geometry/constants)
(require PillWars/Common/point)
(require 2htdp/image)

(module+ test
  (require (submod ".." examples))
  (require PillWars/Geometry/constants)
  (require PillWars/Common/point)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(struct pill [posn] #:transparent)
(struct blue pill [] #:transparent)
(struct red pill [points] #:transparent)

(define BL (circle RS 'solid 'blue))
(define RD (circle RS 'solid 'red))

#; {Pill Image -> Image}
;; add `pill` to the given `scene0` 
(define (add-pill this s0)
  (define-values (p img)
    (match this
      [[red posn value] (values posn RD)]
      [[blue posn]      (values posn BL)]))
  (define-values [x y] (->values p))
  (place-image img x y s0))

#; {Pill Figher -> Boolean}
(define (on-pill? this posn)
  (<= (distance posn (pill-posn this)) RS))

;; ---------------------------------------------------------------------------------------------------
(module+ examples
  (define blue0 (blue (make-rectangular (/ WIDTH 2) (/ WIDTH 10))))
  (define red0 (red (make-rectangular (/ WIDTH 4) (/ WIDTH 2)) 'dummy)))

(module+ test
  (check-true (on-pill? blue0 (pill-posn blue0)))
  (check-false (on-pill? blue0 (+ RS 1 (pill-posn blue0))))

  (define mt (empty-scene 300 300))
  (define-values [bx by] (->values (pill-posn blue0)))
  (define-values [rx ry] (->values (pill-posn red0)))
  (check-equal? (add-pill blue0 mt) (place-image BL bx by mt))
  (check-equal? (add-pill red0 mt) (place-image RD rx ry mt)))