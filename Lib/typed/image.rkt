#lang typed/racket

;; a function that should become part of 2htdp/image 

(provide
 #; {Image [Listof X] [Image X -> Image] -> Image}
 ;; add the given `objects` to `scene0` 
 add-objects

 #; {Real Real Mode Color -> Image}
 wedge-centered-at-tip

 #; {Real Mode Color -> Image}
 circle-with-cross-hair

 #; {Natural Image Image -> Image}
 above/join)

(provide
 Image

 image?
 empty-scene 
 scene+line  
 place-image 
 place-image/align
 above/align 
 overlay
 overlay/offset
 text        
 circle      
 rectangle   
 wedge       
 color
 rotate
 center-pinhole
 scale
 bitmap/file
 image-width
 image-height
 empty-image)

;; the following doesn't work with types (???)
#;
(provide (all-from-out 2htdp/image))

;; ---------------------------------------------------------------------------------------------------
(require PillWars/Common/typed/direction)

(define-type PER Exact-Rational)
(define-type SymCol (U Symbol Color))

(require/typed 2htdp/image [wedge (-> Integer Radian Symbol SymCol Image)])
(require typed/2htdp/image)

;; ---------------------------------------------------------------------------------------------------
(: add-objects (∀ (X) (-> Image [Listof X] [-> X Image Image] Image)))
(define (add-objects scene0 objects add-1-object)
  (for/fold ([s scene0]) ([f objects])
    (add-1-object f s)))

;; ---------------------------------------------------------------------------------------------------
(: circle-with-cross-hair (-> Natural Mode (U Image-Color Pen) Image))
(define (circle-with-cross-hair r mode color)
  (let* ([s (circle r mode color)]
         [s (scene+line s 0 r (* 2 r) r 'black)]
         [s (scene+line s r 0 r (* 2 r) 'black)])
    s))

;; ---------------------------------------------------------------------------------------------------
(: wedge-centered-at-tip (-> Nonnegative-Real Radian Symbol (U Color Symbol) Image))
(define (wedge-centered-at-tip l α mode c)
  (let* ([s (wedge (~i l) α mode c)]
         [d (circle l 'solid (color 255 255 255 0))]
         [s (place-image/align s (- l (delta-x l α)) (+ l (delta-y l α)) 'left 'bottom d)]
         [s (overlay (circle 3 'solid 'orange) s)])
    s))

(: ~i (-> Real Integer))
(define (~i x) (assert (inexact->exact (round x)) exact-integer?))


;; the next two functions were designed by "visual inspection"

(: delta-x {Real Degree -> Real})
(define (delta-x l α)
  (cond
    [(<= α 90)  0]
    [(<= α 180) (* l (cos (deg->rad (- 180 α))))]
    [(<= α 270) l]
    [else       l]))

(: delta-y {Real Degree -> Real})
(define (delta-y l α)
  (cond
    [(<= α 180) 0]
    [(<= α 270) (* l (sin (deg->rad (- α 180))))]
    [else       l]))

;; ---------------------------------------------------------------------------------------------------
(: above/join (-> Natural Image Image Image))
(define (above/join n img1 img2)
  (above/align 'left img1 (rectangle 1 n 'solid 'white) img2))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (add-objects (empty-scene 100 100) `[,(circle 3 'solid 'red)]
               (λ ({c : Image} {s : Image}) (place-image c 50 50 s))))

(module+ test
  (circle-with-cross-hair 10 'solid 'red))

(module+ test
  (wedge-centered-at-tip 100  30 'solid 'red)
  (wedge-centered-at-tip 100 110 'solid 'green)
  (wedge-centered-at-tip 100 190 'solid 'yellow)
  (wedge-centered-at-tip 100 300 'solid 'blue))

(module+ test
  (above/join 5 (text "hello" 12 'purple) (text "world" 12 'purple)))
