#lang racket

;; a data representation of directions 

;; -----------------------------------------------------------------------------
(provide
 #; {type Direction = Complex}

 #; {-> Direction}
 create-random-direction 
   
 #; {Direction Radian -> Direction}
 dir-rotate

 delta-angle
 
 ;; doesn't really belong here 
 rad->deg
 deg->rad)

;; -----------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
#; {Radian -> Degree}
(define (rad->deg r)
  (* 180 (/ r pi)))

#; {Degree -> Radian}
(define (deg->rad d)
  (* pi (/ d 180)))

(module+ test
  (check-within (rad->deg (deg->rad 30)) 30 .00001))

;; -----------------------------------------------------------------------------
(define (create-random-direction)
  (make-rectangular (random-fraction) (random-fraction)))

(define (random-fraction)
  (+ #i1.0 (random 10)))

(module+ test
  (check-true (number? (create-random-direction))))

;; -----------------------------------------------------------------------------
(define (dir-rotate vel deg)
  (* vel (exp (* 0+i deg))))

(module+ test ;; dir-rotate 
  (define right +1+0i)
  (define down   0+1i)
  (define up     0-1i)
  (define left  -1+0i)
  (define no-we  (* 1 (exp (* 0+1i (* -3/4 pi)))))
  (define so-we  (* 1 (exp (* 0+1i (* -5/4 pi)))))
  
  (check-within (dir-rotate right (/ pi -2)) up .001)
  (check-within (dir-rotate right (/ pi -1)) left .001)
  (check-within (dir-rotate right (/ pi +2)) down .001)
  (check-within (dir-rotate right (/ pi +1)) left .001)

  (check-within (dir-rotate up (/ pi -2)) left .001)
  (check-within (dir-rotate up (/ pi +2)) right .001)

  (check-within (dir-rotate no-we (/ pi -4)) left .001)

  (check-within (dir-rotate left (/ pi -2)) down .001)
  (check-within (dir-rotate left (/ pi +2)) up .001)

  (check-within (dir-rotate down (/ pi +2)) left .001)
  (check-within (dir-rotate down (/ pi -2)) right .001))

;; -----------------------------------------------------------------------------
;; direction-from-fighter-to-mouse vs direction of fighter 
(define (delta-angle new-0 old-0)
  (define dif (- (2pi (angle new-0)) (2pi (angle old-0))))
  (modulo-for-angles dif))

#; {Radian -> Radian}
;; the difference between two angles can be in [-2pi,+2pi]; bring it back to [-pi,+pi]
(define (modulo-for-angles r)
  (cond
    [(<= 0 r (+ pi)) r]
    [(>= 0 r (- pi)) r]
    [(>=   r (+ pi)) (- r (* 2 pi))]
    [(<=   r (- pi)) (+ (* 2 pi) r)]))

#; {Radian -> Radian}
;; our complex numbers have angles between [0,pi] and [0,-pi), so let's normalize 
(define (2pi a)
  (if (< (abs a) pi) a (- (* 2 pi) a)))

(module+ test
  #; {Direction Direction Radian String -> Void}
  ;; check expected angle and also whether the propose rotation achieves its purpose 
  (define (prop-check new current exp mg)
    (check-within (delta-angle new current) exp .001 (~a "expected result: " mg))
    (check-within (angle (dir-rotate current (delta-angle new current))) (angle new) .001 (~a 'P mg)))

  (prop-check up   down  (/ pi -1) "turn 180 left")
  (prop-check down up    (/ pi +1) "turn 180 right")
  (prop-check up   right (/ pi -2) "turn 90 left")
  (prop-check down right (/ pi +2) "turn 90 right")
  (prop-check no-we so-we (/ pi +2) "turn right, across pi")
  (prop-check left no-we (/ pi -4) "odd -- failing"))

;; -----------------------------------------------------------------------------
(module+ test ;; from the test case that caused it all 
  (define mouse-at 620+544i)
  (define fighter-at 801.253269456771+535.1370438852057i)
  (define f.vel -8.705176594493532-5.416631837099699i)
  
  (check-true (<= (delta-angle (- mouse-at fighter-at) f.vel) (/ pi 3)) "bug 1"))
