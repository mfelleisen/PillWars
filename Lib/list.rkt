#lang racket

(provide
 #; {[NEListof X] -> [NEListof X]}
 list-rotate)

;; ----------------------------------------------------------------------------------------
(define (list-rotate lox)
  (snoc (rest lox) (first lox)))

(define (snoc l x) (append l (list x)))
  
  