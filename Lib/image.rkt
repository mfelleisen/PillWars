#lang racket

;; a function that should become part of 2htdp/image 

(provide add-objects)
  
(require 2htdp/image)

#; {Image [Listof X] [Image X -> Image] -> Image}
;; add the given `objects` to `scene0` 
(define (add-objects scene0 objects add-1-object)
  (for/fold ([s scene0]) ([f objects])
    (add-1-object f s)))