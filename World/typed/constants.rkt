#lang typed/racket

;; world-local constants 

(provide BG AI-BG)

;; -----------------------------------------------------------------------------
(require PillWars/Common/typed/constants)
(require PillWars/Lib/typed/image)

;; -----------------------------------------------------------------------------
(define BG (empty-scene WIDTH HEIGHT))
(define AI-BG (empty-scene 200 100))
