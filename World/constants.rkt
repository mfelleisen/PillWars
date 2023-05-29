#lang racket

;; world-local constants 

(provide BG AI-BG)

;; -----------------------------------------------------------------------------
(require PillWars/Common/constants)
(require 2htdp/image)

;; -----------------------------------------------------------------------------
(define BG (empty-scene WIDTH HEIGHT))
(define AI-BG (empty-scene 200 100))
