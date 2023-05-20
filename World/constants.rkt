#lang racket

;; world-local constants 

(provide BG)

;; -----------------------------------------------------------------------------
(require PillWars/Common/constants)
(require 2htdp/image)

;; -----------------------------------------------------------------------------
(define BG (empty-scene WIDTH HEIGHT))
