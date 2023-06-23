#lang info
(define collection "PillWars")
(define pkg-desc "Fall 2023 Sw Dev project (deprecated)")
(define pkg-authors '(matthias))
(define version "0.1")

(define sw-dev "git://github.com/mfelleisen/SwDev.git")

(define compile-omit-paths
  '( "Profile"))

(define deps
  `("base"
     "typed-racket-lib"
     "rackunit-typed"
     "2htdp-typed"
     "contract-profile"
     "profile-lib"
     "htdp-lib"
     "rackunit-lib"
     "sandbox-lib"
     ,sw-dev))

(define build-deps
  `( ,sw-dev
     "typed-racket-lib"
      "rackunit-typed"
      "rackunit-lib"))
