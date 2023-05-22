#lang info
(define collection "PillWars")
(define pkg-desc "Fall 2023 Sw Dev project (deprecated)")
(define pkg-authors '(matthias))
(define version "0.1")

(define sw-dev "git://github.com/mfelleisen/SwDev.git")

(define compile-omit-paths
  '( "Old"
     "Exploratory"))

(define deps
  `("base"
;     "net-lib"
;     "draw-lib"
;     "pict-lib"
;     "pict-abbrevs"
;     "scribble-lib"
;     "typed-racket-lib"
;     "scribble-abbrevs"
     "htdp-lib"
;     "gregor-lib"
;     "gui-lib"
;     "racket-doc"
;     "profile-lib"
     "rackunit-lib"
;     "pict-abbrevs"
     "sandbox-lib"
     ,sw-dev))

#;
(define build-deps
  `( ,sw-dev
;      "gui-lib"
;      "draw-lib"
;      "data-enumerate-lib"
;      "at-exp-lib"
;      "sandbox-lib"
      "rackunit-lib"))
