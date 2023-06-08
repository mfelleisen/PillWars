#lang typed/racket

#;
(define (good-index? lox)
  (flat-named-contract "proper index" (λ (i) (> (length lox) i))))

(provide
 list-rotate
 remove-ref

 ;; for type checking 
 #;
 (contract-out
  [list-rotate (-> (and/c cons? list?) (and/c cons? list?))]
  [remove-ref (->i ([lox list?] [i (lox) (and/c natural? (good-index? lox))]) (result list?))]))

;; ---------------------------------------------------------------------------------------------------
(: list-rotate (∀ (X) (-> [Listof X] [Listof X])))
(define (list-rotate lox)
  (snoc (rest lox) (first lox)))

(: snoc (∀ (X) (-> [Listof X] X [Listof X])))
(define (snoc l x) (append l (list x)))

(: remove-ref (∀ (X) (-> [Listof X] Natural [Listof X])))
(define (remove-ref lox0 i0)
  (let rem ([i i0] [lox lox0])
    (cond
      [(zero? i) (rest lox)]
      [else (cons (first lox) (rem (sub1 i) (rest lox)))])))

(module+ test
  (require (submod ".."))
  (require typed/rackunit)
  (check-exn #px"proper index" (λ () (remove-ref '[a] 1))))