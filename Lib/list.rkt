#lang racket

(define (good-index? lox)
  (flat-named-contract "proper index" (λ (i) (> (length lox) i))))

(provide
 (contract-out
  [list-rotate (-> (and/c cons? list?) (and/c cons? list?))]
  [remove-ref (->i ([lox list?] [i (lox) (and/c natural? (good-index? lox))]) (result list?))]))

;; ---------------------------------------------------------------------------------------------------
(define (list-rotate lox)
  (snoc (rest lox) (first lox)))

(define (snoc l x) (append l (list x)))
  
(define (remove-ref lox0 i0)
  (let rem ([i i0] [lox lox0])
    (cond
      [(zero? i) (rest lox)]
      [else (cons (first lox) (rem (sub1 i) (rest lox)))])))

(module+ test
  (require (submod ".."))
  (require rackunit)
  (check-exn #px"proper index" (λ () (remove-ref '[a] 1))))