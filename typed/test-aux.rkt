#lang typed/racket

(provide
 get-files
 file-name
 dir
 in
 out)

(require (for-syntax syntax/parse))

(define dir "Resources/")
(define in  "-test-input.rktd")
(define out "-test-output.rktd")

(: file-name (-> Natural String String))
(define (file-name i in-or-out)
  (~a "../" dir i in-or-out))

(: get-files (->* (String) (String) [Listof String]))
(define (get-files in-or-out [prefix "../"])
  (parameterize ([current-directory (~a prefix dir)])
    (define all-files
      (for*/list : [Listof String]
       ([f : Path (in-directory)]
        [g : String (in-value (path->string f))]
        #:when (regexp-match (pregexp in-or-out) g))
       g))
    (define sorted (sort all-files string<?))
    (pretty-print sorted)
    sorted))