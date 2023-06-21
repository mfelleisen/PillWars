## Red Blue War, the Typed Version

The `game-step1.rkt` file pitches two "AIs" against each other. The
scenarios are the test files in `../Resources`.

### Performance Problems 

Run the following 

```
$ raco make game-step1.rkt
$ raco test game-step1.rkt
```

in both `../` and `./` to get timing for the untyped and typed version,
respectively. Here is the result of the standard setup:

```
fully typed: cpu time: 1491 real time: 1512 gc time: 30
untyped:     cpu time:  276 real time:  278 gc time: 63
```

That is, the fully typed version is almost six times (6x) slower than the
untyped base version. 

We can restore the usual "balance" by including the starter-scenarios directly
in the programs:

```
fully typed: cpu time: 245 real time: 247 gc time: 29
```

#### Problem 

So I/O messes things up, even if we don't include the I/O effort in the timing
part.

#### Diagnosis

Typed Racket wraps even immutable nests of structs and lists into an `Any`
wrapper, and that imposes this huge performance penalty.

#### TODO

Still working on this. 


### Conversion Experience

Summary: 

1. Using macros to assign types to a strange form of polymorphic abstraction
2. `check-equal?` and friends fail on `Any` opaque structures.
3. The ML-ish diamond problem shows up and causes problems.
4. Rewriting code to accommodate the type checker has become rare.
5. `cast` is necessary. 

The conversion process went smoothly for most modules.

ad 1. Using macros to assign types to a strange form of polymorphic abstraction 

The abstraction in `../game-step1.rkt` that turns a single-player `big-bang`
into a 2-player variant posed an interesting challenge. In essence, the
`enable` function (in the untyped variant of the module) consumes higher-order
functions of variable but fixed arity each (and different types for these
various arities). After experimenting with these things for a bit, I gave up
and used a macro to get this form of "strange" polymorphism implemented: 

```
(require (for-syntax syntax/parse))
; (: enable {-> Symbol Symbol [Handler State State] (-> Interactive Any * Interactive)})
(define-syntax (enable stx)
  (syntax-parse stx
    [(_ tag other handler {rem (~literal :) Rem} ...)
     #'(λ ({i : Interactive} {rem : Rem} ...)
         (match-define [interactive whose state0] i)
         (cond
           [(only-one-player state0)
            (define state++ (handler state0 rem ...))
            (interactive AI (cast state++ State))]
           [(equal? whose other) i]
           [else
            (define state++ (handler state0 rem ...))
            (if (boolean? state++)
                i
                (interactive other (swap-two-players state++)))]))]))
```

ad 2. `check-equal?` and friends fail on `Any` opaque structures.

The `2htdp/universe` module exports "sample worlds" for testing purposes. They
are instances of an opaque struct. (It's opaque because it hides ports and IP
addresses from beginning students.)

- The untyped variant of `check-equal?` falls back on `eq?` for such struct
  instances, meaning I get tests to pass easily.

- The typed variant does not. (Conjecture: Some `Any` type/contract gets in
  between.) Hence the tests fail. 

I left a couple of dozen tests for the universe variant of the game alone, and
they fail in the untyped version.

ad 3. The ML-ish diamond problem shows up and causes problems.

Initially I wrote my own typed wrapper for `2htdp/image` as I went. See
`Lib/typed/image.rkt`. Then I remembered Alexis's type-wrapper for this
module. To shorten the process, I imported it into some modules. That's when
I got an error message like the following:

```
type Image expected; found type Image 
```

It took me a while to realize that, even though the image structs are imported
from the same untyped module, they refer to different type definitions
.. though both originate from `require` specs such as these:

```
(require/typed 2htdp/image [#:opaque Image image?])
```

ML solves this by calculating type equality (through sharing constraints
sometimes) backwards thru chains of imports. Why can't we? 

The only way to solve this problem was to convert everything into terms of
Alexis's wrappers, including additions to `2htdp/image` I had created. Argh.

ad 4. Rewriting code to accommodate the type checker has become rare.

(a) Actual type checking -- as opposed to manipulating types in my head --
    helped clarify one function (in the 2,000 lines code base) a lot: 

This function in the untyped variant 

```
#; {Image -> State -> Image}
(define ((draw-state-with-winners BG) s)
  (define scene0 [(draw-state BG) s])
  (define ranks  (winners s))
  (define texts  (map (λ (r) (text r 12 'purple)) ranks))
  (define 1text  (foldl (λ (next-r so-far) (above/join 5 so-far next-r)) empty-image texts))
  (place-image 1text (/ WIDTH 10) (/ HEIGHT 10) scene0))
```

became these two functions: 

```
(: draw-state-with-winners (-> Image (-> State Image)))
(define ((draw-state-with-winners BG) s)
  (define scene0 [(draw-state BG) s])
  (define 1text  (winner-text (winners s)))
  (place-image 1text (/ WIDTH 10) (/ HEIGHT 10) scene0))

(: winner-text (-> [Listof String] Image))
(define (winner-text ranks)
  (define texts  (map (λ ({r : String}) (text r 12 'purple)) ranks))
  (foldl (λ ({next-r : Image} {so-far : Image}) (above/join 5 so-far next-r)) empty-image texts))
```

This is a win. 


(b) The first rewrite is a typical `for/*` problem. I could not figure out how
to get `for/first` to accommodate the desired typing; it was faster to spell
out the loop myself: 

```
(: on-any-pill (Fighter [Listof Pill] -> (U False Point)))
;; is the fighter sitting on any pill? 
;; `pill*` is the list of center points for the targeted pills
(define (on-any-pill this pill*)
  (define mine (fighter-posn this))
  (let some-first ([pill* pill*])
    (cond
      [(empty? pill*) #false]
      [else
       (define p (first pill*))
       (or (and (on-pill? p mine) (pill-posn p)) (some-first (rest pill*)))]))
  ;; rewrite to accommodate the type checker 
  #;
  (for/first : [U False Pill] ([p : Pill pill*] #:when (on-pill? p mine)) : [U False Pill] p))
```


(c) The second rewrite was an attempt to loop over natural numbers instead of
integers. The untyped variant creates red pills with the following function: 

```
#; {N -> [Listof Pill || red?]}
(define (create-red-pills r# creator)
  (define try (build-list r# creator))
  (define pure (remove-duplicates try))
  (if (= (length pure) r#)
      pure
      (append pure (create-red-pills (- r# (length pure))))))
```

This had to be rewritten for type checker: 

```
(: create-red-pills {Integer [Integer -> Pill] -> [Listof Pill #; red?]})
(define (create-red-pills r# creator)
  (let create-red-pills ([r# r#])
    (define try : [Listof Pill] (build-list r# creator))
    (define pure (remove-duplicates try))
    (if (= (length pure) r#)
        pure
        (append pure (create-red-pills (- r# (length pure)))))))
```

It's not clear anymore whether I needed this or whether I'd do it again. 

ad 5. `cast` is necessary.

The conversion needed 14 casts, not counting the casts in `game-step1.rkt`,
which are needed to manage the test scenarios.

### Superfluous Code

The master branch for typed code still refers to `2htdp/image` and
`2htdp/universe`.  For just measuring performance or running
logic-debugging scenarios, this portion of the code can be eliminated.
