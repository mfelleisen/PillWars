## Red Blue War, the Typed Version


Run the following 

```
$ raco make game-step1.rkt
$ raco test game-step1.rkt
```

in both `../` and `./` to get timing for the untyped and typed
version, respectively.

This test pitches two AIs against each other. The scenarios
are the test files in `../Resources`. 

Surprisingly, the fully typed version is much slower than the untyped
one: 

```
fully typed: cpu time: 1491 real time: 1512 gc time: 30
untyped:     cpu time: 276 real time: 278 gc time: 63
```

### Conversion Experience 


### TODO

- investigate whether I inadvertently left a boundary in the typed
  variant. 

