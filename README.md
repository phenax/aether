# Aether lang
It's a programming language. It's based on scheme. It's mine. Leave me alone.


## Install
Clone and build it yourself
- Via nix: `nix build`
- Via cabal: `cabal build`

Or just run directly it with nix flakes: `nix run github:phenax/aether -- repl`.


## Running
- Run the repl with `aether repl`. Use rlwrap for a better experience (`rlwrap aether repl`)
- Evaluate a script file with `aether run ./path/to/file.scm`

There is no help menu. I don't care about you.



## Examples
Some normal stuff you can do in aether so far. 
Take a look inside [stdlib](./stdlib/) directory to see what more is available.

### Functions
```scheme
(define (factorial n)
  (if (<= n 1) 1
    (factorial (- n 1))))

(displayNl (factorial 20))

(for (range 0 10) { -> [n]
  (displayNl n "! is " (factorial n)) })
; `()`, `[]`, `{}` are the same thing as you'd expect
```

### Macros
```scheme
(defmacro (when condition ... blocks)
  '(if ,condition
    (do ,@blocks)
    #nil))

(set n 5)
(when (= n 5)
  (set yay "Yay!")
  (displayNl "N is 5 and everyone you love will die some day! " yay)
  (displayNl "Good bye"))
```

### Error handling
```scheme
(define (divide! a b)
  (if (zero? b)
    (error! 'division-by-zero "Cant divide by zero bro")
    (/ a b)))

(expand [error value] (try (divide! 20))) ; expand macro destructures list into symbols

(cond
  [ (nil? error)
      { displayNl "Result: " value } ]

  [ (= 'division-by-zero (error/label error))
      { displayNl "DivByZero error: " (error/message error) } ]

  [ else
      { displayNl "Unexpected error: " (error/message error) } ])
```

### Infix syntax
```scheme
; Always evaluated left to right. No fixity because fuck you
($ 5 + 4 * 3 - 2)      ; 25
; Equivalent to (- (* (+ 5 4) 3) 2)

($ 5 + ($ 4 * 3) - 2)  ; 15
; Equivalent to (- (+ 5 (* 4 3)) 2)
```

### Pipe/currying and other functional nerd stuff
```scheme
(set nums (list 1 2 3 4 5))
(|> nums
  (_^ map (-> [x] (* x 2)))  ; curried map x2
  reverse                    ; reverse list
  (^_ for displayNl))       ; display each item

; _^ is curry second (same as `curry`): ((a, b) -> c) -> a -> b -> c
; ^_ is curry first: ((a, b) -> c) -> b -> a -> c
```

### Records
```scheme
(record Person
  :person/name  ; :person/ is just for namespacing. Can be any symbol
  :person/age)

(set john (Person "John" 8))

(displayNl (:person/name john) " is " (:person/age john) " years old")

; NOTE: updating record properties has not been implemented yet
```

