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
The [examples](./examples/) directory has some examples to look at. For a (sort of) usable CLI example, take a look at [crop-video-cli](./examples/crop-video-cli.scm).

Heres some normal stuff you can do in aether so far. Take a look inside [stdlib](./stdlib/) directory to see what more is available.

### Functions
`define` is used to define functions `(define (symbol ... args) ... exprs)`.

It can also be used to define values `(define symbol value)`.

```scheme
(define (factorial num)
  (if (<= num 1) 1
    (* num (factorial (- num 1)))))

(displayNl (factorial 5))

(set results '[])
(for (range 10 20) { -> [n]
  (let [ (result (factorial n)) ]
    (set results (concat results result))
    (displayNl n "! is " result))
}) ; `()`, `[]`, `{}` are the same thing

(displayNl results)
```


### Macros

```scheme
(defmacro (when condition ... exprs)
  '(if ,condition
    (progn ,@exprs)
    #nil))

(set n 5)
(when (>= n 5)
  (set yay "Yay!")
  (displayNl "N is >= 5 and everyone you love will die some day! " yay)
  (displayNl "Good bye"))
```


### Error handling
`try` function handles errors in expressions inside it and returns `'(error value)`. `(try (error-prone-code))`

`error!` function is used to throw exceptions. `(error! 'error-identifier "Error message")`

```scheme
(define (divide! a b)
  (if (zero? b)
    (error! 'division-by-zero "Cant divide by zero bro")
    (/ a b)))

(expand [error value] (try (divide! 20))) ; expand macro destructures list into symbols

(cond
  [ (nil? error)
      {displayNl "Result: " value} ]

  [ (= 'division-by-zero (error/label error))
      {displayNl "DivByZero error: " (error/message error)} ]

  [ else
      {displayNl "Unexpected error: " (error/message error)} ])
```


### Process handling
`!` function spawns a process and waits for it to end. Returns `'(stdout stderrr)`.

On non-zero exit code, it throws an error `(error! 'proc/non-zero-exit-code "... <stderr>")`

```scheme
; Example: Takes a screenshot of the screen of the focussed window and saves it in given directory
(expand [window-id, _] (! xdotool getwindowfocus)) ; expand macro destructures list into symbols
(! import -window ,window-id "/home/user/Pictures")

; WIP: Process handling + pipes
```


### Import scripts
`import` function imports script files, relative to cwd, into the current scope (symbol and strings both work).

```scheme
(import 'path/to/script-file-1 "../foobar/script-file-2" "./path/to/script-file-3")
```


### Infix syntax
Why not?

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
`record` macro creates record structures. It's just a list with accessors so nothing fancy.

```scheme
(record Person
  :person/name  ; :person/ is just for namespacing. Can be any symbol
  :person/age)

(set john (Person "John" 8))

(displayNl (:person/name john) " is " (:person/age john) " years old")

; WIP: updating record properties has not been implemented yet
```

