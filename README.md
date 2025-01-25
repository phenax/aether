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
(define (factorial num)
  (if (<= num 1) 1
    (* num (factorial (- num 1)))))

(displayNl (factorial 5))

(set results '[])
(for (range 10 20) { -> [n]
  (let [ (result (factorial n)) ]
    (set results (concat results result))
    (displayNl n "! is " result))
})

(displayNl results)
; `()`, `[]`, `{}` are the same thing
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


### Process handling
```scheme
; Example: Takes a screenshot of the screen of the focussed window and saves it in given directory
(expand [window-id, _] (! xdotool getwindowfocus)) ; expand macro destructures list into symbols
(! import -window ,window-id "/home/user/Pictures")

; !: Waits for process to end, returns '(stdout, stderrr)
; Throws: on non-zero exit code 'proc/non-zero-exit-code "... <stderr>"

; WIP: Process handling + pipes
```


### Import scripts
Imports script files relative to cwd (symbol and strings both work)
```scheme
(import 'path/to/script-file-1 "../foobar/script-file-2" "./path/to/script-file-3")
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

