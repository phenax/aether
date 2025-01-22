; Primitives
(define (id x) x)
(define (const x) (-> [_] x))
(define (not a) (a #F #T))
(define (apply fn args) (eval '(,fn ,@args)))
(define = eq?)
(define < lt?)
(define > gt?)
(define <= lte?)
(define >= gte?)

; Function
(define (curry fn x) (-> [y] (fn x y)))
(define _^ curry)
(define (^_ fn x) (-> [y] (fn y x)))
(define (flip fn) (-> [x y] (fn y x)))

;; Example:
;;   (|> 5 (^_ * 5) (^_ + 2))
;;   (|> list
;;       (curry filter positive?)
;;       (curry map math/sqrt))
;;
;; Example with infix:
;;   ($ 5
;;     |> (^_ * 5)
;;     |> (^_ + 2))
(define (|> val ... fns) (fold (-> [v fn] (fn v)) val fns))

; Numbers
(define (positive? num) (gt? num 0))
(define (negative? num) (lt? num 0))
(define (zero? num) (eq? num 0))

;; If conditionals
;; Example:
;;   (if (gt? num 10)
;;     "multi-digit"
;;     "single-digit")
(defmacro (if bool then else)
  '(,bool ,then ,else))

;; Let bindings
;; Example:
;;   (let [ (value1 200) (value2 50) ]
;;        (+ value1 value2))
(defmacro (let bindings ... body)
  '(do
    ,@(map (-> [bind] '(define ,@bind)) bindings)
    ,@body))

(define else #T)

;; Cond conditional
;; Example:
;;   (cond
;;     [(lt? age 3)   "baby"]
;;     [(lt? age 13)  "child"]
;;     [(lt? age 18)  "adolescent"]
;;     [(lt? age 60)  "adult"]
;;     [else          "almost dead"])
(defmacro (cond ... cases)
  (define (create-case cases)
    (if (empty? cases)
      #nil
      (list if (car (car cases))
        (cadr (car cases))
        (create-case (cdr cases)))))
  (create-case cases))

;; Infix operator notation. (Always evaluates left to right)
;; Example:
;;    ($ 5 + ($ 2 * 2) * 3 - 1)
;; Equivalent to: (- (* (+ 5 (* 2 2)) 3) 1)
(defmacro ($ start ... args)
  (define (infix-in args value)
    (if (empty? args)
      value
      (infix-in (cdr (cdr args))
        (list (car args) value (cadr args)))))
  (infix-in args start))

;; Create a record
;; Example:
;;   (record Person
;;     :name
;;     :age
;;     :gender)
;;
;;   (define john (Person "John" 25 'male))
;;   (displayNl (:gender john))
(defmacro (record type-name ... properties)
  (define (mk-get index) (-> [obj] (elem-at index obj)))

  '(progn
    ,(list define type-name list)
    ,@(zip-with
        (-> [prop index] (list 'define prop (mk-get index)))
        properties
        (indexes properties))))

;; Expand/Destructure values of a list
;; Example:
;;   (expand [a b c] (list 5 8 2))
;;   (displayNl a ", " b ", " c)
;; 
;; TODO: Implement ... for rest of the items
(defmacro (expand symbols values)
  (concat
    (list
      'progn
      (list define '... values))

    (zip-with
      (-> [sym index]
        (list define sym
          '(elem-at ,index ...)))
      symbols
      (indexes symbols))

    #nil))
