; Primitives
(define (not a) (a #F #T))
(define else #T)

; Function
(define (id x) x)
(define (const x) (-> [_] x))
(define (curry fn x) (-> [y] (fn x y)))
(define _^ curry)
(define (^_ fn x) (-> [y] (fn y x)))
(define (flip fn) (-> [x y] (fn y x)))
(define (apply fn args) (eval '(,fn ,@args)))

;; Pipe function application
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
(define = eq?)
(define < lt?)
(define > gt?)
(define <= lte?)
(define >= gte?)
(define (positive? num) (> num 0))
(define (negative? num) (< num 0))
(define (zero? num) (= num 0))
(define (even? n) (= (remainder n 2) 0))
(define (odd? n) (= (remainder n 2) 1))
(define (divisible? a b) (= (remainder a b) 0))

;; If conditionals
;; Example:
;;   (if (gt? num 10)
;;     "multi-digit"
;;     "single-digit")
(defmacro (if bool then else)
  '(,bool ,then ,else))

;; When conditional
;; Example:
;;   (when (= n 5)
;;     (displayNl "N is 5 and everyone you love will die some day! Yay!")
;;     (displayNl "Good bye"))
(defmacro (when condition ... exprs)
  '(,condition (progn ,@exprs) #nil))

;; Unless conditional
;; Example:
;;   (unless (= n 5)
;;     (displayNl "N is definitly not 5 and everyone you love will still die some day")
;;     (displayNl "Good bye"))
(defmacro (unless condition ... exprs)
  '(,condition #nil (progn ,@exprs)))

;; Let bindings
;; Example:
;;   (let [ (value1 200) (value2 50) ]
;;        (+ value1 value2))
(defmacro (let bindings ... body)
  '(do
    ,@(map (-> [bind] '(define ,@bind)) bindings)
    ,@body))

;; Cond conditional
;; Example:
;;   (cond
;;     [(<= age 3)   "baby"]
;;     [(<= age 13)  "child"]
;;     [(<= age 18)  "adolescent"]
;;     [(<= age 60)  "adult"]
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
  (define (mk-set index) (-> [value obj]
     (elem-update (-> [] value) index obj)))

  '(progn
    ,(list define type-name list)
    ,@(zip-with
        (-> [prop index] (list 'progn
            (list 'define prop (mk-get index))
            (list 'define
              (make-symbol (string "set@" prop))
              (mk-set index))))
        properties
        (indexes properties))))

;; Expand/Destructure values of a list
;; Example:
;;   (expand [a b c] (list 5 8 2))
;;   (displayNl a ", " b ", " c)
;;
;; TODO: Implement ... for rest of the items
(defmacro (expand symbols values)
  '(progn
    (define ...all ,values)

    ,@(zip-with
      (-> [sym index]
        (list 'define sym
          '(elem-at ,index ...all)))
      symbols
      (indexes symbols))))
