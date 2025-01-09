; Primitives
(define (id x) x)
(define (const x) (-> [_] x))
(define (not a) (a #F #T))
(define (apply fn args) (eval '(,fn ,@args)))

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
    ,@(map (-> [bind] '(set ,@bind)) bindings)
    ,@body))

(set else #T)

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
;;   (set john (Person "John" 25 'male))
;;   (displayNl (:gender john))
(defmacro (record type-name ... properties)
  (define (mk-get index) (-> [obj] (elem-at index obj)))

  '(progn
    ,(list 'set type-name list)
    ,@(zip-with
        (-> [prop index] (list 'set prop (mk-get index)))
        properties
        (indexes properties))))

