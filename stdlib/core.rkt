; Symbol overrides for builtin operators
; TODO: Implement varargs whereever relevant
(define (+ a b) (+ a b))
(define (- a b) (- a b))
(define (* a b) (* a b))
(define (/ a b) (/ a b))
(define (lt? a b) (lt? a b))
(define (gt? a b) (gt? a b))
(define (lte? a b) (lte? a b))
(define (gte? a b) (gte? a b))
(define (eq? a b) (eq? a b))
(define (&& a b) (&& a b))
(define (|| a b) (|| a b))
(define (set k v) (set k v))

; Primitives
(define (id x) x)
(define (const x) (-> [_] x))
(define (not a) (a #F #T))

; If conditionals
; Example:
;   (if (gt? num 10)
;     "multi-digit"
;     "single-digit")
(defmacro (if cond then else)
  '(,(eval cond) ,then ,else))

; Let bindings
; Example:
;   (let [ '(value1 200) '(value2 50) ]
;        (+ value1 value2))
(defmacro (let bindings ... body)
  (concat
    (cons 'do (map (-> [bind] (cons 'set bind)) bindings))
    (if (eq? (type body) 'list) body '(,body))))

;; (defmacro (apply fn args)
;;   (cons fn args))
