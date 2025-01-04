
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
(define (not a) (not a))
(define (&& a b) (&& a b))
(define (|| a b) (|| a b))

(define (set k v) (set k v))

; Primitives
(define (id x) x)

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

; List operations
(define (list ... ls) ls)

(define (concat ls1 ls2)
  (if (empty? ls1)
    ls2
    (cons (car ls1) (concat (cdr ls1) ls2))))

(define (null? x) (eq? x #nil))
(set empty? null?)

(define (fold fn init list)
  (if (empty? list)
    init
    (fold fn
      (fn init (car list))
      (cdr list))))

(define (reverse list)
  (fold (-> [result val] (cons val result)) '[] list))

(define (map fn list)
  (reverse
    (fold
      (-> [result val] (cons (fn val) result))
      '[]
      list)))
