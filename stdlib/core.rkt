
; Symbol overrides for builtin operators
; TODO: Implement varargs whereever relevant
(define (+ a b) (+ a b))
(define (- a b) (- a b))
(define (* a b) (* a b))
(define (/ a b) (/ a b))
; TODO: Debug why this is so slow
(define (lt? a b) (lt? a b))
(define (gt? a b) (gt? a b))
(define (lte? a b) (lte? a b))
(define (gte? a b) (gte? a b))
(define (eq? a b) (eq? a b))
(define (not a) (not a))
(define (&& a b) (&& a b))
(define (|| a b) (|| a b))

; Primitives
(define (id x) x)

(defmacro (if cond then else)
  (eval '(,(eval cond) ,then ,else)))

; List operations
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

(define (map mapfn list)
  (reverse
    (fold
      (-> [result val] (cons (mapfn val) result))
      '[]
      list)))
