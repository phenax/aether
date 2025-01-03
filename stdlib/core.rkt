
; Symbol overrides for builtin operators
; TODO: Implement varargs whereever relevant
(set + (-> [a b] (+ a b)))
(set - (-> [a b] (- a b)))
(set * (-> [a b] (* a b)))
(set / (-> [a b] (/ a b)))
(set lt? (-> [a b] (lt? a b)))
(set gt? (-> [a b] (gt? a b)))
(set lte? (-> [a b] (lte? a b)))
(set gte? (-> [a b] (gte? a b)))
(set eq? (-> [a b] (eq? a b)))
(set not (-> [a] (not a)))
(set && (-> [a b] (&& a b)))
(set || (-> [a b] (&& a b)))

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
