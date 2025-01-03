
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

; Conditionals. (booleans are directly callable for use as conditionals)
(defmacro (if cond then else)
  (eval '(,(eval cond) ,then ,else)))
