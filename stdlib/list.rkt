(define (list ... ls) ls)

(define (null? x) (eq? x #nil))
(set empty? null?)

(define (concat ls1 ls2)
  (if (empty? ls1)
    ls2
    (cons (car ls1) (concat (cdr ls1) ls2))))

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
