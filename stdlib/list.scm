(define (list ... ls) ls)

(define (null? null_item) (eq? null_item #nil))
(set empty? null?)

(define (length list)
  (define (length-inner ls acc)
    (if (empty? ls)
      acc
      (length-inner (cdr ls) (+ acc 1))))
  (length-inner list 0))

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
