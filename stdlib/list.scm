(define (list ... ls) ls)
(define (tuple ... ls) ls)

(define (null? null_item) (eq? null_item #nil))
(set nil? null?)
(set empty? null?)
(define (list? ls) (eq? (type ls) 'list))

(define (cadr ls) (car (cdr ls)))
(define (caddr ls) (cadr (cdr ls)))
(set first car)
(set second cadr)
(set third caddr)
(set head car)
(set tail cdr)

(define (length ls)
  (define (length-inner ls acc)
    (if (empty? ls)
      acc
      (length-inner (cdr ls) (+ acc 1))))
  (length-inner ls 0))

(define (concat ls1 ls2)
  (if (empty? ls1)
    ls2
    (cons (car ls1) (concat (cdr ls1) ls2))))

(define (fold fn init ls)
  (if (empty? ls)
    init
    (fold fn
      (fn init (car ls))
      (cdr ls))))

(define (reverse ls)
  (fold (-> [result val] (cons val result)) '[] ls))

(define (map fn ls)
  (reverse
    (fold
      (-> [result val] (cons (fn val) result))
      '[]
      ls)))

(define (for ls fn) (map fn ls) #nil)

(define (range start end)
  (if (gt? start end)
    '()
    (cons start (range (+ start 1) end))))

(define (indexes ls) (range 0 (- (length ls) 1)))

(define (elem-at index ls)
  (cond
    [(lt? index 0)  #nil]
    [(eq? index 0)  (car ls)]
    [else           (elem-at (- index 1) (cdr ls))]))

(define (zip-with fn l1 l2)
  (cond
    [(empty? l1)  '[]]
    [(empty? l2)  '[]]
    [else         (cons '(fn (car l1) (car l2)) (zip-with fn (cdr l1) (cdr l2)))]))

(define (zip l1 l2) (zip-with tuple l1 l2))

(define (index-of value ls)
  (define (index-of-in value ls index)
    (cond
      [(empty? ls)           #nil]
      [(eq? value (car ls))  index]
      [else                  (index-of-in value (cdr ls) (+ index 1))]))
  (index-of-in value ls 0))

(define (contains value ls) (not (eq? (index-of value ls) #nil)))

