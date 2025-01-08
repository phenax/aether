
(define (factorial num)
  (if (lte? num 1) 1
    (* num (factorial (- num 1)))))

(factorial 10)
