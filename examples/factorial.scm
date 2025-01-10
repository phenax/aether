
(define (factorial num)
  (if (lte? num 1) 1
    (* num (factorial (- num 1)))))

(for (range 0 10) (-> [n]
  (displayNl (cons n (factorial n)))))
