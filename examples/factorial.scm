
;; (define (factorial num)
;;   (if (<= num 1) 1
;;     (* num (factorial (- num 1)))))
;;
;; (for (range 0 10) { -> [n]
;;   (displayNl n "! = " (factorial n)) })

;; (define (fibo n)
;;   (if (<= n 1) 1
;;     (+ (fibo (- n 1)) (fibo (- n 2)))))
;;
;; (displayNl (fibo 25))

;; (define (get-spec-property value)
;;   (-> [... spec]
;;     (cond
;;       [(= (quote 'to_equal) (car spec))
;;         (list (= value (cadr spec)) "equal")
;;       ]
;;       [(= (quote 'to_not_equal) (car spec))
;;         (list (not (= value (cadr spec))) "not equal")
;;       ]
;;       [else  (error! "Invalid expect expression")])))
;;
;; (define (expect value ... args)
;;    (define result (apply (curry get-spec-property value) args))
;;    (displayNl result)
;;    
;;    (if (= (car result) #T)
;;      #nil
;;      (progn
;;        (displayNl "Expected " value " to " (cadr result))
;;        (error! (quote 'assertion-error) "Assertion Error")
;;        )
;;      )
;;   )
;;
;; (expect 1 'to_equal 2)
; (expect 1 'to_equal 2)

