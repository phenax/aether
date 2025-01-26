
(import 'examples/imported-factorial.scm)

(define (factorial num)
  (if (<= num 1) 1
    (* num (factorial (- num 1)))))

(displayNl (factorial 7))

(define results (run-factorials-for (range 10 20) factorial))

(displayNl results)

(displayNl (get-args))

(record Person
  :person/name  ; :person/ is just for namespacing. Can be any symbol
  :person/age)

(define (show-person p)
  (displayNl (:person/name p) " is " (:person/age p) " years old"))

(set john (Person "John" 8))
(show-person john)

(set updated-john
  (|> john
    (_^ set@:person/name "Johneshwar")
    (_^ set@:person/age 10)))
(show-person updated-john)


;; (describe "some-function"
;;   (define-value value '(1 2 3))
;;
;;   (context "when something is something"
;;     (it "does something"
;;       (set result (map (^_ + 1) value))
;;       (expect result (:to-equal '(2 3 4))))
;;   )
;; )

