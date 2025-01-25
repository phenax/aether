; Helper for factorial.scm to demo imports

(set results '[])

(define (run-factorials-for input-values factorial-fn)
  (for input-values { -> [n]
    (let [ (value (factorial-fn n)) ]
      (set results (concat results value))
      (displayNl n "! is " value))
  })

  results)



;; TODO: Fix this issue. Result should be 6 7 8 9
(define (testin)
  (set foobar 5)
  (-> []
    (set foobar (+ foobar 1))
    foobar
  ))

(define test-inst (testin))

(displayNl (test-inst))
(displayNl (test-inst))
(displayNl (test-inst))
(displayNl (test-inst))

;; (displayNl foobar)
