
;; Error constructor
(record Error
  :error/label
  :error/message)

;; Result constructor
(record Result
  :result/error
  :result/value)

