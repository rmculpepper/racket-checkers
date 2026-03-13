#lang racket/base
(require checkers
         checkers/gui)

(define (go)
  (test #:name "first"
    (sleep 1)
    (test #:name "inner" (check 1 #:is 2)))
  (test #:name "named"
    (test (check 2 #:is 2)))
  (test #:name "unusual"
    (let/ec escape (test "escapes" (escape (void))))))

(test/gui go)
