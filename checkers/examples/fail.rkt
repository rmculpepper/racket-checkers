;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         raco/testing
         checkers)

(test #:name "outer"
  (test #:name "inner"
    (check 1 #:is 2))
  (check 1 #:is 3))

(test (check 1 #:is 2))
(test (check 1 #:is (values 1 2)))
(test (check (values 1 2) #:is 0))
(test (check (values 1 2) #:is (values 2 3)))
(test (check (/ 1 0) #:is 0))
(test (check (/ 1 0) #:is (values 1 2)))

(test (check 1 #:is-not 1))
(test (check 1 #:is-not (values 1 2)))
(test (check (values 1 2) #:is-not 0))
(test (check (values 1 2) #:is-not (values 1 2)))
(test (check (/ 1 0) #:is-not 0))
(test (check (/ 1 0) #:is-not (values 1 2)))

(test (check #f #:is-true))
(test (check (values 1 2) #:is-true))
(test (check (/ 1 0) #:is-true))

(test (check 1 #:error exn:fail?))
(test (check 1 #:error #rx"wrong"))
(test (check (values 1 2) #:error exn:fail?))
(test (check (values 1 2) #:error #rx"wrong"))
(test (check (error 'foo) #:error exn:fail:contract?))
(test (check (error 'foo) #:error #rx"bar: ^"))

(test (check (error 'foo) #:no-error))

(test (check 1 #:with even?))
(test (check (values 1 2) #:with even?))
(test (check (/ 1 0) #:with even?))

(test (check 1 #:with (checker:predicate void #b100)))
(test (check (values 1 2) #:with (checker:predicate void #b111000)))
(test (check (values 1 2) #:with (checker:predicate void (arithmetic-shift -1 3))))
(test (check (/ 1 0) #:with (checker:predicate void)))

(test (check 1 #:with (checker:compare > 10)))
(test (check (values 1 2) #:with (checker:compare > 10)))
(test (check (/ 1 0) #:with (checker:compare > 10)))

(match (test-report)
  [(cons failed total)
   (unless (= failed total)
     (error 'examples "not all tests failed!\n  total: ~s\n  failed: ~s"
            total failed))])

(module test racket/base
  (void))
