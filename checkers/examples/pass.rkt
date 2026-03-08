;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         checkers)

(test #:name "identity"
  (check (+ 7 0) #:is 7)
  (check (* 8 1) #:is 8))

(test #:name "arithmetic"
  (check (+ 1 2) #:is 3)
  (check (+ 4 6) #:with even?)
  (check (quotient/remainder 10 3) #:is (values 3 1))
  (check (/ 1 0) #:error exn:fail:contract:divide-by-zero?))

(test
  (check (modulo 5 0)
    #:error exn:fail:contract:divide-by-zero?
    #:error #rx"^modulo: "))
(test
  (check (range 10)
    #:with list?
    #:with (lambda (v) (= (length v) 10))))

(test (check 1 #:is-true))
(test (check 1 #:is-not 2))
(test (check 1 #:no-error))

;; check does not catch exn:break
(test (check (with-handlers ([exn:break? (lambda (e) 'pass)])
               (check (break-thread (current-thread)) #:error exn:break?)
               'fail)
        #:is 'pass))

;; check catches exn:fail (subtype), even w/o check clauses
(test (check (/ 1 0)))
