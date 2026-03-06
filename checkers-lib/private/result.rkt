;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/struct)
(provide (all-defined-out))

;; A Result represents the result of evaluating an expression. Continuation
;; jumps are not caught, only escapes via `raise`.

;; A Result if one of
;; - (Listof Any)       -- evaluation returned zero or more values
;; - (raise-result Any) -- raised a value (maybe exn, maybe not)
(struct raise-result (e)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (v) 'raise)
   (lambda (v) (list (raise-result-e v)))))

(define (single-result? r)
  (and (pair? r) (null? (cdr r))))

(define (multi-values-result? r)
  (and (list? r) (not (single-result? r))))

;; ----------------------------------------

;; A PrintResult is one of
;; - Any                            -- returned one value
;; - (values-result (Listof Any))   -- returned zero, two, or more values
;; - (raise-result Any)             -- raised a value
(struct values-result (vs) #:transparent
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (v) 'values)
   (lambda (v) (values-result-vs v))))

(define (result->print-result r)
  (match r
    [(list v) v]
    [(? list? vs) (values-result vs)]
    [(? raise-result?) r]))

(define (print-result->result pr)
  (match pr
    [(values-result vs) vs]
    [(? raise-result?) pr]
    [v (list v)]))
