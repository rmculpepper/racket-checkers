;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/list
         racket/contract/base
         syntax/srcloc
         "private/result.rkt"
         "private/test.rkt"
         "private/check.rkt"
         "private/terminal.rkt")
(provide test
         check
         checker?
         (rename-out [checker:equal* checker:equal])
         (rename-out [checker:not-equal* checker:not-equal])
         (contract-out
          [checker:predicate
           (->* [procedure?]
                [#:arity-mask exact-integer?
                 #:property (or/c string? #f)]
                checker?)]
          [checker:compare
           (-> (-> any/c any/c any/c) any/c checker?)]
          [checker:error
           (-> (or/c (-> any/c any/c) regexp?) checker?)]
          [run-tests
           (->* [(-> any)]
                [#:out (or/c output-port? (-> output-port?))
                 #:progress? boolean?
                 #:tell-raco? boolean?]
                void?)]))

;; This is my bikeshed. There are many like it, but this one is mine.

;; ============================================================
;; Tests

(begin-for-syntax
  (define (stx->loc-expr stx)
    #`(quote-syntax #,(datum->syntax #f 'SRCLOC stx)))

  (define-splicing-syntax-class maybe-name
    (pattern (~seq #:name (~var n (expr/c #'(or/c string? #f))))
             #:with name #'n.c))
  (define-splicing-syntax-class maybe-location
    (pattern (~seq #:location (~var loc (expr/c #'source-location?)))
             #:with location #'loc.c)
    (pattern (~seq #:location-syntax term)
             #:with location (stx->loc-expr #'term))))

(define-syntax test
  (syntax-parser
    [(_ (~optional n:maybe-name) (~optional l:maybe-location) body:expr ...)
     (with-syntax ([loc-expr (stx->loc-expr this-syntax)]
                   [proc-expr (syntax/loc this-syntax (lambda () body ... (void)))])
       #'(test* (~? n.name #f) (~? l.location loc-expr) proc-expr))]))

;; ============================================================
;; Check

(begin-for-syntax
  (define-splicing-syntax-class checker-clause
    #:attributes (checker)
    ;; checkers and single-value predicates
    (pattern (~seq #:with cc:expr)
             #:with checker #'(convert-to-checker cc))
    ;; multi-value checkers (may accept single-valued results too)
    (pattern (~seq #:is expected:expr)
             #:with checker #'(checker:equal (catch-values expected)))
    (pattern (~seq #:is-not unexpected:expr)
             #:with checker #'(checker:not-equal (catch-values unexpected)))
    (pattern (~seq #:no-error)
             #:with checker #'(checker:no-error))
    ;; single-value checkers
    (pattern (~seq #:is-true)
             #:with checker #'(checker:is-true))
    ;; raise/error checkers
    (pattern (~seq #:error predicate/regexp:expr)
             #:with checker #'(checker:error predicate/regexp)))

  (define-splicing-syntax-class maybe-forward
    #:attributes ([checker 1] fwd?)
    (pattern (~seq)
             #:with (checker ...) '()
             #:with fwd? #'#f)
    (pattern (~seq #:values)
             #:with (checker ...) (list #'(checker:no-error))
             #:with fwd? #'#t)
    (pattern (~seq #:forward)
             #:with (checker ...) '()
             #:with fwd? #'#t)))

;; Note: evaluation is left-to-right
(define-syntax check
  (syntax-parser
    [(_ actual:expr c:checker-clause ... fwd:maybe-forward)
     #`(let ([info `((location ,#,(stx->loc-expr this-syntax)))])
         (check* fwd.fwd? info actual c.checker ... fwd.checker ...))]))

(define-syntax checker:equal*
  (syntax-parser
    [(_ expected:expr)
     #'(checker:equal (catch-values expected))]))

(define-syntax checker:not-equal*
  (syntax-parser
    [(_ unexpected:expr)
     #'(checker:not-equal (catch-values unexpected))]))

;; ============================================================
;; Run

(define (run-tests proc
                   #:out [out (current-error-port)]
                   #:progress? [progress? #f]
                   #:tell-raco? [tell-raco? #t]
                   #:count-states [count-states '(fail incomplete)])
  (define progress (and progress? (make-terminal-progress)))
  (define listener
    (make-test-listener #:out out
                        #:progress progress
                        #:tell-raco? tell-raco?))
  (parameterize ((current-test-context null)
                 (current-test-listeners (list listener)))
    (call-with-continuation-barrier proc))
  (define ch (listener null 'get-counters))
  (let ([start (hash-ref ch 'start 0)]
        [pass (hash-ref ch 'pass 0)]
        [fail (hash-ref ch 'fail 0)]
        [incomplete (hash-ref ch 'incomplete 0)])
    (when progress (progress #f pass fail))
    (print-summary start pass fail incomplete)
    #;(for/sum ([st (in-list count-states)]) (hash-ref ch st 0))
    (void)))

(define (make-terminal-progress)
  (define terminal-status (make-terminal-status))
  (and terminal-status (make-terminal-progress* terminal-status)))

(define (make-terminal-progress* terminal-status)
  (define progress
    (case-lambda
      [()
       (terminal-status 'retract)
       (void)]
      [(ctx pass fail)
       (define parts
         (if ctx
             (for/list ([frame (in-list (reverse ctx))])
               (define name (or (test-frame-short-name frame) "?"))
               (string->linebuf name (if (test-frame-fail? frame) 'red 'green)))
             (list (string->linebuf "finished"))))
       (define left (linebuf-join (filter linebuf? parts) #:sep (string->linebuf " > ")))
       (define right (linebuf-append (string->linebuf (format "~s" pass) 'green)
                                     (string->linebuf "+")
                                     (string->linebuf (format "~s" fail) 'red)
                                     (string->linebuf " ")))
       (terminal-status 'update left right)]))
  (and terminal-status progress))

(define (print-summary start pass fail incomplete)
  (define (ifnz n label) (if (zero? n) "" (format ", ~s ~a" n label)))
  (write-string
   (string-append (format "~s test(s) run: ~s pass" start pass)
                  (ifnz fail "fail")
                  (ifnz incomplete "incomplete")
                  "\n"))
  (void))
