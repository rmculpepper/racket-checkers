#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/list
         racket/match
         racket/contract/base
         syntax/srcloc
         "private/result.rkt"
         "private/test.rkt"
         "private/check.rkt")
(provide test
         check
         checker?
         skip-test
         (contract-out
          [checker:predicate
           (->* [procedure?] [exact-integer?] checker?)]
          [checker:compare
           (-> (-> any/c any/c any/c) any/c checker?)]
          [checker:error
           (-> (or/c (-> any/c any/c) regexp?) checker?)]
          [run-tests
           (->* [(-> any)]
                [#:trace (or/c exact-nonnegative-integer? +inf.0 #t)
                 #:tell-raco? boolean?
                 #:count-states (listof symbol?)]
                exact-nonnegative-integer?)]))

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
     #`(test* (~? n.name #f) (~? l.location #,(stx->loc-expr this-syntax))
              (lambda () body ... (void)))]))

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
    (pattern (~seq #:match pattern:expr)
             #:with checker #'(checker:predicate
                               (lambda (v) (match v [pattern #t] [_ #f]))
                               #:info `((#:expected "result value matching pattern")
                                        (pattern (quote pattern)))))
    ;; raise/error checkers
    (pattern (~seq #:error predicate/regexp:expr)
             #:with checker #'(checker:error predicate/regexp))
    ))

;; Note: evaluation is left-to-right
(define-syntax check
  (syntax-parser
    [(_ actual:expr c:checker-clause ...)
     #`(let ([info `((#:location ,#,(stx->loc-expr this-syntax)))])
         (check* info actual c.checker ...))]))

;; ============================================================
;; Run

(define (run-tests proc
                   #:trace [level 0]
                   #:tell-raco? [tell-raco? #t]
                   #:count-states [count-states '(fail incomplete)])
  (define listener
    (make-test-listener #:tell-raco? tell-raco?
                        #:trace (case level [(#t) +inf.0] [else level])))
  (parameterize ((current-test-context null)
                 (current-test-listeners (list listener)))
    (call-with-continuation-barrier proc))
  (define ch (listener null 'get-counters))
  (let ([start (hash-ref ch 'start 0)]
        [pass (hash-ref ch 'pass 0)]
        [fail (hash-ref ch 'fail 0)]
        [skip (hash-ref ch 'skip 0)]
        [incomplete (hash-ref ch 'incomplete 0)])
    (print-summary start pass fail skip incomplete)
    (for/sum ([st (in-list count-states)]) (hash-ref ch st 0))))

(define (print-summary start pass fail skip incomplete)
  (define (ifnz n label) (if (zero? n) "" (format ", ~s ~a" n label)))
  (write-string
   (string-append (format "~s test(s) run: ~s pass" start pass)
                  (ifnz skip "skip")
                  (ifnz fail "fail")
                  (ifnz incomplete "incomplete")
                  "\n"))
  (void))
