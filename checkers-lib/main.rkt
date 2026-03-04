#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/list
         racket/match
         racket/contract/base
         racket/struct
         syntax/srcloc
         "private/result.rkt"
         "private/test.rkt"
         "private/check.rkt")
(provide test
         check)

;; This is my bikeshed. There are many like it, but this one is mine.

;; Design:

;; A Test is the unit of testing. A Test
;; - returns (void)
;; - does not catch exceptions or escapes other than check failures and skips
;; - can contain nested tests (ie, no distinction between test-case and test-suite)
;;   - sub-test failure does not abort the enclosing test

;; Tests are implemented using `check` expressions:
;;   (check Expr Checker ...) : Expr[Void]
;; A `check` expression
;; - returns (void), but raises a special value on failure to abort enclosing test
;; - arbitrary contextual information can be attached to checks with `with-check-info`
;; - checks cannot be selectively skipped

;; A Checker verifies properties about the behavior of an expression. A Checker
;; - does not actually control the execution context of the expression
;; - in general, may receive zero or more values or a caught exception

;; TODO:
;; - `current-checker-conversions` for converting other values (eg, expectations)
;;    to checkers
;; - `print-test-summary` print "N tests passed"/"K tests failed, N tests passed"


;; ============================================================
;; Tests

;; A Test is an expression; testing is done by effects.

;; A (test _) expression
;; - catches exceptions (but not continuation escapes, (exit), etc)
;; - is selectively executable: that is, users SHOULD design tests so that
;;   surrounding code does not break if a (test _) form is not evaluated.
;;   For example, the following is bad:
;;     (test .... (open-output-file "the-file.txt") ...)
;;     (delete-file "the-file.txt")
;;   because if the test does not run, the file will not exist and the delete
;;   will fail.

;; TODO:
;; - around-hooks
;;   - built-in around-hook for selective execution
;;   - use around-hook for skipping?
;;   - global (parameter) vs local around-hooks?

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
     #`(test* (~? n.name #f) (~? l.location #,(stx->loc-expr this-syntax)) '()
              (lambda () body ... (void)))]))

;; ============================================================
;; Check

(begin-for-syntax
  (define-splicing-syntax-class checker-clause
    #:attributes (checker)
    ;; checkers and single-value predicates
    (pattern (~seq #:with cc:expr)
             #:with checker #'(convert-to-checker cc))
    ;; only #:is automatically handles multiple values.
    (pattern (~seq #:is expected:expr)
             #:with checker #'(checker:equal (catch-values expected)))
    ;; single-value checkers
    (pattern (~seq #:is-value)
             #:with checker #'(checker:is-value))
    (pattern (~seq #:is-not value:expr)
             #:with checker #'(checker:not-equal value))
    (pattern (~seq #:is-true)
             #:with checker #'(checker:is-true))
    (pattern (~seq #:satisfies predicate:expr)
             #:with checker #'(checker:predicate predicate))
    (pattern (~seq #:match pattern:expr)
             #:with checker #'(checker:predicate
                               (lambda (v) (match v [pattern #t] [_ #f]))
                               #:info `((#:expected "result value matching pattern")
                                        (pattern (quote pattern)))))
    ;; multi-value checkers (may accept single-valued results too)
    (pattern (~seq #:is-values)
             #:with checker #'(checker:is-values))
    (pattern (~seq #:values-satisfy predicate:expr)
             #:with checker #'(checker:values-predicate predicate))
    ;; raise/error checkers
    (pattern (~seq #:error predicate/regexp:expr)
             #:with checker #'(checker:error predicate/regexp))
    ))

(define-syntax check
  (syntax-parser
    [(_ actual:expr c:checker-clause ...)
     #`(let ([info `((#:location ,#,(stx->loc-expr this-syntax)))])
         (check* info actual c.checker ...))]))
