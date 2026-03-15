;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/struct
         racket/string
         syntax/srcloc
         "result.rkt")
(provide (all-defined-out))

;; ============================================================
;; Checkers

;; A Checker is one of
;; - (checker:equal ValuesResult)
;; - (checker:custom ArityMask (Xs -> Fault)/#f (Any -> Fault)/#f)
;;   INV: if vsmask != 0, vscheck present
(struct checker ())
(struct checker:equal checker (toresult))
(struct checker:custom checker (vsmask vscheck rcheck info)
  #:reflection-name 'checker)

(define NONE-MASK 0)
(define ONE-MASK #b10)
(define ANY-MASK (bitwise-not NONE-MASK)) ;; -1

(define (checker:not-equal othervs)
  (define vsmask (arithmetic-shift 1 (length othervs)))
  (define info
    `((expnotvs ,othervs)
      #;(vsmask ,vsmask)))
  (define (vscheck vs) (if (equal? vs othervs) '() #f))
  (checker:custom vsmask vscheck #f info))

(define (checker:predicate pred
                           #:arity-mask [arity-mask ANY-MASK]
                           #:property [property #f])
  (define vsmask (bitwise-and arity-mask (procedure-arity-mask pred)))
  (define info
    (cond [(= vsmask ONE-MASK)
           `((expected "value satisfying predicate")
             (predicate ,pred)
             (property ,property))]
          [else
           `((expected "value(s) satifying predicate")
             (predicate ,pred)
             (vsmask ,vsmask)
             (property ,property))]))
  (define (vscheck vs) (if (apply pred vs) #f '()))
  (checker:custom vsmask vscheck #f info))

(define (checker:compare compare toval)
  (define info
    `((expected "value satisfying comparison")
      (comparison ,compare)
      (compare-v ,toval)))
  (define (vscheck vs) (if (compare (car vs) toval) #f '()))
  (checker:custom ONE-MASK vscheck #f info))

(define (checker:error pred/rx)
  (match pred/rx
    [(? procedure? pred)
     (define info
       `((expected "raises exception satisfying predicate")
         (predicate ,pred)))
     (define (rcheck v) (if (pred v) #f '()))
     (checker:custom NONE-MASK #f rcheck info)]
    [(? regexp? rx)
     (define info
       `((expected "raises exception with message matching regexp")
         (regexp ,rx)))
     (define (rcheck v)
       (cond [(not (exn? v))  ;; currently not possible
              `((failure "raised value is not an exception"))]
             [(not (regexp-match? rx (exn-message v)))
              `((failure "exception message does not match regexp"))]
             [else #f]))
     (checker:custom NONE-MASK #f rcheck info)]))

(define (checker:is-true)
  (let ([info `((expected "any true result value"))]
        [vscheck (lambda (vs) (if (car vs) #f '()))])
    (checker:custom ONE-MASK vscheck #f info)))

(define (checker:no-error)
  (let ([info `((expected "does not raise an exception"))]
        [vscheck (lambda (vs) #f)])
    (checker:custom ANY-MASK vscheck #f info)))

;; ----------------------------------------

;; apply-checkers : (Listof Checker) Result -> Fault
(define (apply-checkers cs r)
  (for/or ([c (in-list cs)]) (apply-checker c r)))

;; apply-checker : Checker Result -> Fault
(define (apply-checker c r)
  (match c
    [(checker:equal evs)
     (match r
       [(? list? vs)
        (cond [(equal? vs evs) #f]
              [else `((expectvs ,evs)
                      (prefail ,(maybe-wrong-arity (length vs) (length evs))))])]
       [(? raise-result?)
        `((expectvs ,evs)
          (prefail "the expression raised an exception"))])]
    [(checker:custom vsmask vscheck rcheck info)
     (define fault
       (match r
         [(? list? vs)
          (define vslen (length vs))
          (cond [(bitwise-bit-set? vsmask vslen) (vscheck vs)]
                [else `((prefail ,(maybe-wrong-arity/mask vslen vsmask)))])]
         [(raise-result v)
          (cond [rcheck (rcheck v)]
                [else `((prefail "the expression raised an exception"))])]))
     (and fault (append fault info))]))

;; maybe-wrong-arity : Nat Nat -> String/#f
(define (maybe-wrong-arity ngot nwanted)
  (and (not (= ngot nwanted))
       (format "wrong number of values: got ~s, expected ~s" ngot nwanted)))

;; maybe-wrong-arity : Nat Integer -> String/#f
(define (maybe-wrong-arity/mask ngot wantedmask)
  (if (zero? wantedmask)
      "the expression did not raise an exception"
      (and (not (bitwise-bit-set? wantedmask ngot))
           (format "wrong number of values: got ~s, expected ~a"
                   ngot (arity-mask->text wantedmask)))))

;; arity-mask->text : Integer -> String
(define (arity-mask->text vsmask)
  (define parts
    (let loop ([n 0] [vsmask vsmask])
      (cond [(= vsmask 0) null]
            [(= vsmask -1)
             (list (if (zero? n) "any number" (format "at least ~s" n)))]
            [(bitwise-bit-set? vsmask 0)
             (cons (number->string n) (loop (add1 n) (arithmetic-shift vsmask -1)))]
            [else (loop (add1 n) (arithmetic-shift vsmask -1))])))
  (string-join parts ", " #:before-last (if (> (length parts) 2) ", or " " or ")))

;; ----------------------------------------

(define (convert-to-checker v)
  (cond [(checker? v) v]
        [((current-checker-converter) v) => values]
        [(and (procedure? v) (procedure-arity-includes? v 1))
         (checker:predicate v #:arity-mask ONE-MASK)]
        [else (error 'check "could not convert to checker: ~e" v)]))

(define current-checker-converter
  (make-parameter (lambda (v) #f)))

;; ============================================================
;; Info and Faults

;; InfoList = (Listof (List Key Any))
;; where Key = Symbol | String

;; Fault = #f or InfoList, with the keys listed below. May be constructed
;; with keys in any order, will be displayed into order below.

(define fault-info-keys
  `(;; Set by check:
    (location   "location"      display ,source-location->string)
    (actual     "actual"        value   ,result->print-result)
    ;; ----------------------------------------
    ;; Set by checker, w/o actual result:
    (expected   "expected"      display #f)
    (expectvs   "expected"      value   ,result->print-result) ;; omit 'expected
    (expnotvs   "expect not"    value   ,result->print-result) ;; omit 'expected
    (predicate  "predicate"     value   #f)
    (regexp     "regexp"        value   #f)
    (comparison "comparison"    value   #f)
    (compare-v  "compare to"    value   #f)
    (pattern    "pattern"       write   #f)
    (vsmask     "arity"         display  ,arity-mask->text)
    (property   "property"      display #f)
    ;; ----------------------------------------
    ;; Depends on actual result
    (failure    "failure"       display #f)  ;; why failed
    (prefail    "failure"       display #f)  ;; why inapplicable
    ))

(define (print-fault fault printkv)
  (for ([e (in-list fault-info-keys)])
    (match-define (list key label mode convert) e)
    (match (assoc key fault)
      [(list _ v)
       (when (case mode [(display) v] [else #t])
         (printkv label mode (if convert (convert v) v)))]
      [#f (void)])))

;; ============================================================
;; Check

;; Unlike rackunit, top-level checks do not get wrapped as test cases.

(struct test-signal (info))
(struct check-failure test-signal ())

(define (make-check-failure info result fault)
  (check-failure (append info `((actual ,result)) fault)))

;; Note: evaluation of expr, checkers is left-to-right
(define-syntax-rule (check* fwd? info expr checker ...)
  (let ([r (catch-result expr)])
    (let ([fault (apply-checkers (list checker ...) r)])
      (when fault (raise (make-check-failure info r fault))))
    (if fwd? (reflect-result r) (void))))

;; ----------------------------------------

;; (catch-result Expr) : Expr[Result]
;; FIXME: make catch predicate configurable (don't catch exn:break, though!)
;; FIXME: try to optimize away some with-handlers/call-with-values
(define-syntax-rule (catch-result expr)
  (with-handlers ([exn:fail? raise-result])
    (catch-values expr)))

;; (catch-values Expr) : Expr[ValuesResult]
;; FIXME: try to optimize away some call-with-values
(define-syntax-rule (catch-values expr)
  (call-with-values (lambda () expr) list))
