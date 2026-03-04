#lang racket/base
(require racket/match
         racket/struct
         racket/string
         "result.rkt")
(provide (all-defined-out))

;; ============================================================
;; Checkers

;; A Checker is one of
;; - (checker:equal ValuesResult)
;; - (checker:custom (X -> Fault)/#f ArityMask (Xs -> Fault)/#f (Any -> Fault)/#f)
;;   INV: if vcheck, vsmask has bit 1 set; if vsmask != 0, vcheck or vscheck present
(struct checker ())
(struct checker:equal checker (toresult))
(struct checker:custom checker (vcheck vsmask vscheck rcheck info)
  #:reflection-name 'checker)

(define NONE-MASK 0)
(define ONE-MASK #b10)
(define ANY-MASK (bitwise-not NONE-MASK)) ;; -1

(define (checker:predicate pred #:info [info0 #f])
  (define info
    (or info0
        `((#:expected "value satisfying predicate")
          (predicate ,pred))))
  (define (vcheck v) (if (pred v) #f '()))
  (checker:custom vcheck ONE-MASK #f #f info))

(define (checker:compare compare toval)
  (define info
    `((#:expected "value satisfying comparison")
      ("comparison" ,compare)
      ("compare to" ,toval)))
  (define (vcheck v) (if (compare v toval) #f '()))
  (checker:custom vcheck ONE-MASK #f #f info))

(define (checker:values-predicate pred #:info [info0 #f])
  (define info
    (or info0
        `((#:expected "values satisfying predicate")
          (predicate ,pred))))
  (define vsmask (procedure-arity-mask pred))
  (define (vscheck vs) (if (apply pred vs) #f '()))
  (checker:custom #f vsmask vscheck #f info))

(define (checker:error pred/rx)
  (match pred/rx
    [(? procedure? pred)
     (define info
       `((#:expected "raised value satisfying predicate")
         (predicate ,pred)))
     (define (rcheck v) (if (pred v) #f '()))
     (checker:custom #f NONE-MASK #f rcheck info)]
    [(? regexp? rx)
     (define info
       `((#:expected "raised exception with message matching regexp")
         (regexp ,rx)))
     (define (rcheck v)
       (cond [(not (exn? v))
              `((#:failure "raised value is not an exception"))]
             [(not (regexp-match? rx (exn-message v)))
              `((#:failure "exception message does not match regexp"))]
             [else #f]))
     (checker:custom #f NONE-MASK #f rcheck info)]))

(define (checker:is-true)
  (let ([info `((#:expected "any true result value"))]
        [vcheck (lambda (v) (if v #f '()))])
    (checker:custom vcheck ONE-MASK #f #f info)))

(define (checker:is-value)
  (let ([info `((#:expected "any result value"))]
        [vcheck (lambda (v) #f)])
    (checker:custom vcheck ONE-MASK #f #f info)))

(define (checker:is-values)
  (let ([info `((#:expected "any result values"))]
        [vscheck (lambda (vs) #f)])
    (checker:custom vscheck ANY-MASK vscheck #f info)))

;; ----------------------------------------

;; apply-checkers : (Listof Checker) Result -> Fault
(define (apply-checkers cs r)
  (for/or ([c (in-list cs)]) (apply-checker c r)))

;; apply-checker : Checker Result -> Fault
(define (apply-checker c r)
  (match c
    [(checker:equal tr)
     (match r
       [(? list?)
        (cond [(equal? r tr) #f]
              [else `((#:expectvs ,tr)
                      (#:subfail ,(maybe-wrong-arity (length r) (length tr))))])]
       [(? raise-result?)
        `((#:expectvs ,tr)
          (#:subfail "the expression raised an exception"))])]
    [(checker:custom vcheck vsmask vscheck rcheck info)
     (define fault
       (match r
         [(list v)
          #:when vcheck ;; implies (bitwise-bit-set? vsmask 1)
          (vcheck v)]
         [(? list? vs)
          (define vslen (length vs))
          (cond [(bitwise-bit-set? vsmask vslen) (vscheck vs)]
                [else `((#:subfail ,(maybe-wrong-arity/mask vslen vsmask)))])]
         [(raise-result v)
          (cond [rcheck (rcheck v)]
                [else `((#:subfail "the expression raised an exception"))])]))
     (and fault (append fault info))]))

;; maybe-wrong-arity : Nat Nat -> String/#f
(define (maybe-wrong-arity ngot nwanted)
  (and (not (= ngot nwanted))
       (format "wrong number of values: received ~s, expected ~s" ngot nwanted)))

;; maybe-wrong-arity : Nat Integer -> String/#f
(define (maybe-wrong-arity/mask ngot wantedmask)
  (if (zero? wantedmask)
      "the expression did not raise an exception"
      (and (not (bitwise-bit-set? wantedmask ngot))
           (format "wrong number of values: received ~s, expected ~a"
                   ngot (arity-mask->text wantedmask)))))

;; arity-mask->text : Integer -> String
(define (arity-mask->text vsmask)
  (define parts
    (let loop ([n 0] [vsmask vsmask])
      (cond [(= vsmask 0) null]
            [(= vsmask -1) (list (format "at least ~s" n))]
            [(bitwise-bit-set? vsmask 0)
             (cons (number->string n) (loop (add1 n) (arithmetic-shift vsmask -1)))]
            [else (loop (add1 n) (arithmetic-shift vsmask -1))])))
  (string-join parts ", " #:before-last (if (> (length parts) 2) ", or " " or ")))

;; ----------------------------------------

(define (convert-to-checker v)
  (cond [(checker? v) v]
        [((current-checker-converter) v) => values]
        [(and (procedure? v) (procedure-arity-includes? v 1))
         (checker:predicate v)]
        [else (error 'check "could not convert to checker: ~e" v)]))

(define current-checker-converter
  (make-parameter (lambda (v) #f)))

;; ============================================================
;; Info and Faults

;; InfoList = (Listof (List Key Any))
;; where Key = Keyword | Symbol | String; keywords reserved for this library.

;; Fault = #f or InfoList, with the following fault keys:
;; - '#:location  -- added by check/test
;; - '#:actual    -- value, added by check
;; - '#:expected  -- string, from checker
;; - '#:expectvs  -- Result, from checker -- prints as "expected"
;; - 'predicate, 'regexp, etc   -- (non-kw keys) value, from checker
;; - '#:failure   -- string, from checker (omit if obvious)
;; - '#:subfail   -- string, from checker
;; Fault may be constructed with keys in any order, will be sorted by displayer
;; into order above. Order of non-kw keys matters, though.


;; ============================================================
;; Check

;; A check either returns void or raises a `check-failure` instance (not an exn
;; subtype). Unlike rackunit, top-level checks do not get wrapped as test cases.

(define current-info-stack (make-parameter null))

(struct stop-test (info-stack info))
(struct check-failure stop-test ())
(struct skip-test stop-test ())

(define (make-check-failure info-stack info result fault)
  (check-failure info-stack (append info `((#:actual ,result)) fault)))

;; Note: evaluation is left-to-right (except info)
(define-syntax-rule (check* info expr checker ...)
  (let ([r (catch-result expr)])
    (let ([fault (apply-checkers (list checker ...) r)])
      (when fault (raise (make-check-failure (current-info-stack) info r fault)))
      (void))))

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
